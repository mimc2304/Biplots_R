# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/pca_core.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  Núcleo común: factorización X ~ U D V'  y tablas "de atrás" del biplot
#  Todos los métodos (SVD, DPCA/Disjoint, CenetSVD) devuelven un objeto
#  `fact` con U (n x Q, columnas de norma 1), d (Q), V (p x Q).
#  Así el mismo código dibuja biplots (JK/GH/HJ) y calcula las tablas.
# =====================================================================

# Construye `fact` a partir de unas cargas V: puntuaciones S = X V,
# d = norma de cada puntuación, U = S / d
make_fact <- function(X, V, method = "") {
  V <- as.matrix(V)
  S <- X %*% V
  d <- sqrt(colSums(S^2))
  d[d < 1e-12] <- 1e-12
  U <- sweep(S, 2, d, "/")
  dimnames(V) <- list(colnames(X), paste0("Eje", seq_len(ncol(V))))
  dimnames(U) <- list(rownames(X), paste0("Eje", seq_len(ncol(V))))
  list(U = U, d = d, V = V, S = S, method = method)
}

# Preprocesado global: centrar / estandarizar
prep_matrix <- function(df, center = TRUE, scale = TRUE) {
  X <- as.matrix(df)
  storage.mode(X) <- "double"
  sc <- scale(X, center = center, scale = scale)
  sc[is.na(sc)] <- 0
  attr(sc, "scaled:center") <- NULL
  attr(sc, "scaled:scale") <- NULL
  sc
}

# ---------------- SVD clásica (PCA / HJ-Biplot) ----------------------
svd_fact <- function(X, Q = 3) {
  Q <- min(Q, ncol(X), nrow(X) - 1)
  s <- svd(X, nu = 0, nv = Q)
  make_fact(X, s$v[, seq_len(Q), drop = FALSE], "svd")
}

# ---------------- Coordenadas de los marcadores ----------------------
# type: "JK" (A = UD, B = V), "GH" (A = U, B = VD) o "HJ" (A = UD, B = VD)
biplot_coords <- function(fact, type = "HJ") {
  n <- nrow(fact$U)
  D <- fact$d
  s <- sqrt(max(n - 1, 1))
  U <- fact$U; V <- fact$V
  dB <- if (!is.null(fact$dB)) fact$dB else D          # CDBiplot: Lambda de la descomposición de Z
  if (type == "JK") {
    A <- sweep(U, 2, D, "*"); B <- V
  } else if (type == "GH") {
    A <- U * s; B <- sweep(V, 2, dB, "*") / s
  } else {                                  # HJ
    A <- sweep(U, 2, D, "*"); B <- sweep(V, 2, dB, "*")
  }
  list(rows = A, cols = B)
}

# ---------------- Tablas "de atrás" ----------------------------------
# Autovalores: lambda_q = d_q^2/(n-1); % sobre la inercia total de X
eigen_table <- function(X, fact) {
  n <- nrow(X)
  tot <- sum(X^2)
  dd  <- if (!is.null(fact$dZ)) fact$dZ else fact$d    # CDBiplot: autovalores de Z (devianza entre clases)
  ev  <- dd^2 / (n - 1)
  pct <- 100 * dd^2 / tot
  data.frame(Eje = paste0("Eje ", seq_along(ev)),
             `Valor propio` = ev,
             `Varianza explicada (%)` = pct,
             `Acumulada (%)` = cumsum(pct),
             check.names = FALSE)
}

# Individuos: coordenadas, contribución (CTR, %) y calidad de representación
# (COS2, % de la norma de la fila explicada por el eje)
rows_table <- function(X, fact, type = "HJ", digits = 3, unit = 100) {
  Q <- ncol(fact$V); ul <- if (unit == 1000) " (‰)" else " (%)"
  co <- biplot_coords(fact, type)$rows
  S  <- fact$U * rep(fact$d, each = nrow(fact$U))     # puntuaciones
  ctr  <- unit * sweep(S^2, 2, colSums(S^2), "/")
  cos2 <- unit * S^2 / pmax(rowSums(X^2), 1e-12)
  out <- data.frame(Individuo = rownames(X), check.names = FALSE)
  for (q in seq_len(Q)) out[[paste0("Coord", q)]] <- co[, q]
  for (q in seq_len(Q)) out[[paste0("CA", q, ul)]]  <- ctr[, q]
  for (q in seq_len(Q)) out[[paste0("CR", q, ul)]] <- cos2[, q]
  out[[paste0("Calidad ejes 1-Q", ul)]] <- unit * span_quality(t(X), fact$V)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- round(out[num], digits)
  out
}

# Variables: coordenadas, cargas V, contribución (carga^2 en %) y calidad
# (r^2 entre la variable y la puntuación del eje, en %)
cols_table <- function(X, fact, type = "HJ", digits = 3, unit = 100) {
  Q <- ncol(fact$V); ul <- if (unit == 1000) " (‰)" else " (%)"
  co <- biplot_coords(fact, type)$cols
  ctr <- unit * sweep(fact$V^2, 2, colSums(fact$V^2), "/")
  Xn <- sweep(X, 2, pmax(sqrt(colSums(X^2)), 1e-12), "/")
  r  <- crossprod(Xn, fact$U)                          # correlaciones
  cos2 <- unit * r^2
  out <- data.frame(Variable = colnames(X), check.names = FALSE)
  for (q in seq_len(Q)) out[[paste0("Coord", q)]] <- co[, q]
  for (q in seq_len(Q)) out[[paste0("Carga", q)]]  <- fact$V[, q]
  for (q in seq_len(Q)) out[[paste0("CA", q, ul)]]  <- ctr[, q]
  for (q in seq_len(Q)) out[[paste0("CR", q, ul)]] <- cos2[, q]
  out[[paste0("Calidad ejes 1-Q", ul)]] <- unit * span_quality(X, fact$U)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- round(out[num], digits)
  out
}

# Bondad de ajuste global: ||X - X V V'||^2/||X||^2 (error relativo)
rel_error <- function(X, V) {
  1 - sum((X %*% V)^2) / sum(X^2)
}

# Calidad total: fracción de la norma de cada columna de M que cae en el
# subespacio generado por las columnas de W (proyección ortogonal). Se usa
# la proyección completa porque en DPCA/Cenet los ejes no son ortogonales
# en la métrica de los datos y sumar COS2 por eje podría pasar de 100 %.
span_quality <- function(M, W) {
  Q <- qr.Q(qr(W))
  num <- colSums((crossprod(Q, M))^2)
  num / pmax(colSums(M^2), 1e-12)
}
