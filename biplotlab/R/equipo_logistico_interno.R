# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/logistic.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  Biplot logístico binario  (Vicente-Villardón, Galindo & Blázquez-Zaballos 2006)
#     logit P(x_ij = 1) = b0_j + a_i' b_j
#  Estimación por regresiones logísticas alternadas (IRLS con un pequeño
#  término ridge para evitar la separación perfecta): dadas las filas A
#  se ajusta una regresión por variable; dadas las columnas B (y b0) se
#  ajusta una regresión por individuo.
# =====================================================================

.irls <- function(y, Z, offset = 0, lambda = 1e-2, pen = NULL, beta = NULL, maxit = 8) {
  m <- ncol(Z)
  if (is.null(pen)) pen <- rep(1, m)
  P <- diag(lambda * pen, m)
  if (is.null(beta)) beta <- rep(0, m)
  offset <- rep(offset, length.out = length(y))
  for (it in seq_len(maxit)) {
    eta <- as.numeric(Z %*% beta) + offset
    mu  <- plogis(eta)
    w   <- pmax(mu * (1 - mu), 1e-5)
    z   <- (eta - offset) + (y - mu) / w
    bn  <- tryCatch(solve(crossprod(Z, w * Z) + P, crossprod(Z, w * z)), error = function(e) beta)
    bn  <- as.numeric(bn)
    if (max(abs(bn - beta)) < 1e-6) { beta <- bn; break }
    beta <- bn
  }
  beta
}

logistic_deviance <- function(X, b0, A, B) {
  eta <- sweep(A %*% t(B), 2, b0, "+")
  P <- plogis(eta)
  P <- pmin(pmax(P, 1e-10), 1 - 1e-10)
  -2 * sum(X * log(P) + (1 - X) * log(1 - P))
}

logistic_biplot <- function(X, k = 2, maxit = 100, tol = 1e-5, lambda = 0.1, progress = NULL) {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X)
  if (!all(X %in% c(0, 1))) stop("El biplot logístico requiere una matriz binaria (0/1).")
  k <- min(k, p - 1, n - 1)
  # inicialización: SVD de la matriz binaria centrada
  Xc <- scale(X, scale = FALSE)
  s <- svd(Xc, nu = k, nv = k)
  A <- scale(s$u[, seq_len(k), drop = FALSE]); A[is.na(A)] <- 0
  B <- matrix(0, p, k); b0 <- qlogis(pmin(pmax(colMeans(X), 0.02), 0.98))
  dev_old <- Inf; hist <- numeric(0)
  for (it in seq_len(maxit)) {
    for (j in seq_len(p)) {
      cf <- .irls(X[, j], cbind(1, A), lambda = lambda, pen = c(0, rep(1, k)),
                  beta = c(b0[j], B[j, ]))
      b0[j] <- cf[1]; B[j, ] <- cf[-1]
    }
    for (i in seq_len(n)) A[i, ] <- .irls(X[i, ], B, offset = b0, lambda = lambda, beta = A[i, ])
    dev <- logistic_deviance(X, b0, A, B)
    hist <- c(hist, dev)
    if (!is.null(progress)) progress(it / maxit)
    if (abs(dev_old - dev) / (abs(dev) + 1e-8) < tol) break
    dev_old <- dev
  }
  # reparametrización en ejes principales (identificabilidad): A B' = U D V'
  Th <- A %*% t(B)
  cm <- colMeans(Th); b0 <- b0 + cm
  Thc <- sweep(Th, 2, cm)
  sv <- svd(Thc, nu = k, nv = k)
  A <- sv$u[, seq_len(k), drop = FALSE] * sqrt(n - 1)
  B <- sv$v[, seq_len(k), drop = FALSE] %*% diag(sv$d[seq_len(k)], k) / sqrt(n - 1)
  dimnames(A) <- list(rownames(X), paste0("Dim", seq_len(k)))
  dimnames(B) <- list(colnames(X), paste0("Dim", seq_len(k)))
  names(b0) <- colnames(X)
  list(A = A, B = B, b0 = b0, X = X, k = k, dev_hist = hist,
       deviance = logistic_deviance(X, b0, A, B), iterations = length(hist))
}

# Estadísticos por variable: CCR, sensibilidad, especificidad, R2 (Cox-Snell,
# Nagelkerke, McFadden) y devianza
logistic_stats <- function(fit, digits = 3) {
  X <- fit$X; n <- nrow(X)
  P <- plogis(sweep(fit$A %*% t(fit$B), 2, fit$b0, "+"))
  out <- lapply(seq_len(ncol(X)), function(j) {
    y <- X[, j]; pj <- pmin(pmax(P[, j], 1e-10), 1 - 1e-10)
    yh <- as.numeric(pj > 0.5)
    pm <- mean(y); pm <- min(max(pm, 1e-10), 1 - 1e-10)
    dev  <- -2 * sum(y * log(pj) + (1 - y) * log(1 - pj))
    dev0 <- -2 * sum(y * log(pm) + (1 - y) * log(1 - pm))
    cs <- 1 - exp((dev - dev0) / n)
    data.frame(Variable = colnames(X)[j],
               `Intercepto b0` = fit$b0[j],
               `CCR (%)` = 100 * mean(yh == y),
               `Sensibilidad (%)` = if (sum(y == 1) > 0) 100 * mean(yh[y == 1] == 1) else NA,
               `Especificidad (%)` = if (sum(y == 0) > 0) 100 * mean(yh[y == 0] == 0) else NA,
               `R2 Cox-Snell` = cs,
               `R2 Nagelkerke` = cs / (1 - exp(-dev0 / n)),
               `R2 McFadden` = 1 - dev / dev0,
               Devianza = dev, check.names = FALSE)
  })
  out <- do.call(rbind, out)
  num <- vapply(out, is.numeric, logical(1)); out[num] <- round(out[num], digits)
  out
}

logistic_global <- function(fit) {
  X <- fit$X
  P <- plogis(sweep(fit$A %*% t(fit$B), 2, fit$b0, "+"))
  yh <- (P > 0.5) * 1
  tab <- table(Observado = factor(as.vector(X), c(0, 1)), Predicho = factor(as.vector(yh), c(0, 1)))
  list(tabla = tab, ccr = 100 * sum(diag(tab)) / sum(tab))
}

# Coordenadas para dibujar: variable j -> punto donde P=0.5 y flecha hacia P=prob
logistic_arrows <- function(fit, dims = c(1, 2), prob = 0.75) {
  B <- fit$B[, dims, drop = FALSE]; b0 <- fit$b0
  n2 <- pmax(rowSums(B^2), 1e-12)
  p50 <- -b0 / n2 * B
  pp  <- (qlogis(prob) - b0) / n2 * B
  data.frame(Variable = rownames(B), x0 = p50[, 1], y0 = p50[, 2],
             x1 = pp[, 1], y1 = pp[, 2], check.names = FALSE)
}

# Tablas de coordenadas
logistic_rows_table <- function(fit, digits = 3) {
  out <- data.frame(Individuo = rownames(fit$A), round(fit$A, digits), check.names = FALSE, row.names = NULL)
  out
}
logistic_cols_table <- function(fit, digits = 3) {
  data.frame(Variable = rownames(fit$B), b0 = round(fit$b0, digits), round(fit$B, digits),
             check.names = FALSE, row.names = NULL)
}
