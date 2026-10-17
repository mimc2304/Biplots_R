# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/logistic_ext.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  Biplot Logístico EXTERNO (Demey, Vicente-Villardón, Galindo & Zambrano 2008,
#  Bioinformatics 24:2832; Vicente-Villardón, Galindo & Blázquez 2006).
#  Es el procedimiento que se usa en clase con MultBiplot:
#    (i)  PCoA (Gower 1966) de la matriz de disimilaridades  delta_ij = 1 - s_ij
#         calculada con un coeficiente de similaridad para datos binarios;
#    (ii) una regresión logística por variable con las coordenadas principales
#         como regresoras:   logit(pi_ij) = b_j0 + sum_s b_js y_is ;
#    (iii) dibujo del biplot filtrando variables (p-valor, Bonferroni, R2);
#    (iv) clusters (Ward) sobre las coordenadas principales.
#  Geometría: el punto de la dirección b_j que predice la probabilidad p es
#     y(p) = (logit(p) - b_j0) / ||b_j||^2 * b_j
#  (p = 0.5 es el "punto gordo"; el segmento 0.5 -> 0.75 indica la dirección
#   de la probabilidad creciente y su longitud es inversa a la capacidad
#   discriminante de la variable).
# =====================================================================

similarity_choices <- c("Simple Matching" = "simple_matching", "Jaccard" = "jaccard",
                        "Dice" = "dice", "Russell y Rao" = "russell_rao",
                        "Rogers y Tanimoto" = "rogers_tanimoto", "Sokal y Sneath" = "sokal_sneath")

# Similaridades entre filas de una matriz binaria. a = presencias comunes,
# d = ausencias comunes, b y c = discordancias.
binary_similarity <- function(X, method = "simple_matching") {
  X <- 1 * (as.matrix(X) > 0); p <- ncol(X); Xn <- 1 - X
  a <- tcrossprod(X); d <- tcrossprod(Xn); b <- X %*% t(Xn); cc <- t(b)
  S <- switch(method,
    simple_matching = (a + d) / p,
    jaccard         = a / (a + b + cc),
    dice            = 2 * a / (2 * a + b + cc),
    russell_rao     = a / p,
    rogers_tanimoto = (a + d) / (a + d + 2 * (b + cc)),
    sokal_sneath    = a / (a + 2 * (b + cc)),
    stop("Coeficiente de similaridad desconocido: ", method))
  S[is.nan(S)] <- 1                       # 0/0: dos filas vacías se consideran idénticas
  dimnames(S) <- list(rownames(X), rownames(X))
  S
}

# PCoA clásico: B = -1/2 J D^2 J ; coordenadas Y = V Lambda^{1/2} (autovalores > 0)
pcoa_scores <- function(D) {
  n <- nrow(D); J <- diag(n) - 1 / n
  Bm <- -0.5 * J %*% (D^2) %*% J
  e <- eigen((Bm + t(Bm)) / 2, symmetric = TRUE)
  pos <- e$values > 1e-8 * max(e$values)
  lam <- e$values[pos]
  Y <- sweep(e$vectors[, pos, drop = FALSE], 2, sqrt(lam), "*")
  dimnames(Y) <- list(rownames(D), paste0("Dim", seq_along(lam)))
  list(Y = Y, eig = lam, neg = sum(e$values[!pos & e$values < -1e-8 * max(e$values)]))
}

# Ajuste del biplot logístico externo
elb_fit <- function(X, k = 2, sim = "simple_matching", sqrt_d = FALSE, progress = NULL) {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X)
  if (!all(X %in% c(0, 1))) stop("El biplot logístico requiere una matriz binaria (0/1).")
  S <- binary_similarity(X, sim)
  D <- 1 - S; diag(D) <- 0               # delta_ij = 1 - s_ij  (distancia de un individuo a sí mismo = 0)
  if (sqrt_d) D <- sqrt(pmax(D, 0))      # opción Gower-Legendre (garantiza configuración euclídea)
  pc <- pcoa_scores(D)
  k <- min(k, ncol(pc$Y))
  Y <- pc$Y[, seq_len(k), drop = FALSE]
  B <- matrix(NA_real_, p, k, dimnames = list(colnames(X), colnames(Y))); b0 <- numeric(p)
  names(b0) <- colnames(X); sep <- logical(p)
  for (j in seq_len(p)) {
    Z <- cbind(1, Y)
    g <- suppressWarnings(stats::glm.fit(Z, X[, j], family = stats::binomial(), control = list(maxit = 100)))
    pr <- g$fitted.values
    if (!g$converged || any(pr < 1e-8) || any(pr > 1 - 1e-8)) {   # separación: ridge mínimo
      cf <- .irls(X[, j], Z, lambda = 1e-3, pen = c(0, rep(1, k)), maxit = 100); sep[j] <- TRUE
    } else cf <- g$coefficients
    b0[j] <- cf[1]; B[j, ] <- cf[-1]
    if (!is.null(progress)) progress(j / p)
  }
  list(A = Y, B = B, b0 = b0, X = X, k = k, type = "external", sim = sim, sqrt_d = sqrt_d,
       Yfull = pc$Y, eig = pc$eig, sep = sep, iterations = 1, dev_hist = NA,
       deviance = logistic_deviance(X, b0, Y, B))
}

# Tabla de autovalores del PCoA (valores propios y absorción de inercia)
elb_eigen_table <- function(fit, nmax = 10) {
  e <- fit$eig; tot <- sum(e); m <- min(nmax, length(e))
  data.frame(Eje = paste0("Eje ", seq_len(m)), `Valor propio` = e[seq_len(m)],
             `Varianza explicada (%)` = 100 * e[seq_len(m)] / tot,
             `Acumulada (%)` = 100 * cumsum(e)[seq_len(m)] / tot, check.names = FALSE)
}

# Bondad de ajuste por variable (como MultBiplot: devianza, p-valor, R2, % correcto)
elb_stats <- function(fit, alpha = 0.05, bonferroni = FALSE, r2min = 0, digits = 3) {
  X <- fit$X; n <- nrow(X); p <- ncol(X); k <- fit$k
  P <- plogis(sweep(fit$A %*% t(fit$B), 2, fit$b0, "+"))
  P <- pmin(pmax(P, 1e-12), 1 - 1e-12)
  out <- lapply(seq_len(p), function(j) {
    y <- X[, j]; pm <- min(max(mean(y), 1e-12), 1 - 1e-12); yh <- as.numeric(P[, j] > 0.5)
    dev  <- -2 * sum(y * log(P[, j]) + (1 - y) * log(1 - P[, j]))
    dev0 <- -2 * sum(y * log(pm) + (1 - y) * log(1 - pm))
    cs <- 1 - exp((dev - dev0) / n)
    data.frame(Variable = colnames(X)[j],
               `Intercepto b0` = fit$b0[j],
               Devianza = dev, `Chi2 (RV)` = dev0 - dev,
               `p-valor` = stats::pchisq(dev0 - dev, df = k, lower.tail = FALSE),
               `R2 Cox-Snell` = cs, `R2 Nagelkerke` = cs / (1 - exp(-dev0 / n)),
               `R2 McFadden` = 1 - dev / dev0,
               `% Correcto` = 100 * mean(yh == y),
               `Sensibilidad (%)` = if (sum(y == 1) > 0) 100 * mean(yh[y == 1] == 1) else NA,
               `Especificidad (%)` = if (sum(y == 0) > 0) 100 * mean(yh[y == 0] == 0) else NA,
               `Separación` = if (isTRUE(fit$sep[j])) "sí" else "no", check.names = FALSE)
  })
  out <- do.call(rbind, out)
  thr <- if (bonferroni) alpha / p else alpha
  out$`Dibujada` <- ifelse(out$`p-valor` <= thr & out$`R2 Nagelkerke` >= r2min, "sí", "no")
  num <- vapply(out, is.numeric, logical(1)); out[num] <- round(out[num], digits)
  out$`p-valor` <- signif(out$`p-valor`, 3)
  out
}

elb_select <- function(fit, alpha = 0.05, bonferroni = FALSE, r2min = 0)
  elb_stats(fit, alpha, bonferroni, r2min, digits = 6)$Dibujada == "sí"

# Individuos: coordenadas, contribución absoluta (CA) y calidad de representación (QR)
# QR = (distancia^2 en el subespacio) / (distancia^2 en todo el espacio PCoA)
elb_rows_table <- function(fit, digits = 3) {
  Yf <- fit$Yfull; k <- fit$k; Y <- Yf[, seq_len(k), drop = FALSE]
  tot <- pmax(rowSums(Yf^2), 1e-12)
  ca <- 100 * sweep(Y^2, 2, colSums(Yf^2)[seq_len(k)], "/")
  qr <- 100 * Y^2 / tot
  out <- data.frame(Individuo = if (is.null(rownames(Y))) seq_len(nrow(Y)) else rownames(Y), check.names = FALSE)
  for (s in seq_len(k)) out[[paste0("Coord", s)]] <- Y[, s]
  for (s in seq_len(k)) out[[paste0("CA", s, " (%)")]] <- ca[, s]
  for (s in seq_len(k)) out[[paste0("QR", s, " (%)")]] <- qr[, s]
  out[["QR plano (%)"]] <- 100 * rowSums(Y^2) / tot
  num <- vapply(out, is.numeric, logical(1)); out[num] <- round(out[num], digits)
  out
}

# Clusters jerárquicos (Ward) sobre las coordenadas principales retenidas
elb_cluster <- function(fit, ncl = 3) {
  Y <- fit$A; ncl <- max(2, min(ncl, nrow(Y) - 1))
  hc <- stats::hclust(stats::dist(Y), "ward.D2")
  cl <- factor(paste0("Cluster ", stats::cutree(hc, ncl)))
  Yf <- fit$Yfull
  cen <- do.call(rbind, lapply(levels(cl), function(l) colMeans(Yf[cl == l, , drop = FALSE])))
  cen_k <- cen[, seq_len(fit$k), drop = FALSE]
  tab <- data.frame(Cluster = levels(cl), n = as.integer(table(cl)),
                    round(cen_k, 3),
                    `Calidad (%)` = round(100 * rowSums(cen_k^2) / pmax(rowSums(cen^2), 1e-12), 2),
                    check.names = FALSE)
  names(tab)[3:(2 + fit$k)] <- paste0("Centroide", seq_len(fit$k))
  list(cl = cl, tabla = tab, hc = hc, centroides = cen_k)
}
