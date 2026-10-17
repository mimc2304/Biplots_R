# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/cdpca.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  CDBiplot: Clustering Disjoint HJ-Biplot (Nieto-Librero et al. 2017,
#  Chemosphere 176:389) = CDPCA de Vichi & Saporta (2009, CSDA 53:3194) con
#  marcadores HJ.
#
#  Modelo:  X = U Ybar B' + E,   U (n x P) partición de individuos,
#           B (p x Q) partición DISJUNTA de variables (una variable carga en
#           un solo componente, columnas de norma 1),  Ybar = centroides en el
#           espacio reducido.
#  Objetivo (ec. 1 del paper): min ||X - U Ybar B'||^2, equivalente a maximizar
#  la devianza ENTRE clases del espacio reducido ||U Ybar||^2.
#  ALS: (1) dado U, se reubican las variables (greedy, maximizando la suma de
#  los mayores autovalores de G_Z = Z'Z, Z = U Xbar = matriz de centroides) y
#  se obtienen las cargas B; (2) dado B, k-means en el espacio reducido XB
#  (asignación al centroide más cercano). Si P = n se obtiene el DPCA.
# =====================================================================

# Si k-means deja un cluster vacío (el paper, §2.3.1): se pasa la mitad de los
# individuos más lejanos del cluster más grande al cluster vacío; se repite
# mientras haya vacíos. Así siempre hay exactamente P clusters no vacíos.
.reassign_empty <- function(u, Y, P) {
  # clusters vacíos: se pasa la mitad (los más lejanos) del mayor cluster al vacío
  repeat {
    tb <- tabulate(u, P); emp <- which(tb == 0)
    if (!length(emp)) break
    big <- which.max(tb); idx <- which(u == big)
    cen <- colMeans(Y[idx, , drop = FALSE])
    far <- idx[order(rowSums(sweep(Y[idx, , drop = FALSE], 2, cen)^2), decreasing = TRUE)]
    u[far[seq_len(max(1, floor(length(idx) / 2)))]] <- emp[1]
  }
  u
}

# Z = U Xbar: cada individuo se sustituye por el centroide (en el espacio original)
# de su cluster. Es la matriz sobre la que se hace el HJ-Biplot / la partición de variables.
.centroid_matrix <- function(X, u, P) {
  n <- nrow(X)
  Xbar <- rowsum(X, u) / tabulate(u, P)         # P x p
  Xbar[u, , drop = FALSE]                        # Z = U Xbar (n x p)
}

# Un arranque del ALS.
#   u   vector de pertenencia de individuos (1..P)       [matriz U del paper]
#   cl  vector de pertenencia de variables (1..Q)        [matriz V del paper]
# Cada vuelta tiene dos medios pasos (Vichi-Saporta §4.5; Nieto-Librero §2.3.2):
#   PASO 1 (variables): con U fijo, Z = U Xbar y G_Z = Z'Z; se reubican las variables
#          con el mismo greedy del DPCA (dpca_greedy_one) y se obtienen las cargas B
#          (primer vector de cada bloque de Z = "HJ-Biplot sobre los centroides").
#   PASO 2 (individuos): con B fijo, se proyecta Y = XB y se asigna cada individuo al
#          centroide más cercano EN EL ESPACIO REDUCIDO (un paso de k-means).
# Ambos pasos no empeoran F = ||X - U Ybar B'||^2/||X||^2, por lo que converge.
cdpca_one <- function(X, P, Q, cl0 = NULL, tol = 1e-8, maxit = 50) {
  n <- nrow(X); p <- ncol(X); tot <- sum(X^2); G <- NULL
  u <- stats::kmeans(X, P, nstart = 1)$cluster              # U0: k-means sobre X (Step 1 de la inicialización)
  u <- .reassign_empty(u, X, P)
  cl <- if (is.null(cl0)) .rand_partition(p, Q) else cl0
  Fold <- Inf; hist <- numeric(0)
  for (it in seq_len(maxit)) {
    Z <- .centroid_matrix(X, u, P)
    G <- crossprod(Z)
    cl <- dpca_greedy_one(G, tot, cl, Q)$cl                  # paso variables (con U fijo)
    B <- disjoint_loadings(Z, cl, Q)                          # HJ sobre centroides, 1er vector por bloque
    Y <- X %*% B
    Yc <- rowsum(Y, u) / tabulate(u, P)                       # centroides reducidos
    D2 <- sapply(seq_len(P), function(c) rowSums(sweep(Y, 2, Yc[c, ])^2))
    u_new <- .reassign_empty(max.col(-D2, ties.method = "first"), Y, P)   # paso individuos (k-means)
    Z2 <- .centroid_matrix(X, u_new, P)
    Fnew <- 1 - sum(Z2 %*% B * (Z2 %*% B)) / tot             # 1 - devianza entre / total
    hist <- c(hist, Fnew)
    u <- u_new
    if (abs(Fold - Fnew) < tol) break
    Fold <- Fnew
  }
  Z <- .centroid_matrix(X, u, P); G <- crossprod(Z)
  cl <- dpca_greedy_one(G, tot, cl, Q)$cl
  B <- disjoint_loadings(Z, cl, Q)
  list(u = u, cl = cl, B = B, F = 1 - sum((Z %*% B)^2) / tot, hist = hist)
}

# Función principal: varios arranques, se queda con el de menor F.
#   P = nº de clusters de individuos, Q = nº de componentes disjuntos.
# Con P = n cada individuo es su propio centroide y se recupera el DPCA.
cdpca <- function(X, P, Q, nstart = 10, tol = 1e-8, maxit = 50, seed = NULL, progress = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(X); p <- ncol(X)
  P <- max(2, min(P, n - 1)); Q <- min(Q, p)
  hc <- tryCatch(stats::cutree(stats::hclust(stats::as.dist(1 - abs(stats::cor(X))), "complete"), Q),
                 error = function(e) NULL)
  runs <- vector("list", nstart)
  for (s in seq_len(nstart)) {
    cl0 <- if (s == 1 && !is.null(hc) && length(unique(hc)) == Q) as.integer(hc) else NULL
    runs[[s]] <- cdpca_one(X, P, Q, cl0, tol, maxit)
    if (!is.null(progress)) progress(s / nstart)
  }
  Fs <- vapply(runs, `[[`, numeric(1), "F")
  best <- runs[[which.min(Fs)]]
  finish_cdpca(X, best, P, Q, Fs)
}

# Construye el objeto final. Marcadores HJ del CDBiplot:
#   individuos     A = X B                      (puntuaciones en los componentes disjuntos)
#   variables      B Lambda, Lambda_q = ||Z b_q||  (descomposición de la matriz de centroides Z)
#   autovalores    Lambda_q^2 = devianza ENTRE clases explicada por el componente q
# fact$d (de X) se conserva para las filas; fact$dZ / fact$dB llevan Lambda para
# autovalores y flechas de variables (ver biplot_coords y eigen_table en pca_core.R).
finish_cdpca <- function(X, best, P, Q, Fstarts) {
  u <- best$u; cl <- best$cl; B <- best$B
  Z <- .centroid_matrix(X, u, P)
  dZ <- sqrt(colSums((Z %*% B)^2))                 # Lambda de la descomposición de Z (HJ sobre centroides)
  ord <- order(dZ, decreasing = TRUE)
  B <- B[, ord, drop = FALSE]; dZ <- dZ[ord]; cl <- match(cl, ord)
  colnames(B) <- paste0("Eje", seq_len(Q))
  fact <- make_fact(X, B, "cdpca")
  fact$dZ <- dZ                                    # Lambda_q de Z: escala de los marcadores de variables y autovalores
  fact$dB <- dZ
  u <- factor(paste0("Cluster ", u), levels = paste0("Cluster ", seq_len(P)))
  Y <- fact$S; Yc <- rowsum(Y, as.integer(u)) / tabulate(as.integer(u), P)
  rownames(Yc) <- levels(u); colnames(Yc) <- colnames(B)
  tot <- sum(X^2); between <- sum(dZ^2)
  s1 <- svd(X, nu = 0, nv = Q)
  list(fact = fact, cl = cl, u = u, centroids = Yc, P = P, Q = Q, F = 1 - between / tot,
       F_pca = 1 - sum(s1$d[seq_len(Q)]^2) / tot, between = 100 * between / tot,
       hist = best$hist, Fstarts = Fstarts, algo = "cdpca")
}

# Tabla de clusters de individuos (pestaña "Clusters"): tamaño, centroides y devianza intra
cluster_table <- function(X, res, digits = 3) {
  u <- res$u; Y <- res$fact$S
  within <- vapply(levels(u), function(l) sum(sweep(Y[u == l, , drop = FALSE], 2, res$centroids[l, ])^2), 0)
  data.frame(Cluster = levels(u), n = as.integer(table(u)),
             round(res$centroids, digits), `Devianza intra` = round(within, digits),
             check.names = FALSE, row.names = NULL)
}
