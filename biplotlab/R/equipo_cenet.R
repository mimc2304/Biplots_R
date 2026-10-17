# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/cenet.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  CenetSVD / CenetPCA / CenetBiplot
#  González-García, Nieto-Librero & Galindo-Villardón (2021),
#  Adv. Data Anal. Classif. 17:5-19.  DOI 10.1007/s11634-021-00468-1
#
#  IDEA EN UNA FRASE
#  -----------------
#  La SVD clásica da, para cada eje q, el par (u_q, v_q) que maximiza
#  u'Xv.  CenetSVD añade a cada v_q (y opcionalmente a u_q) una restricción
#  elastic-net que fuerza ceros (sparsity) SIN perder la ortogonalidad entre
#  ejes, algo que el Sparse HJ-Biplot en dos etapas no garantiza.
#
#  PROBLEMA QUE SE RESUELVE (para el eje q)
#  ----------------------------------------
#       max_{u,v}  d = u' X v
#       s.a.  v ∈ B_enet(tau) ∩ B_2(1) ∩ V_{q-1}^perp
#             u ∈ B_2(1)  ∩ U_{q-1}^perp          (∩ B_enet(tau_u) si se pide sparsity en u)
#
#  donde
#     B_enet(tau) = { x : (1-alpha)||x||_1 + alpha ||x||_2^2 <= tau }   bola elastic-net
#     B_2(1)      = { x : ||x||_2 <= 1 }                                 bola unidad
#     V_{q-1}^perp = complemento ortogonal de los ejes ya calculados     (=> ortogonalidad)
#
#  Cada uno de esos conjuntos es CONVEXO y cerrado, y existe la proyección
#  euclídea exacta sobre cada uno. Eso permite usar POCS
#  (Projections Onto Convex Sets, Algoritmo 1 del paper): alternar
#     u <- proj_{ conjuntos de u }( X v )
#     v <- proj_{ conjuntos de v }( X' u )
#  hasta que d = u'Xv deja de cambiar.  La proyección sobre la INTERSECCIÓN de
#  varios conjuntos no es la composición de proyecciones, por eso se usa el
#  algoritmo de Dykstra (proj_inter).
#
#  ARCHIVO
#  -------
#   proj_enet      proyección sobre la bola elastic-net (bisección sobre mu)
#   proj_inter     proyección sobre la intersección (Dykstra)
#   tau_range, tau_from_level   traducción "nivel de sparsity" <-> tau
#   cenet_svd      el algoritmo completo (devuelve un objeto `fact` común a la app)
#   cenet_bic      selección de tau por BIC (ec. 9 del paper)
#   cenet_cv_alpha selección de alpha por validación cruzada (ec. 8)
#   sparsity_table resumen de ceros / norma / convergencia por eje
# =====================================================================

# ---------------------------------------------------------------------
# Proyección exacta sobre  C = { x : (1-a)||x||_1 + a||x||^2 <= tau }
#
# Si x ya está dentro, la proyección es x.  Si no, el punto proyectado z
# está en la frontera y por KKT cumple, para un multiplicador mu > 0:
#       z(mu) = softthreshold(x, mu (1-a)) / (1 + 2 mu a)
# (el soft-threshold viene del término L1 y el divisor del término L2/ridge).
# Como g(z(mu)) = (1-a)||z||_1 + a||z||^2 es DECRECIENTE en mu, se busca por
# bisección el mu que hace g(z(mu)) = tau.
# ---------------------------------------------------------------------
proj_enet <- function(x, alpha, tau) {
  g <- function(z) (1 - alpha) * sum(abs(z)) + alpha * sum(z^2)   # "norma" elastic-net
  if (g(x) <= tau) return(x)                                      # ya está en la bola
  zf <- function(mu) sign(x) * pmax(abs(x) - mu * (1 - alpha), 0) / (1 + 2 * mu * alpha)
  lo <- 0; hi <- 1
  while (g(zf(hi)) > tau && hi < 1e12) hi <- hi * 2               # 1) acotar mu por arriba
  for (i in 1:80) {                                               # 2) bisección (80 pasos: precisión máquina)
    mid <- (lo + hi) / 2
    if (g(zf(mid)) > tau) lo <- mid else hi <- mid
  }
  zf(hi)
}

# ---------------------------------------------------------------------
# Dykstra: proyección sobre  C_enet(tau) ∩ B_2(1) ∩ M^perp
#
#   x       vector a proyectar
#   M       matriz cuyas columnas son los ejes previos (para M^perp); NULL en el 1er eje
#   sparse  TRUE: incluir la bola elastic-net; FALSE (u sin sparsity): solo B_2 y M^perp
#
# Dykstra mantiene un "incremento" inc[[i]] por conjunto que corrige el sesgo de
# proyectar cíclicamente (a diferencia de POCS simple, converge a la proyección
# sobre la INTERSECCIÓN y no a un punto cualquiera de ella).
# ---------------------------------------------------------------------
proj_inter <- function(x, alpha, tau, M = NULL, sparse = TRUE, maxit = 150, tol = 1e-9) {
  projs <- list()
  if (sparse) projs[[length(projs) + 1]] <- function(z) proj_enet(z, alpha, tau)   # elastic-net
  projs[[length(projs) + 1]] <- function(z) z / max(1, sqrt(sum(z^2)))             # bola unidad
  if (!is.null(M) && ncol(M) > 0)                                                  # ortogonal a ejes previos
    projs[[length(projs) + 1]] <- function(z) z - as.numeric(M %*% crossprod(M, z))
  k <- length(projs)
  if (k == 1) return(projs[[1]](x))
  z <- x
  inc <- replicate(k, numeric(length(x)), simplify = FALSE)   # incrementos de Dykstra
  for (it in seq_len(maxit)) {
    z_old <- z
    for (i in seq_len(k)) {
      y <- z + inc[[i]]          # se suma el incremento guardado
      z <- projs[[i]](y)         # proyección sobre el conjunto i
      inc[[i]] <- y - z          # y se actualiza lo que "sobró"
    }
    if (sqrt(sum((z - z_old)^2)) < tol * (1 + sqrt(sum(z^2)))) break
  }
  z
}

# ---------------------------------------------------------------------
# Parametrización de tau (paper, Sec. 5.2).
#   tau < 1  ->  la intersección con ||x||_2 = 1 podría quedar vacía
#   tau > (1-a) sqrt(J) + a  ->  la restricción no actúa (SVD clásica)
# Por comodidad la interfaz usa un "nivel" s en [0,1]: s = 1 sin restricción,
# s = 0 un solo coeficiente distinto de cero.
# ---------------------------------------------------------------------
tau_range <- function(p, alpha) c(1, (1 - alpha) * sqrt(p) + alpha)
tau_from_level <- function(s, p, alpha) { r <- tau_range(p, alpha); r[1] + s * (r[2] - r[1]) }

# ---------------------------------------------------------------------
# CenetSVD (Algoritmo 1 del paper aplicado eje por eje)
#
#   X        matriz n x p ya centrada/estandarizada
#   Q        número de ejes
#   alpha    mezcla lasso (0) / ridge (1) del elastic-net
#   level_v  nivel de sparsity en las cargas v (1 = sin restricción)
#   level_u  nivel de sparsity en las puntuaciones u; NULL = u sin sparsity
#   eps      tolerancia sobre el cambio relativo de d = u'Xv
#
# Devuelve un objeto con `fact` (misma estructura que svd_fact: U, d, V, S) para
# poder dibujar JK/GH/HJ y construir las tablas exactamente igual que en el PCA.
# ---------------------------------------------------------------------
cenet_svd <- function(X, Q, alpha = 0.5, level_v = 0.5, level_u = NULL,
                      eps = 1e-7, maxit = 200, progress = NULL) {
  n <- nrow(X); p <- ncol(X)
  Q <- min(Q, p, n - 1)
  tau_v <- tau_from_level(level_v, p, alpha)
  sparse_u <- !is.null(level_u)
  tau_u <- if (sparse_u) tau_from_level(level_u, n, alpha) else Inf
  s0 <- svd(X, nu = Q, nv = Q)             # la SVD ordinaria sirve de punto de partida
  U <- matrix(0, n, 0); V <- matrix(0, p, 0); d <- numeric(0); iters <- integer(0)
  conv <- vector("list", Q)                # historial de d por eje (gráfico de convergencia)
  for (q in seq_len(Q)) {
    v <- s0$v[, q]; u <- s0$u[, q]
    # Se empieza con v ya dentro de las restricciones (elastic-net + ortogonal a V previos)
    v <- proj_inter(v, alpha, tau_v, V, TRUE)
    dq <- sum(u * (X %*% v)); hist <- dq
    for (t in seq_len(maxit)) {            # ---- bucle POCS ----
      # paso u: proyectar X v sobre sus restricciones (B_2, U^perp y, si se pide, elastic-net)
      u <- proj_inter(as.numeric(X %*% v), alpha, tau_u, U, sparse_u)
      # paso v: proyectar X' u sobre B_enet ∩ B_2 ∩ V^perp
      v <- proj_inter(as.numeric(crossprod(X, u)), alpha, tau_v, V, TRUE)
      dn <- sum(u * (X %*% v)); hist <- c(hist, dn)
      if (abs(dn - dq) < eps * max(1, abs(dq))) { dq <- dn; break }   # criterio de parada
      dq <- dn
    }
    # No hay "deflación" de X: la ortogonalidad con los ejes anteriores
    # la imponen directamente las proyecciones sobre U^perp y V^perp.
    U <- cbind(U, u); V <- cbind(V, v); d <- c(d, dq); iters <- c(iters, t)
    conv[[q]] <- hist
    if (!is.null(progress)) progress(q / Q)
  }
  colnames(V) <- colnames(U) <- paste0("Eje", seq_len(ncol(V)))
  # `fact` se reconstruye a partir de las cargas: S = X V, d = ||S||, U = S/d.
  # Los u y d "internos" del algoritmo se guardan aparte (u_enet, d_enet).
  fact <- make_fact(X, V, "cenet")
  fact$u_enet <- U; fact$d_enet <- d
  list(fact = fact, alpha = alpha, tau_v = tau_v, tau_u = tau_u,
       level_v = level_v, level_u = level_u, iters = iters, conv = conv,
       V_svd = s0$v[, seq_len(Q), drop = FALSE])
}

# ---------------------------------------------------------------------
# Selección de tau por BIC (ec. 9):
#   BIC(tau) = ||X - X Ve Ve'||^2 / ||X - X V V'||^2 + df(tau) log(I)/I
# donde Ve = cargas CenetSVD, V = cargas SVD con el mismo Q, y df = nº de
# coeficientes no nulos.  Se calcula en una malla de niveles; se elige el mínimo.
# ---------------------------------------------------------------------
cenet_bic <- function(X, Q, alpha, levels = seq(0.1, 1, length.out = 8), progress = NULL) {
  n <- nrow(X); p <- ncol(X)
  s <- svd(X, nu = 0, nv = Q); V <- s$v[, seq_len(Q), drop = FALSE]
  den <- sum((X - X %*% V %*% t(V))^2)                 # error de la SVD (denominador)
  out <- data.frame(nivel = levels, tau = NA_real_, BIC = NA_real_, df = NA_integer_)
  for (i in seq_along(levels)) {
    fit <- cenet_svd(X, Q, alpha, levels[i])
    Ve <- fit$fact$V
    df <- sum(abs(Ve) > 1e-8)                          # grados de libertad = coeficientes no nulos
    out$tau[i] <- fit$tau_v; out$df[i] <- df
    out$BIC[i] <- sum((X - X %*% Ve %*% t(Ve))^2) / den + df * log(n) / n
    if (!is.null(progress)) progress(i / length(levels))
  }
  out$optimo <- out$BIC == min(out$BIC, na.rm = TRUE)
  out
}

# ---------------------------------------------------------------------
# Selección de alpha por validación cruzada (ec. 8):
# para cada alpha y cada fold se ajusta CenetSVD en entrenamiento y se mide el
# error de reconstrucción  ||Xte - Xte Ve Ve'||^2 / (nº de celdas)  en test.
# ---------------------------------------------------------------------
cenet_cv_alpha <- function(X, Q, level_v, alphas = c(0, 0.25, 0.5, 0.75, 0.9),
                           folds = 5, seed = 1, progress = NULL) {
  set.seed(seed)
  n <- nrow(X)
  fold <- sample(rep(seq_len(folds), length.out = n))   # asignación aleatoria de filas a folds
  res <- data.frame(alpha = alphas, MSE = NA_real_)
  tot <- length(alphas) * folds; k <- 0
  for (a in seq_along(alphas)) {
    mse <- numeric(folds)
    for (f in seq_len(folds)) {
      Xtr <- X[fold != f, , drop = FALSE]; Xte <- X[fold == f, , drop = FALSE]
      fit <- cenet_svd(Xtr, Q, alphas[a], level_v)
      Ve <- fit$fact$V
      mse[f] <- sum((Xte - Xte %*% Ve %*% t(Ve))^2) / length(Xte)
      k <- k + 1
      if (!is.null(progress)) progress(k / tot)
    }
    res$MSE[a] <- mean(mse)
  }
  res$optimo <- res$MSE == min(res$MSE)
  res
}

# Resumen de sparsity por eje.  "Norma v" < 1 es normal con elastic-net: la parte
# ridge encoge el vector (la bola elastic-net corta antes que la bola unidad).
sparsity_table <- function(fit) {
  V <- fit$fact$V
  data.frame(Eje = colnames(V),
             `Coef. no nulos` = colSums(abs(V) > 1e-8),
             `Coef. nulos` = colSums(abs(V) <= 1e-8),
             `% ceros` = round(100 * colMeans(abs(V) <= 1e-8), 1),
             `Norma v` = round(sqrt(colSums(V^2)), 3),
             `d = u'Xv` = round(fit$fact$d_enet, 3),
             Iteraciones = fit$iters,
             check.names = FALSE)
}
