# =============================================================================
# Código del compañero (app BiplotLab de DPCA, Disjoint, Cenet y Logístico),
# archivo original R/dpca.R. Se integra sin cambios en los cálculos.
# =============================================================================

# =====================================================================
#  DPCA: Análisis de Componentes Principales Disjuntos
#  - Greedy (Vichi & Saporta 2009; Ferrara et al. 2016; Ramirez-Figueroa
#    et al. 2021, Sección 2.2): reasigna variables de una en una.
#  - CBPSO-PCA (Ramirez-Figueroa et al. 2021, Sección 3): PSO binario con
#    restricciones sobre la matriz de partición V.
#
#  MODELO
#  ------
#      min ||X - A B'||^2 / ||X||^2     con  A = X B
#  B (p x Q) es la matriz de cargas con dos restricciones:
#    (a) soportes DISJUNTOS: cada variable (fila de B) tiene un solo elemento
#        distinto de cero, es decir, cada variable pertenece a un único componente;
#    (b) cada columna tiene norma 1.
#  Por (a) las columnas de B no comparten posiciones no nulas, así que son
#  ORTOGONALES automáticamente: B'B = I.
#
#  TRUCO QUE HACE EL PROBLEMA TRATABLE
#  -----------------------------------
#  Como B'B = I:   ||X - X B B'||^2 = ||X||^2 - sum_q ||X b_q||^2.
#  Minimizar el error equivale a maximizar la varianza que capturan los Q
#  componentes.  Y para un bloque fijo de variables idx, la mejor b_q (norma 1,
#  soportada en idx) es el primer autovector de G[idx,idx], con G = X'X, y
#      ||X b_q||^2 = mayor autovalor de G[idx,idx].
#  Entonces TODO el problema se reduce a elegir la PARTICIÓN de las variables
#  (un problema combinatorio); las cargas salen de autovectores.
#
#  ALGORITMOS
#  ----------
#   dpca_greedy  reubicación variable a variable (Vichi & Saporta 2009) con varios arranques
#   dpca_pso     CBPSO-PCA: enjambre binario (Ramirez-Figueroa et al.)
#   finish_dpca  ordena componentes por varianza y construye el objeto `fact`
#   partition_table  tabla variable -> componente -> carga
#   (El CDBiplot, que además agrupa individuos, está en cdpca.R y reutiliza estas funciones)
# =====================================================================

# Valor de un bloque = varianza que captura su primer componente
# = mayor autovalor de G[idx, idx].  Bloque vacío vale 0; bloque de una variable
# vale su suma de cuadrados G[j,j] (la carga es 1 en esa variable).
.block_val <- function(G, idx) {
  if (length(idx) == 0) return(0)
  if (length(idx) == 1) return(G[idx, idx])
  max(eigen(G[idx, idx, drop = FALSE], symmetric = TRUE, only.values = TRUE)$values)
}

# Cargas B dado el vector de pertenencia cl (cl[j] = componente de la variable j):
# en cada bloque, 1er autovector de G[idx,idx], con signo elegido para que la suma
# de cargas sea positiva (los autovectores están definidos salvo signo).
disjoint_loadings <- function(X, cl, Q) {
  p <- ncol(X)
  B <- matrix(0, p, Q)
  G <- crossprod(X)
  for (q in seq_len(Q)) {
    idx <- which(cl == q)
    if (length(idx) == 1) {
      B[idx, q] <- 1
    } else if (length(idx) > 1) {
      ev <- eigen(G[idx, idx, drop = FALSE], symmetric = TRUE)
      v <- ev$vectors[, 1]
      if (sum(v) < 0) v <- -v
      B[idx, q] <- v
    }
  }
  B
}

# Partición aleatoria de p variables en Q grupos sin ningún grupo vacío.
.rand_partition <- function(p, Q) {
  cl <- sample(seq_len(Q), p, replace = TRUE)
  while (length(unique(cl)) < Q) cl[sample(p, Q)] <- seq_len(Q)   # sin columnas vacías
  cl
}

# ---- Algoritmo greedy -------------------------------------------------
# Un arranque. Recorre las variables; para cada una prueba moverla a cada otro
# componente y ejecuta el movimiento que más aumenta (valor_bloque_origen +
# valor_bloque_destino).  Se repite hasta que F no mejora en un barrido completo.
# F = 1 - (suma de valores de bloque)/||X||^2 baja o se mantiene en cada paso, así que converge,
# pero a un óptimo LOCAL (por eso hay varios arranques en dpca_greedy).
#   G    = X'X (precalculada una vez)      tot = ||X||^2
dpca_greedy_one <- function(G, tot, cl, Q, tol = 1e-8, maxit = 100) {
  p <- length(cl)
  vals <- vapply(seq_len(Q), function(q) .block_val(G, which(cl == q)), numeric(1))
  Fk <- 1 - sum(vals) / tot
  hist <- Fk
  for (it in seq_len(maxit)) {
    for (j in seq_len(p)) {
      a <- cl[j]
      if (sum(cl == a) == 1) next                     # no vaciar un componente (Q debe mantenerse)
      best_gain <- 0; best_q <- a; best_va <- NA; best_vb <- NA
      idx_a_new <- setdiff(which(cl == a), j)
      va_new <- .block_val(G, idx_a_new)
      for (b in setdiff(seq_len(Q), a)) {
        vb_new <- .block_val(G, c(which(cl == b), j))
        gain <- (va_new + vb_new) - (vals[a] + vals[b])
        if (gain > best_gain + 1e-12) { best_gain <- gain; best_q <- b; best_va <- va_new; best_vb <- vb_new }
      }
      if (best_q != a) { cl[j] <- best_q; vals[a] <- best_va; vals[best_q] <- best_vb }
    }
    Fnew <- 1 - sum(vals) / tot
    hist <- c(hist, Fnew)
    if (abs(Fk - Fnew) < tol) { Fk <- Fnew; break }
    Fk <- Fnew
  }
  list(cl = cl, F = Fk, hist = hist)
}

# Greedy con varios arranques.  El primer arranque es una partición jerárquica de
# variables con distancia 1-|r| (variables muy correlacionadas -> mismo componente), un
# buen punto de partida; los demás son aleatorios.  Se devuelve el de menor F.
dpca_greedy <- function(X, Q, nstart = 20, tol = 1e-8, maxit = 100, seed = NULL, progress = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p <- ncol(X); G <- crossprod(X); tot <- sum(X^2)
  starts <- vector("list", nstart)
  # 1er arranque: partición jerárquica de variables (1-|r|); resto: aleatorios
  hc <- tryCatch(cutree(hclust(as.dist(1 - abs(cor(X))), "complete"), Q), error = function(e) NULL)
  for (s in seq_len(nstart)) {
    cl0 <- if (s == 1 && !is.null(hc) && length(unique(hc)) == Q) as.integer(hc) else .rand_partition(p, Q)
    starts[[s]] <- dpca_greedy_one(G, tot, cl0, Q, tol, maxit)
    if (!is.null(progress)) progress(s / nstart)
  }
  Fs <- vapply(starts, `[[`, numeric(1), "F")
  best <- starts[[which.min(Fs)]]
  finish_dpca(X, best$cl, Q, best$F, best$hist, Fs, "greedy")
}

# ---- CBPSO-PCA --------------------------------------------------------
# Cada partícula es una partición (matriz p x Q binaria). Sus "posiciones continuas"
# se mueven con velocidades PSO y el operador O devuelve a una partición válida.
# Operador O: matriz real -> partición binaria (mayor |valor| por fila;
# si una columna queda vacía se le asigna la fila con mayor |valor| en ella
# entre las que provienen de columnas con más de un elemento).
.op_O <- function(M) {
  p <- nrow(M); Q <- ncol(M)
  cl <- max.col(abs(M), ties.method = "first")
  for (q in seq_len(Q)) {
    if (!any(cl == q)) {
      cand <- which(tabulate(cl, Q)[cl] > 1)
      if (length(cand) == 0) break
      j <- cand[which.max(abs(M[cand, q]))]
      cl[j] <- q
    }
  }
  cl
}

# Parámetros: P partículas, nIter iteraciones, inercia que decrece de maxIner a minIner,
# wCog / wSoc = pesos cognitivo (mejor propio) y social (mejor global).
# L(z) = 2 sigmoide(z) - 1 lleva cualquier real a (-1,1) para mantener acotadas
# velocidades y posiciones.  F de cada partícula = error relativo del DPCA para su partición.
dpca_pso <- function(X, Q, P = 30, nIter = 30, maxIner = 0.9, minIner = 0.4,
                     wCog = 1.5, wSoc = 1.5, seed = NULL, progress = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p <- ncol(X); G <- crossprod(X); tot <- sum(X^2)
  evalF <- function(cl) 1 - sum(vapply(seq_len(Q), function(q) .block_val(G, which(cl == q)), numeric(1))) / tot
  L <- function(z) 2 / (1 + exp(-z)) - 1
  Bof <- function(cl) disjoint_loadings(X, cl, Q)
  pos <- lapply(seq_len(P), function(i) .rand_partition(p, Q))
  Bp  <- lapply(pos, Bof)
  Fp  <- vapply(pos, evalF, numeric(1))
  vel <- lapply(seq_len(P), function(i) matrix(runif(p * Q, -1, 1), p, Q))
  Bloc <- Bp; Floc <- Fp; clloc <- pos
  g <- which.min(Fp); Bglob <- Bp[[g]]; Fglob <- Fp[g]; clglob <- pos[[g]]
  hist <- Fglob
  for (k in seq_len(nIter)) {
    inertia <- maxIner - (maxIner - minIner) / nIter * k
    for (i in seq_len(P)) {
      r1 <- runif(1); r2 <- runif(1)
      # velocidad = inercia*v + atracción a la mejor posición propia + atracción a la mejor global
      newVel <- inertia * vel[[i]] + r1 * wCog * (Bloc[[i]] - Bp[[i]]) + r2 * wSoc * (Bglob - Bp[[i]])
      vel[[i]] <- L(newVel)
      Vtemp <- L(Bp[[i]] + vel[[i]])
      pos[[i]] <- .op_O(Vtemp)            # operador O: vuelve a una partición disjunta válida
      Bp[[i]] <- Bof(pos[[i]]); Fp[i] <- evalF(pos[[i]])
      if (Fp[i] < Floc[i]) { Floc[i] <- Fp[i]; Bloc[[i]] <- Bp[[i]]; clloc[[i]] <- pos[[i]] }
      if (Fp[i] < Fglob)   { Fglob <- Fp[i]; Bglob <- Bp[[i]]; clglob <- pos[[i]] }
    }
    hist <- c(hist, Fglob)
    if (!is.null(progress)) progress(k / nIter)
  }
  finish_dpca(X, clglob, Q, Fglob, hist, Floc, "pso")
}

# ---- cierre común: ordena componentes por varianza y arma el objeto ----
# F_pca = error del PCA SIN restricción disjunta con el mismo Q (cota inferior de F);
# la diferencia F - F_pca es "lo que cuesta" imponer que cada variable cargue en un solo eje.
# `make_fact` calcula puntuaciones S = XB, d = ||S|| y U = S/d para poder dibujar el biplot.
finish_dpca <- function(X, cl, Q, Fval, hist, Fstarts, algo) {
  B <- disjoint_loadings(X, cl, Q)
  S <- X %*% B
  ord <- order(colSums(S^2), decreasing = TRUE)
  B <- B[, ord, drop = FALSE]
  cl <- match(cl, ord)
  colnames(B) <- paste0("Eje", seq_len(Q))
  fact <- make_fact(X, B, paste0("dpca_", algo))
  s1 <- svd(X, nu = 0, nv = Q)
  F_pca <- 1 - sum(s1$d[seq_len(Q)]^2) / sum(X^2)       # cota inferior (PCA sin restricción)
  list(fact = fact, cl = cl, F = Fval, F_pca = F_pca, hist = hist,
       Fstarts = Fstarts, algo = algo, Q = Q)
}

# Tabla de partición: a qué componente pertenece cada variable y con qué carga
partition_table <- function(X, res, digits = 3) {
  B <- res$fact$V
  data.frame(Variable = colnames(X),
             Componente = paste0("Eje ", res$cl),
             Carga = round(B[cbind(seq_len(ncol(X)), res$cl)], digits),
             check.names = FALSE)
}
