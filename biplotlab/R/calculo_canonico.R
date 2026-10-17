# =============================================================================
# BIPLOT CANÓNICO / MANOVA BIPLOT: cálculo (una vía, dos factores y bootstrap)
# Integrado en BiplotLab desde la aplicación del Biplot canónico del grupo
# (versión 2.1, con la corrección de escala de las elipses del 7/10/2026).
# El cálculo se conserva sin cambios; solo se añadieron las marcas @export.
# =============================================================================

# =============================================================================
#  BIPLOT CANONICO / MANOVA BIPLOT  -  version independiente (extraida de BiGaRx 0.3.0)
# =============================================================================
#  Autores: Kleber Garzon y Roxana Barrezueta. Licencia GPL (>= 3). Paquete BiGaRx.
#
#  Este archivo contiene TODO lo necesario para el biplot canonico de una via,
#  sin instalar BiGaRx. Basta con:   source("biplot_canonico_manova.R")
#
#  Dependencias: solo R base (stats, graphics, grDevices, utils).
#  Opcionales:   ggplot2 (para autoplot) y MASS (solo para validar).
#
#  Funciones principales (publicas):
#    bigarx_canonical(X, groups, ...)  Ajusta el biplot canonico / MANOVA biplot
#    summary(cb), print(cb)            Resultados: MANOVA, inercia, calidades
#    plot(cb), autoplot(cb)            Grafico base o ggplot2
#    canonical_regions(cb)             Circulos o elipses de confianza (tabla)
#    manova_tests(cb)                  Wilks, Pillai, Hotelling-Lawley y Roy
#    univariate_anovas(cb)             ANOVA de una via por variable
#    predict(cb, nuevos)               Proyecta individuos nuevos
#    canonical_tidy(cb)                Coordenadas en formato largo
#    impute_svd(X)                     Imputacion de NA por SVD iterativa
#
#  Referencias:
#    Amaro, Vicente-Villardon y Galindo (2004). Interciencia 29(1), 26-32.
#    Amaro, Vicente-Villardon y Galindo (2008). Rev. Investigacion Operacional 29(3), 231-241.
#    Gabriel (1972, 1995); Krzanowski (1989); Gabriel y Zamir (1979).
#
#  Validado: valores propios y Wilks iguales a stats::manova(); individuos iguales
#  a MASS::lda(); elipses reproducen las tablas 4-7 de Amaro et al. (2008);
#  geometria identica a MultBiplotR::CanonicalBiplot (escala W = E/(n-K)).
# =============================================================================

.transforms <- c("standardize", "center", "double", "grand", "none")

# Prepara la matriz numérica y valida la entrada.
.as_data_matrix <- function(X, arg = "X", allow_na = FALSE) {
  if (is.data.frame(X)) {
    num <- vapply(X, is.numeric, logical(1))
    if (!all(num)) {
      stop(sprintf(
        "`%s` tiene columnas no num\u00E9ricas: %s. Qu\u00EDtalas o p\u00E1salas como `groups`.",
        arg, paste(names(X)[!num], collapse = ", ")), call. = FALSE)
    }
    X <- as.matrix(X)
  }
  if (!is.matrix(X) || !is.numeric(X)) {
    stop(sprintf("`%s` debe ser una matriz o un data.frame num\u00E9rico.", arg),
         call. = FALSE)
  }
  if (anyNA(X) && !allow_na) {
    stop(sprintf(paste0("`%s` contiene valores NA. Usa `na_action = \"impute\"` para ",
                        "imputarlos con SVD iterativa, o elim\u00EDnalos antes del an\u00E1lisis."), arg),
         call. = FALSE)
  }
  if (is.null(rownames(X))) rownames(X) <- paste0("F", seq_len(nrow(X)))
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))
  X
}

# Aplica la transformación a la matriz activa y guarda los parámetros
# necesarios para transformar después filas o columnas suplementarias.
.preprocess <- function(X, transform) {
  transform <- match.arg(transform, .transforms)
  n <- nrow(X)
  info <- list(transform = transform, col_center = NULL, col_scale = NULL,
               row_center = NULL, grand = NULL)
  Z <- X
  if (transform %in% c("standardize", "center")) {
    m <- colMeans(X)
    Z <- sweep(X, 2, m, "-")
    info$col_center <- m
    if (transform == "standardize") {
      s <- sqrt(colSums(Z^2) / (n - 1))
      if (any(s == 0)) {
        stop("Hay columnas constantes (desviaci\u00F3n t\u00EDpica 0): ",
             paste(colnames(X)[s == 0], collapse = ", "), call. = FALSE)
      }
      Z <- sweep(Z, 2, s, "/")
      info$col_scale <- s
    }
  } else if (transform == "double") {
    g <- mean(X)
    rm <- rowMeans(X)
    cm <- colMeans(X)
    Z <- X - outer(rm, rep(1, ncol(X))) - outer(rep(1, n), cm) + g
    info$col_center <- cm
    info$row_center <- rm
    info$grand <- g
  } else if (transform == "grand") {
    g <- mean(X)
    Z <- X - g
    info$grand <- g
  }
  list(Z = Z, info = info)
}

# Transforma filas suplementarias con los parámetros de la matriz activa.
.transform_sup_rows <- function(Xs, info) {
  switch(info$transform,
    standardize = sweep(sweep(Xs, 2, info$col_center, "-"), 2, info$col_scale, "/"),
    center = sweep(Xs, 2, info$col_center, "-"),
    grand = Xs - info$grand,
    none = Xs,
    double = {
      # Cada fila suplementaria se centra con su propia media y con las
      # medias de columna de la matriz activa.
      Xs - outer(rowMeans(Xs), rep(1, ncol(Xs))) -
        outer(rep(1, nrow(Xs)), info$col_center) + info$grand
    })
}

# Deshace la transformacion: lleva una matriz del espacio preprocesado a la
# escala original, con los parametros guardados en `info`.
.unpreprocess <- function(Z, info) {
  n <- nrow(Z); p <- ncol(Z)
  switch(info$transform,
    standardize = sweep(sweep(Z, 2, info$col_scale, "*"), 2, info$col_center, "+"),
    center = sweep(Z, 2, info$col_center, "+"),
    grand = Z + info$grand,
    none = Z,
    double = Z + outer(info$row_center, rep(1, p)) +
      outer(rep(1, n), info$col_center) - info$grand)
}

# Imputa datos faltantes con una SVD de rango reducido
#
# Rellena las celdas `NA` con la aproximación de rango bajo de la matriz,
# iterando hasta que los valores imputados se estabilizan. Es el ajuste por
# mínimos cuadrados alternados descrito por Gabriel y Zamir (1979) y
# revisado por Galindo (1986, sección 3.1).
#
# Algoritmo:
# 1. Se rellenan los `NA` con la media de su columna.
# 2. Se aplica la transformación elegida (por ejemplo, estandarizar).
# 3. Se calcula la SVD y la aproximación de rango `rank`.
# 4. Se vuelve a la escala original y se reemplazan solo las celdas
#    faltantes por los valores aproximados.
# 5. Se repiten los pasos 2 a 4 hasta que el cambio relativo de los valores
#    imputados sea menor que `tol`.
#
# Los valores observados nunca se modifican.
#
# @param X Matriz o data.frame numérico con `NA`.
# @param rank Rango de la aproximación (por defecto 2, el plano del biplot).
#   Debe ser menor que el número de filas y de columnas.
# @param transform Transformación usada en cada iteración, como en
#   bigarx_canonical(). Conviene usar la misma del análisis posterior.
# @param maxit Número máximo de iteraciones.
# @param tol Tolerancia para el cambio relativo de los valores imputados.
#
# @return Un objeto de clase `"bigarx_imputation"`: lista con
# \describe{
#   \item{`X`}{La matriz completa.}
#   \item{`missing`}{Matriz de posiciones (fila, columna) imputadas.}
#   \item{`values`}{data.frame con fila, columna y valor imputado.}
#   \item{`iterations`, `converged`, `rank`, `transform`}{Detalles del ajuste.}
# }
#
# @references
# Gabriel, K. R. y Zamir, S. (1979). Lower rank approximation of matrices
# by least squares with any choice of weights. *Technometrics*, 21(4),
# 489-498.
#
# Galindo, M. P. (1986). Una alternativa de representación simultánea:
# HJ-Biplot. *Qüestiió*, 10(1), 13-23.
#
#' @noRd
#' @export
impute_svd <- function(X, rank = 2,
                       transform = c("standardize", "center", "double", "grand", "none"),
                       maxit = 500, tol = 1e-6) {
  X <- .as_data_matrix(X, allow_na = TRUE)
  transform <- match.arg(transform)
  miss <- is.na(X)
  if (!is.numeric(rank) || length(rank) != 1 || rank < 1 ||
      rank >= min(dim(X))) {
    stop(sprintf("`rank` debe ser un entero entre 1 y %d.", min(dim(X)) - 1),
         call. = FALSE)
  }
  rank <- as.integer(rank)
  vacias_col <- colSums(!miss) == 0
  if (any(vacias_col)) {
    stop("Columnas sin ning\u00FAn dato: ", paste(colnames(X)[vacias_col], collapse = ", "),
         call. = FALSE)
  }
  vacias_fil <- rowSums(!miss) == 0
  if (any(vacias_fil)) {
    stop("Filas sin ning\u00FAn dato: ", paste(rownames(X)[vacias_fil], collapse = ", "),
         ". Elim\u00EDnalas antes de imputar.", call. = FALSE)
  }

  Xc <- X
  it <- 0L
  converged <- TRUE
  if (any(miss)) {
    cm <- colMeans(X, na.rm = TRUE)
    Xc[miss] <- cm[col(X)[miss]]
    converged <- FALSE
    for (it in seq_len(maxit)) {
      pp <- .preprocess(Xc, transform)
      s <- svd(pp$Z, nu = rank, nv = rank)
      Zh <- s$u %*% (s$d[seq_len(rank)] * t(s$v))
      Xh <- .unpreprocess(Zh, pp$info)
      old <- Xc[miss]
      new <- Xh[miss]
      Xc[miss] <- new
      change <- sqrt(sum((new - old)^2)) / max(sqrt(sum(old^2)), .Machine$double.eps)
      if (change < tol) {
        converged <- TRUE
        break
      }
    }
    if (!converged) {
      warning(sprintf("La imputaci\u00F3n no convergi\u00F3 en %d iteraciones; aumenta `maxit`.", maxit),
              call. = FALSE)
    }
  }
  pos <- which(miss, arr.ind = TRUE)
  out <- list(
    X = Xc,
    missing = pos,
    values = data.frame(fila = rownames(X)[pos[, 1]],
                        columna = colnames(X)[pos[, 2]],
                        valor = Xc[pos],
                        row.names = NULL),
    iterations = it,
    converged = converged,
    rank = rank,
    transform = transform
  )
  class(out) <- "bigarx_imputation"
  out
}

#' @noRd
#' @export
print.bigarx_imputation <- function(x, ...) {
  cat(sprintf("Imputaci\u00F3n por SVD de rango %d (transformaci\u00F3n: %s)\n", x$rank, x$transform))
  cat(sprintf("Celdas imputadas: %d | iteraciones: %d | %s\n", nrow(x$missing), x$iterations,
              if (x$converged) "convergi\u00F3" else "NO convergi\u00F3"))
  if (nrow(x$values) > 0) {
    print(utils::head(x$values, 10), row.names = FALSE)
    if (nrow(x$values) > 10) cat(sprintf("... %d m\u00E1s (ver $values)\n", nrow(x$values) - 10))
  }
  invisible(x)
}

.inertia_table <- function(d) {
  ev <- d^2
  pct <- 100 * ev / sum(ev)
  data.frame(
    eje = paste0("Eje", seq_along(d)),
    valor_singular = d,
    valor_propio = ev,
    inercia = pct,
    acumulada = cumsum(pct),
    row.names = NULL
  )
}

.quality <- function(M) {
  ss <- rowSums(M^2)
  ss[ss == 0] <- NA_real_
  100 * M^2 / ss
}

.contrib <- function(M) {
  100 * sweep(M^2, 2, colSums(M^2), "/")
}

.fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)

.check_axes <- function(x, axes) {
  if (length(axes) != 2 || any(axes > x$ndim) || any(axes < 1)) {
    stop(sprintf("`axes` debe tener dos ejes entre 1 y %d (usa `ndim` mayor al ajustar).",
                 x$ndim), call. = FALSE)
  }
}

# Biplot canónico (MANOVA biplot) de una vía
#
# Representa las diferencias entre grupos definidos de antemano y las
# variables responsables de esas diferencias, con regiones de confianza para
# las medias de cada grupo (Gabriel, 1972, 1995; Vicente-Villardón, 1992;
# Amaro, Vicente-Villardón y Galindo, 2004).
#
# **Cálculo.** Con \eqn{M} la matriz \eqn{K \times p} de medias de grupo
# (datos centrados), \eqn{N = \mathrm{diag}(n_k)} y
# \eqn{W = E/(n-K)} la matriz de covarianzas combinada dentro de los
# grupos, se calcula la descomposición en valores singulares generalizada
# \deqn{N^{1/2} M W^{-1/2} = U D V^T}
# (en la notación de Amaro et al., \eqn{R^{-1/2}\hat{D}E^{-1/2}} con
# \eqn{R = N^{-1}}). Los marcadores son:
# * medias de grupo: \eqn{P = M W^{-1/2} V = N^{-1/2} U D};
# * variables: \eqn{Q = W^{1/2} V};
# * individuos: \eqn{(X - \bar{x}) W^{-1/2} V}.
#
# Se cumple \eqn{P Q^T = M}: el producto escalar de una media y una variable
# reproduce la media del grupo en esa variable. Los valores propios de
# \eqn{HE^{-1}} del MANOVA son \eqn{\theta_k = d_k^2/(n-K)}.
#
# **Escala.** Se usa la covarianza combinada \eqn{W} (y no la matriz de sumas
# de cuadrados \eqn{E}) para que, en el espacio canónico, la varianza dentro
# de los grupos sea 1 en cada eje. Así los radios de confianza se expresan
# directamente en las unidades del gráfico.
#
# **Regiones de confianza** (`region`), centradas en la media de cada grupo:
# * `"chi2"`: radio \eqn{\sqrt{\chi^2_{2,1-\alpha}/n_k}}; región conjunta en el
#   plano (Amaro et al., 2008).
# * `"t"`: radio \eqn{t_{n-K,1-\alpha/2}/\sqrt{n_k}}; su proyección sobre una
#   variable aproxima un intervalo univariante (Amaro et al., 2004).
# * `"bonferroni"`: como `"t"`, con \eqn{\alpha} dividido entre las
#   \eqn{K(K-1)/2} comparaciones por pares.
# * `"ellipse"`: elipses de Amaro, Vicente-Villardón y Galindo (2008), que
#   además de la variabilidad de las medias incorporan la de los propios
#   ejes canónicos, con las varianzas y covarianzas asintóticas de
#   Krzanowski (1989) (ecuaciones 3 y 4 del artículo). Cada elipse cumple
#   \eqn{(z - p_i)^T \Sigma_i^{-1} (z - p_i) = \chi^2_{2,1-\alpha}}. Ver
#   [canonical_regions()] para sus ejes, ángulo y área.
#
# Si las regiones de dos grupos no se tocan, los grupos difieren
# significativamente. La prueba es conservadora: si se tocan, la diferencia
# puede estar en otro eje, y conviene una prueba \eqn{T^2} de Hotelling.
#
# @param X Matriz o data.frame numérico (individuos en filas).
# @param groups Factor con el grupo de cada fila. Se necesitan al menos tres
#   grupos para obtener un plano.
# @param transform `"center"` (por defecto) o `"standardize"`. El
#   análisis canónico no cambia con la escala de las variables; la
#   estandarización solo cambia la longitud de las flechas.
# @param conf Nivel de confianza de las regiones (por defecto 0,95).
# @param region `"chi2"` (por defecto), `"t"` o `"bonferroni"`.
# @param ndim Ejes que se guardan (por defecto 3, o menos si no hay más).
# @param fix_signs Orienta cada eje para que la carga mayor sea positiva.
# @param na_action,impute_rank `"fail"` o `"impute"` (imputa con impute_svd()); rango de la SVD. La imputación no usa los
#   grupos.
#
# @return Un objeto de clase `c("bigarx_canonical", "bigarx")` con
# \describe{
#   \item{`rows`}{Coordenadas de las medias de grupo.}
#   \item{`cols`}{Marcadores de las variables.}
#   \item{`individuals`}{Coordenadas de cada individuo.}
#   \item{`radius`}{Radio de la región de confianza de cada grupo.}
#   \item{`manova`}{Lambda de Wilks, aproximación chi-cuadrado de Bartlett,
#     grados de libertad y valor p.}
#   \item{`canonical`}{Valores propios de \eqn{HE^{-1}} y correlaciones
#     canónicas.}
#   \item{`inertia`, `gof`}{Inercia por eje y bondad de ajuste de las medias
#     en el plano 1-2.}
#   \item{`quality_rows`}{Calidad de representación de cada media (en %).}
#   \item{`quality_cols`}{Calidad de cada variable (en %): correlación al
#     cuadrado, dentro de los grupos, entre la variable y el eje canónico.}
#   \item{`structure`}{Esas mismas correlaciones con signo.}
# }
#
# @references
# Amaro, I. R., Vicente-Villardón, J. L. y Galindo, M. P. (2004). MANOVA
# Biplot para arreglos de tratamientos con dos factores basado en modelos
# lineales generales multivariantes. *Interciencia*, 29(1), 26-32.
#
# Amaro, I. R., Vicente-Villardón, J. L. y Galindo, M. P. (2008).
# Contribuciones al MANOVA-Biplot: regiones de confianza alternativas.
# *Revista Investigación Operacional*, 29(3), 231-241.
#
# Gabriel, K. R. (1972). Analysis of meteorological data by means of
# canonical decomposition and biplots. *Journal of Applied Meteorology*,
# 11, 1071-1077.
#
#' @noRd
#' @export
bigarx_canonical <- function(X, groups, transform = c("center", "standardize"),
                             conf = 0.95,
                             region = c("chi2", "t", "bonferroni", "ellipse", "ellipse_amaro", "boot"),
                             ndim = 3, fix_signs = TRUE,
                             na_action = c("fail", "impute"), impute_rank = 2,
                             singular = c("stop", "ginv"), theta_est = c("uno_mas", "ms")) {
  cl <- match.call()
  na_action <- match.arg(na_action)
  transform <- match.arg(transform)
  region <- match.arg(region)
  singular <- match.arg(singular)
  theta_est <- match.arg(theta_est)
  X <- .as_data_matrix(X, allow_na = na_action == "impute")
  if (missing(groups) || length(groups) != nrow(X)) {
    stop("`groups` debe tener una entrada por fila de `X`.", call. = FALSE)
  }
  if (anyNA(groups)) stop("`groups` no puede tener NA.", call. = FALSE)
  g <- droplevels(as.factor(groups))
  K <- nlevels(g)
  n <- nrow(X)
  p <- ncol(X)
  if (K < 3) {
    stop("Se necesitan al menos 3 grupos: con 2 grupos el análisis canónico tiene un ",
         "solo eje (usa una prueba T2 de Hotelling).", call. = FALSE)
  }
  if (n - K < p && singular == "stop") {
    stop(sprintf(paste0("Hay pocos individuos: n - K = %d es menor que el número de ",
                        "variables (%d), y la matriz de covarianzas dentro de los grupos ",
                        "no se puede invertir."), n - K, p), call. = FALSE)
  }
  if (!is.numeric(conf) || conf <= 0 || conf >= 1) {
    stop("`conf` debe estar entre 0 y 1.", call. = FALSE)
  }
  imputation <- NULL
  if (anyNA(X)) {
    imputation <- impute_svd(X, rank = impute_rank, transform = transform)
    X <- imputation$X
  }
  pp <- .preprocess(X, transform)
  Z <- pp$Z
  nk <- as.vector(table(g))
  M <- rowsum(Z, g) / nk
  E <- crossprod(Z - M[as.integer(g), , drop = FALSE])
  out <- .canon_core(Z = Z, g = g, M = M, nk = nk, E = E, dfE = n - K, H = NULL, dfH = K - 1,
                     conf = conf, region = region, ndim = ndim, fix_signs = fix_signs,
                     singular = singular, var_names = colnames(X), theta_est = theta_est)
  out$preprocessing <- pp$info
  out$imputation <- imputation
  out$call <- cl
  out
}

# Nucleo comun del biplot canonico.
#   Z   : datos preprocesados (n x p), filas = individuos.
#   g   : factor que agrupa a los individuos (marcadores de medias = niveles de g).
#   M   : medias de los marcadores (K x p), centradas en la media global.
#   E   : SCPC dentro (residual del modelo), con dfE grados de libertad.
#   H   : SCPC de la hipotesis (por defecto, sum n_k m_k m_k'), con dfH g.l.
# Ejes: vectores propios de W^-1/2 H W^-1/2 (W = E / dfE); d^2 = sus valores
# propios, theta = d^2 / dfE = valores propios de H E^-1.
.canon_core <- function(Z, g, M, nk, E, dfE, H = NULL, dfH, conf = 0.95,
                        region = "chi2", ndim = 3, fix_signs = TRUE, singular = "stop",
                        var_names = colnames(Z), theta_est = "uno_mas") {
  n <- nrow(Z); p <- ncol(Z); K <- nrow(M)
  W <- E / dfE
  eW <- eigen(W, symmetric = TRUE)
  tolW <- max(eW$values) * 1e-10
  if (min(eW$values) <= tolW) {
    if (singular == "stop") {
      stop("La matriz de covarianzas dentro de los grupos es singular: hay variables ",
           "colineales, constantes dentro de los grupos o que suman una constante ",
           "(datos composicionales). Usa la opción 'inversa generalizada' o una ",
           "transformación log-cociente.", call. = FALSE)
    }
  }
  pos <- eW$values > tolW
  p_eff <- sum(pos)
  Ev <- eW$vectors[, pos, drop = FALSE]; lv <- eW$values[pos]
  Wmh <- Ev %*% (t(Ev) / sqrt(lv))
  Wh <- Ev %*% (t(Ev) * sqrt(lv))
  if (is.null(H)) H <- crossprod(sqrt(nk) * M)
  S <- Wmh %*% H %*% Wmh
  S <- (S + t(S)) / 2
  eS <- eigen(S, symmetric = TRUE)
  ev <- pmax(eS$values, 0)
  tol <- max(ev) * .Machine$double.eps^(2 / 3)
  r <- min(sum(ev > tol), dfH, p_eff)
  if (r < 2) stop("Las medias no ocupan un plano: solo hay un eje canónico.", call. = FALSE)
  d <- sqrt(ev[seq_len(r)])
  V <- eS$vectors[, seq_len(r), drop = FALSE]
  if (fix_signs) {
    sg <- apply(V, 2, function(v) sign(v[which.max(abs(v))]))
    V <- sweep(V, 2, sg, "*")
  }
  Pm <- M %*% Wmh %*% V
  Q <- Wh %*% V
  Ind <- Z %*% Wmh %*% V
  axn <- paste0("Eje", seq_len(r))
  dimnames(Pm) <- list(rownames(M), axn)
  dimnames(Q) <- list(var_names, axn)
  dimnames(Ind) <- list(rownames(Z), axn)
  ndim <- max(2L, min(as.integer(ndim), r))
  keep <- seq_len(ndim)

  theta <- d^2 / dfE
  wilks <- prod(1 / (1 + theta))
  bart <- -(dfE - (p_eff - dfH + 1) / 2) * log(wilks)
  df <- p_eff * dfH
  alpha <- 1 - conf
  rad <- switch(region,
    chi2 = , ellipse = , ellipse_amaro = , boot = sqrt(stats::qchisq(conf, 2) / nk),
    t = stats::qt(1 - alpha / 2, dfE) / sqrt(nk),
    bonferroni = stats::qt(1 - alpha / (2 * K * (K - 1) / 2), dfE) / sqrt(nk))
  names(rad) <- rownames(M)
  wjj <- diag(W)
  # Calidad "principal" (Amaro et al., 2004): coseno^2 de Q escalado por theta
  Qp <- sweep(Q, 2, theta, "*")
  qprin <- 100 * Qp^2 / rowSums(Qp^2)
  # Correlaciones totales (tabla 9 de Amaro et al., 2008)
  tot <- suppressWarnings(stats::cor(Z, Ind))
  dimnames(tot) <- list(var_names, axn)
  out <- list(
    rows = Pm[, keep, drop = FALSE],
    cols = Q[, keep, drop = FALSE],
    individuals = Ind[, keep, drop = FALSE],
    sup_rows = NULL, sup_cols = NULL,
    d = stats::setNames(d, axn),
    eigen = stats::setNames(d^2, axn),
    inertia = .inertia_table(d),
    canonical = data.frame(eje = axn, valor_propio = theta,
                           valor_singular_E = sqrt(theta),
                           correlacion_canonica = sqrt(theta / (1 + theta)),
                           row.names = NULL),
    manova = list(wilks = wilks, chi2 = bart, df = df,
                  p_value = stats::pchisq(bart, df, lower.tail = FALSE)),
    quality_rows = .quality(Pm),
    quality_cols = 100 * Q^2 / wjj,
    quality_cols_corr = 100 * Q^2 / wjj,
    quality_cols_principal = qprin,
    contrib_rows = .contrib(sqrt(nk) * Pm),
    contrib_cols = .contrib(Q),
    structure = Q / sqrt(wjj),
    structure_total = tot,
    gof = c(medias = 100 * sum(d[1:2]^2) / sum(d^2)),
    radius = rad,
    region = region,
    conf = conf,
    n_groups = stats::setNames(nk, rownames(M)),
    groups = g,
    method = "canonical",
    alpha = NA_real_,
    pca_scaling = FALSE,
    preprocessing = NULL,
    rank = r,
    ndim = ndim,
    imputation = NULL,
    singular = p_eff < p,
    .Wmh = Wmh, .V = V, .Z = Z, .W = W, .M = M,
    # Elementos de las regiones de Krzanowski (1989) / Amaro et al. (2008).
    #   P      : coordenadas de las medias en la escala de W (todas las r dimensiones);
    #            es la misma escala de x$rows, del termino 1/n_i y de los individuos.
    #   d2     : d^2 = autovalores de W^-1/2 SCH W^-1/2 = gl_E * (autovalores de H E^-1).
    #   theta  : autovalores poblacionales de (SCH / gl_H) W^-1 (Anderson, 1984), estimados con
    #            1 + d^2 / gl_H ("uno_mas") o con d^2 / gl_H ("ms").
    #   xiE, theta_amaro: solo para la version publicada (Amaro et al., 2008), que mezcla escalas.
    .krz = list(P = Pm, d2 = d^2,
                theta = if (identical(theta_est, "ms")) d^2 / dfH else 1 + d^2 / dfH,
                theta_est = theta_est,
                xiE = Pm / sqrt(dfE), theta_amaro = 1 + (d / sqrt(dfE)) / dfH,
                gamma = dfH / dfE, n = dfE + K, K = K, nk = nk,
                dfE = dfE, dfH = dfH, p_eff = p_eff),
    call = NULL
  )
  class(out) <- c("bigarx_canonical", "bigarx")
  out
}

# Cambia el signo de los ejes indicados (solo cambia la orientacion del grafico).
#' @noRd
#' @export
invertir_ejes <- function(x, ejes) {
  ejes <- ejes[ejes >= 1 & ejes <= ncol(x$rows)]
  if (!length(ejes)) return(x)
  f <- function(A) { if (is.null(A)) return(A); k <- ejes[ejes <= ncol(A)]; A[, k] <- -A[, k]; A }
  x$rows <- f(x$rows); x$cols <- f(x$cols); x$individuals <- f(x$individuals)
  x$structure <- f(x$structure); x$structure_total <- f(x$structure_total)
  x$.V <- f(x$.V); x$.krz$xiE <- f(x$.krz$xiE); x$.krz$P <- f(x$.krz$P)
  x
}

#' @noRd
#' @export
print.bigarx_canonical <- function(x, ...) {
  cat("Biplot can\u00F3nico / MANOVA biplot (Amaro, Vicente-Villard\u00F3n y Galindo, 2004)\n")
  cat(sprintf("%d grupos | %d variables | %d individuos | transformaci\u00F3n: %s\n",
              nrow(x$rows), nrow(x$cols), nrow(x$individuals), x$preprocessing$transform))
  m <- x$manova
  cat(sprintf("MANOVA: Lambda de Wilks = %s | chi2(%d) = %s | p = %s\n",
              format(signif(m$wilks, 4)), m$df, .fmt(m$chi2, 2), format.pval(m$p_value, digits = 3)))
  k <- min(3, nrow(x$inertia))
  cat("Inercia por eje (%): ",
      paste(.fmt(x$inertia$inercia[seq_len(k)]), collapse = ", "),
      sprintf("  | plano 1-2: %s%%\n", .fmt(x$inertia$acumulada[2])), sep = "")
  cat(sprintf("Regiones de confianza: %s, nivel %s%%\n", x$region, format(100 * x$conf)))
  if (!is.null(x$imputation)) {
    cat(sprintf("Datos imputados: %d celdas (ver $imputation)\n", nrow(x$imputation$missing)))
  }
  cat("Usa summary() para ver calidades y correlaciones con los ejes.\n")
  invisible(x)
}

#' @noRd
#' @export
summary.bigarx_canonical <- function(object, axes = c(1, 2), ...) {
  .check_axes(object, axes)
  medias <- data.frame(object$rows[, axes, drop = FALSE],
                       calidad = rowSums(object$quality_rows[, axes, drop = FALSE]),
                       n = object$n_groups, check.names = FALSE)
  if (!.es_elipse(object$region)) medias$radio <- object$radius
  vars <- data.frame(object$cols[, axes, drop = FALSE],
                     calidad = rowSums(object$quality_cols[, axes, drop = FALSE]),
                     correlacion = object$structure[, axes, drop = FALSE],
                     check.names = FALSE)
  out <- list(axes = axes, regions = canonical_regions(object, axes),
              manova = object$manova, canonical = object$canonical,
              inertia = object$inertia, gof = object$gof, rows = medias, cols = vars,
              region = object$region, conf = object$conf)
  class(out) <- "summary.bigarx_canonical"
  out
}

#' @noRd
#' @export
print.summary.bigarx_canonical <- function(x, digits = 3, ...) {
  m <- x$manova
  cat("Biplot can\u00F3nico / MANOVA biplot\n\n")
  cat(sprintf("Contraste MANOVA (Wilks): Lambda = %s, chi2(%d) = %s, p = %s\n\n",
              format(signif(m$wilks, 4)), m$df, .fmt(m$chi2, 2), format.pval(m$p_value, digits = 3)))
  cat("Valores propios de HE^-1 y correlaciones can\u00F3nicas\n")
  cc <- x$canonical
  cc[-1] <- lapply(cc[-1], round, digits)
  inr <- x$inertia
  cc$inercia <- round(inr$inercia, 2)
  cc$acumulada <- round(inr$acumulada, 2)
  print(cc, row.names = FALSE)
  lab <- paste(x$axes, collapse = "-")
  cat(sprintf("\nMedias de grupo (calidad en el plano %s, %%)\n", lab))
  print(round(x$rows, digits))
  if (.es_elipse(x$region)) {
    cat(sprintf("\nElipses de confianza al %s%% (Amaro et al., 2008)\n", format(100 * x$conf)))
  } else {
    cat(sprintf("\nRegiones de confianza: c\u00EDrculos %s al %s%% (radio por grupo arriba)\n",
                x$region, format(100 * x$conf)))
  }
  if (.es_elipse(x$region)) {
    rg <- x$regions
    rg[-1] <- lapply(rg[-1], round, digits + 1)
    print(rg, row.names = FALSE)
  }
  cat(sprintf("\nVariables (calidad en el plano %s, %%; correlaci\u00F3n con cada eje)\n", lab))
  print(round(x$cols, digits))
  invisible(x)
}

.circle_df <- function(cx, cy, r, grupo, npts = 100) {
  t <- seq(0, 2 * pi, length.out = npts)
  do.call(rbind, lapply(seq_along(cx), function(i) {
    data.frame(x = cx[i] + r[i] * cos(t), y = cy[i] + r[i] * sin(t), grupo = grupo[i])
  }))
}

.canon_scale <- function(x, axes, col_scale) {
  if (is.numeric(col_scale)) return(col_scale)
  if (identical(col_scale, "none")) return(1)
  ext <- max(abs(x$individuals[, axes]))
  0.9 * ext / max(sqrt(rowSums(x$cols[, axes, drop = FALSE]^2)))
}

# Gráfico del biplot canónico
#
# Muestra los individuos (opcional), las medias de grupo con su región de
# confianza y las variables como flechas.
#
# @param x,object Objeto `"bigarx_canonical"`.
# @param axes Ejes a representar.
# @param individuals Si es `TRUE` (por defecto), dibuja los individuos.
# @param circles Si es `TRUE` (por defecto), dibuja las regiones de confianza
#   (círculos o elipses, según `region` en [bigarx_canonical()]).
# @param col_scale `"auto"` (por defecto) reescala las flechas para que
#   ocupen el rango de los individuos; `"none"` o un número.
# @param min_quality Calidad mínima (en %) de las variables para dibujarlas.
# @param labels Rotular medias y variables.
# @param arrow_col Color de las flechas.
# @param main,xlab,ylab Título y nombres de los ejes. Por defecto, "Biplot
#   canónico" y el porcentaje de inercia de cada eje.
# @param ... Argumentos adicionales para [graphics::plot()].
# @return `x` de forma invisible (`plot`) o un objeto `ggplot` (`autoplot`).
#' @noRd
#' @export
plot.bigarx_canonical <- function(x, axes = c(1, 2), individuals = TRUE, circles = TRUE,
                                  col_scale = "auto", min_quality = 0, labels = TRUE,
                                  arrow_col = "grey20",
                                  main = NULL, xlab = NULL, ylab = NULL, ...) {
  .check_axes(x, axes)
  k <- .canon_scale(x, axes, col_scale)
  P <- x$rows[, axes, drop = FALSE]
  C <- x$cols[, axes, drop = FALSE] * k
  I <- x$individuals[, axes, drop = FALSE]
  qc <- rowSums(x$quality_cols[, axes, drop = FALSE]) >= min_quality
  pal <- grDevices::hcl.colors(nlevels(x$groups), "Dark 3")
  rg <- .regions_df(x, axes, grupo = seq_len(nrow(P)))
  lim <- range(c(if (individuals) I, rg$x, rg$y, C[qc, ], 0)) * 1.1
  ax <- x$inertia[axes, ]
  if (is.null(main)) main <- "Biplot can\u00F3nico"
  plot(P, type = "n", asp = 1, xlim = lim, ylim = lim, main = main,
       xlab = if (is.null(xlab)) sprintf("Eje %d (%s%%)", axes[1], .fmt(ax$inercia[1])) else xlab,
       ylab = if (is.null(ylab)) sprintf("Eje %d (%s%%)", axes[2], .fmt(ax$inercia[2])) else ylab,
       ...)
  graphics::abline(h = 0, v = 0, lty = 3, col = "grey60")
  if (individuals) {
    graphics::points(I, pch = 19, cex = 0.5,
                     col = grDevices::adjustcolor(pal[as.integer(x$groups)], 0.45))
  }
  if (circles) {
    cd <- .regions_df(x, axes, grupo = seq_len(nrow(P)))
    for (i in seq_len(nrow(P))) {
      sub <- cd[cd$grupo == i, ]
      graphics::lines(sub$x, sub$y, col = pal[i], lwd = 2)
    }
  }
  graphics::points(P, pch = 15, cex = 1.2, col = pal)
  graphics::arrows(0, 0, C[qc, 1], C[qc, 2], length = 0.08, col = arrow_col, lwd = 1.5)
  if (labels) {
    graphics::text(P, labels = rownames(P), pos = 3, font = 2, col = pal)
    graphics::text(C[qc, , drop = FALSE] * 1.08, labels = rownames(C)[qc], cex = 0.8,
                   col = arrow_col, font = 2)
  }
  invisible(x)
}

autoplot.bigarx_canonical <- function(object, axes = c(1, 2), individuals = TRUE,
                                      circles = TRUE, col_scale = "auto", min_quality = 0,
                                      labels = TRUE, arrow_col = "grey20", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Instala 'ggplot2' para usar autoplot(): install.packages(\"ggplot2\")", call. = FALSE)
  }
  .check_axes(object, axes)
  x <- object
  k <- .canon_scale(x, axes, col_scale)
  ax <- paste0("Eje", axes)
  med <- data.frame(x = x$rows[, axes[1]], y = x$rows[, axes[2]], grupo = rownames(x$rows))
  ind <- data.frame(x = x$individuals[, axes[1]], y = x$individuals[, axes[2]],
                    grupo = as.character(x$groups))
  qc <- rowSums(x$quality_cols[, axes, drop = FALSE]) >= min_quality
  vars <- data.frame(x = x$cols[qc, axes[1]] * k, y = x$cols[qc, axes[2]] * k,
                     elemento = rownames(x$cols)[qc])
  cd <- .regions_df(x, axes, grupo = med$grupo)
  inr <- x$inertia[axes, ]
  g <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = 3, colour = "grey60")
  if (individuals) {
    g <- g + ggplot2::geom_point(data = ind, ggplot2::aes(x = .data$x, y = .data$y,
                                                          colour = .data$grupo),
                                 alpha = 0.4, size = 1.2)
  }
  if (circles) {
    g <- g + ggplot2::geom_path(data = cd, ggplot2::aes(x = .data$x, y = .data$y,
                                                        colour = .data$grupo,
                                                        group = .data$grupo),
                                linewidth = 0.9)
  }
  g <- g +
    ggplot2::geom_point(data = med, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$grupo),
                        shape = 15, size = 3) +
    ggplot2::geom_segment(data = vars, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                          colour = arrow_col,
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"))) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(x = sprintf("Eje %d (%s%%)", axes[1], .fmt(inr$inercia[1])),
                  y = sprintf("Eje %d (%s%%)", axes[2], .fmt(inr$inercia[2])),
                  title = "Biplot can\u00F3nico", colour = NULL)
  if (labels) {
    g <- g +
      ggplot2::geom_text(data = med, ggplot2::aes(x = .data$x, y = .data$y, label = .data$grupo,
                                                  colour = .data$grupo),
                         vjust = -1.2, fontface = "bold", show.legend = FALSE) +
      ggplot2::geom_text(data = vars, ggplot2::aes(x = .data$x * 1.1, y = .data$y * 1.1,
                                                   label = .data$elemento),
                         colour = arrow_col, size = 3.2, fontface = "bold")
  }
  g
}

#' @noRd
#' @export
predict.bigarx_canonical <- function(object, newdata, ...) {
  Xs <- .as_data_matrix(newdata, "newdata")
  if (ncol(Xs) != nrow(object$cols)) {
    stop("`newdata` debe tener las mismas columnas que los datos activos.", call. = FALSE)
  }
  info <- object$preprocessing
  Zs <- .transform_sup_rows(Xs, info)
  out <- Zs %*% object$.Wmh %*% object$.V
  colnames(out) <- names(object$d)
  out[, seq_len(object$ndim), drop = FALSE]
}

.es_elipse <- function(region) region %in% c("ellipse", "ellipse_amaro")

# Matriz 2x2 de varianzas y covarianzas de cada media en los ejes `axes`
# (Amaro et al., 2008, ecuaciones 3 y 4; Krzanowski, 1989).
#
# region = "ellipse" (por defecto, coherente): todo en la escala de W.
#   Var(P_il) = 1/n_i + [ xi_il^2 / 2 + sum_{j != l} theta_l (theta_j + g theta_l) /
#                         (g (theta_j - theta_l)^2) xi_ij^2 ] / gl_E
#   Cov(P_ij, P_il) = - theta_j theta_l (1 + g) / (gl_E g (theta_j - theta_l)^2) xi_ij xi_il
#   con xi = P (escala W, la misma de x$rows), theta = autovalores de (SCH/gl_H) W^-1
#   y g = gl_H / gl_E.
# region = "ellipse_amaro": la version publicada (xi = P / sqrt(gl_E) y theta construido
#   con el valor singular), que reproduce las tablas 4 a 7 del articulo pero mezcla escalas.
.krz_cov <- function(x, axes) {
  kz <- x$.krz
  amaro <- identical(x$region, "ellipse_amaro")
  xi <- if (amaro) kz$xiE else kz$P
  th <- if (amaro) kz$theta_amaro else kz$theta
  gam <- kz$gamma
  s <- length(th)
  if (anyDuplicated(signif(th, 12))) {
    stop("Hay valores propios repetidos: las ecuaciones de Krzanowski no están definidas.", call. = FALSE)
  }
  lapply(seq_len(nrow(xi)), function(i) {
    v <- vapply(axes, function(l) {
      o <- setdiff(seq_len(s), l)
      1 / kz$nk[i] + (0.5 * xi[i, l]^2 +
        sum(th[l] * (th[o] + gam * th[l]) / (gam * (th[o] - th[l])^2) * xi[i, o]^2)) / kz$dfE
    }, numeric(1))
    j <- axes[1]; l <- axes[2]
    cv <- -th[j] * th[l] * (1 + gam) / (kz$dfE * gam * (th[j] - th[l])^2) * xi[i, j] * xi[i, l]
    S <- matrix(c(v[1], cv, cv, v[2]), 2, 2)
    ev <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
    attr(S, "definida_positiva") <- isSymmetric(S) && min(ev) > 0
    S
  })
}

# Puntos del contorno (z - P_i)' S_i^-1 (z - P_i) = chi2_2 de una elipse; comprueba la ecuacion.
.contorno_elipse <- function(centro, S, q, npts = 120) {
  e <- eigen(S, symmetric = TRUE)
  if (min(e$values) <= 0) {
    stop("La matriz de covarianzas de la región no es definida positiva.", call. = FALSE)
  }
  t <- seq(0, 2 * pi, length.out = npts)
  pts <- e$vectors %*% (sqrt(e$values * q) * rbind(cos(t), sin(t)))
  err <- max(abs(colSums(pts * solve(S, pts)) - q))
  if (err > 1e-8 * q) stop("El contorno no cumple su ecuación (error numérico).", call. = FALSE)
  list(x = centro[1] + pts[1, ], y = centro[2] + pts[2, ], error = err)
}

# Puntos del contorno de cada region de confianza (circulo o elipse), en la escala de x$rows.
.regions_df <- function(x, axes, grupo, npts = 120) {
  P <- x$rows[, axes, drop = FALSE]
  if (!.es_elipse(x$region)) {
    return(.circle_df(P[, 1], P[, 2], x$radius, grupo, npts))
  }
  q <- stats::qchisq(x$conf, 2)
  S <- .krz_cov(x, axes)
  do.call(rbind, lapply(seq_len(nrow(P)), function(i) {
    if (!isTRUE(attr(S[[i]], "definida_positiva"))) {
      warning(sprintf("La región del grupo %s no es definida positiva y no se dibuja.", rownames(P)[i]),
              call. = FALSE)
      return(NULL)
    }
    ce <- .contorno_elipse(P[i, ], S[[i]], q, npts)
    data.frame(x = ce$x, y = ce$y, grupo = grupo[i])
  }))
}

# Regiones de confianza del biplot canónico
#
# Devuelve, para cada media de grupo, la región de confianza en el plano
# elegido: centro, varianzas y covarianza, semiejes, ángulo de inclinación y
# área. `angulo` es la inclinación de los ejes de la elipse respecto de los
# ejes del gráfico (entre -45 y 45 grados), como en la tabla 7 de Amaro et
# al. (2008); `direccion_eje_mayor` es el ángulo del eje mayor medido desde
# el eje horizontal. Con `region = "ellipse"` reproduce las tablas 4 a 7 de Amaro,
# Vicente-Villardón y Galindo (2008); con las regiones circulares, los dos
# semiejes son iguales al radio.
#
# @param x Objeto de [bigarx_canonical()].
# @param axes Plano (por defecto `c(1, 2)`).
# @return Un data.frame con una fila por grupo.
# @references
# Amaro, I. R., Vicente-Villardón, J. L. y Galindo, M. P. (2008).
# Contribuciones al MANOVA-Biplot: regiones de confianza alternativas.
# *Revista Investigación Operacional*, 29(3), 231-241.
#
# Krzanowski, W. J. (1989). On confidence regions in canonical variate
# analysis. *Biometrika*, 76(1), 107-116.
#' @noRd
#' @export
canonical_regions <- function(x, axes = c(1, 2)) {
  if (!inherits(x, "bigarx_canonical")) {
    stop("`x` debe venir de bigarx_canonical().", call. = FALSE)
  }
  .check_axes(x, axes)
  P <- x$rows[, axes, drop = FALSE]
  q <- stats::qchisq(x$conf, 2)
  if (.es_elipse(x$region)) {
    S <- .krz_cov(x, axes)
    filas <- lapply(seq_len(nrow(P)), function(i) {
      e <- eigen(S[[i]], symmetric = TRUE)
      dp <- isTRUE(attr(S[[i]], "definida_positiva"))
      err <- if (dp) .contorno_elipse(P[i, ], S[[i]], q)$error else NA_real_
      v <- e$vectors[, 1]
      dir <- atan2(v[2], v[1]) * 180 / pi
      dir <- ((dir + 90) %% 180) - 90
      s1 <- S[[i]][1, 1]; s2 <- S[[i]][2, 2]; s12 <- S[[i]][1, 2]
      data.frame(var1 = s1, var2 = s2, cov = s12,
                 semieje_mayor = sqrt(e$values[1] * q),
                 semieje_menor = sqrt(e$values[2] * q),
                 angulo = 0.5 * atan(2 * s12 / (s1 - s2)) * 180 / pi,
                 direccion_eje_mayor = dir,
                 area = pi * sqrt(max(det(S[[i]]), 0)) * q,
                 definida_positiva = dp, error_contorno = err)
    })
    reg <- do.call(rbind, filas)
  } else {
    r <- x$radius
    reg <- data.frame(var1 = NA_real_, var2 = NA_real_, cov = NA_real_,
                      semieje_mayor = r, semieje_menor = r, angulo = 0,
                      direccion_eje_mayor = NA_real_,
                      area = pi * r^2, definida_positiva = TRUE, error_contorno = 0)
  }
  out <- data.frame(grupo = rownames(P), centro1 = P[, 1], centro2 = P[, 2], reg,
                    row.names = NULL)
  attr(out, "region") <- x$region
  attr(out, "escala") <- if (identical(x$region, "ellipse_amaro"))
    "centros en escala W; covarianzas de la version publicada (mezcla de escalas)" else "escala W (varianza dentro de los grupos = 1)"
  attr(out, "conf") <- x$conf
  out
}

#' @noRd
#' @export
canonical_tidy <- function(x, matrix = c("both", "rows", "cols", "eigen"),
                                         axes = c(1, 2), ...) {
  matrix <- match.arg(matrix)
  if (matrix == "eigen") return(x$inertia)
  mk <- function(coords, Q, tipo, grupo) {
    df <- data.frame(elemento = rownames(coords), tipo = tipo, suplementario = FALSE,
                     coords, row.names = NULL, check.names = FALSE)
    df$calidad <- if (is.null(Q)) NA_real_ else rowSums(Q[, axes, drop = FALSE])
    df$grupo <- grupo
    df
  }
  parts <- list()
  if (matrix %in% c("both", "rows")) {
    parts$m <- mk(x$rows, x$quality_rows, "media", rownames(x$rows))
    parts$i <- mk(x$individuals, NULL, "individuo", as.character(x$groups))
  }
  if (matrix %in% c("both", "cols")) {
    parts$c <- mk(x$cols, x$quality_cols, "columna", NA)
  }
  out <- do.call(rbind, unname(parts))
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# Contrastes MANOVA completos (las mismas aproximaciones F que summary.manova)
# -----------------------------------------------------------------------------
# theta = valores propios de H E^-1; p = variables; q = K - 1; df.res = n - K.
#' @noRd
#' @export
manova_tests <- function(x) {
  if (!inherits(x, "bigarx_canonical")) stop("`x` debe venir de bigarx_canonical().", call. = FALSE)
  th <- x$canonical$valor_propio
  p <- if (is.null(x$.krz$p_eff)) nrow(x$cols) else x$.krz$p_eff
  q <- if (is.null(x$.krz$dfH)) x$.krz$K - 1 else x$.krz$dfH
  dfr <- if (is.null(x$.krz$dfE)) x$.krz$n - x$.krz$K else x$.krz$dfE
  s <- min(p, q)
  m <- (abs(p - q) - 1) / 2
  nn <- (dfr - p - 1) / 2
  # Pillai
  V <- sum(th / (1 + th))
  pil <- c(V, (2 * nn + s + 1) / (2 * m + s + 1) * V / (s - V), s * (2 * m + s + 1), s * (2 * nn + s + 1))
  # Wilks (aproximacion de Rao)
  W <- prod(1 / (1 + th))
  rr <- dfr - (p - q + 1) / 2
  u <- (p * q - 2) / 4
  tt <- if (p^2 + q^2 - 5 > 0) sqrt((p^2 * q^2 - 4) / (p^2 + q^2 - 5)) else 1
  wil <- c(W, ((W^(-1 / tt) - 1) * (rr * tt - 2 * u)) / (p * q), p * q, rr * tt - 2 * u)
  # Hotelling-Lawley
  Tt <- sum(th)
  hl <- c(Tt, 2 * (s * nn + 1) * Tt / (s^2 * (2 * m + s + 1)), s * (2 * m + s + 1), 2 * (s * nn + 1))
  # Roy (raiz mayor)
  d1 <- max(p, q); d2 <- dfr - d1 + q
  roy <- c(max(th), d2 / d1 * max(th), d1, d2)
  tab <- rbind(Pillai = pil, Wilks = wil, `Hotelling-Lawley` = hl, Roy = roy)
  out <- data.frame(estadistico = tab[, 1], F_aprox = tab[, 2], gl1 = tab[, 3], gl2 = tab[, 4],
                    p_valor = stats::pf(tab[, 2], tab[, 3], tab[, 4], lower.tail = FALSE))
  out
}

# -----------------------------------------------------------------------------
# ANOVA univariante por variable (sobre los datos ya transformados)
# -----------------------------------------------------------------------------
#' @noRd
#' @export
univariate_anovas <- function(x) {
  if (!inherits(x, "bigarx_canonical")) stop("`x` debe venir de bigarx_canonical().", call. = FALSE)
  if (!is.null(x$.anova_uni)) return(x$.anova_uni)
  Z <- x$.Z
  g <- x$groups
  K <- nlevels(g); n <- nrow(Z)
  M <- rowsum(Z, g) / as.vector(table(g))
  sct <- colSums(sweep(Z, 2, colMeans(Z))^2)
  scd <- colSums((Z - M[as.integer(g), , drop = FALSE])^2)
  sce <- sct - scd
  Fv <- (sce / (K - 1)) / (scd / (n - K))
  data.frame(SC_total = sct, SC_entre = sce, SC_dentro = scd, F = Fv,
             gl1 = K - 1, gl2 = n - K, p_valor = stats::pf(Fv, K - 1, n - K, lower.tail = FALSE))
}


# =============================================================================
#  MANOVA biplot para arreglos de tratamientos con dos factores
#  Amaro, Vicente-Villardon y Galindo (2004), Interciencia 29(1), 26-32.
#  Autores: Kleber Garzon y Roxana Barrezueta. Licencia GPL (>= 3).
# =============================================================================
#  Modelo completo con interaccion: E = SCPC residual (dentro de las celdas),
#  gl_E = n - (numero de celdas). Biplots disponibles:
#    total       : las celdas como grupos de un MANOVA de una via.
#    filas       : medias del factor A; ejes que maximizan la F del efecto A.
#    columnas    : medias del factor B; ejes que maximizan la F del efecto B.
#    interaccion : ejes que maximizan la F de la interaccion A x B; se proyectan
#                  las medias de las celdas (interpretacion por paralelogramos).
#  Las SCPC de los efectos son secuenciales (tipo I: A, B, A x B). En disenos
#  balanceados coinciden con las de cualquier otro tipo.
#  Si el efecto tiene un solo grado de libertad (un factor con dos niveles), no
#  hay plano propio: como en Amaro et al. (2004, figura 5), las medias de ese
#  factor se proyectan sobre el biplot total.
# =============================================================================

.scpc_efectos <- function(Z, A, B) {
  fit <- stats::manova(Z ~ A * B)
  ss <- summary(fit)$SS
  df <- stats::anova(stats::lm(Z[, 1] ~ A * B))$Df
  names(df) <- c("A", "B", "A:B", "Residuals")
  list(H = list(A = ss[[1]], B = ss[[2]], AB = ss[[3]]), E = ss[["Residuals"]],
       df = c(A = df[[1]], B = df[[2]], AB = df[[3]]), dfE = df[[4]])
}

#' @noRd
#' @export
bigarx_manova2 <- function(X, A, B, efecto = c("total", "filas", "columnas", "interaccion"),
                           transform = c("center", "standardize"), conf = 0.95,
                           region = c("chi2", "t", "bonferroni", "ellipse", "ellipse_amaro", "boot"), ndim = 3,
                           fix_signs = TRUE, na_action = c("fail", "impute"), impute_rank = 2,
                           singular = c("stop", "ginv"), nombre_A = "A", nombre_B = "B",
                           theta_est = c("uno_mas", "ms")) {
  theta_est <- match.arg(theta_est)
  efecto <- match.arg(efecto); transform <- match.arg(transform); region <- match.arg(region)
  na_action <- match.arg(na_action); singular <- match.arg(singular)
  X <- .as_data_matrix(X, allow_na = na_action == "impute")
  if (length(A) != nrow(X) || length(B) != nrow(X)) stop("Los factores deben tener una entrada por fila.", call. = FALSE)
  if (anyNA(A) || anyNA(B)) stop("Los factores no pueden tener NA.", call. = FALSE)
  A <- droplevels(as.factor(A)); B <- droplevels(as.factor(B))
  if (nlevels(A) < 2 || nlevels(B) < 2) stop("Cada factor necesita al menos 2 niveles.", call. = FALSE)
  celda <- interaction(A, B, sep = "-", lex.order = FALSE, drop = TRUE)
  tab <- table(A, B)
  if (any(tab == 0)) stop("Hay combinaciones de niveles sin observaciones: el modelo con interacción necesita todas las celdas.", call. = FALSE)
  n <- nrow(X); p <- ncol(X); nc <- nlevels(celda)
  if (n - nc < 1) stop("Hace falta al menos una réplica por celda para estimar el error.", call. = FALSE)
  if (n - nc < p && singular == "stop") {
    stop(sprintf("Hay pocos grados de libertad del error (%d) para %d variables.", n - nc, p), call. = FALSE)
  }
  imputation <- NULL
  if (anyNA(X)) { imputation <- impute_svd(X, rank = impute_rank, transform = transform); X <- imputation$X }
  pp <- .preprocess(X, transform)
  Z <- pp$Z
  colnames(Z) <- colnames(X)
  sc <- .scpc_efectos(Z, A, B)
  E <- sc$E; dfE <- sc$dfE

  medias <- function(f) { nk <- as.vector(table(f)); list(M = rowsum(Z, f) / nk, nk = nk) }
  proyectado <- FALSE
  if (efecto == "total") {
    mc <- medias(celda)
    out <- .canon_core(Z, celda, mc$M, mc$nk, E, dfE, NULL, nc - 1, conf, region, ndim, fix_signs, singular, colnames(X), theta_est = theta_est)
  } else if (efecto == "interaccion") {
    if (sc$df[["AB"]] < 2) stop("La interacción tiene un solo grado de libertad: no hay plano de interacción.", call. = FALSE)
    mc <- medias(celda)
    out <- .canon_core(Z, celda, mc$M, mc$nk, E, dfE, sc$H$AB, sc$df[["AB"]], conf,
                       if (region == "boot") "chi2" else region, ndim, fix_signs, singular, colnames(X), theta_est = theta_est)
  } else {
    f <- if (efecto == "filas") A else B
    key <- if (efecto == "filas") "A" else "B"
    mf <- medias(f)
    if (sc$df[[key]] >= 2) {
      out <- .canon_core(Z, f, mf$M, mf$nk, E, dfE, sc$H[[key]], sc$df[[key]], conf,
                         if (region == "boot") "chi2" else region, ndim, fix_signs, singular, colnames(X), theta_est = theta_est)
    } else {
      # Un solo grado de libertad: proyectar las medias del factor sobre el biplot total
      mc <- medias(celda)
      out <- .canon_core(Z, celda, mc$M, mc$nk, E, dfE, NULL, nc - 1, conf, "chi2", ndim, fix_signs,
                         singular, colnames(X), theta_est = theta_est)
      P <- mf$M %*% out$.Wmh %*% out$.V
      colnames(P) <- paste0("Eje", seq_len(ncol(P)))
      out$rows <- P[, seq_len(out$ndim), drop = FALSE]
      out$quality_rows <- .quality(P)
      out$groups <- f
      out$n_groups <- stats::setNames(mf$nk, rownames(mf$M))
      reg <- if (region %in% c("ellipse", "ellipse_amaro", "boot")) "chi2" else region
      out$region <- reg
      Kf <- nlevels(f); alpha <- 1 - conf
      out$radius <- stats::setNames(switch(reg,
        chi2 = sqrt(stats::qchisq(conf, 2) / mf$nk),
        t = stats::qt(1 - alpha / 2, dfE) / sqrt(mf$nk),
        bonferroni = stats::qt(1 - alpha / (2 * max(1, Kf * (Kf - 1) / 2)), dfE) / sqrt(mf$nk)), rownames(mf$M))
      out$.M <- mf$M
      proyectado <- TRUE
    }
  }
  out$preprocessing <- pp$info
  out$imputation <- imputation
  out$diseno <- list(tipo = "dos_factores", efecto = efecto, proyectado = proyectado,
                     A = A, B = B, celda = celda, nombre_A = nombre_A, nombre_B = nombre_B,
                     pruebas = manova2_tests(sc, p_eff = out$.krz$p_eff, nombre_A, nombre_B),
                     anova = anova2_univariada(Z, A, B, nombre_A, nombre_B))
  out$.anova_uni <- NULL
  out
}

# Contrastes multivariantes por efecto (las mismas aproximaciones F que summary.manova)
.pruebas_theta <- function(th, p, q, dfr) {
  s <- min(p, q); m <- (abs(p - q) - 1) / 2; nn <- (dfr - p - 1) / 2
  V <- sum(th / (1 + th))
  pil <- c(V, (2 * nn + s + 1) / (2 * m + s + 1) * V / (s - V), s * (2 * m + s + 1), s * (2 * nn + s + 1))
  W <- prod(1 / (1 + th))
  rr <- dfr - (p - q + 1) / 2; u <- (p * q - 2) / 4
  tt <- if (p^2 + q^2 - 5 > 0) sqrt((p^2 * q^2 - 4) / (p^2 + q^2 - 5)) else 1
  wil <- c(W, ((W^(-1 / tt) - 1) * (rr * tt - 2 * u)) / (p * q), p * q, rr * tt - 2 * u)
  Tt <- sum(th)
  hl <- c(Tt, 2 * (s * nn + 1) * Tt / (s^2 * (2 * m + s + 1)), s * (2 * m + s + 1), 2 * (s * nn + 1))
  d1 <- max(p, q); d2 <- dfr - d1 + q
  roy <- c(max(th), d2 / d1 * max(th), d1, d2)
  tab <- rbind(Pillai = pil, Wilks = wil, `Hotelling-Lawley` = hl, Roy = roy)
  bart <- -(dfr - (p - q + 1) / 2) * log(W)
  data.frame(contraste = rownames(tab), estadistico = tab[, 1], F_aprox = tab[, 2], gl1 = tab[, 3],
             gl2 = tab[, 4], p_valor = stats::pf(tab[, 2], tab[, 3], tab[, 4], lower.tail = FALSE),
             chi2_bartlett = c(NA, bart, NA, NA), gl_chi2 = c(NA, p * q, NA, NA),
             p_chi2 = c(NA, stats::pchisq(bart, p * q, lower.tail = FALSE), NA, NA),
             row.names = NULL)
}

#' @noRd
#' @export
manova2_tests <- function(sc, p_eff, nombre_A = "A", nombre_B = "B") {
  Einv <- MASS_ginv(sc$E)
  ef <- c(A = nombre_A, B = nombre_B, AB = paste(nombre_A, "×", nombre_B))
  do.call(rbind, lapply(names(ef), function(k) {
    th <- Re(eigen(Einv %*% sc$H[[k]], only.values = TRUE)$values)
    th <- sort(pmax(th, 0), decreasing = TRUE)[seq_len(min(p_eff, sc$df[[k]]))]
    data.frame(efecto = ef[[k]], .pruebas_theta(th, p_eff, sc$df[[k]], sc$dfE))
  }))
}

MASS_ginv <- function(A, tol = sqrt(.Machine$double.eps)) {
  s <- svd(A); pos <- s$d > max(tol * s$d[1], 0)
  s$v[, pos, drop = FALSE] %*% (t(s$u[, pos, drop = FALSE]) / s$d[pos])
}

#' @noRd
#' @export
anova2_univariada <- function(Z, A, B, nombre_A = "A", nombre_B = "B") {
  ef <- c(nombre_A, nombre_B, paste(nombre_A, "×", nombre_B))
  do.call(rbind, lapply(colnames(Z), function(v) {
    a <- stats::anova(stats::lm(Z[, v] ~ A * B))
    data.frame(variable = v, efecto = ef, SC = a$`Sum Sq`[1:3], gl1 = a$Df[1:3],
               F = a$`F value`[1:3], gl2 = a$Df[4], p_valor = a$`Pr(>F)`[1:3], row.names = NULL)
  }))
}

# -----------------------------------------------------------------------------
# Comparaciones por pares: T^2 de Hotelling con la W combinada del modelo
# -----------------------------------------------------------------------------
#' @noRd
#' @export
hotelling_pares <- function(x, ajuste = c("bonferroni", "holm", "none")) {
  ajuste <- match.arg(ajuste)
  M <- x$.M; nk <- x$n_groups; dfE <- x$.krz$dfE; p <- x$.krz$p_eff
  Mw <- M %*% x$.Wmh                         # d' W^+ d = ||d W^-1/2||^2
  pares <- utils::combn(rownames(M), 2)
  res <- do.call(rbind, lapply(seq_len(ncol(pares)), function(k) {
    i <- pares[1, k]; j <- pares[2, k]
    D2 <- sum((Mw[i, ] - Mw[j, ])^2)
    T2 <- nk[[i]] * nk[[j]] / (nk[[i]] + nk[[j]]) * D2
    gl2 <- dfE - p + 1
    Fv <- gl2 / (dfE * p) * T2
    data.frame(grupo1 = i, grupo2 = j, D2_Mahalanobis = D2, T2 = T2, F = Fv, gl1 = p, gl2 = gl2,
               p_valor = stats::pf(Fv, p, gl2, lower.tail = FALSE))
  }))
  res$p_ajustado <- stats::p.adjust(res$p_valor, method = ajuste)
  res
}

# -----------------------------------------------------------------------------
# Datos composicionales (proporciones que suman una constante)
# -----------------------------------------------------------------------------
#' @noRd
#' @export
es_composicional <- function(X, tol = 1e-3) {
  X <- as.matrix(X)
  if (any(X < 0, na.rm = TRUE) || ncol(X) < 3) return(FALSE)
  s <- rowSums(X)
  all(is.finite(s)) && stats::sd(s) <= tol * max(1, mean(s))
}

# Reemplazo multiplicativo de ceros: delta = 0.65 * minimo positivo de la columna
.reemplazar_ceros <- function(X) {
  X <- as.matrix(X)
  tot <- rowSums(X)
  for (j in seq_len(ncol(X))) {
    z <- X[, j] == 0 & !is.na(X[, j])
    if (any(z)) {
      pos <- X[X[, j] > 0 & !is.na(X[, j]), j]
      X[z, j] <- 0.65 * if (length(pos)) min(pos) else 1e-6
    }
  }
  X / rowSums(X) * tot
}

#' @noRd
#' @export
transformar_composicional <- function(X, metodo = c("ninguna", "clr", "alr"), referencia = NULL) {
  metodo <- match.arg(metodo)
  if (metodo == "ninguna") return(X)
  nms <- colnames(X)
  Xp <- .reemplazar_ceros(X)
  L <- log(Xp)
  if (metodo == "clr") {
    out <- L - rowMeans(L)
    colnames(out) <- nms
  } else {
    if (is.null(referencia) || !referencia %in% nms) referencia <- nms[length(nms)]
    out <- L[, nms != referencia, drop = FALSE] - L[, referencia]
    colnames(out) <- nms[nms != referencia]
  }
  as.data.frame(out, check.names = FALSE)
}

# -----------------------------------------------------------------------------
# Inversion de ejes en el bootstrap (para que coincida con el grafico)
# -----------------------------------------------------------------------------
#' @noRd
#' @export
invertir_boot <- function(b, ejes) {
  if (is.null(b) || !length(ejes)) return(b)
  for (k in ejes) {
    pat <- paste0("^(media|variable|correlacion_variable)\\|.*\\|Eje", k, "$")
    cols <- grep(pat, colnames(b$replicates))
    b$replicates[, cols] <- -b$replicates[, cols]
    rows <- b$table$tipo %in% c("media", "variable", "correlacion_variable") & b$table$eje == paste0("Eje", k)
    t <- b$table
    lo <- -t$superior[rows]; hi <- -t$inferior[rows]
    t$estimacion[rows] <- -t$estimacion[rows]; t$media_boot[rows] <- -t$media_boot[rows]
    t$sesgo[rows] <- -t$sesgo[rows]; t$inferior[rows] <- lo; t$superior[rows] <- hi
    b$table <- t
  }
  b
}

# Regiones bootstrap: resumen numerico de las elipses de las medias
#' @noRd
#' @export
regiones_boot <- function(b, conf = b$conf) {
  nm <- boot_clouds(b, "media")
  q <- stats::qchisq(conf, 2)
  do.call(rbind, lapply(names(nm), function(gr) {
    P <- nm[[gr]]; S <- stats::cov(P); e <- eigen(S, symmetric = TRUE)
    data.frame(grupo = gr, centro1 = mean(P[, 1]), centro2 = mean(P[, 2]), var1 = S[1, 1], var2 = S[2, 2],
               cov = S[1, 2], semieje_mayor = sqrt(e$values[1] * q), semieje_menor = sqrt(max(e$values[2], 0) * q),
               angulo = 0.5 * atan(2 * S[1, 2] / (S[1, 1] - S[2, 2])) * 180 / pi,
               area = pi * sqrt(max(det(S), 0)) * q)
  }))
}


# =============================================================================
#  Bootstrap del biplot canonico
#  Autores: Kleber Garzon y Roxana Barrezueta. Licencia GPL (>= 3).
# =============================================================================
#  Remuestreo estratificado: en cada replica se extraen, con reemplazo, n_k
#  individuos de cada grupo k (se conservan los tamanos de grupo). Se repite el
#  preprocesamiento y el biplot canonico, y se alinea el signo de cada eje con
#  el analisis original (comparando las coordenadas de las medias de grupo).
#
#  Parametros con intervalo de confianza:
#    inercia (%), valor_propio (theta de HE^-1), correlacion_canonica,
#    media (coordenadas de cada media de grupo en cada eje del plano),
#    variable (coordenadas de cada flecha en cada eje del plano),
#    calidad_variable (calidad de cada variable en el plano),
#    correlacion_variable (correlacion de cada variable con cada eje).
#
#  Referencias: Efron y Tibshirani (1993); Nieto, Galindo, Leiva y
#  Vicente-Galindo (2015); Krzanowski y Radley (1989), bootstrap en el analisis
#  de variables canonicas.
# =============================================================================

#' @noRd
#' @export
boot_canonical <- function(X, groups, B = 1000, transform = c("center", "standardize"),
                           axes = c(1, 2), conf = 0.95, ci = c("percentile", "normal"),
                           seed = NULL, na_action = c("fail", "impute"), impute_rank = 2,
                           progress = NULL, singular = c("stop", "ginv")) {
  singular <- match.arg(singular)
  transform <- match.arg(transform)
  ci <- match.arg(ci)
  na_action <- match.arg(na_action)
  if (!is.numeric(B) || B < 50) stop("`B` debe ser al menos 50.", call. = FALSE)
  fit <- bigarx_canonical(X, groups, transform = transform, ndim = max(3, axes),
                          na_action = na_action, impute_rank = impute_rank, singular = singular)
  if (max(axes) > fit$rank) stop("El plano pedido supera el número de ejes canónicos.", call. = FALSE)
  Xc <- if (!is.null(fit$imputation)) fit$imputation$X else .as_data_matrix(X)
  g <- fit$groups
  P0 <- fit$rows[, axes, drop = FALSE]
  est <- .boot_canon_stats(fit, axes, P0)

  if (!is.null(seed)) set.seed(seed)
  idx_g <- split(seq_len(nrow(Xc)), g)
  reps <- matrix(NA_real_, B, length(est), dimnames = list(NULL, names(est)))
  fallos <- 0L
  for (b in seq_len(B)) {
    idx <- unlist(lapply(idx_g, function(ii) ii[sample.int(length(ii), length(ii), replace = TRUE)]),
                  use.names = FALSE)
    s <- tryCatch({
      fb <- bigarx_canonical(Xc[idx, , drop = FALSE], g[idx], transform = transform,
                             ndim = max(3, axes), fix_signs = FALSE, singular = singular)
      if (fb$rank < max(axes)) NULL else .boot_canon_stats(fb, axes, P0)
    }, error = function(e) NULL)
    if (is.null(s)) fallos <- fallos + 1L else reps[b, ] <- s
    if (is.function(progress) && b %% max(1, B %/% 50) == 0) progress(b / B)
  }
  reps <- reps[stats::complete.cases(reps), , drop = FALSE]
  if (nrow(reps) < 20) stop("Muy pocas réplicas válidas; revisa los tamaños de grupo.", call. = FALSE)

  a <- 1 - conf
  mu <- colMeans(reps)
  se <- apply(reps, 2, stats::sd)
  if (ci == "percentile") {
    lo <- apply(reps, 2, stats::quantile, probs = a / 2, names = FALSE)
    hi <- apply(reps, 2, stats::quantile, probs = 1 - a / 2, names = FALSE)
  } else {
    q <- if (nrow(Xc) >= 25) stats::qnorm(1 - a / 2) else stats::qt(1 - a / 2, nrow(Xc) - 1)
    lo <- est - q * se
    hi <- est + q * se
  }
  info <- do.call(rbind, strsplit(names(est), "|", fixed = TRUE))
  tab <- data.frame(tipo = info[, 1], elemento = info[, 2], eje = info[, 3],
                    estimacion = unname(est), media_boot = unname(mu), ee = unname(se),
                    sesgo = unname(mu - est), inferior = unname(lo), superior = unname(hi),
                    row.names = NULL, stringsAsFactors = FALSE)
  structure(list(fit = fit, table = tab, replicates = reps, B = nrow(reps), fallos = fallos,
                 conf = conf, ci = ci, axes = axes), class = "boot_canonical")
}

# Estadisticos de una replica, con el signo de cada eje alineado con P0.
.boot_canon_stats <- function(f, axes, P0) {
  P <- f$rows[, axes, drop = FALSE]
  sg <- sign(colSums(P * P0)); sg[sg == 0] <- 1
  P <- sweep(P, 2, sg, "*")
  Q <- sweep(f$cols[, axes, drop = FALSE], 2, sg, "*")
  S <- sweep(f$structure[, axes, drop = FALSE], 2, sg, "*")
  ejes <- paste0("Eje", axes)
  grp <- rownames(P); vars <- rownames(Q)
  k <- seq_len(min(3, f$rank))
  ax_all <- paste0("Eje", k)
  out <- c(
    stats::setNames(f$inertia$inercia[k], paste0("inercia|", ax_all, "|", ax_all)),
    stats::setNames(f$canonical$valor_propio[k], paste0("valor_propio|", ax_all, "|", ax_all)),
    stats::setNames(f$canonical$correlacion_canonica[k], paste0("correlacion_canonica|", ax_all, "|", ax_all)),
    stats::setNames(as.vector(P), paste0("media|", rep(grp, 2), "|", rep(ejes, each = length(grp)))),
    stats::setNames(as.vector(Q), paste0("variable|", rep(vars, 2), "|", rep(ejes, each = length(vars)))),
    stats::setNames(rowSums(f$quality_cols[, axes, drop = FALSE]),
                    paste0("calidad_variable|", vars, "|", paste(axes, collapse = "-"))),
    stats::setNames(as.vector(S), paste0("correlacion_variable|", rep(vars, 2), "|",
                                         rep(ejes, each = length(vars))))
  )
  out
}

#' @noRd
#' @export
print.boot_canonical <- function(x, ...) {
  cat(sprintf("Bootstrap del biplot canónico: %d réplicas (estratificado por grupo), IC %s al %s%%\n",
              x$B, x$ci, format(100 * x$conf)))
  t <- x$table[x$table$tipo %in% c("inercia", "correlacion_canonica"), ]
  print(t[, c("tipo", "elemento", "estimacion", "ee", "inferior", "superior")], row.names = FALSE, digits = 4)
  invisible(x)
}

# Nube de replicas (lista de matrices B x 2) para medias o para variables.
#' @noRd
#' @export
boot_clouds <- function(x, tipo = c("media", "variable")) {
  tipo <- match.arg(tipo)
  ejes <- paste0("Eje", x$axes)
  els <- unique(x$table$elemento[x$table$tipo == tipo])
  stats::setNames(lapply(els, function(e) {
    cbind(x$replicates[, paste0(tipo, "|", e, "|", ejes[1])],
          x$replicates[, paste0(tipo, "|", e, "|", ejes[2])])
  }), els)
}

# Elipse que contiene la proporcion `conf` de una nube bivariante (normal).
#' @noRd
#' @export
cloud_ellipse <- function(P, conf = 0.95, npts = 100) {
  mu <- colMeans(P)
  e <- eigen(stats::cov(P), symmetric = TRUE)
  t <- seq(0, 2 * pi, length.out = npts)
  r <- sqrt(stats::qchisq(conf, 2))
  pts <- e$vectors %*% (sqrt(pmax(e$values, 0)) * r * rbind(cos(t), sin(t)))
  data.frame(x = mu[1] + pts[1, ], y = mu[2] + pts[2, ])
}
