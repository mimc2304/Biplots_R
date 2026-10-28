# =============================================================================
# Cálculo del HJ-Biplot (Galindo-Villardón, 1986) y de los biplots clásicos GH y JK
# (Gabriel, 1971).
#
# Notación (igual que en el material de clase):
#   X = U D V'            descomposición en valores singulares (DVS / SVD)
#   HJ-Biplot   filas J = U D      columnas H = V D
#   GH-Biplot   filas G = U        columnas H = V D   (conserva las columnas)
#   JK-Biplot   filas J = U D      columnas K = V     (conserva las filas)
#
# Validado contra la salida de MULTBIPLOT de la práctica PISA 2025
# (ver tests/testthat/test-hj_biplot.R).
# =============================================================================

#' Transformaciones disponibles para la matriz de datos
#'
#' Vector con nombre: el nombre es la etiqueta que ve el usuario y el valor es el
#' código que recibe [hj_biplot()].
#' @format vector de caracteres con nombre.
#' @export
TRANSFORMACIONES <- c(
  "Estandarizar columnas"   = "estandarizar",
  "Centrar columnas"        = "centrar",
  "Doble centrado"          = "doble_centrado",
  "Restar la media global"  = "media_global",
  "Datos originales"        = "ninguna"
)

#' Transforma la matriz antes de la descomposición
#'
#' @param X matriz numérica (individuos en filas, variables en columnas).
#' @param transformacion uno de los códigos de [TRANSFORMACIONES].
#' @param log si es `TRUE` aplica antes logaritmo natural (exige valores > 0).
#' @return la matriz transformada, con los mismos nombres de filas y columnas.
#' @export
transformar_matriz <- function(X, transformacion = "estandarizar", log = FALSE) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  if (isTRUE(log)) {
    if (any(X <= 0)) {
      stop("La transformación logarítmica necesita que todos los valores sean mayores que 0.",
           call. = FALSE)
    }
    X <- base::log(X)
  }
  medias <- colMeans(X)
  Y <- switch(
    transformacion,
    estandarizar = {
      s <- apply(X, 2, stats::sd)
      constantes <- names(s)[s == 0]
      if (length(constantes)) {
        stop("Estas variables son constantes y no se pueden estandarizar: ",
             paste(constantes, collapse = ", "), call. = FALSE)
      }
      sweep(sweep(X, 2, medias, "-"), 2, s, "/")
    },
    centrar        = sweep(X, 2, medias, "-"),
    doble_centrado = X - outer(rowMeans(X), rep(1, ncol(X))) -
                     outer(rep(1, nrow(X)), medias) + mean(X),
    media_global   = X - mean(X),
    ninguna        = X,
    stop("Transformación no reconocida: ", transformacion, call. = FALSE)
  )
  dimnames(Y) <- dimnames(X)
  Y
}

#' HJ-Biplot (y biplots clásicos GH y JK)
#'
#' @param datos data.frame o matriz. Los nombres de fila se usan como etiquetas
#'   de los individuos.
#' @param variables nombres de las variables activas (numéricas). Si es `NULL`
#'   se usan todas las columnas numéricas.
#' @param transformacion código de [TRANSFORMACIONES]; por defecto
#'   `"estandarizar"` (estandarización por columnas, como en MULTBIPLOT).
#' @param log aplicar logaritmo antes de transformar.
#' @param dimensiones número de ejes a conservar en los resultados.
#' @param tipo `"HJ"` (Galindo-Villardón, 1986), `"GH"` o `"JK"` (Gabriel, 1971).
#' @param referencia convenio de signos: nombre de una variable activa que debe
#'   quedar en el lado positivo de todos los ejes. Con `NULL` se deja el signo
#'   que da la SVD (el mismo convenio que MultBiplotR).
#' @param invertir_ejes números de los ejes cuyo signo se invierte después de
#'   aplicar `referencia` (para dejar el gráfico como en otro programa).
#' @return objeto de clase `biplot_hj` con la inercia, las coordenadas, la
#'   calidad de representación y las contribuciones.
#' @export
#' @examples
#' pisa <- ejemplo_biplot("pisa")
#' res <- hj_biplot(pisa, variables = c("MATH", "READ", "SCIE", "CMPS", "CURIO",
#'                  "PERSEV", "COGABIL", "GOALSET", "SELFREG", "ENPROBS"))
#' res
#' grafico_biplot(res, grupos = pisa$REGION)
hj_biplot <- function(datos, variables = NULL, transformacion = "estandarizar",
                      log = FALSE, dimensiones = 3, tipo = c("HJ", "GH", "JK"),
                      referencia = NULL, invertir_ejes = integer()) {
  tipo <- match.arg(tipo)
  datos <- as.data.frame(datos, check.names = FALSE)

  if (is.null(variables)) {
    variables <- names(datos)[vapply(datos, is.numeric, logical(1))]
  }
  faltan <- setdiff(variables, names(datos))
  if (length(faltan)) {
    stop("No existen en los datos: ", paste(faltan, collapse = ", "), call. = FALSE)
  }
  if (length(variables) < 2) {
    stop("Selecciona al menos 2 variables activas.", call. = FALSE)
  }
  no_num <- variables[!vapply(datos[variables], is.numeric, logical(1))]
  if (length(no_num)) {
    stop("Estas variables no son numéricas: ", paste(no_num, collapse = ", "),
         ". Úsalas como variable de grupo, no como activas.", call. = FALSE)
  }
  X0 <- as.matrix(datos[, variables, drop = FALSE])
  if (anyNA(X0)) {
    filas_na <- rownames(datos)[!stats::complete.cases(X0)]
    stop("Hay ", sum(is.na(X0)), " datos faltantes en las variables activas (filas: ",
         paste(utils::head(filas_na, 8), collapse = ", "),
         if (length(filas_na) > 8) ", ..." else "",
         "). Elimínalos o impútalos antes del análisis.", call. = FALSE)
  }
  if (nrow(X0) < 3) stop("Se necesitan al menos 3 individuos.", call. = FALSE)
  rownames(X0) <- make.unique(rownames(datos) %||% as.character(seq_len(nrow(X0))))

  X <- transformar_matriz(X0, transformacion, log)

  # --- Descomposición en valores singulares ---------------------------------
  s <- svd(X)
  tol <- max(dim(X)) * max(s$d) * .Machine$double.eps
  r <- sum(s$d > tol)                           # rango de la matriz
  if (r < 2) stop("La matriz tiene rango menor que 2; no se puede hacer un biplot.",
                  call. = FALSE)
  d <- s$d[seq_len(r)]
  U <- s$u[, seq_len(r), drop = FALSE]
  V <- s$v[, seq_len(r), drop = FALSE]
  ejes <- paste("Eje", seq_len(r))

  # --- Orientación de los ejes (convenio de signos) --------------------------
  # El signo de cada eje de la SVD es arbitrario: cambiarlo refleja el gráfico
  # (efecto "espejo") sin alterar inercia, calidades, ángulos ni distancias.
  signos <- rep(1, r)
  if (!is.null(referencia) && nzchar(referencia)) {
    j <- match(referencia, colnames(X))
    if (is.na(j)) stop("La variable de referencia debe ser una variable activa.", call. = FALSE)
    signos <- ifelse(V[j, ] < 0, -1, 1)
  }
  invertir_ejes <- as.integer(invertir_ejes)
  invertir_ejes <- invertir_ejes[invertir_ejes >= 1 & invertir_ejes <= r]
  signos[invertir_ejes] <- -signos[invertir_ejes]
  U <- sweep(U, 2, signos, "*")
  V <- sweep(V, 2, signos, "*")

  # --- Marcadores según el tipo de biplot -----------------------------------
  D <- diag(d, nrow = r)
  filas    <- switch(tipo, HJ = U %*% D, GH = U,         JK = U %*% D)
  columnas <- switch(tipo, HJ = V %*% D, GH = V %*% D,   JK = V)
  dimnames(filas)    <- list(rownames(X), ejes)
  dimnames(columnas) <- list(colnames(X), ejes)

  # --- Calidad de representación (coseno al cuadrado) ------------------------
  # "Contribución relativa del factor al elemento" de MULTBIPLOT:
  # coordenada al cuadrado / suma de las coordenadas al cuadrado en todos los ejes.
  cos2 <- function(M) {
    tot <- rowSums(M^2)
    out <- M^2 / ifelse(tot > 0, tot, 1)
    out[tot == 0, ] <- 0
    out
  }
  calidad_filas    <- cos2(filas)
  calidad_columnas <- cos2(columnas)

  # --- Contribución del elemento a la inercia del eje ------------------------
  # Clásica del ACP: u_ik^2 (filas) y v_jk^2 (columnas); en cada eje suman 1.
  contrib_filas    <- U^2
  contrib_columnas <- V^2
  dimnames(contrib_filas)    <- dimnames(filas)
  dimnames(contrib_columnas) <- dimnames(columnas)

  # --- Inercia ----------------------------------------------------------------
  autovalores <- d^2
  inercia <- data.frame(
    Eje             = seq_len(r),
    `Valor propio`  = autovalores,
    `Valor singular` = d,
    `% Inercia`     = 100 * autovalores / sum(autovalores),
    `% Acumulado`   = cumsum(100 * autovalores / sum(autovalores)),
    check.names = FALSE
  )

  k <- max(2, min(as.integer(dimensiones), r))
  recorta <- function(M) M[, seq_len(k), drop = FALSE]

  structure(
    list(
      tipo             = tipo,
      transformacion   = transformacion,
      log              = isTRUE(log),
      referencia       = referencia,
      signos           = signos,
      variables        = variables,
      n                = nrow(X),
      p                = ncol(X),
      rango            = r,
      dimensiones      = k,
      inercia          = inercia,
      filas            = recorta(filas),
      columnas         = recorta(columnas),
      calidad_filas    = recorta(calidad_filas),
      calidad_columnas = recorta(calidad_columnas),
      contrib_filas    = recorta(contrib_filas),
      contrib_columnas = recorta(contrib_columnas),
      matriz           = X,
      original         = X0,
      svd              = list(u = U, d = d, v = V)
    ),
    class = "biplot_hj"
  )
}

#' Imprimir un HJ-Biplot
#' @param x objeto `biplot_hj`.
#' @param ... no se usa.
#' @export
print.biplot_hj <- function(x, ...) {
  etiqueta <- names(TRANSFORMACIONES)[TRANSFORMACIONES == x$transformacion]
  cat(sprintf("%s-Biplot  |  %d individuos x %d variables  |  %s%s\n",
              x$tipo, x$n, x$p, etiqueta, if (x$log) " (tras logaritmo)" else ""))
  cat("\nValores propios e inercia explicada:\n")
  inr <- x$inercia
  inr[-1] <- round(inr[-1], 3)
  print(inr, row.names = FALSE)
  cat(sprintf("\nEl plano 1-2 retiene el %.2f %% de la inercia.\n", x$inercia[2, 5]))
  invisible(x)
}

#' Tablas de resultados de un biplot
#'
#' Devuelve los resultados en forma de data.frames listos para mostrar o
#' exportar a Excel.
#'
#' @param res objeto devuelto por [hj_biplot()].
#' @param ejes los dos ejes del plano que se va a interpretar (para la columna
#'   de calidad en el plano).
#' @param unidad `"porcentaje"` o `"milesima"` (por mil, como MULTBIPLOT).
#' @param decimales número de decimales.
#' @export
tablas_biplot <- function(res, ejes = c(1, 2), unidad = c("milesima", "porcentaje"),
                          decimales = 3) {
  unidad <- match.arg(unidad)
  f <- if (unidad == "porcentaje") 100 else 1000
  dec_q <- if (unidad == "porcentaje") 1 else 0
  plano <- sprintf("Plano %d-%d", ejes[1], ejes[2])

  con_nombre <- function(M, nombre, escala = 1, dec = decimales, agregar_plano = FALSE) {
    df <- as.data.frame(round(M * escala, dec), check.names = FALSE)
    if (agregar_plano) {
      df[[plano]] <- round((M[, ejes[1]] + M[, ejes[2]]) * escala, dec)
    }
    cbind(stats::setNames(data.frame(rownames(M), check.names = FALSE), nombre), df,
          row.names = NULL)
  }

  inr <- res$inercia
  inr[-1] <- round(inr[-1], decimales)

  list(
    inercia            = inr,
    coordenadas_filas    = con_nombre(res$filas, "Individuo"),
    coordenadas_columnas = con_nombre(res$columnas, "Variable"),
    calidad_filas      = con_nombre(res$calidad_filas, "Individuo", f, dec_q, TRUE),
    calidad_columnas   = con_nombre(res$calidad_columnas, "Variable", f, dec_q, TRUE),
    contrib_filas      = con_nombre(res$contrib_filas, "Individuo", f, dec_q),
    contrib_columnas   = con_nombre(res$contrib_columnas, "Variable", f, dec_q),
    correlaciones      = {
      cm <- round(stats::cor(res$original), decimales)
      cbind(Variable = rownames(cm), as.data.frame(cm, check.names = FALSE), row.names = NULL)
    }
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a
