# =============================================================================
# TABLAS: GGE-Biplot
#
# R/calculo_gge.R tiene las funciones de cálculo y los gráficos (las de la
# autora, sin cambios). Este archivo añade lo que necesita la app:
#   - calcular_gge(): prepara los datos de la app y llama a gge_centrado(),
#     gge_escalado() y gge_descomposicion();
#   - gge_tablas(): inercia, coordenadas, calidad (CRFE) y contribución (CREF);
#   - gge_tabla_herramienta(): la tabla de cada herramienta de Yan.
#
# Las tablas usan las coordenadas de la partición SIN el ajuste visual d del
# gráfico: en GH, JK y SQ el producto escalar fila · columna es la celda
# aproximada (Yan y Tinker, 2006, Ec. 1). d es un escalar común: no cambia
# ningún orden.
# =============================================================================

#' @rawNamespace export(gge_centrado, gge_escalado, gge_descomposicion, gge_resultados, gge_coordenadas_grafico, gge_biplot_basico)
#' @rawNamespace export(gge_examinar_ambiente, gge_examinar_genotipo, gge_relacion_ambientes, gge_comparar_genotipos, gge_quien_gano_donde)
#' @rawNamespace export(gge_dis_rep, gge_media_estabilidad, gge_ranking_ambientes, gge_ranking_genotipos)
NULL

# Códigos de las vistas (el menú con los nombres está en mod_gge.R)
GGE_HERRAMIENTAS <- c("biplot", "examinar_ambiente", "examinar_genotipo", "relacion_ambientes",
                      "comparar_genotipos", "quien_gano_donde", "discriminacion",
                      "media_estabilidad", "ranking_ambientes", "ranking_genotipos")

# -----------------------------------------------------------------------------
# 1. Cálculo para la app
# -----------------------------------------------------------------------------

#' GGE-Biplot: centrado, escalado y partición sobre los datos de la app
#'
#' Comprueba los datos y llama a [gge_centrado()], [gge_escalado()] y
#' [gge_descomposicion()] (R/calculo_gge.R).
#' @param datos data.frame con los individuos en filas (nombres de fila = etiquetas).
#' @param variables columnas que entran al análisis; por defecto, todas las numéricas.
#' @param centrado `"ninguno"`, `"global"`, `"tester"` o `"doble"`.
#' @param escalado `"no"` o `"std"`.
#' @param particion `"GH"`, `"JK"`, `"HJ"` o `"SQ"`.
#' @return la lista de [gge_descomposicion()] más `tabla` (matriz analizada),
#'   `dimensiones`, `inercia`, `centrado`, `escalado`, `datos`, `n` y `p`.
calcular_gge <- function(datos, variables = NULL, centrado = "tester", escalado = "no",
                         particion = "GH") {
  if (is.null(variables)) variables <- names(datos)[vapply(datos, is.numeric, logical(1))]
  faltan <- setdiff(variables, colnames(datos))
  if (length(faltan)) stop("No existen estas columnas: ", paste(faltan, collapse = ", "), call. = FALSE)
  no_num <- variables[!vapply(as.data.frame(datos)[variables], is.numeric, logical(1))]
  if (length(no_num)) stop("Estas columnas no son numéricas: ", paste(no_num, collapse = ", "), call. = FALSE)
  if (length(variables) < 2) stop("Elige al menos 2 variables.", call. = FALSE)

  X <- as.matrix(as.data.frame(datos)[, variables, drop = FALSE])
  if (anyNA(X)) stop("Hay datos faltantes en las columnas elegidas: el GGE necesita la tabla completa.",
                     call. = FALSE)
  if (nrow(X) < 3) stop("Hacen falta al menos 3 individuos.", call. = FALSE)
  if (is.null(rownames(X)) || anyDuplicated(rownames(X))) {
    stop("Las filas necesitan nombres únicos (la primera columna del archivo).", call. = FALSE)
  }

  res <- gge_descomposicion(gge_escalado(gge_centrado(X, centrado), escalado), particion)

  res$tabla <- res$datos_centrados
  # Solo los ejes con valor singular distinto de cero (el centrado quita rango)
  res$dimensiones <- max(2L, sum(res$valor_singular > 1e-10 * res$valor_singular[1]))
  res$inercia <- gge_resultados(res)$inercia
  res$centrado <- centrado
  res$escalado <- escalado
  res$datos <- X
  res$n <- nrow(X)
  res$p <- ncol(X)
  res
}

#' Tablas generales del GGE-Biplot (inercia, coordenadas, CRFE y CREF)
#' @param res resultado de [calcular_gge()].
#' @param ejes plano (columna `Plano`).
#' @param etiqueta_filas,etiqueta_columnas nombre de la primera columna.
gge_tablas <- function(res, ejes = c(1, 2), etiqueta_filas = "Genotipo",
                       etiqueta_columnas = "Ambiente") {
  r <- gge_resultados(res, ejes)
  k <- res$dimensiones
  plano <- sprintf("Plano %d-%d", ejes[1], ejes[2])
  a_tabla <- function(m, etiqueta, columnas = seq_len(k), con_plano = FALSE) {
    t <- as.data.frame(m[, c(columnas, if (con_plano) ncol(m)), drop = FALSE], check.names = FALSE)
    if (con_plano) names(t)[ncol(t)] <- plano
    cbind(stats::setNames(data.frame(rownames(m)), etiqueta), t)
  }
  list(
    inercia              = r$inercia[seq_len(k), ],
    coordenadas_filas    = a_tabla(res$filas, etiqueta_filas),
    coordenadas_columnas = a_tabla(res$columnas, etiqueta_columnas),
    calidad_filas        = a_tabla(round(r$calidad_filas), etiqueta_filas, con_plano = TRUE),
    calidad_columnas     = a_tabla(round(r$calidad_columnas), etiqueta_columnas, con_plano = TRUE),
    contrib_filas        = a_tabla(round(r$contrib_filas), etiqueta_filas),
    contrib_columnas     = a_tabla(round(r$contrib_columnas), etiqueta_columnas)
  )
}

# -----------------------------------------------------------------------------
# 2. Tabla de cada herramienta
# -----------------------------------------------------------------------------

# Coordenadas de la partición en el plano (sin el ajuste d)
gge_plano <- function(res, ejes) {
  list(g = res$filas[, ejes, drop = FALSE], e = res$columnas[, ejes, drop = FALSE])
}

# Texto de la posición respecto al promedio
gge_posicion <- function(v) ifelse(v > 0, "Encima del promedio", "Debajo del promedio")

# Ángulo (grados) entre dos vectores
gge_angulo <- function(a, b) {
  acos(max(-1, min(1, sum(a * b) / sqrt(sum(a^2) * sum(b^2))))) * 180 / pi
}

# Ordena, numera y pone el nombre de la primera columna
gge_ordenar <- function(tabla, orden, etiqueta, puesto = TRUE) {
  tabla <- tabla[orden, , drop = FALSE]
  if (puesto) tabla$Puesto <- seq_len(nrow(tabla))
  names(tabla)[if (puesto) 2 else 1] <- etiqueta
  rownames(tabla) <- NULL
  tabla
}

#' Tabla de una herramienta de Yan
#'
#' Mismos cálculos que el gráfico de R/calculo_gge.R, con las coordenadas de la
#' partición (sin el ajuste d).
#' @param res resultado de [calcular_gge()].
#' @param herramienta ver `GGE_HERRAMIENTAS`.
#' @param ejes los dos ejes del plano.
#' @param ambiente,genotipo,genotipo1,genotipo2 lo que pide cada herramienta.
#' @param etiqueta_filas,etiqueta_columnas nombres de la primera columna.
#' @return lista con `tabla` y, según la herramienta, `tabla2` y `nota`
#'   (`NULL` en el biplot).
gge_tabla_herramienta <- function(res, herramienta, ejes = c(1, 2), ambiente = NULL,
                                  genotipo = NULL, genotipo1 = NULL, genotipo2 = NULL,
                                  etiqueta_filas = "Genotipo", etiqueta_columnas = "Ambiente") {
  p <- gge_plano(res, ejes)
  X <- res$tabla
  filas <- rownames(p$g); columnas <- rownames(p$e)
  media <- colMeans(p$e)                         # ambiente medio (AEA)
  u <- media / sqrt(sum(media^2))

  switch(herramienta,
    biplot = NULL,

    examinar_ambiente = {
      if (!(ambiente %in% columnas)) stop("Ambiente no encontrado. Usa: ", paste(columnas, collapse = ", "), call. = FALSE)
      est <- as.vector(p$g %*% p$e[ambiente, ])
      t <- data.frame(Puesto = NA_integer_, Fila = filas, `Producto escalar` = est,
                      `Valor en la tabla` = as.vector(X[, ambiente]),
                      `Posición` = gge_posicion(est), check.names = FALSE)
      list(tabla = gge_ordenar(t, order(est, decreasing = TRUE), etiqueta_filas))
    },

    examinar_genotipo = {
      if (!(genotipo %in% filas)) stop("Genotipo no encontrado. Usa: ", paste(filas, collapse = ", "), call. = FALSE)
      est <- as.vector(p$e %*% p$g[genotipo, ])
      t <- data.frame(Puesto = NA_integer_, Columna = columnas, `Producto escalar` = est,
                      `Valor en la tabla` = as.vector(X[genotipo, ]),
                      `Posición` = gge_posicion(est), check.names = FALSE)
      list(tabla = gge_ordenar(t, order(est, decreasing = TRUE), etiqueta_columnas))
    },

    relacion_ambientes = {
      largo <- sqrt(rowSums(p$e^2))
      coseno <- (p$e %*% t(p$e)) / outer(largo, largo)
      a_tabla <- function(m) cbind(stats::setNames(data.frame(rownames(m)), etiqueta_columnas),
                                   as.data.frame(m, check.names = FALSE))
      list(tabla = a_tabla(coseno), tabla2 = a_tabla(stats::cor(X)),
           nota = if (!res$particion %in% c("GH", "HJ"))
             "Yan y Tinker (2006): la relación entre variables solo se ve bien con SVP = GH (f = 0). HJ da las mismas variables que GH.")
    },

    comparar_genotipos = {
      est <- as.vector(p$e %*% (p$g[genotipo1, ] - p$g[genotipo2, ]))
      t <- data.frame(Columna = columnas, `Diferencia (producto escalar)` = est,
                      `Diferencia en la tabla` = as.vector(X[genotipo1, ] - X[genotipo2, ]),
                      `Mejor según el biplot` = ifelse(est > 0, genotipo1, genotipo2),
                      check.names = FALSE)
      names(t)[1] <- etiqueta_columnas
      list(tabla = t)
    },

    quien_gano_donde = {
      est <- p$g %*% t(p$e)                      # individuos × variables
      gan_biplot <- filas[apply(est, 2, which.max)]
      gan_tabla <- rownames(X)[apply(X, 2, which.max)]
      t <- data.frame(Columna = columnas, `Ganador según el biplot` = gan_biplot,
                      `Ganador en la tabla` = gan_tabla,
                      Coinciden = ifelse(gan_biplot == gan_tabla, "Sí", "No"), check.names = FALSE)
      names(t)[1] <- etiqueta_columnas
      sectores <- split(columnas, gan_biplot)
      t2 <- data.frame(Fila = names(sectores),
                       Columnas = vapply(sectores, paste, character(1), collapse = ", "),
                       `Número` = vapply(sectores, length, integer(1)), check.names = FALSE)
      names(t2)[1:2] <- c(paste(etiqueta_filas, "ganador (vértice)"), paste(etiqueta_columnas, "del sector"))
      t2 <- t2[order(-t2$`Número`), ]
      rownames(t2) <- NULL
      list(tabla = t, tabla2 = t2)
    },

    discriminacion = {
      largo <- sqrt(rowSums(p$e^2))
      angulo <- apply(p$e, 1, gge_angulo, b = media)
      t <- data.frame(Columna = columnas, `Longitud del vector (discriminación)` = largo,
                      `Ángulo con el AEA (°)` = angulo,
                      `Coseno con el AEA (representatividad)` = cos(angulo * pi / 180),
                      `Desv. estándar en la tabla` = apply(X, 2, stats::sd), check.names = FALSE)
      list(tabla = gge_ordenar(t, order(-largo), etiqueta_columnas, puesto = FALSE),
           nota = if (!res$particion %in% c("GH", "HJ"))
             "La longitud de los vectores de las variables aproxima su desviación estándar con centrado por variable y SVP = GH (Yan y Tinker, 2006).")
    },

    media_estabilidad = {
      proy <- as.vector(p$g %*% u)               # sombra sobre el AEA
      pie <- proy %*% t(u)
      t <- data.frame(Puesto = NA_integer_, Fila = filas,
                      `Proyección sobre el AEA (media)` = proy,
                      `Media estimada (producto escalar)` = as.vector(p$g %*% media),
                      `Media en la tabla` = rowMeans(X),
                      `Distancia al AEA (inestabilidad)` = sqrt(rowSums((p$g - pie)^2)),
                      check.names = FALSE)
      list(tabla = gge_ordenar(t, order(proy, decreasing = TRUE), etiqueta_filas),
           nota = if (!identical(res$particion, "JK"))
             "Yan y Tinker (2006) usan SVP = JK (f = 1) para evaluar individuos: así la distancia al AEA está en la métrica de los individuos.")
    },

    ranking_ambientes = {
      largo <- sqrt(rowSums(p$e^2))
      ideal <- u * max(largo)                    # como gge_ranking_ambientes()
      dist <- sqrt(rowSums(sweep(p$e, 2, ideal)^2))
      t <- data.frame(Puesto = NA_integer_, Columna = columnas, `Distancia al ideal` = dist,
                      `Longitud del vector` = largo,
                      `Ángulo con el AEA (°)` = apply(p$e, 1, gge_angulo, b = media),
                      check.names = FALSE)
      list(tabla = gge_ordenar(t, order(dist), etiqueta_columnas),
           nota = if (!res$particion %in% c("GH", "HJ"))
             "Para evaluar variables Yan y Tinker (2006) recomiendan SVP = GH (f = 0).")
    },

    ranking_genotipos = {
      proy <- as.vector(p$g %*% u)
      ideal <- max(proy, 0) * u                  # como gge_ranking_genotipos()
      dist <- sqrt(rowSums(sweep(p$g, 2, ideal)^2))
      t <- data.frame(Puesto = NA_integer_, Fila = filas, `Distancia al ideal` = dist,
                      `Proyección sobre el AEA (media)` = proy,
                      `Media en la tabla` = rowMeans(X), check.names = FALSE)
      list(tabla = gge_ordenar(t, order(dist), etiqueta_filas),
           nota = if (!identical(res$particion, "JK"))
             "Para evaluar individuos Yan y Tinker (2006) recomiendan SVP = JK (f = 1).")
    },

    stop("Herramienta no reconocida. Usa: ", paste(GGE_HERRAMIENTAS, collapse = ", "), call. = FALSE)
  )
}
