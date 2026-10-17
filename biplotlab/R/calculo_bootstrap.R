# =============================================================================
# Bootstrap del HJ-Biplot
# Sigue la metodología de biplots con bootstrap (artículo de 2015 visto en
# clase y paquete biplotbootGUI): se remuestrean los individuos con
# reemplazo, se repite el HJ-Biplot en cada réplica y se obtienen intervalos de
# confianza para valores propios, inercia, calidades (CRFE), contribuciones
# (CREF), longitudes de los vectores y ángulos.
#
# Como el signo de cada eje es arbitrario (y dos ejes con inercia parecida
# pueden intercambiarse de una réplica a otra), en cada réplica las
# coordenadas de las variables se alinean con las originales mediante una
# rotación de Procrustes antes de calcular coordenadas, longitudes y ángulos.
# Valores propios, inercia, CRFE y CREF se toman de los ejes de cada réplica.
# =============================================================================

#' Bootstrap de un HJ-Biplot
#'
#' @inheritParams hj_biplot
#' @param ejes plano para el que se calculan las estadísticas.
#' @param B número de réplicas bootstrap.
#' @param nivel nivel de confianza (por ejemplo 0.95).
#' @param semilla semilla aleatoria, para que el resultado sea reproducible.
#' @param progreso función opcional `function(i, B)` para informar el avance.
#' @return objeto de clase `bootstrap_biplot`.
#' @export
#' @examples
#' \donttest{
#' pisa <- ejemplo_biplot("pisa")
#' bt <- bootstrap_biplot(pisa, variables = names(pisa)[3:12], B = 200)
#' bt
#' grafico_bootstrap_biplot(bt)
#' }
bootstrap_biplot <- function(datos, variables, transformacion = "estandarizar", log = FALSE,
                             tipo = "HJ", referencia = NULL, invertir_ejes = integer(),
                             ejes = c(1, 2), B = 500, nivel = 0.95, semilla = 123,
                             progreso = NULL) {
  ejes <- as.integer(ejes)
  q <- max(ejes)
  B <- as.integer(B)
  if (B < 50) stop("Usa al menos 50 réplicas (se recomiendan 500 o más).", call. = FALSE)
  if (nivel <= 0.5 || nivel >= 1) stop("El nivel de confianza debe estar entre 0,5 y 1.", call. = FALSE)
  datos <- as.data.frame(datos, check.names = FALSE)

  original <- hj_biplot(datos, variables, transformacion = transformacion, log = log,
                        tipo = tipo, dimensiones = max(q, 3), referencia = referencia,
                        invertir_ejes = invertir_ejes)
  if (original$dimensiones < q) stop("La matriz no tiene ", q, " ejes.", call. = FALSE)
  K <- min(original$rango, q + 1)          # dimensiones usadas para alinear
  # Marcadores de las columnas con todos los ejes: V D (HJ y GH) o V (JK)
  marcadores <- function(res) {
    if (res$tipo == "JK") res$svd$v else res$svd$v %*% diag(res$svd$d, nrow = length(res$svd$d))
  }
  H0 <- marcadores(original)[, seq_len(K), drop = FALSE]
  vars <- original$variables
  a <- ejes[1]; b <- ejes[2]

  estadisticos <- function(res, alinear = TRUE) {
    H <- res$columnas[, seq_len(q), drop = FALSE]
    if (alinear) {
      # Rotación ortogonal de Procrustes de la réplica sobre la solución original
      Hb <- marcadores(res)[, seq_len(K), drop = FALSE]
      sv <- svd(crossprod(Hb, H0))
      H <- (Hb %*% (sv$u %*% t(sv$v)))[, seq_len(q), drop = FALSE]
    }
    lam <- res$inercia$`Valor propio`
    inr <- res$inercia$`% Inercia`
    crfe <- 1000 * res$calidad_columnas
    cref <- 1000 * res$contrib_columnas
    Hp <- H[, c(a, b), drop = FALSE]
    long <- sqrt(rowSums(Hp^2))
    ang_eje <- function(j) acos(pmax(-1, pmin(1, H[, j] / long))) * 180 / pi
    out <- c(
      stats::setNames(lam[seq_len(q)], paste0("Valor propio ", seq_len(q))),
      stats::setNames(inr[seq_len(q)], paste0("% Inercia eje ", seq_len(q))),
      stats::setNames(inr[a] + inr[b], sprintf("%% Inercia plano %d-%d", a, b)),
      stats::setNames(crfe[, a] + crfe[, b], paste0("CRFE plano: ", vars)),
      stats::setNames(crfe[, a], paste0("CRFE eje ", a, ": ", vars)),
      stats::setNames(crfe[, b], paste0("CRFE eje ", b, ": ", vars)),
      stats::setNames(cref[, a], paste0("CREF eje ", a, ": ", vars)),
      stats::setNames(cref[, b], paste0("CREF eje ", b, ": ", vars)),
      stats::setNames(long, paste0("Longitud: ", vars)),
      stats::setNames(ang_eje(a), paste0("Ángulo con eje ", a, ": ", vars)),
      stats::setNames(ang_eje(b), paste0("Ángulo con eje ", b, ": ", vars)),
      stats::setNames(Hp[, 1], paste0("Coordenada eje ", a, ": ", vars)),
      stats::setNames(Hp[, 2], paste0("Coordenada eje ", b, ": ", vars))
    )
    if (length(vars) >= 2) {
      pares <- utils::combn(length(vars), 2)
      angs <- apply(pares, 2, function(ij) {
        cosv <- sum(Hp[ij[1], ] * Hp[ij[2], ]) / (long[ij[1]] * long[ij[2]])
        acos(max(-1, min(1, cosv))) * 180 / pi
      })
      out <- c(out, stats::setNames(angs, paste0("Ángulo ", vars[pares[1, ]], " - ", vars[pares[2, ]])))
    }
    out
  }

  est0 <- estadisticos(original, alinear = FALSE)
  replicas <- matrix(NA_real_, nrow = B, ncol = length(est0), dimnames = list(NULL, names(est0)))
  set.seed(semilla)
  n <- nrow(datos)
  fallos <- 0L
  for (i in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    db <- datos[idx, , drop = FALSE]
    rownames(db) <- make.unique(rownames(datos)[idx])
    rb <- tryCatch(
      suppressWarnings(hj_biplot(db, variables, transformacion = transformacion, log = log,
                                 tipo = tipo, dimensiones = max(q, 3))),
      error = function(e) NULL)
    if (is.null(rb) || rb$dimensiones < q) { fallos <- fallos + 1L; next }
    replicas[i, ] <- estadisticos(rb)
    if (!is.null(progreso) && (i %% max(1, B %/% 50) == 0 || i == B)) progreso(i, B)
  }
  validas <- stats::complete.cases(replicas)
  replicas <- replicas[validas, , drop = FALSE]
  if (nrow(replicas) < 30) stop("Demasiadas réplicas fallidas; revisa los datos.", call. = FALSE)

  alfa <- (1 - nivel) / 2
  z <- stats::qnorm(1 - alfa)
  media <- colMeans(replicas)
  ee <- apply(replicas, 2, stats::sd)
  li <- apply(replicas, 2, stats::quantile, probs = alfa, names = FALSE)
  ls <- apply(replicas, 2, stats::quantile, probs = 1 - alfa, names = FALSE)
  categoria <- function(nm) {
    ifelse(grepl("^Valor propio|^% Inercia", nm), "Global",
    ifelse(grepl("^CRFE", nm), "CRFE (calidad)",
    ifelse(grepl("^CREF", nm), "CREF (contribución)",
    ifelse(grepl("^Longitud", nm), "Longitud de los vectores",
    ifelse(grepl("^Ángulo con eje", nm), "Ángulo variable-eje",
    ifelse(grepl("^Coordenada", nm), "Coordenadas de las variables",
           "Ángulo entre variables"))))))
  }
  resumen <- data.frame(
    Grupo = categoria(names(est0)),
    Estadístico = names(est0),
    Estimación = unname(est0),
    `Media bootstrap` = unname(media),
    Sesgo = unname(media - est0),
    `Error estándar` = unname(ee),
    `LI percentil` = li, `LS percentil` = ls,
    `LI normal` = unname(est0 - z * ee), `LS normal` = unname(est0 + z * ee),
    check.names = FALSE, row.names = NULL
  )

  structure(list(
    original = original, ejes = ejes, B = B, validas = nrow(replicas), fallos = fallos,
    nivel = nivel, semilla = semilla, estimacion = est0, replicas = replicas,
    resumen = resumen
  ), class = "bootstrap_biplot")
}

#' @export
print.bootstrap_biplot <- function(x, ...) {
  cat(sprintf("Bootstrap del %s-Biplot - %d réplicas válidas de %d - IC al %s %%\n\n",
              x$original$tipo, x$validas, x$B, format(100 * x$nivel)))
  r <- x$resumen[x$resumen$Grupo == "Global", -1]
  r[-1] <- round(r[-1], 3)
  print(r, row.names = FALSE)
  invisible(x)
}

#' Histograma de las réplicas bootstrap de un estadístico
#' @param bt objeto devuelto por [bootstrap_biplot()].
#' @param estadistico nombre del estadístico (columna `Estadístico` del resumen).
#' @export
grafico_bootstrap_hist <- function(bt, estadistico) {
  if (!estadistico %in% colnames(bt$replicas)) stop("Estadístico no encontrado.", call. = FALSE)
  v <- bt$replicas[, estadistico]
  fila <- bt$resumen[bt$resumen$Estadístico == estadistico, ]
  ggplot2::ggplot(data.frame(v = v), ggplot2::aes(.data$v)) +
    ggplot2::geom_histogram(bins = 30, fill = "#9cc0ea", colour = "white", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = fila$Estimación, colour = COLOR_TINTA, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = c(fila$`LI percentil`, fila$`LS percentil`),
                        colour = COLOR_VECTOR, linetype = "22", linewidth = 0.6) +
    ggplot2::labs(x = estadistico, y = "Réplicas",
                  subtitle = sprintf("Línea continua: estimación - discontinuas: IC percentil al %s %%",
                                     format(100 * bt$nivel))) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(colour = "#6b6a65", size = 9.5))
}

#' Gráfico Q-Q normal de las réplicas de un estadístico
#' @inheritParams grafico_bootstrap_hist
#' @export
grafico_bootstrap_qq <- function(bt, estadistico) {
  v <- bt$replicas[, estadistico]
  ggplot2::ggplot(data.frame(v = v), ggplot2::aes(sample = .data$v)) +
    ggplot2::stat_qq(colour = COLOR_FILAS, size = 1, alpha = 0.6) +
    ggplot2::stat_qq_line(colour = COLOR_VECTOR) +
    ggplot2::labs(x = "Cuantiles teóricos (normal)", y = "Cuantiles de las réplicas") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

#' Biplot de variables con regiones de confianza bootstrap
#'
#' Dibuja los vectores de la muestra original y, en el extremo de cada uno, la
#' elipse que contiene el `nivel` de las réplicas bootstrap (alineadas).
#' @inheritParams grafico_bootstrap_hist
#' @param colores_variables colores por variable (opcional).
#' @param tam_texto tamaño de las etiquetas.
#' @export
grafico_bootstrap_biplot <- function(bt, colores_variables = NULL, tam_texto = 3.2) {
  a <- bt$ejes[1]; b <- bt$ejes[2]
  vars <- bt$original$variables
  nube <- do.call(rbind, lapply(vars, function(v) {
    data.frame(variable = v,
               x = bt$replicas[, paste0("Coordenada eje ", a, ": ", v)],
               y = bt$replicas[, paste0("Coordenada eje ", b, ": ", v)])
  }))
  orig <- data.frame(variable = vars,
                     x = bt$estimacion[paste0("Coordenada eje ", a, ": ", vars)],
                     y = bt$estimacion[paste0("Coordenada eje ", b, ": ", vars)])
  col <- stats::setNames(rep(COLOR_VECTOR, length(vars)), vars)
  if (length(colores_variables)) col[names(colores_variables)] <- colores_variables
  inr <- bt$original$inercia
  flecha <- grid::arrow(length = grid::unit(0.16, "cm"), type = "closed")
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d3", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d3", linewidth = 0.4) +
    ggplot2::geom_point(data = nube, ggplot2::aes(.data$x, .data$y, colour = .data$variable),
                        size = 0.5, alpha = 0.15, show.legend = FALSE) +
    ggplot2::stat_ellipse(data = nube, ggplot2::aes(.data$x, .data$y, colour = .data$variable),
                          type = "norm", level = bt$nivel, linewidth = 0.5, show.legend = FALSE) +
    ggplot2::geom_segment(data = orig, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y,
                                                    colour = .data$variable),
                          linewidth = 0.6, arrow = flecha, show.legend = FALSE) +
    ggrepel::geom_text_repel(data = orig, ggplot2::aes(.data$x, .data$y, label = .data$variable),
                             colour = COLOR_TINTA, fontface = "bold", size = tam_texto,
                             nudge_x = 0.06 * orig$x, nudge_y = 0.06 * orig$y,
                             min.segment.length = Inf, seed = 2) +
    ggplot2::scale_colour_manual(values = col) +
    ggplot2::expand_limits(x = 0, y = 0) +          # el origen, de donde salen los vectores
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.08)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = sprintf("Eje %d (%s %%)", a, format(round(inr[a, 4], 2), decimal.mark = ",")),
      y = sprintf("Eje %d (%s %%)", b, format(round(inr[b, 4], 2), decimal.mark = ",")),
      subtitle = sprintf("Regiones de confianza al %s %% - %d réplicas",
                         format(100 * bt$nivel), bt$validas)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#efeee9", linewidth = 0.3),
                   plot.subtitle = ggplot2::element_text(colour = "#6b6a65", size = 10),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  con_proporcion_minima(p)
}
