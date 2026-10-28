# =============================================================================
# Gráfico del biplot (ggplot2) y guía automática de lectura.
# Lo usan el HJ-Biplot, el Clúster y los módulos que devuelvan filas y columnas.
# =============================================================================

# Paleta categórica por defecto (orden fijo). El usuario puede cambiar cada color.
PALETA_GRUPOS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
                   "#4a3aa7", "#e34948", "#7a5c3e", "#0e7490", "#9d174d", "#4d7c0f")
FORMAS_GRUPOS <- c(16, 17, 15, 18, 3, 4, 8, 1, 2, 0, 5, 6)
COLOR_FILAS   <- "#2a78d6"
COLOR_VECTOR  <- "#9a3412"
COLOR_TINTA   <- "#1f1f1d"
COLOR_SUAVE   <- "#a8a7a1"

#' Colores por defecto para los niveles de un grupo
#' @param niveles niveles del grupo.
#' @export
colores_por_defecto <- function(niveles) {
  stats::setNames(rep_len(PALETA_GRUPOS, length(niveles)), niveles)
}

#' Gráfico de un biplot
#'
#' @param res objeto devuelto por [hj_biplot()].
#' @param ejes los dos ejes a representar.
#' @param grupos vector (uno por individuo) para colorear los puntos, opcional.
#' @param etiquetas_filas,etiquetas_columnas mostrar los nombres.
#' @param calidad_min_filas,calidad_min_columnas calidad mínima en el plano
#'   (CRFE acumulada, de 0 a 1000) para mostrar un individuo o una variable.
#' @param mal_representados `"atenuar"` (gris, sin etiqueta) u `"ocultar"`.
#' @param calidad_min (compatibilidad) umbral común en porcentaje.
#' @param escala_vectores multiplicador visual de los vectores. Con `"auto"` el
#'   HJ no se reescala y en GH/JK se usa el factor de MultBiplotR (Salamanca)
#'   para que filas y columnas se vean; no cambia ángulos ni proyecciones.
#' @param invertir vector lógico de longitud 2 para reflejar cada eje solo en el
#'   gráfico (para orientar también las tablas usa `invertir_ejes` en
#'   [hj_biplot()]).
#' @param mostrar `"ambos"`, `"filas"` o `"columnas"`.
#' @param colores_grupos colores de los grupos (con nombre = nivel).
#' @param color_filas color de los individuos cuando no hay grupos.
#' @param color_vectores color de los vectores.
#' @param colores_variables colores por variable (con nombre), p. ej. por bloques.
#' @param envolventes dibujar la envolvente convexa de cada grupo.
#' @param tam_texto,tam_puntos tamaños.
#' @param titulo título opcional.
#' @export
grafico_biplot <- function(res, ejes = c(1, 2), grupos = NULL,
                           etiquetas_filas = TRUE, etiquetas_columnas = TRUE,
                           calidad_min_filas = 0, calidad_min_columnas = 0,
                           mal_representados = c("atenuar", "ocultar"),
                           calidad_min = NULL, escala_vectores = "auto",
                           invertir = c(FALSE, FALSE),
                           mostrar = c("ambos", "filas", "columnas"),
                           colores_grupos = NULL, color_filas = COLOR_FILAS,
                           color_vectores = COLOR_VECTOR, colores_variables = NULL,
                           envolventes = FALSE, tam_texto = 3.2, tam_puntos = 2.2,
                           titulo = NULL) {
  mostrar <- match.arg(mostrar)
  mal_representados <- match.arg(mal_representados)
  if (!is.null(calidad_min)) {
    calidad_min_filas <- calidad_min_columnas <- 10 * calidad_min
  }
  ejes <- as.integer(ejes)
  if (any(ejes > ncol(res$filas))) {
    stop("Solo se calcularon ", ncol(res$filas), " ejes.", call. = FALSE)
  }
  signo <- ifelse(invertir, -1, 1)

  F <- sweep(res$filas[, ejes, drop = FALSE], 2, signo, "*")
  G <- sweep(res$columnas[, ejes, drop = FALSE], 2, signo, "*")

  # --- Escala visual -----------------------------------------------------------
  # GH y JK: factor de MultBiplotR, scf = (media de b^2 / media de a^2)^(1/4);
  # filas * scf y columnas / scf (los productos escalares no cambian).
  scf <- 1
  if (res$tipo != "HJ") {
    sca <- sum(res$filas^2) / nrow(res$filas)
    scb <- sum(res$columnas^2) / nrow(res$columnas)
    scf <- sqrt(sqrt(scb / sca))
    F <- F * scf
    G <- G / scf
  }
  f_usuario <- if (identical(escala_vectores, "auto")) 1 else as.numeric(escala_vectores)
  G <- G * f_usuario

  # --- Calidad en el plano (CRFE acumulada, 0-1000) -----------------------------
  q_f <- 1000 * (res$calidad_filas[, ejes[1]] + res$calidad_filas[, ejes[2]])
  q_c <- 1000 * (res$calidad_columnas[, ejes[1]] + res$calidad_columnas[, ejes[2]])

  df_f <- data.frame(x = F[, 1], y = F[, 2], etiqueta = rownames(F),
                     ok = q_f >= calidad_min_filas, stringsAsFactors = FALSE)
  df_c <- data.frame(x = G[, 1], y = G[, 2], etiqueta = rownames(G),
                     ok = q_c >= calidad_min_columnas, stringsAsFactors = FALSE)
  df_c$color <- color_vectores
  if (length(colores_variables)) {
    m <- match(df_c$etiqueta, names(colores_variables))
    df_c$color[!is.na(m)] <- colores_variables[m[!is.na(m)]]
  }

  # --- Grupos --------------------------------------------------------------------
  hay_grupos <- !is.null(grupos) && length(grupos) == nrow(df_f)
  if (hay_grupos) {
    g <- as.character(grupos)
    g[is.na(g) | g == ""] <- "(sin dato)"
    niveles <- if (is.factor(grupos)) intersect(levels(grupos), unique(g)) else
      sort(unique(g))
    niveles <- c(niveles, setdiff(unique(g), niveles))
    df_f$grupo <- factor(g, levels = niveles)
    pal <- colores_por_defecto(niveles)
    if (length(colores_grupos)) {
      if (is.null(names(colores_grupos))) names(colores_grupos) <- niveles[seq_along(colores_grupos)]
      comunes <- intersect(names(colores_grupos), niveles)
      pal[comunes] <- colores_grupos[comunes]
    }
    formas <- stats::setNames(rep_len(FORMAS_GRUPOS, length(niveles)), niveles)
  }

  inr <- res$inercia
  lab_eje <- function(k) sprintf("Eje %d (%s %%)", k,
                                 format(round(inr[k, 4], 2), decimal.mark = ","))

  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d3", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d3", linewidth = 0.4)

  visibles_f <- if (mal_representados == "ocultar") df_f[df_f$ok, , drop = FALSE] else df_f
  visibles_c <- if (mal_representados == "ocultar") df_c[df_c$ok, , drop = FALSE] else df_c

  # --- Filas (individuos) ----------------------------------------------------------
  if (mostrar != "columnas") {
    malas <- df_f[!df_f$ok, , drop = FALSE]
    buenas <- df_f[df_f$ok, , drop = FALSE]
    if (hay_grupos && envolventes) {
      cascos <- do.call(rbind, lapply(split(buenas, buenas$grupo), function(d) {
        if (nrow(d) < 3) return(NULL)
        d[grDevices::chull(d$x, d$y), ]
      }))
      if (!is.null(cascos) && nrow(cascos)) {
        p <- p + ggplot2::geom_polygon(
          data = cascos, ggplot2::aes(.data$x, .data$y, fill = .data$grupo,
                                      colour = .data$grupo, group = .data$grupo),
          alpha = 0.08, linewidth = 0.4, show.legend = FALSE) +
          ggplot2::scale_fill_manual(values = pal, drop = FALSE, guide = "none")
      }
    }
    if (nrow(malas) && mal_representados == "atenuar") {
      p <- p + ggplot2::geom_point(data = malas, ggplot2::aes(.data$x, .data$y),
                                   colour = "#d6d5cf", size = tam_puntos * 0.8)
    }
    if (hay_grupos) {
      p <- p +
        ggplot2::geom_point(data = buenas,
                            ggplot2::aes(.data$x, .data$y, colour = .data$grupo,
                                         shape = .data$grupo), size = tam_puntos + 0.2) +
        ggplot2::scale_colour_manual(values = pal, name = NULL, drop = FALSE) +
        ggplot2::scale_shape_manual(values = formas, name = NULL, drop = FALSE)
    } else {
      p <- p + ggplot2::geom_point(data = buenas, ggplot2::aes(.data$x, .data$y),
                                   colour = color_filas, size = tam_puntos)
    }
    if (etiquetas_filas && nrow(buenas)) {
      p <- p + ggrepel::geom_text_repel(
        data = buenas, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta),
        size = tam_texto * 0.9, colour = "#52514e", max.overlaps = 25,
        segment.colour = "#c9c8c2", segment.size = 0.3, seed = 1)
    }
  }

  # --- Columnas (variables) ---------------------------------------------------------
  if (mostrar != "filas") {
    flecha <- grid::arrow(length = grid::unit(0.16, "cm"), type = "closed")
    malas_c <- df_c[!df_c$ok, , drop = FALSE]
    buenas_c <- df_c[df_c$ok, , drop = FALSE]
    if (nrow(malas_c) && mal_representados == "atenuar") {
      p <- p + ggplot2::geom_segment(
        data = malas_c, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
        colour = "#d6d5cf", linewidth = 0.5, arrow = flecha, linetype = "22")
    }
    if (nrow(buenas_c)) {
      p <- p + ggplot2::geom_segment(
        data = buenas_c, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
        colour = buenas_c$color, linewidth = 0.6, arrow = flecha)
    }
    etiq_c <- visibles_c
    if (etiquetas_columnas && nrow(etiq_c)) {
      col_txt <- ifelse(etiq_c$ok,
                        ifelse(etiq_c$color == color_vectores, COLOR_TINTA, etiq_c$color),
                        COLOR_SUAVE)
      p <- p + ggrepel::geom_text_repel(
        data = etiq_c, ggplot2::aes(.data$x, .data$y, label = .data$etiqueta),
        colour = col_txt, fontface = "bold",
        size = tam_texto, max.overlaps = Inf, min.segment.length = Inf,
        nudge_x = 0.04 * etiq_c$x, nudge_y = 0.04 * etiq_c$y, seed = 2)
    }
  }

  # --- Encuadre rectangular (Ecuaciones_encuadre_biplot, ecuaciones 1 y 2) ---------
  # Límites de las dos nubes visibles; se incluye el origen cuando las variables
  # se dibujan como vectores desde él.
  todos <- rbind(if (mostrar != "columnas") visibles_f[, c("x", "y")],
                 if (mostrar != "filas") visibles_c[, c("x", "y")])
  if (is.null(todos) || !nrow(todos)) todos <- data.frame(x = c(-1, 1), y = c(-1, 1))
  con_origen <- mostrar != "filas"
  lim <- limites_encuadre(todos$x, todos$y, incluir_origen = con_origen)

  subt <- sprintf("%s-Biplot - %s%s", res$tipo,
                  names(TRANSFORMACIONES)[TRANSFORMACIONES == res$transformacion],
                  if (abs(f_usuario - 1) > 1e-8) sprintf(" - vectores ×%s",
                    format(signif(f_usuario, 3), decimal.mark = ",")) else "")
  p <- p +
    # Misma unidad en los dos ejes; los límites (ya ampliados para las etiquetas)
    # fijan la proporción ancho/alto = delta_x / delta_y (ecuación 3).
    ggplot2::coord_equal(xlim = lim$x, ylim = lim$y, expand = FALSE) +
    ggplot2::labs(x = lab_eje(ejes[1]), y = lab_eje(ejes[2]), title = titulo,
                  subtitle = subt) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#efeee9", linewidth = 0.3),
      plot.subtitle = ggplot2::element_text(colour = "#6b6a65", size = 10),
      axis.title = ggplot2::element_text(colour = "#52514e", size = 10.5),
      axis.text = ggplot2::element_text(colour = "#8a8984", size = 9),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
  attr(p, "aspecto") <- diff(lim$y) / diff(lim$x)
  p
}

#' Límites del encuadre rectangular de un biplot
#'
#' Ecuaciones 1 y 2 del documento de encuadre: límites de las nubes
#' (incluido el origen si se pide) y amplitudes. Después se amplían los
#' límites en una proporción `margen` de cada amplitud para dejar sitio a las
#' etiquetas, y las amplitudes se recalculan con los límites ampliados.
#' @param x,y coordenadas de todos los elementos dibujados (individuos y
#'   extremos de los vectores).
#' @param incluir_origen incluir el punto (0, 0) en los límites.
#' @param margen ampliación de cada lado, como fracción de la amplitud.
#' @param proporcion_min proporción mínima ancho/alto (el plano no puede ser
#'   más alto que ancho).
#' @return lista con `x` y `y` (límites ampliados) y `dx`, `dy` (amplitudes).
#' @export
limites_encuadre <- function(x, y, incluir_origen = TRUE, margen = 0.08,
                             proporcion_min = PROPORCION_MIN_BIPLOT) {
  x <- x[is.finite(x)]; y <- y[is.finite(y)]
  if (incluir_origen) { x <- c(0, x); y <- c(0, y) }
  x_min <- min(x); x_max <- max(x)             # ecuación 1
  y_min <- min(y); y_max <- max(y)
  dx <- x_max - x_min; dy <- y_max - y_min     # ecuación 2
  if (dx <= 0) dx <- max(dy, 1)                # las amplitudes deben ser positivas
  if (dy <= 0) dy <- max(dx, 1)
  xl <- c(x_min, x_min + dx) + c(-1, 1) * margen * dx
  yl <- c(y_min, y_min + dy) + c(-1, 1) * margen * dy
  # El plano nunca es más alto que ancho: el eje horizontal (el de más inercia)
  # es el lado largo. Si hace falta, se amplía el eje horizontal por igual a
  # los dos lados; la escala sigue siendo la misma en los dos ejes.
  if (diff(xl) < proporcion_min * diff(yl)) {
    xl <- mean(xl) + c(-1, 1) * proporcion_min * diff(yl) / 2
  }
  list(x = xl, y = yl, dx = diff(xl), dy = diff(yl))
}

#' Proporción mínima ancho/alto del plano
#'
#' El primer eje absorbe al menos tanta inercia como el segundo, así que el
#' plano se dibuja apaisado: ancho/alto nunca menor que este valor (4:3).
#' @export
PROPORCION_MIN_BIPLOT <- 4 / 3

#' Aplica la proporción mínima a un gráfico con límites automáticos
#' @param p gráfico de ggplot2 con `coord_equal()`.
#' @param proporcion_min proporción mínima ancho/alto.
#' @export
con_proporcion_minima <- function(p, proporcion_min = PROPORCION_MIN_BIPLOT) {
  r <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  xl <- r$x.range; yl <- r$y.range
  if (diff(xl) >= proporcion_min * diff(yl)) return(p)
  xl <- mean(xl) + c(-1, 1) * proporcion_min * diff(yl) / 2
  suppressMessages(p + ggplot2::coord_equal(xlim = xl, ylim = yl, expand = FALSE))
}

#' Dimensiones del gráfico con encuadre rectangular
#'
#' Ecuaciones 3 a 5 del documento de encuadre. Con una ventana disponible de
#' ancho `W` y alto `H`, calcula la escala común `s` (unidades de pantalla por
#' unidad de coordenadas) y el tamaño del área interior, `s * delta_x` por
#' `s * delta_y`, de modo que ancho/alto = delta_x / delta_y. A ese tamaño se
#' suman los márgenes fijos (títulos, ejes y leyenda), medidos sobre el propio
#' gráfico, para obtener el tamaño total del dibujo.
#' @param p gráfico de ggplot2 con `coord_equal()`.
#' @param W,H ancho y alto disponibles.
#' @param unidad `"px"` (pantalla) o `"in"` (pulgadas, para guardar archivos).
#' @param res píxeles por pulgada cuando `unidad = "px"`.
#' @return lista con `ancho`, `alto` (tamaño total), `ancho_panel`,
#'   `alto_panel` (área interior), `s`, `dx`, `dy`, `xlim` y `ylim`.
#' @export
encuadre_biplot <- function(p, W, H, unidad = c("px", "in"), res = 96) {
  unidad <- match.arg(unidad)
  a_pulgadas <- if (unidad == "px") 1 / res else 1
  construido <- ggplot2::ggplot_build(p)
  rangos <- construido$layout$panel_params[[1]]
  xlim <- rangos$x.range; ylim <- rangos$y.range
  dx <- diff(xlim); dy <- diff(ylim)
  if (!(dx > 0 && dy > 0)) stop("Las amplitudes deben ser positivas.", call. = FALSE)

  # Márgenes fijos: todo lo que no es el área interior del gráfico
  tabla <- ggplot2::ggplot_gtable(construido)
  panel <- tabla$layout[grepl("^panel", tabla$layout$name), , drop = FALSE]
  cols <- unique(unlist(Map(seq, panel$l, panel$r)))
  filas <- unique(unlist(Map(seq, panel$t, panel$b)))
  # Se miden en un dispositivo del mismo tipo que el de destino (fuentes iguales)
  previo <- grDevices::dev.cur()
  if (unidad == "px") {
    temporal <- tempfile(fileext = ".png")
    grDevices::png(temporal, width = W, height = H, res = res)
  } else grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    if (previo > 1) grDevices::dev.set(previo)
    if (unidad == "px") unlink(temporal)
  }, add = TRUE)
  fijo_w <- sum(grid::convertWidth(tabla$widths[-cols], "in", valueOnly = TRUE))
  fijo_h <- sum(grid::convertHeight(tabla$heights[-filas], "in", valueOnly = TRUE))

  # Ecuación 4: escala común en la ventana disponible (en pulgadas)
  W_in <- W * a_pulgadas; H_in <- H * a_pulgadas
  s <- min((W_in - fijo_w) / dx, (H_in - fijo_h) / dy)
  # Ecuación 5: dimensiones del área interior y del gráfico completo
  ancho_panel <- s * dx; alto_panel <- s * dy
  vuelta <- if (unidad == "px") res else 1
  list(ancho = (ancho_panel + fijo_w) * vuelta, alto = (alto_panel + fijo_h) * vuelta,
       ancho_panel = ancho_panel * vuelta, alto_panel = alto_panel * vuelta,
       s = s * vuelta, dx = dx, dy = dy, xlim = xlim, ylim = ylim, unidad = unidad)
}

# --- Ayudas para Shiny: el mismo encuadre en pantalla y en las descargas ------------
ALTO_VENTANA_BIPLOT <- 720   # alto disponible en pantalla (px)

render_biplot <- function(grafico, session, salida = "biplot", alto = ALTO_VENTANA_BIPLOT) {
  tam <- shiny::reactive({
    W <- session$clientData[[paste0("output_", session$ns(salida), "_width")]] %||% 900
    encuadre_biplot(grafico(), W = W, H = alto)
  })
  shiny::renderPlot(suppressWarnings(print(traducir_grafico(grafico()))),
                    width = function() max(200, floor(tam()$ancho)),
                    height = function() max(200, floor(tam()$alto)), res = 96)
}

descarga_biplot <- function(grafico, nombre, ext, ..., W = 10, H = 8) {
  shiny::downloadHandler(
    filename = nombre,
    content = function(file) {
      e <- encuadre_biplot(grafico(), W = W, H = H, unidad = "in")
      suppressWarnings(ggplot2::ggsave(file, traducir_grafico(grafico()), width = e$ancho, height = e$alto,
                                       units = "in", ...))
    })
}

#' Guía automática de lectura de un biplot
#'
#' Sigue el orden de interpretación visto en clase: inercia, calidad de
#' representación, variables, individuos y posibles atípicos.
#'
#' @param res objeto devuelto por [hj_biplot()].
#' @param ejes plano que se interpreta.
#' @param umbral CRFE en el plano (0-1000) a partir de la cual un elemento se
#'   considera bien representado.
#' @return vector de frases.
#' @export
lectura_biplot <- function(res, ejes = c(1, 2), umbral = 500) {
  pct <- function(v) paste0(format(round(v, 1), decimal.mark = ","), " %")
  pm <- function(v) paste0(round(v), " por mil")
  lista <- function(v, max = 6) {
    if (!length(v)) return("ninguno")
    paste0(paste(utils::head(v, max), collapse = ", "),
           if (length(v) > max) sprintf(" y %d más", length(v) - max) else "")
  }
  a <- ejes[1]; b <- ejes[2]
  inr <- res$inercia
  plano <- inr[a, 4] + inr[b, 4]
  out <- character()

  out <- c(out, sprintf(
    "**Inercia.** El plano %d-%d retiene el %s de la inercia (eje %d: %s; eje %d: %s). %s",
    a, b, pct(plano), a, pct(inr[a, 4]), b, pct(inr[b, 4]),
    if (plano >= 70) "Es un plano adecuado para interpretar." else
      if (plano >= 50) "Es aceptable, pero revisa también el eje 3." else
        "Es bajo: conviene mirar otros planos antes de concluir."))

  qc <- 1000 * (res$calidad_columnas[, a] + res$calidad_columnas[, b])
  out <- c(out, sprintf(
    "**Variables bien representadas** (CRFE en el plano de al menos %s): %s. **Mal representadas:** %s; no las interpretes en este plano.",
    pm(umbral), lista(names(sort(qc[qc >= umbral], decreasing = TRUE)), 10),
    lista(names(sort(qc[qc < umbral])), 10)))

  # Ejes: variables con mayor calidad en cada eje (nombran el eje)
  for (k in c(a, b)) {
    qk <- sort(1000 * res$calidad_columnas[, k], decreasing = TRUE)
    top <- names(qk)[qk >= 400]
    if (!length(top)) top <- names(qk)[1]
    signos <- ifelse(res$columnas[top, k] >= 0, "+", "-")
    out <- c(out, sprintf("**Eje %d** (variables con CRFE alta en ese eje): %s.", k,
                          paste0(top, " (", signos, ")", collapse = ", ")))
  }

  # Pares de variables muy correlacionadas o casi opuestas en el plano
  G <- res$columnas[qc >= umbral, c(a, b), drop = FALSE]
  if (nrow(G) >= 2) {
    ang <- function(i, j) {
      cosv <- sum(G[i, ] * G[j, ]) / sqrt(sum(G[i, ]^2) * sum(G[j, ]^2))
      acos(max(-1, min(1, cosv))) * 180 / pi
    }
    pares <- utils::combn(nrow(G), 2)
    angs <- apply(pares, 2, function(ij) ang(ij[1], ij[2]))
    nom <- apply(pares, 2, function(ij) paste(rownames(G)[ij], collapse = "/"))
    agudos <- nom[angs < 20]; obtusos <- nom[angs > 160]; rectos <- nom[abs(angs - 90) < 10]
    if (length(agudos))  out <- c(out, sprintf("**Ángulo agudo (<20°), correlación positiva alta:** %s.", lista(agudos, 8)))
    if (length(obtusos)) out <- c(out, sprintf("**Ángulo casi llano (>160°), correlación negativa:** %s.", lista(obtusos, 8)))
    if (length(rectos))  out <- c(out, sprintf("**Ángulo cercano a 90°, poca relación:** %s.", lista(rectos, 8)))
  }

  qf <- 1000 * (res$calidad_filas[, a] + res$calidad_filas[, b])
  out <- c(out, sprintf(
    "**Individuos.** %d de %d tienen CRFE en el plano de al menos %s; los que estén por debajo (p. ej. %s) no se interpretan aquí.",
    sum(qf >= umbral), length(qf), pm(umbral), lista(names(sort(qf))[qf[order(qf)] < umbral], 5)))

  # Atípicos: un individuo que aporta mucho a un eje
  for (k in c(a, b)) {
    ck <- 100 * res$contrib_filas[, k]
    dom <- names(ck)[ck > max(25, 300 / res$n)]
    if (length(dom)) {
      out <- c(out, sprintf(
        "**Atención, eje %d:** %s %s el %s de su inercia (CREF). Revisa si es un atípico o un error de digitación antes de interpretar.",
        k, lista(dom, 3), if (length(dom) == 1) "aporta" else "aportan",
        pct(sum(ck[dom]))))
    }
  }
  out
}
