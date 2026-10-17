# =============================================================================
# HJ-Biplot de puntuaciones neutrosóficas (módulo del compañero).
# Los cálculos son los suyos (R/neutrosofico_calculo.R): S = T - I - F, centrado
# por columnas, SVD y coordenadas HJ (J = U D, H = V D). Aquí se agrega:
#   - cómo obtener T, I y F desde los datos del panel Datos: recodificando
#     respuestas categóricas (por ejemplo, las etiquetas de un .sav de SPSS) o
#     leyendo ternas ya calculadas en columnas con sufijo _T, _I y _F;
#   - el gráfico con el encuadre rectangular (mínimo 4:3) y la opción de voltear;
#   - tablas, exportación, Teoría y Sintaxis R, como en el resto de biplots.
# =============================================================================

#' @rawNamespace export(puntuar_neutrosofico, hj_neutrosofico, indicadores_por_eje, calidades_hj)
NULL

#' @rdname registro_analisis
analisis_neutrosofico <- function() {
  list(id = "neutro", titulo = "HJ Neutrosófico", grupo = "Biplot", icono = "circle-half-stroke",
       descripcion = "HJ-Biplot de puntuaciones neutrosóficas S = T - I - F",
       listo = TRUE,
       opciones_ui = mod_neutro_opciones_ui, resultados_ui = mod_neutro_resultados_ui,
       server = function(id, datos, compartido) mod_neutro_server(id, datos))
}

CATEGORIAS_NEUTRO <- c("Verdad (T = 1)" = "T", "Indeterminación (I = 1)" = "I",
                       "Falsedad (F = 1)" = "F", "Neutral (0, 0, 0)" = "N",
                       "Excluir la fila" = "X")

# Propuesta inicial de la regla según el texto de la respuesta. Es solo una
# propuesta: el usuario debe revisarla y justificarla.
proponer_categoria <- function(valor) {
  v <- tolower(trimws(as.character(valor)))
  if (grepl("no aplica|no responde|no contesta|ns/nr|sin dato|perdido", v)) return("X")
  if (grepl("no sabe|indetermin|no estoy segur|dudo|duda", v)) return("I")
  if (grepl("^ni |ni .* ni|neutral|indiferente|regular", v)) return("N")
  if (grepl("insegur|desacuerdo|nada|malo|mala|nunca|^no$|falso", v)) return("F")
  if (grepl("segur|de acuerdo|mucho|bueno|buena|siempre|^s[ií]$|verdad", v)) return("T")
  "X"
}

#' Ternas T, I, F a partir de respuestas categóricas
#'
#' Cada respuesta distinta se asigna a verdad, indeterminación, falsedad,
#' neutral (0, 0, 0) o "excluir". Se excluyen las filas con alguna respuesta
#' excluida o faltante en las variables elegidas.
#' @param df data.frame.
#' @param variables columnas con las respuestas.
#' @param regla vector con nombres: respuesta -> "T", "I", "F", "N" o "X".
#' @return lista con `T`, `I`, `F`, `conservadas` (lógico por fila) y `resumen`.
#' @export
ternas_desde_respuestas <- function(df, variables, regla) {
  R <- vapply(df[variables], function(z) trimws(as.character(z)), character(nrow(df)))
  if (is.null(dim(R))) R <- matrix(R, nrow = nrow(df))
  colnames(R) <- variables
  cat <- matrix(unname(regla[R]), nrow(R), ncol(R), dimnames = list(rownames(df), variables))
  cat[is.na(R)] <- "X"
  cat[is.na(cat)] <- "X"
  conservar <- rowSums(cat == "X") == 0
  cat <- cat[conservar, , drop = FALSE]
  T <- 1 * (cat == "T"); I <- 1 * (cat == "I"); F <- 1 * (cat == "F")
  resumen <- data.frame(Variable = variables,
                        T = colSums(T), I = colSums(I), F = colSums(F),
                        Neutral = colSums(cat == "N"),
                        `Media de I` = colMeans(I), check.names = FALSE, row.names = NULL)
  list(T = T, I = I, F = F, conservadas = conservar, resumen = resumen)
}

#' Ternas T, I, F leídas de columnas con sufijo _T, _I y _F
#' @param df data.frame.
#' @param variables nombres base (sin sufijo); por defecto, todos los detectados.
#' @export
ternas_desde_columnas <- function(df, variables = NULL) {
  bases <- variables_ternas(df)
  if (!is.null(variables)) bases <- intersect(variables, bases)
  if (length(bases) < 2) stop("Se necesitan al menos 2 variables con columnas _T, _I y _F.", call. = FALSE)
  col <- function(b, s) names(df)[toupper(names(df)) == toupper(paste0(b, "_", s))][1]
  M <- function(s) {
    m <- as.matrix(df[vapply(bases, col, character(1), s = s)]); storage.mode(m) <- "double"
    dimnames(m) <- list(rownames(df), bases); m
  }
  T <- M("T"); I <- M("I"); F <- M("F")
  conservar <- stats::complete.cases(cbind(T, I, F))
  list(T = T[conservar, , drop = FALSE], I = I[conservar, , drop = FALSE], F = F[conservar, , drop = FALSE],
       conservadas = conservar,
       resumen = data.frame(Variable = bases, `Media de T` = colMeans(T[conservar, , drop = FALSE]),
                            `Media de I` = colMeans(I[conservar, , drop = FALSE]),
                            `Media de F` = colMeans(F[conservar, , drop = FALSE]),
                            check.names = FALSE, row.names = NULL))
}

#' Variables con ternas en columnas (nombre_T, nombre_I, nombre_F)
#' @param df data.frame.
#' @export
variables_ternas <- function(df) {
  nm <- names(df)
  m <- regmatches(nm, regexec("^(.*)_([TtIiFf])$", nm))
  ok <- lengths(m) == 3
  if (!any(ok)) return(character())
  bases <- vapply(m[ok], `[`, character(1), 2)
  suf <- toupper(vapply(m[ok], `[`, character(1), 3))
  completas <- names(which(tapply(suf, bases, function(s) all(c("T", "I", "F") %in% s))))
  completas[order(match(completas, bases))]
}

#' Gráfico del HJ-Biplot neutrosófico (ggplot2, con encuadre rectangular)
#'
#' El tamaño de cada punto crece con su indeterminación media, como en el
#' gráfico original del módulo.
#' @export
grafico_neutrosofico <- function(res, ejes = c(1, 2), calidad_min_puntos = 0, calidad_min_vectores = 0,
                                 grupos = NULL, etiquetas_puntos = FALSE, etiquetas_variables = TRUE,
                                 escala_vectores = 1, invertir_ejes = integer(), tamano_por_I = TRUE) {
  cal <- calidades_hj(res, ejes)
  J <- res$coordenadas_observaciones[, ejes, drop = FALSE]
  H <- res$coordenadas_variables[, ejes, drop = FALSE] * escala_vectores
  signo <- ifelse(ejes %in% invertir_ejes, -1, 1)
  J <- sweep(J, 2, signo, "*"); H <- sweep(H, 2, signo, "*")
  P <- data.frame(x = J[, 1], y = J[, 2], lab = rownames(J), I = res$indeterminacion_observaciones,
                  cal = cal$observaciones$Calidad_pct)
  V <- data.frame(x = H[, 1], y = H[, 2], lab = rownames(H), cal = cal$variables$Calidad_pct)
  P$ver <- !is.na(P$cal) & P$cal >= calidad_min_puntos
  V$ver <- !is.na(V$cal) & V$cal >= calidad_min_vectores
  if (!is.null(grupos)) P$g <- factor(grupos)
  inercia <- res$inercia$Porcentaje
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9ced3", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9ced3", linewidth = 0.3)
  Pv <- P[P$ver, , drop = FALSE]
  if (nrow(Pv)) {
    estetica <- if (!is.null(grupos)) ggplot2::aes(.data$x, .data$y, fill = .data$g, size = .data$I)
                else ggplot2::aes(.data$x, .data$y, size = .data$I)
    if (!tamano_por_I) Pv$I <- 0
    p <- p + ggplot2::geom_point(data = Pv, mapping = estetica, shape = 21, colour = "#33414a",
                                 alpha = 0.75, fill = if (is.null(grupos)) "#4c84c4" else NULL) +
      ggplot2::scale_size_continuous(range = c(1.8, 6), limits = c(0, 1), name = "Indeterminación media",
                                     guide = if (tamano_por_I) "legend" else "none")
    if (!is.null(grupos)) p <- p + ggplot2::scale_fill_manual(values = colores_por_defecto(nlevels(P$g)), name = NULL)
    if (etiquetas_puntos)
      p <- p + ggrepel::geom_text_repel(data = Pv, ggplot2::aes(.data$x, .data$y, label = .data$lab),
                                        size = 3, colour = "#33414a", max.overlaps = 25)
  }
  Vv <- V[V$ver & (V$x^2 + V$y^2) > 0, , drop = FALSE]
  if (nrow(Vv)) {
    p <- p + ggplot2::geom_segment(data = Vv, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                                   arrow = grid::arrow(length = grid::unit(0.2, "cm")),
                                   colour = "#9c2f22", linewidth = 0.7)
    if (etiquetas_variables)
      p <- p + ggrepel::geom_text_repel(data = Vv, ggplot2::aes(.data$x, .data$y, label = .data$lab),
                                        colour = "#7a1d12", fontface = "bold", size = 3.6, max.overlaps = 40)
  }
  todos <- rbind(Pv[c("x", "y")], Vv[c("x", "y")])
  lim <- limites_encuadre(todos$x, todos$y, incluir_origen = TRUE)
  p + ggplot2::coord_equal(xlim = lim$x, ylim = lim$y, expand = FALSE, clip = "off") +
    ggplot2::labs(x = sprintf("Eje %d (%s %%)", ejes[1], format(round(inercia[ejes[1]], 2), decimal.mark = ",")),
                  y = sprintf("Eje %d (%s %%)", ejes[2], format(round(inercia[ejes[2]], 2), decimal.mark = ",")),
                  subtitle = paste0("HJ-Biplot de puntuaciones neutrosóficas (S = T - I - F)",
                                    if (escala_vectores != 1) paste0(" - flechas × ", escala_vectores) else "")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom",
                   plot.subtitle = ggplot2::element_text(colour = "#666666"))
}

# ----------------------------------------------------------------- Opciones --
mod_neutro_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  shiny::tagList(
    encabezado_opciones("HJ Neutrosófico", "Puntuación S = T - I - F y HJ-Biplot (Galindo, 1986)"),
    seccion_opciones(
      "Datos de entrada",
      shiny::radioButtons(ns("formato"), "¿Cómo vienen T, I y F?",
                          c("Respuestas categóricas: recodificar" = "respuestas",
                            "Ternas ya calculadas: columnas _T, _I y _F" = "ternas")),
      shiny::conditionalPanel(
        "input.formato == 'respuestas'", ns = ns,
        shiny::selectizeInput(ns("vars_resp"), "Preguntas (variables)", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"))),
        ayuda("Elige preguntas con la misma escala de respuesta. Con un .sav de SPSS, deja marcada «Usar las etiquetas de valor» en Datos para ver las respuestas por su nombre.")),
      shiny::conditionalPanel(
        "input.formato == 'ternas'", ns = ns,
        shiny::selectizeInput(ns("vars_ternas"), "Variables con ternas", choices = NULL, multiple = TRUE,
                              options = list(plugins = list("remove_button"))),
        ayuda("Para cada variable deben existir tres columnas: nombre_T, nombre_I y nombre_F, con valores entre 0 y 1.")),
      shiny::selectInput(ns("grupo"), "Variable de grupo (colorea; no construye ejes)", choices = NULL)
    ),
    shiny::conditionalPanel(
      "input.formato == 'respuestas'", ns = ns,
      seccion_opciones(
        "Regla de recodificación",
        ayuda("Cada respuesta se asigna a verdad, indeterminación, falsedad o neutral. La propuesta inicial se basa en el texto de la respuesta: revísala, porque la regla debe estar justificada. «Un dato ausente no equivale a indeterminación»: los faltantes siempre se excluyen."),
        shiny::uiOutput(ns("reglas"))
      )
    ),
    seccion_opciones(
      "Gráfico",
      shiny::fluidRow(shiny::column(6, shiny::selectInput(ns("eje_x"), "Eje horizontal", 1:2, 1)),
                      shiny::column(6, shiny::selectInput(ns("eje_y"), "Eje vertical", 1:2, 2))),
      shiny::fluidRow(shiny::column(6, shiny::checkboxInput(ns("voltear_h"), "Voltear de izquierda a derecha", FALSE)),
                      shiny::column(6, shiny::checkboxInput(ns("voltear_v"), "Voltear de arriba a abajo", FALSE))),
      shiny::sliderInput(ns("cal_p"), "Calidad mínima de los puntos (%)", 0, 100, 0, step = 5),
      shiny::sliderInput(ns("cal_v"), "Calidad mínima de los vectores (%)", 0, 100, 0, step = 5),
      shiny::numericInput(ns("escala"), "Escala visual de las flechas", 1, 0.001, 100, step = 0.1),
      ayuda("Solo cambia el largo dibujado de las flechas; las coordenadas y calidades no cambian."),
      shiny::checkboxInput(ns("tam_I"), "Tamaño del punto según su indeterminación media", TRUE),
      shiny::checkboxInput(ns("et_p"), "Etiquetas de las observaciones", FALSE),
      shiny::checkboxInput(ns("et_v"), "Etiquetas de las variables", TRUE)
    ),
    seccion_opciones("Exportar",
                     shiny::downloadButton(ns("dl_xlsx"), "Todas las tablas (.xlsx)", class = "btn-sm"),
                     abierta = FALSE)
  )
}

mod_neutro_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  shiny::tagList(
    encabezado_resultados("HJ-Biplot neutrosófico", ns("subtitulo")),
    shiny::div(class = "bl-card", shiny::tabsetPanel(
      id = ns("tabs"),
      shiny::tabPanel("Biplot",
                      shiny::div(class = "bl-descargas",
                                 shiny::downloadButton(ns("dl_png"), "PNG", class = "btn-sm"),
                                 shiny::downloadButton(ns("dl_pdf"), "PDF (vectorial)", class = "btn-sm")),
                      shiny::plotOutput(ns("biplot"), height = "auto")),
      shiny::tabPanel("Inercia", DT::DTOutput(ns("inercia")), shiny::verbatimTextOutput(ns("control"))),
      shiny::tabPanel("Observaciones", ayuda("Coordenadas HJ (J = U D), calidad de representación (CRFE) y contribución al eje (CREF), en por mil."),
                      DT::DTOutput(ns("obs"))),
      shiny::tabPanel("Variables", ayuda("Coordenadas HJ (H = V D), calidad, contribución y media de I de cada variable."),
                      DT::DTOutput(ns("vars"))),
      shiny::tabPanel("Recodificación", ayuda("Cuántas respuestas quedaron como T, I, F o neutral, y cuántas filas se excluyeron."),
                      shiny::uiOutput(ns("filas")), DT::DTOutput(ns("resumen"))),
      shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Teoría"), teoria_ui("neutro")),
      shiny::tabPanel(shiny::span(shiny::icon("code"), "Sintaxis R"), shiny::verbatimTextOutput(ns("sintaxis")))
    ))
  )
}

mod_neutro_server <- function(id, datos) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(datos(), {
      d <- datos(); df <- d$df
      cand <- names(df)[vapply(df, function(z) { u <- length(unique(stats::na.omit(z))); u >= 2 && u <= 40 }, logical(1))]
      sel <- intersect(shiny::isolate(input$vars_resp), cand)
      shiny::updateSelectizeInput(session, "vars_resp", choices = cand, selected = sel)
      tern <- variables_ternas(df)
      shiny::updateSelectizeInput(session, "vars_ternas", choices = tern, selected = tern)
      if (length(tern) >= 2 && is.null(shiny::isolate(input$vars_resp)))
        shiny::updateRadioButtons(session, "formato", selected = "ternas")
      grupos <- intersect(d$nominales, names(df))
      shiny::updateSelectInput(session, "grupo", choices = c("(ninguna)" = "", grupos),
                               selected = intersect(shiny::isolate(input$grupo), grupos))
    })

    # Respuestas distintas de las preguntas elegidas
    valores <- shiny::reactive({
      shiny::req(input$vars_resp)
      df <- datos()$df
      v <- unique(stats::na.omit(unlist(lapply(df[input$vars_resp], function(z) trimws(as.character(z))))))
      v <- v[nzchar(v)]
      num <- suppressWarnings(as.numeric(v))
      if (!anyNA(num)) v[order(num)] else sort(v)
    })
    output$reglas <- shiny::renderUI({
      v <- valores()
      if (length(v) > 40) return(caja_error("Hay más de 40 respuestas distintas: elige preguntas categóricas."))
      shiny::tagList(lapply(seq_along(v), function(i) {
        anterior <- shiny::isolate(input[[paste0("regla_", i)]])
        shiny::selectInput(ns(paste0("regla_", i)), v[i], CATEGORIAS_NEUTRO,
                           selected = anterior %||% proponer_categoria(v[i]))
      }))
    })
    regla <- shiny::reactive({
      v <- valores()
      r <- vapply(seq_along(v), function(i) input[[paste0("regla_", i)]] %||% proponer_categoria(v[i]), character(1))
      stats::setNames(r, v)
    })

    ternas <- shiny::reactive({
      df <- datos()$df
      if (identical(input$formato, "ternas")) {
        shiny::validate(shiny::need(length(input$vars_ternas) >= 2, "Elige al menos 2 variables con columnas _T, _I y _F."))
        ternas_desde_columnas(df, input$vars_ternas)
      } else {
        shiny::validate(shiny::need(length(input$vars_resp) >= 2, "Elige al menos 2 preguntas."))
        ternas_desde_respuestas(df, input$vars_resp, regla())
      }
    })

    res <- shiny::reactive({
      t <- ternas()
      tryCatch(hj_neutrosofico(t$T, t$I, t$F),
               error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
    })

    shiny::observeEvent(res(), {
      k <- ncol(res()$coordenadas_observaciones)
      shiny::updateSelectInput(session, "eje_x", choices = seq_len(k), selected = min(as.integer(input$eje_x %||% 1), k))
      shiny::updateSelectInput(session, "eje_y", choices = seq_len(k), selected = min(as.integer(input$eje_y %||% 2), k))
    })
    ejes <- shiny::reactive({
      e <- as.integer(c(input$eje_x %||% 1, input$eje_y %||% 2))
      shiny::validate(shiny::need(ncol(res()$coordenadas_observaciones) >= 2,
                                  "Solo hay un eje con variación: no se puede dibujar un plano."))
      if (e[1] == e[2]) e <- c(1L, 2L)
      e
    })
    grupos <- shiny::reactive({
      g <- input$grupo
      if (!nzchar(g %||% "")) return(NULL)
      as.character(datos()$df[[g]][ternas()$conservadas])
    })

    output$subtitulo <- shiny::renderUI({
      t <- ternas()
      shiny::span(class = "bl-sub", sprintf("%s - %d observaciones × %d variables (%d filas excluidas)",
                                            datos()$etiqueta, nrow(t$T), ncol(t$T), sum(!t$conservadas)))
    })

    grafico <- shiny::reactive({
      e <- ejes()
      grafico_neutrosofico(res(), e, input$cal_p, input$cal_v, grupos(), isTRUE(input$et_p), isTRUE(input$et_v),
                           escala_vectores = input$escala %||% 1,
                           invertir_ejes = c(if (isTRUE(input$voltear_h)) e[1], if (isTRUE(input$voltear_v)) e[2]),
                           tamano_por_I = isTRUE(input$tam_I))
    })
    output$biplot <- render_biplot(grafico, session)
    output$dl_png <- descarga_biplot(grafico, "hj_neutrosofico.png", "png", dpi = 300, bg = "white")
    output$dl_pdf <- descarga_biplot(grafico, "hj_neutrosofico.pdf", "pdf")

    tablas <- shiny::reactive({
      r <- res(); ind <- indicadores_por_eje(r); e <- ejes(); cal <- calidades_hj(r, e)
      armar <- function(A, ind_i, nombre, calidad) {
        k <- ncol(A)
        out <- data.frame(x = rownames(A), check.names = FALSE); names(out) <- nombre
        for (j in seq_len(k)) out[[paste0("Eje ", j)]] <- A[, j]
        for (j in seq_len(k)) out[[paste0("CRFE ", j, " (‰)")]] <- 10 * ind_i$calidad_por_eje[, j]
        out[[sprintf("CRFE plano %d-%d (‰)", e[1], e[2])]] <- 10 * calidad
        for (j in seq_len(k)) out[[paste0("CREF ", j, " (‰)")]] <- 10 * ind_i$contribucion_al_eje[, j]
        out
      }
      obs <- armar(r$coordenadas_observaciones, ind$observaciones, "Observación", cal$observaciones$Calidad_pct)
      obs[["Media de I"]] <- r$indeterminacion_observaciones
      vars <- armar(r$coordenadas_variables, ind$variables, "Variable", cal$variables$Calidad_pct)
      vars[["Media de I"]] <- r$indeterminacion_variables
      inercia <- data.frame(Eje = r$inercia$Eje, `% de inercia` = r$inercia$Porcentaje,
                            `% acumulado` = r$inercia$Acumulado, check.names = FALSE)
      list(inercia = inercia, obs = obs, vars = vars, resumen = ternas()$resumen)
    })
    output$inercia <- DT::renderDT(tabla_resultados(tablas()$inercia, 3, 20))
    output$control <- shiny::renderText(sprintf(
      "Comprobación: error máximo al reconstruir Z con U D V' = %s\nConvenio: %s; puntuación %s",
      format(res()$error_reconstruccion, digits = 3), res()$convenio, res()$regla_puntuacion))
    output$obs <- DT::renderDT(tabla_resultados(tablas()$obs, 3, 15))
    output$vars <- DT::renderDT(tabla_resultados(tablas()$vars, 3, 15))
    output$resumen <- DT::renderDT(tabla_resultados(tablas()$resumen, 3, 20))
    output$filas <- shiny::renderUI({
      t <- ternas()
      shiny::p(sprintf("Filas conservadas: %d de %d. Excluidas (respuesta excluida o faltante): %d.",
                       sum(t$conservadas), length(t$conservadas), sum(!t$conservadas)))
    })
    output$dl_xlsx <- shiny::downloadHandler(
      filename = "hj_neutrosofico_tablas.xlsx",
      content = function(f) writexl::write_xlsx(list(Inercia = tablas()$inercia, Observaciones = tablas()$obs,
                                                     Variables = tablas()$vars, Recodificacion = tablas()$resumen), f))

    output$sintaxis <- shiny::renderText({
      d <- datos(); e <- ejes()
      entrada <- if (identical(input$formato, "ternas")) {
        sprintf("t <- ternas_desde_columnas(datos, %s)", vector_a_r(input$vars_ternas, 33))
      } else {
        r <- regla()
        c("# Regla de recodificación: respuesta -> T (verdad), I (indeterminación),",
          "# F (falsedad), N (neutral: 0, 0, 0) o X (excluir la fila)",
          sprintf("regla <- c(%s)", paste(sprintf("%s = \"%s\"", encodeString(names(r), quote = "`"), r), collapse = ",\n           ")),
          sprintf("t <- ternas_desde_respuestas(datos, %s, regla)", vector_a_r(input$vars_resp, 37)))
      }
      paste(c("# HJ-Biplot neutrosófico generado con BiplotLab", sintaxis_paquete(), "",
              sintaxis_lectura(d$origen), "", entrada, "",
              "res <- hj_neutrosofico(t$T, t$I, t$F)   # S = T - I - F, centrado, SVD, J = U D, H = V D",
              "res$inercia",
              sprintf("calidades_hj(res, ejes = c(%d, %d))", e[1], e[2]),
              sprintf("grafico_neutrosofico(res, ejes = c(%d, %d))", e[1], e[2])), collapse = "\n")
    })
  })
}
