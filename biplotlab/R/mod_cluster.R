# =============================================================================
# MÓDULO: Clúster sobre las coordenadas del biplot
#
# Trabaja sobre el HJ-Biplot configurado en su módulo (mismas variables,
# transformación, exclusiones y convenio de signos): así el núcleo principal
# se mantiene en un solo lugar. El cálculo está en R/calculo_cluster.R.
# =============================================================================

#' Entrada del Clúster en el registro de análisis
#' @export
analisis_cluster <- function() {
  list(
    id = "cluster", titulo = "Clúster", grupo = "Biplot", icono = "circle-nodes",
    descripcion = "Clúster jerárquico o k-medias sobre las coordenadas del HJ-Biplot",
    listo = TRUE,
    opciones_ui = mod_cluster_opciones_ui,
    resultados_ui = mod_cluster_resultados_ui,
    server = mod_cluster_server
  )
}

mod_cluster_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("Clúster sobre coordenadas", "Clasificación sobre el biplot"),
    caja_dependencia_hj("Usa el HJ-Biplot tal como está configurado (variables, transformación y exclusiones)."),
    seccion_opciones(
      "Qué se agrupa",
      shiny::radioButtons(ns("sobre"), NULL,
                          c("Individuos (filas)" = "filas", "Variables (columnas)" = "columnas"),
                          inline = TRUE),
      shiny::checkboxGroupInput(ns("ejes_uso"), "Coordenadas de los ejes",
                                choices = c("1" = 1, "2" = 2, "3" = 3), selected = c(1, 2),
                                inline = TRUE),
      shiny::p(class = "bl-help",
               "Por defecto se usa el plano 1-2. Con más ejes se recoge más inercia, pero el resultado ya no se ve completo en el gráfico.")
    ),
    seccion_opciones(
      "Método",
      shiny::selectInput(ns("metodo"), "Algoritmo", METODOS_CLUSTER, selected = "ward.D2"),
      shiny::sliderInput(ns("k"), "Número de grupos", min = 2, max = 10, value = 3, step = 1),
      shiny::p(class = "bl-help",
               "La pestaña «Número de grupos» ayuda a elegirlo con la silueta media y la suma de cuadrados intra-grupo.")
    ),
    seccion_opciones(
      "Gráfico",
      shiny::checkboxInput(ns("envolventes"), "Envolvente convexa de cada grupo", TRUE),
      shiny::checkboxInput(ns("vectores"), "Mostrar también la otra nube (variables o individuos)", TRUE),
      shiny::checkboxInput(ns("etiquetas"), "Nombres de los elementos", TRUE),
      shiny::uiOutput(ns("colores_ui"))
    ),
    seccion_opciones(
      "Comparar con una variable nominal",
      shiny::selectInput(ns("comparar"), "Variable", c("(ninguna)" = "")),
      shiny::p(class = "bl-help",
               "Tabla cruzada entre los grupos del clúster y una variable de agrupación (p. ej. para ver si un tratamiento o una línea queda en un grupo propio)."),
      abierta = FALSE
    )
  )
}

mod_cluster_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("Clúster sobre coordenadas del biplot", ns("subtitulo")),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Biplot con grupos",
          shiny::plotOutput(ns("biplot"), height = "auto"),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("pdf"), "PDF (vectorial)", class = "btn-sm"),
                     if (requireNamespace("svglite", quietly = TRUE))
                       shiny::downloadButton(ns("svg"), "SVG (editable)", class = "btn-sm"))
        ),
        shiny::tabPanel(
          "Dendrograma",
          shiny::uiOutput(ns("aviso_dendro")),
          shiny::plotOutput(ns("dendrograma"), height = "520px"),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("dendro_png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("dendro_pdf"), "PDF (vectorial)", class = "btn-sm"))
        ),
        shiny::tabPanel(
          "Número de grupos",
          shiny::div(class = "bl-two",
                     shiny::plotOutput(ns("silueta_k"), height = "300px"),
                     shiny::plotOutput(ns("wss_k"), height = "300px")),
          shiny::tableOutput(ns("tabla_k"))
        ),
        shiny::tabPanel("Grupos", shiny::uiOutput(ns("indicadores")), DT::DTOutput(ns("tabla_grupos"))),
        shiny::tabPanel("Perfiles", shiny::uiOutput(ns("nota_perfiles")), DT::DTOutput(ns("tabla_perfiles"))),
        shiny::tabPanel("Tabla cruzada", shiny::uiOutput(ns("cruzada")), shiny::tableOutput(ns("cruzada_tabla"))),
        shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Teoría"), teoria_ui("cluster")),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))),
          shiny::downloadButton(ns("descargar_r"), "Descargar script .R", class = "btn-sm")
        )
      )
    ),
    shiny::div(class = "bl-actions",
               shiny::downloadButton(ns("excel"), "Exportar grupos y tablas a Excel"))
  )
}

mod_cluster_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    hj <- shiny::reactive({
      shiny::req(compartido, compartido$hj)
      r <- tryCatch(compartido$hj(), error = function(e) NULL)
      r
    })
    cfg <- shiny::reactive({
      shiny::req(compartido, compartido$hj_config)
      compartido$hj_config()
    })

    shiny::observeEvent(hj(), {
      k <- hj()$dimensiones
      sel <- intersect(input$ejes_uso %||% c(1, 2), seq_len(k))
      if (length(sel) < 2) sel <- 1:2
      shiny::updateCheckboxGroupInput(session, "ejes_uso", inline = TRUE,
                                      choices = stats::setNames(seq_len(k), seq_len(k)),
                                      selected = sel)
    })
    shiny::observeEvent(datos(), {
      d <- datos()
      opciones <- intersect(d$nominales, names(d$df))
      shiny::updateSelectInput(session, "comparar", choices = c("(ninguna)" = "", opciones),
                               selected = shiny::isolate(input$comparar) %||% "")
    })

    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos."))
      r <- hj()
      if (is.null(r)) return(simpleError("Primero configura el HJ-Biplot (al menos 2 variables activas)."))
      ejes_uso <- sort(as.integer(input$ejes_uso %||% c(1, 2)))
      if (length(ejes_uso) < 1) return(simpleError("Elige al menos un eje."))
      n <- if (input$sobre == "filas") r$n else r$p
      if (input$k >= n) return(simpleError(sprintf("Con %d elementos el máximo de grupos es %d.", n, n - 1)))
      tryCatch(cluster_biplot(r, sobre = input$sobre, ejes = ejes_uso,
                              metodo = input$metodo, k = input$k),
               error = function(e) e)
    })
    cl <- shiny::reactive({
      x <- resultado()
      shiny::validate(shiny::need(!inherits(x, "error"), ""))
      x
    })

    output$error <- shiny::renderUI({
      x <- resultado()
      if (inherits(x, "error")) caja_error(conditionMessage(x))
    })
    output$subtitulo <- shiny::renderUI({
      x <- cl()
      shiny::span(class = "bl-sub", sprintf(
        "%s - %s - ejes %s - %d grupos%s",
        if (x$sobre == "filas") "Individuos" else "Variables",
        names(METODOS_CLUSTER)[METODOS_CLUSTER == x$metodo], paste(x$ejes, collapse = ", "),
        x$k, if (!is.na(x$cofenetica)) sprintf(" - cofenética %s",
                                              format(round(x$cofenetica, 4), decimal.mark = ",")) else ""))
    })

    # Colores de los grupos
    output$colores_ui <- shiny::renderUI({
      k <- input$k
      pal <- colores_por_defecto(paste0("C", seq_len(k)))
      lapply(seq_len(k), function(i) {
        colourpicker::colourInput(session$ns(paste0("col_", i)), paste("Grupo", paste0("C", i)), pal[[i]])
      })
    })
    colores <- shiny::reactive({
      k <- input$k
      pal <- colores_por_defecto(paste0("C", seq_len(k)))
      for (i in seq_len(k)) {
        v <- input[[paste0("col_", i)]]
        if (!is.null(v) && nzchar(v)) pal[[i]] <- v
      }
      pal
    })

    ejes_graf <- shiny::reactive({
      e <- tryCatch(compartido$hj_ejes(), error = function(e) c(1L, 2L))
      if (is.null(e)) c(1L, 2L) else e
    })

    grafico <- shiny::reactive({
      x <- cl(); r <- hj()
      if (x$sobre == "filas") {
        grafico_biplot(r, ejes = ejes_graf(), grupos = x$grupos, colores_grupos = colores(),
                       envolventes = input$envolventes, etiquetas_filas = input$etiquetas,
                       mostrar = if (input$vectores) "ambos" else "filas",
                       titulo = NULL)
      } else {
        cv <- colores()[as.character(x$grupos)]; names(cv) <- names(x$grupos)
        grafico_biplot(r, ejes = ejes_graf(), colores_variables = cv,
                       etiquetas_filas = input$etiquetas && input$vectores,
                       mostrar = if (input$vectores) "ambos" else "columnas",
                       color_filas = "#b9b8b2")
      }
    })
    # Encuadre rectangular: mismo cálculo en pantalla y en las descargas
    output$biplot <- render_biplot(grafico, session)
    guardar <- function(ext, ...) descarga_biplot(grafico, function() sprintf("%s.%s", "cluster_biplot", ext), ext, ...)
    output$png <- guardar("png", dpi = 300, bg = "white")
    output$pdf <- guardar("pdf")
    output$svg <- guardar("svg")

    output$aviso_dendro <- shiny::renderUI({
      if (cl()$metodo == "kmeans") shiny::p(class = "bl-help", "K-medias no produce dendrograma; elige un método jerárquico para verlo.")
    })
    output$dendrograma <- shiny::renderPlot({
      x <- cl(); shiny::req(x$metodo != "kmeans")
      dendrograma_cluster(x, colores(), cex = if (length(x$grupos) > 60) 0.6 else 0.8)
    }, res = 96)
    guardar_dendro <- function(ext) shiny::downloadHandler(
      filename = function() sprintf("dendrograma.%s", ext),
      content = function(file) {
        if (ext == "png") grDevices::png(file, width = 3000, height = 1500, res = 300) else
          grDevices::pdf(file, width = 11, height = 5.5)
        dendrograma_cluster(cl(), colores())
        grDevices::dev.off()
      })
    output$dendro_png <- guardar_dendro("png")
    output$dendro_pdf <- guardar_dendro("pdf")

    grafico_k <- function(columna, titulo) {
      e <- cl()$eleccion_k
      ggplot2::ggplot(e, ggplot2::aes(.data$Grupos, .data[[columna]])) +
        ggplot2::geom_line(colour = COLOR_FILAS, linewidth = 0.7) +
        ggplot2::geom_point(colour = COLOR_FILAS, size = 2.4) +
        ggplot2::geom_vline(xintercept = input$k, linetype = "22", colour = COLOR_VECTOR) +
        ggplot2::scale_x_continuous(breaks = e$Grupos) +
        ggplot2::labs(x = "Número de grupos", y = columna, title = titulo) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(size = 11))
    }
    output$silueta_k <- shiny::renderPlot(grafico_k("Silueta media", "Silueta media (más alto = mejor)"), res = 96)
    output$wss_k <- shiny::renderPlot(grafico_k("SC intra", "Suma de cuadrados intra (buscar el «codo»)"), res = 96)
    output$tabla_k <- shiny::renderTable(cl()$eleccion_k, digits = 3, striped = TRUE,
                                         format.args = list(decimal.mark = ","))

    output$indicadores <- shiny::renderUI({
      x <- cl()
      shiny::p(class = "bl-help", sprintf(
        "Silueta media: %s (0,5 o más = estructura clara; menos de 0,25 = débil).%s",
        format(round(x$silueta, 3), decimal.mark = ","),
        if (!is.na(x$cofenetica)) sprintf(" Correlación cofenética: %s (cuanto más cerca de 1, mejor refleja el dendrograma las distancias).",
                                          format(round(x$cofenetica, 4), decimal.mark = ",")) else ""))
    })
    output$tabla_grupos <- DT::renderDT(tabla_resultados(cl()$tamanos, 3, 10))
    output$nota_perfiles <- shiny::renderUI({
      shiny::p(class = "bl-help", if (cl()$sobre == "filas")
        "Media de cada variable original (sin transformar) en cada grupo y en el total: describe qué caracteriza a cada grupo." else
        "Coordenadas medias de cada grupo de variables en los ejes usados.")
    })
    output$tabla_perfiles <- DT::renderDT({
      x <- cl()
      tabla_resultados(if (x$sobre == "filas") x$perfiles else x$centroides, 3, 15)
    })

    output$cruzada <- shiny::renderUI({
      x <- cl(); v <- input$comparar
      if (x$sobre != "filas") return(shiny::p(class = "bl-help", "Disponible al agrupar individuos."))
      if (is.null(v) || !nzchar(v)) return(shiny::p(class = "bl-help", "Elige una variable nominal en el panel de opciones."))
      shiny::h4(sprintf("Grupos del clúster × %s", v))
    })
    output$cruzada_tabla <- shiny::renderTable({
      x <- cl(); v <- input$comparar
      shiny::req(x$sobre == "filas", nzchar(v %||% ""))
      d <- cfg()$datos
      shiny::req(v %in% names(d))
      as.data.frame.matrix(table(x$grupos, d[names(x$grupos), v]))
    }, rownames = TRUE, striped = TRUE)

    sintaxis <- shiny::reactive({
      x <- cl(); c0 <- cfg()
      paste(c(
        "# Clúster sobre las coordenadas del HJ-Biplot",
        sintaxis_hj_base(c0),
        "",
        sprintf("cl <- cluster_biplot(res, sobre = \"%s\", ejes = %s, metodo = \"%s\", k = %d)",
                x$sobre, vector_a_r(x$ejes), x$metodo, x$k),
        "cl                                   # tamaños, silueta y cofenética",
        "cl$grupos                            # grupo de cada elemento",
        if (x$sobre == "filas") "cl$perfiles                          # medias de cada grupo",
        if (x$metodo != "kmeans") "dendrograma_cluster(cl)",
        "",
        if (x$sobre == "filas")
          sprintf("grafico_biplot(res, ejes = %s, grupos = cl$grupos, envolventes = %s)",
                  vector_a_r(ejes_graf()), input$envolventes)
        else c("colores <- colores_por_defecto(levels(cl$grupos))[as.character(cl$grupos)]",
               "names(colores) <- names(cl$grupos)",
               sprintf("grafico_biplot(res, ejes = %s, colores_variables = colores)", vector_a_r(ejes_graf())))
      ), collapse = "\n")
    })
    output$sintaxis <- shiny::renderText(sintaxis())
    output$descargar_r <- shiny::downloadHandler(
      filename = function() "cluster_biplot.R",
      content = function(file) writeLines(enc2utf8(sintaxis()), file, useBytes = TRUE))

    output$excel <- shiny::downloadHandler(
      filename = function() "cluster_biplot.xlsx",
      content = function(file) {
        x <- cl()
        asign <- data.frame(Elemento = names(x$grupos), Grupo = as.character(x$grupos),
                            Silueta = x$silueta_detalle[, "sil_width"],
                            round(x$coordenadas, 4), check.names = FALSE)
        hojas <- list("Asignacion" = asign, "Grupos" = x$tamanos,
                      "Centroides" = x$centroides, "Numero de grupos" = x$eleccion_k)
        if (!is.null(x$perfiles)) hojas[["Perfiles"]] <- x$perfiles
        writexl::write_xlsx(hojas, file)
      })

    cl
  })
}
