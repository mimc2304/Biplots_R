# =============================================================================
# MÓDULO: Bootstrap del HJ-Biplot
#
# Usa el HJ-Biplot configurado en su módulo y remuestrea los individuos para
# obtener intervalos de confianza. El cálculo está en R/calculo_bootstrap.R.
# =============================================================================

#' Entrada del Bootstrap en el registro de análisis
#' @export
analisis_bootstrap <- function() {
  list(
    id = "bootstrap", titulo = "Bootstrap", grupo = "Biplot", icono = "shuffle",
    descripcion = "Intervalos de confianza bootstrap para el HJ-Biplot",
    listo = TRUE,
    opciones_ui = mod_bootstrap_opciones_ui,
    resultados_ui = mod_bootstrap_resultados_ui,
    server = mod_bootstrap_server
  )
}

mod_bootstrap_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("Bootstrap del HJ-Biplot", "Remuestreo de individuos con reemplazo"),
    caja_dependencia_hj("Remuestrea el HJ-Biplot tal como está configurado (variables, transformación, exclusiones y plano del gráfico)."),
    seccion_opciones(
      "Remuestreo",
      shiny::numericInput(ns("B"), "Número de réplicas", value = 500, min = 50, max = 5000, step = 50),
      shiny::selectInput(ns("nivel"), "Nivel de confianza",
                         c("90 %" = 0.90, "95 %" = 0.95, "99 %" = 0.99), selected = 0.95),
      shiny::numericInput(ns("semilla"), "Semilla (para repetir el mismo resultado)", value = 123,
                          min = 1, step = 1),
      shiny::actionButton(ns("ejecutar"), "Ejecutar bootstrap", icon = shiny::icon("play"),
                          class = "btn-primary w-100"),
      shiny::p(class = "bl-help",
               "Con 500 réplicas tarda unos segundos; 1000 o más dan intervalos más estables.")
    ),
    seccion_opciones(
      "Gráfico",
      shiny::selectizeInput(ns("vars_graf"), "Variables con región de confianza", choices = NULL,
                            multiple = TRUE,
                            options = list(plugins = list("remove_button"), placeholder = "Todas")),
      shiny::checkboxInput(ns("nube"), "Mostrar las réplicas (puntos)", TRUE)
    )
  )
}

mod_bootstrap_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("Bootstrap del HJ-Biplot", ns("subtitulo")),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Regiones de confianza",
          shiny::plotOutput(ns("biplot"), height = "auto"),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("pdf"), "PDF (vectorial)", class = "btn-sm"),
                     if (requireNamespace("svglite", quietly = TRUE))
                       shiny::downloadButton(ns("svg"), "SVG (editable)", class = "btn-sm"))
        ),
        shiny::tabPanel(
          "Intervalos",
          shiny::selectInput(ns("grupo_tabla"), NULL, choices = NULL, width = "340px"),
          shiny::uiOutput(ns("nota_tabla")),
          DT::DTOutput(ns("tabla"))
        ),
        shiny::tabPanel(
          "Distribución",
          shiny::selectizeInput(ns("estadistico"), "Estadístico", choices = NULL, width = "100%"),
          shiny::div(class = "bl-two",
                     shiny::plotOutput(ns("hist"), height = "320px"),
                     shiny::plotOutput(ns("qq"), height = "320px"))
        ),
        shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Teoría"), teoria_ui("bootstrap")),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))),
          shiny::downloadButton(ns("descargar_r"), "Descargar script .R", class = "btn-sm")
        )
      )
    ),
    shiny::div(class = "bl-actions",
               shiny::downloadButton(ns("excel"), "Exportar intervalos y réplicas a Excel"))
  )
}

mod_bootstrap_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    cfg <- shiny::reactive({
      shiny::req(compartido, compartido$hj_config)
      compartido$hj_config()
    })
    ejes <- shiny::reactive({
      e <- tryCatch(compartido$hj_ejes(), error = function(e) c(1L, 2L))
      if (is.null(e)) c(1L, 2L) else e
    })

    shiny::observeEvent(cfg()$variables, {
      shiny::updateSelectizeInput(session, "vars_graf", choices = cfg()$variables,
                                  selected = intersect(input$vars_graf, cfg()$variables))
    })

    estado <- shiny::reactiveVal(NULL)       # resultado o error
    config_usada <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$ejecutar, {
      c0 <- tryCatch(cfg(), error = function(e) NULL)
      if (is.null(datos()) || is.null(c0) || length(c0$variables) < 2) {
        estado(simpleError("Primero carga los datos y configura el HJ-Biplot (al menos 2 variables activas)."))
        return()
      }
      e <- ejes()
      res <- shiny::withProgress(message = "Bootstrap en curso", value = 0, {
        tryCatch(
          bootstrap_biplot(c0$datos, c0$variables, transformacion = c0$transformacion,
                           log = c0$log, tipo = c0$tipo, referencia = c0$referencia,
                           invertir_ejes = c0$invertir_ejes, ejes = e, B = input$B,
                           nivel = as.numeric(input$nivel), semilla = input$semilla,
                           progreso = function(i, B) shiny::setProgress(i / B,
                                        detail = sprintf("%d de %d réplicas", i, B))),
          error = function(err) err)
      })
      estado(res)
      config_usada(list(cfg = c0, ejes = e, B = input$B, nivel = as.numeric(input$nivel),
                        semilla = input$semilla))
    })

    bt <- shiny::reactive({
      x <- estado()
      shiny::validate(shiny::need(!is.null(x) && !inherits(x, "error"), ""))
      x
    })

    output$error <- shiny::renderUI({
      x <- estado()
      if (is.null(x)) {
        return(shiny::div(class = "bl-info", shiny::icon("circle-info"),
                          "Configura el número de réplicas y pulsa «Ejecutar bootstrap»."))
      }
      if (inherits(x, "error")) caja_error(conditionMessage(x))
    })
    output$subtitulo <- shiny::renderUI({
      x <- bt()
      shiny::span(class = "bl-sub", sprintf(
        "%d réplicas válidas de %d - IC al %s %% - plano %d-%d - semilla %s",
        x$validas, x$B, format(100 * x$nivel), x$ejes[1], x$ejes[2], format(x$semilla)))
    })

    grafico <- shiny::reactive({
      x <- bt()
      v <- input$vars_graf
      if (length(v)) {
        x$original$variables <- intersect(x$original$variables, v)
      }
      p <- grafico_bootstrap_biplot(x)
      if (!isTRUE(input$nube)) p$layers[[3]] <- NULL
      p
    })
    # Encuadre rectangular: mismo cálculo en pantalla y en las descargas
    output$biplot <- render_biplot(grafico, session)
    guardar <- function(ext, ...) descarga_biplot(grafico, function() sprintf("%s.%s", "bootstrap_biplot", ext), ext, ...)
    output$png <- guardar("png", dpi = 300, bg = "white")
    output$pdf <- guardar("pdf")
    output$svg <- guardar("svg")

    shiny::observeEvent(bt(), {
      grupos <- unique(bt()$resumen$Grupo)
      shiny::updateSelectInput(session, "grupo_tabla", choices = grupos, selected = grupos[1])
      shiny::updateSelectizeInput(session, "estadistico", choices = bt()$resumen$Estadístico,
                                  selected = bt()$resumen$Estadístico[1], server = TRUE)
    })
    output$nota_tabla <- shiny::renderUI({
      txt <- switch(input$grupo_tabla %||% "",
        "Global" = "Valores propios e inercia. Si el intervalo del % de inercia de un eje es estrecho, ese eje es estable.",
        "CRFE (calidad)" = "Calidad de representación (por mil) de cada variable. Si el límite inferior queda por debajo de 500, su buena representación no es segura.",
        "CREF (contribución)" = "Contribución (por mil) de cada variable a la inercia de cada eje.",
        "Longitud de los vectores" = "Longitud del vector en el plano (aproxima la variabilidad representada).",
        "Ángulo variable-eje" = "Ángulo (grados) entre cada variable y cada eje del plano, tras alinear las réplicas con Procrustes.",
        "Ángulo entre variables" = "Ángulo (grados) entre pares de variables en el plano. Un intervalo que no incluye 90° indica una asociación (positiva si queda por debajo, negativa si queda por encima).",
        "Coordenadas de las variables" = "Coordenadas del extremo de cada vector, tras alinear las réplicas con Procrustes.",
        "")
      shiny::p(class = "bl-help", txt)
    })
    output$tabla <- DT::renderDT({
      r <- bt()$resumen
      r <- r[r$Grupo == (input$grupo_tabla %||% "Global"), -1, drop = FALSE]
      tabla_resultados(r, 3, 25)
    })

    output$hist <- shiny::renderPlot({
      shiny::req(input$estadistico %in% colnames(bt()$replicas))
      grafico_bootstrap_hist(bt(), input$estadistico)
    }, res = 96)
    output$qq <- shiny::renderPlot({
      shiny::req(input$estadistico %in% colnames(bt()$replicas))
      grafico_bootstrap_qq(bt(), input$estadistico)
    }, res = 96)

    sintaxis <- shiny::reactive({
      x <- bt(); u <- config_usada(); c0 <- u$cfg
      paste(c(
        "# Bootstrap del HJ-Biplot",
        sintaxis_hj_base(c0, llamar_hj = FALSE),
        "",
        "bt <- bootstrap_biplot(",
        "  datos,",
        sprintf("  variables = %s,", vector_a_r(c0$variables, 14)),
        sprintf("  transformacion = \"%s\",", c0$transformacion),
        if (isTRUE(c0$log)) "  log = TRUE,",
        sprintf("  tipo = \"%s\",", c0$tipo),
        if (!is.null(c0$referencia)) sprintf("  referencia = \"%s\",", c0$referencia),
        if (length(c0$invertir_ejes)) sprintf("  invertir_ejes = %s,", vector_a_r(c0$invertir_ejes)),
        sprintf("  ejes = c(%d, %d), B = %d, nivel = %s, semilla = %s", u$ejes[1], u$ejes[2],
                as.integer(u$B), u$nivel, u$semilla),
        ")",
        "bt                                   # resumen global",
        "bt$resumen                           # todos los intervalos",
        "grafico_bootstrap_biplot(bt)         # regiones de confianza",
        "grafico_bootstrap_hist(bt, \"% Inercia eje 1\")"
      ), collapse = "\n")
    })
    output$sintaxis <- shiny::renderText(sintaxis())
    output$descargar_r <- shiny::downloadHandler(
      filename = function() "bootstrap_biplot.R",
      content = function(file) writeLines(enc2utf8(sintaxis()), file, useBytes = TRUE))

    output$excel <- shiny::downloadHandler(
      filename = function() "bootstrap_biplot.xlsx",
      content = function(file) {
        x <- bt()
        writexl::write_xlsx(list("Intervalos" = x$resumen,
                                 "Replicas" = as.data.frame(x$replicas, check.names = FALSE)), file)
      })

    bt
  })
}
