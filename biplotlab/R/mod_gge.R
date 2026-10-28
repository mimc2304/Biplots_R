# =============================================================================
# MÓDULO: GGE-Biplot (Yan et al., 2000; Yan y Tinker, 2006)
#
# Pantalla del GGE-Biplot. El cálculo vive en R/calculo_gge.R para poder usarlo
# sin la app (la "Sintaxis R" las llama desde library(biplotlab)).
# Partes, como en la plantilla: B) registro, C) opciones, D) resultados, E) lógica.
#
# Al integrarlo en el aplicativo (12/10) se agregó, como en el resto de biplots:
# el mínimo 4:3 del plano (con_proporcion_minima), los botones «Voltear» y la
# pestaña Teoría. Los cálculos no cambiaron.
# =============================================================================

# -----------------------------------------------------------------------------
# B) REGISTRO
# -----------------------------------------------------------------------------
#' Entrada del GGE-Biplot en el registro de análisis
#' @export
analisis_gge <- function() {
  list(
    id = "gge",
    titulo = "GGE-Biplot",
    grupo = "Biplot",
    icono = "seedling",
    descripcion = "Interacción individuo × variable (G + GE): quién gana dónde",
    listo = TRUE,
    opciones_ui = mod_gge_opciones_ui,
    resultados_ui = mod_gge_resultados_ui,
    server = mod_gge_server
  )
}

# -----------------------------------------------------------------------------
# C) PANEL DE OPCIONES (izquierda)
# -----------------------------------------------------------------------------
# Nombres que ve el usuario (los códigos son los de calculo_gge.R)
GGE_OPCIONES_CENTRADO <- c(
  "Sin centrar"                 = "ninguno",
  "Global (E + G + GE)"         = "global",
  "Por variable (G + GE)"       = "tester",
  "Doble centrado (GE)"         = "doble"
)
GGE_OPCIONES_ESCALADO <- c(
  "Sin escalar"         = "no",
  "Desviación estándar" = "std"
)
# Menú de herramientas con los nombres que el usuario dio a individuos y variables
gge_menu_herramientas <- function(filas = "Genotipos", columnas = "Ambientes") {
  con_generico <- function(nombre, generico) {
    if (identical(tolower(nombre), tolower(generico))) nombre else sprintf("%s (%s)", nombre, generico)
  }
  f <- con_generico(filas, "Individuos"); co <- con_generico(columnas, "Variables")
  stats::setNames(
    c("biplot", "examinar_ambiente", "examinar_genotipo", "relacion_ambientes",
      "comparar_genotipos", "quien_gano_donde", "discriminacion", "media_estabilidad",
      "ranking_ambientes", "ranking_genotipos"),
    c("Biplot",
      paste("Examinar por", co),
      paste("Examinar por", f),
      paste("Relación entre", co),
      paste("Comparar dos", f),
      "Quién ganó dónde",
      "Discriminación vs representatividad",
      "Media vs estabilidad",
      paste("Ranking de", co, "respecto al ideal"),
      paste("Ranking de", f, "respecto al ideal")))
}

GGE_OPCIONES_PARTICION <- c(
  "GH - conserva la métrica de variables"          = "GH",
  "JK - conserva la métrica de individuos"         = "JK",
  "HJ - representación simultánea"                 = "HJ",
  "SQ - simétrica"                                 = "SQ"
)

mod_gge_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  cuando <- function(...) {
    sprintf("[%s].indexOf(input['%s']) > -1",
            paste0("'", c(...), "'", collapse = ","), ns("herramienta"))
  }
  shiny::tagList(
    encabezado_opciones("GGE-Biplot", "Yan et al. (2000) - Yan y Tinker (2006)"),

    seccion_opciones(
      "Datos",
      shiny::selectizeInput(ns("variables"), "Variables",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"),
                                           placeholder = "Elige al menos 2 variables")),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("nombre_filas"), "Los individuos son", "Genotipos")),
        shiny::column(6, shiny::textInput(ns("nombre_columnas"), "Las variables son", "Ambientes"))
      ),
      shiny::selectizeInput(ns("excluir"), "Excluir individuos",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"), placeholder = "Ninguna"))
    ),

    seccion_opciones(
      "Modelo",
      shiny::selectInput(ns("centrado"), "Centrado", GGE_OPCIONES_CENTRADO, selected = "tester"),
      shiny::radioButtons(ns("escalado"), "Escalado (después de centrar)", GGE_OPCIONES_ESCALADO,
                          selected = "no", inline = TRUE),
      shiny::radioButtons(ns("particion"), "Partición de los valores singulares (SVP)",
                          GGE_OPCIONES_PARTICION, selected = "GH")
    ),

    seccion_opciones(
      "Herramientas biplot",
      shiny::selectInput(ns("herramienta"), "Vista", gge_menu_herramientas(), selected = "biplot"),
      shiny::conditionalPanel(
        cuando("examinar_ambiente"),
        shiny::selectInput(ns("ambiente"), "Variable que se examina", choices = NULL)
      ),
      shiny::conditionalPanel(
        cuando("examinar_genotipo"),
        shiny::selectInput(ns("genotipo"), "Individuo que se examina", choices = NULL)
      ),
      shiny::conditionalPanel(
        cuando("comparar_genotipos"),
        shiny::fluidRow(
          shiny::column(6, shiny::selectInput(ns("genotipo1"), "Individuo 1", choices = NULL)),
          shiny::column(6, shiny::selectInput(ns("genotipo2"), "Individuo 2", choices = NULL))
        )
      ),
      shiny::conditionalPanel(
        cuando("biplot"),
        shiny::radioButtons(ns("mostrar"), "Mostrar",
                            c("Ambos" = "ambos", "Solo individuos" = "filas", "Solo variables" = "columnas"),
                            inline = TRUE),
        shiny::p(class = "bl-help",
                 "Calidad de representación (CRFE) en el plano del gráfico, de 0 a 1000 (500 = 50 %). Se ocultan los que no llegan al mínimo."),
        shiny::sliderInput(ns("cal_filas"), "Calidad mínima de individuos", min = 0, max = 1000,
                           value = 0, step = 50),
        shiny::sliderInput(ns("cal_columnas"), "Calidad mínima de variables", min = 0, max = 1000,
                           value = 0, step = 50)
      )
    ),

    seccion_opciones(
      "Gráfico",
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("eje_x"), "Eje horizontal", 1:2, selected = 1)),
        shiny::column(6, shiny::selectInput(ns("eje_y"), "Eje vertical", 1:2, selected = 2))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("voltear_h"), "Voltear de izquierda a derecha", FALSE)),
        shiny::column(6, shiny::checkboxInput(ns("voltear_v"), "Voltear de arriba a abajo", FALSE))
      ),
      shiny::fluidRow(
        shiny::column(6, colourpicker::colourInput(ns("color_filas"), "Color individuos", COLOR_FILAS)),
        shiny::column(6, colourpicker::colourInput(ns("color_columnas"), "Color variables", COLOR_VECTOR))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::sliderInput(ns("tam_texto"), "Texto", min = 2, max = 6,
                                            value = 3, step = 0.2)),
        shiny::column(6, shiny::sliderInput(ns("tam_puntos"), "Puntos", min = 0.5, max = 5,
                                            value = 1.5, step = 0.1))
      )
    ),

    shiny::actionButton(ns("restablecer"), "Restablecer opciones", icon = shiny::icon("rotate-left"),
                        class = "btn-sm")
  )
}

# -----------------------------------------------------------------------------
# D) PANEL DE RESULTADOS (derecha)
# -----------------------------------------------------------------------------
mod_gge_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("GGE-Biplot", ns("subtitulo")),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        id = ns("pestanas"), type = "tabs",
        shiny::tabPanel(
          "Gráfico",
          shiny::uiOutput(ns("aviso")),
          shiny::plotOutput(ns("grafico"), height = "auto"),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("pdf"), "PDF (vectorial)", class = "btn-sm"),
                     if (requireNamespace("svglite", quietly = TRUE))
                       shiny::downloadButton(ns("svg"), "SVG (editable)", class = "btn-sm")),
          shiny::uiOutput(ns("como_leer"))
        ),
        shiny::tabPanel(
          "Tabla de la herramienta",
          shiny::uiOutput(ns("titulo_tabla")),
          shiny::uiOutput(ns("nota_tabla")),
          DT::DTOutput(ns("tabla_herramienta")),
          shiny::uiOutput(ns("titulo_tabla2")),
          DT::DTOutput(ns("tabla_herramienta2"))
        ),
        shiny::tabPanel(
          "Inercia",
          shiny::div(class = "bl-two",
                     shiny::div(shiny::h4("Valores singulares e inercia"),
                                shiny::tableOutput(ns("inercia"))),
                     shiny::div(shiny::h4("Inercia por eje"),
                                shiny::plotOutput(ns("sedimentacion"), height = "280px")))
        ),
        shiny::tabPanel(
          "Individuos",
          shiny::radioButtons(ns("tabla_filas"), NULL, inline = TRUE,
                              c("CRFE (calidad)" = "calidad_filas",
                                "CREF (contribución al eje)" = "contrib_filas",
                                "Coordenadas" = "coordenadas_filas")),
          shiny::uiOutput(ns("nota_filas")),
          DT::DTOutput(ns("tabla_f"))
        ),
        shiny::tabPanel(
          "Variables",
          shiny::radioButtons(ns("tabla_columnas"), NULL, inline = TRUE,
                              c("CRFE (calidad)" = "calidad_columnas",
                                "CREF (contribución al eje)" = "contrib_columnas",
                                "Coordenadas" = "coordenadas_columnas")),
          shiny::uiOutput(ns("nota_columnas")),
          DT::DTOutput(ns("tabla_c"))
        ),
        shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Fundamentos"), teoria_ui("gge")),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::p(class = "bl-muted",
                   "Código autónomo: incluye las funciones del módulo y funciona en cualquier R con ggplot2 y ggrepel (no hace falta instalar BiplotLab)."),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))),
          shiny::downloadButton(ns("descargar_r"), "Descargar script .R", class = "btn-sm")
        )
      )
    ),
    shiny::div(class = "bl-actions",
               shiny::downloadButton(ns("excel"), "Exportar todas las tablas a Excel"))
  )
}

# -----------------------------------------------------------------------------
# E) LÓGICA
# -----------------------------------------------------------------------------
mod_gge_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    escala_de <- function(d) {
      df <- d$df
      setdiff(names(df)[vapply(df, is.numeric, logical(1))], d$nominales)
    }
    nombre_filas <- shiny::reactive(trimws(input$nombre_filas %||% "") %|v|% "Filas")
    nombre_columnas <- shiny::reactive(trimws(input$nombre_columnas %||% "") %|v|% "Columnas")
    herramienta <- shiny::reactive(input$herramienta %||% "biplot")

    # --- Al cambiar de base, proponer columnas y filas ------------------------
    shiny::observeEvent(datos(), {
      d <- datos()
      escala <- escala_de(d)
      sel <- intersect(shiny::isolate(input$variables), escala)
      if (length(sel) < 2) sel <- escala
      shiny::updateSelectizeInput(session, "variables", choices = escala, selected = sel)
      shiny::updateSelectizeInput(session, "excluir", choices = rownames(d$df),
                                  selected = intersect(shiny::isolate(input$excluir), rownames(d$df)),
                                  server = TRUE)
    }, ignoreNULL = TRUE)

    # --- Menú de herramientas con los nombres de filas y columnas ---------------
    shiny::observeEvent(list(nombre_filas(), nombre_columnas()), {
      shiny::updateSelectInput(session, "herramienta",
                               choices = gge_menu_herramientas(nombre_filas(), nombre_columnas()),
                               selected = shiny::isolate(input$herramienta) %||% "biplot")
    }, ignoreInit = TRUE)

    # --- Restablecer todas las opciones a sus valores iniciales ------------------
    shiny::observeEvent(input$restablecer, {
      if (!is.null(datos())) {
        shiny::updateSelectizeInput(session, "variables", selected = escala_de(datos()))
      }
      shiny::updateSelectizeInput(session, "excluir", selected = character())
      shiny::updateTextInput(session, "nombre_filas", value = "Genotipos")
      shiny::updateTextInput(session, "nombre_columnas", value = "Ambientes")
      shiny::updateSelectInput(session, "centrado", selected = "tester")
      shiny::updateRadioButtons(session, "escalado", selected = "no")
      shiny::updateRadioButtons(session, "particion", selected = "GH")
      shiny::updateSelectInput(session, "herramienta", selected = "biplot")
      shiny::updateRadioButtons(session, "mostrar", selected = "ambos")
      shiny::updateSliderInput(session, "cal_filas", value = 0)
      shiny::updateSliderInput(session, "cal_columnas", value = 0)
      shiny::updateSelectInput(session, "eje_x", selected = 1)
      shiny::updateSelectInput(session, "eje_y", selected = 2)
      shiny::updateCheckboxInput(session, "voltear_h", value = FALSE)
      shiny::updateCheckboxInput(session, "voltear_v", value = FALSE)
      colourpicker::updateColourInput(session, "color_filas", value = COLOR_FILAS)
      colourpicker::updateColourInput(session, "color_columnas", value = COLOR_VECTOR)
      shiny::updateSliderInput(session, "tam_texto", value = 3)
      shiny::updateSliderInput(session, "tam_puntos", value = 1.5)
    })

    # Elegir un elemento conservando la elección anterior si sigue existiendo
    actualizar_lista <- function(id_input, opciones, posicion = 1) {
      actual <- shiny::isolate(input[[id_input]])
      shiny::updateSelectInput(session, id_input, choices = opciones,
                               selected = if (!is.null(actual) && actual %in% opciones) actual else
                                 opciones[min(posicion, length(opciones))])
    }
    shiny::observeEvent(input$variables, {
      actualizar_lista("ambiente", input$variables %||% character())
    }, ignoreNULL = FALSE)

    # --- Cálculo ---------------------------------------------------------------
    datos_analisis <- shiny::reactive({
      df <- datos()$df
      if (length(input$excluir)) df <- df[!rownames(df) %in% input$excluir, , drop = FALSE]
      df
    })

    resultado <- shiny::reactive({
      if (is.null(datos())) {
        return(simpleError("Primero carga una base de datos: botón «Datos» de la cinta superior."))
      }
      if (length(input$variables) < 2) {
        return(simpleError("Elige al menos 2 variables en el panel de opciones."))
      }
      tryCatch(calcular_gge(datos_analisis(), variables = input$variables,
                            centrado = input$centrado, escalado = input$escalado,
                            particion = input$particion),
               error = function(e) e)
    })
    res_ok <- shiny::reactive({
      r <- resultado()
      shiny::validate(shiny::need(!inherits(r, "error"), ""))
      r
    })

    shiny::observeEvent(rownames(res_ok()$filas), {
      g <- rownames(res_ok()$filas)
      actualizar_lista("genotipo", g)
      actualizar_lista("genotipo1", g, 1)
      actualizar_lista("genotipo2", g, 2)
    })

    shiny::observeEvent(res_ok()$dimensiones, {
      k <- res_ok()$dimensiones
      sel_x <- min(as.integer(input$eje_x %||% 1), k)
      sel_y <- min(as.integer(input$eje_y %||% 2), k)
      if (sel_x == sel_y) { sel_x <- 1; sel_y <- 2 }
      shiny::updateSelectInput(session, "eje_x", choices = seq_len(k), selected = sel_x)
      shiny::updateSelectInput(session, "eje_y", choices = seq_len(k), selected = sel_y)
    })
    ejes <- shiny::reactive({
      e <- as.integer(c(input$eje_x %||% 1, input$eje_y %||% 2))
      if (e[1] == e[2] || any(e > res_ok()$dimensiones)) c(1L, 2L) else e
    })

    # Voltear: cambia el signo del eje en individuos y variables a la vez (los
    # productos escalares, la inercia y las calidades no cambian)
    res_volteado <- shiny::reactive({
      r <- res_ok(); e <- ejes()
      k <- c(if (isTRUE(input$voltear_h)) e[1], if (isTRUE(input$voltear_v)) e[2])
      if (length(k)) { r$filas[, k] <- -r$filas[, k]; r$columnas[, k] <- -r$columnas[, k] }
      r
    })

    opciones_grafico <- shiny::reactive(list(
      color_filas = input$color_filas %||% COLOR_FILAS,
      color_columnas = input$color_columnas %||% COLOR_VECTOR,
      tam_texto = input$tam_texto %||% 3, tam_puntos = input$tam_puntos %||% 1.5
    ))

    # Vista elegida: gráfico (calculo_gge.R), tablas y nota (tablas_gge.R)
    vista <- shiny::reactive({
      r <- res_volteado()
      h <- herramienta()
      g <- rownames(r$filas)
      if (h == "examinar_ambiente") shiny::req(input$ambiente %in% colnames(r$tabla))
      if (h == "examinar_genotipo") shiny::req(input$genotipo %in% g)
      if (h == "comparar_genotipos") {
        shiny::req(input$genotipo1 %in% g, input$genotipo2 %in% g)
        shiny::validate(shiny::need(input$genotipo1 != input$genotipo2,
                                    "Elige dos individuos distintos para compararlos."))
      }
      titulo <- names(gge_menu_herramientas(nombre_filas(), nombre_columnas()))[GGE_HERRAMIENTAS == h]
      grafico <- do.call(gge_grafico_herramienta, c(
        list(r, h, ejes(), ambiente = input$ambiente, genotipo = input$genotipo,
             genotipo1 = input$genotipo1, genotipo2 = input$genotipo2,
             mostrar = input$mostrar %||% "ambos", cal_filas = input$cal_filas %||% 0,
             cal_columnas = input$cal_columnas %||% 0, titulo = titulo),
        opciones_grafico()))
      tablas <- gge_tabla_herramienta(r, h, ejes(), ambiente = input$ambiente,
                                      genotipo = input$genotipo, genotipo1 = input$genotipo1,
                                      genotipo2 = input$genotipo2,
                                      etiqueta_filas = nombre_filas(), etiqueta_columnas = nombre_columnas())
      c(list(grafico = grafico), tablas)
    })
    # Plano nunca más alto que ancho (mínimo 4:3, observación de la Dra.)
    grafico <- shiny::reactive(con_proporcion_minima(vista()$grafico))

    tablas <- shiny::reactive(gge_tablas(res_volteado(), ejes(), nombre_filas(), nombre_columnas()))

    # --- Salidas ---------------------------------------------------------------
    output$error <- shiny::renderUI({
      r <- resultado()
      if (inherits(r, "error")) caja_error(conditionMessage(r))
    })

    output$subtitulo <- shiny::renderUI({
      r <- res_ok()
      shiny::span(class = "bl-sub", sprintf(
        "%s - %d %s × %d %s - %s - SVP %s%s",
        datos()$etiqueta, r$n, tolower(nombre_filas()), r$p, tolower(nombre_columnas()),
        names(GGE_OPCIONES_CENTRADO)[GGE_OPCIONES_CENTRADO == r$centrado], r$particion,
        if (length(input$excluir)) sprintf(" - %d excluidas", length(input$excluir)) else ""))
    })

    output$grafico <- render_biplot(grafico, session, salida = "grafico")
    nombre_archivo <- function(ext) function() {
      sprintf("gge-%s_%s_ejes%d-%d.%s", herramienta(), tolower(res_ok()$particion),
              ejes()[1], ejes()[2], ext)
    }
    guardar <- function(ext, ...) descarga_biplot(grafico, nombre_archivo(ext), ext, ...)
    output$png <- guardar("png", dpi = 300, bg = "white")
    output$pdf <- guardar("pdf")
    output$svg <- guardar("svg")

    output$aviso <- shiny::renderUI({
      v <- vista()
      shiny::tagList(
        if (!is.null(v$nota)) shiny::div(class = "bl-info", shiny::icon("lightbulb"), shiny::span(v$nota))
      )
    })

    output$como_leer <- shiny::renderUI({
      txt <- texto_como_leer_gge(herramienta(), input, nombre_filas(), nombre_columnas())
      shiny::div(class = "bl-info", shiny::icon("circle-info"), shiny::span(txt))
    })

    output$titulo_tabla <- shiny::renderUI({
      v <- vista()
      if (is.null(v$tabla)) return(NULL)
      f <- tolower(nombre_filas()); co <- tolower(nombre_columnas())
      shiny::h4(switch(herramienta(),
        examinar_ambiente  = sprintf("Ranking de %s en %s", f, input$ambiente),
        examinar_genotipo  = sprintf("Ranking de %s para %s", co, input$genotipo),
        relacion_ambientes = sprintf("Coseno de los ángulos entre %s en el biplot", co),
        comparar_genotipos = sprintf("Diferencia %s − %s en cada variable (%s)", input$genotipo1, input$genotipo2, co),
        quien_gano_donde   = sprintf("Ganador en cada variable (%s)", co),
        discriminacion     = sprintf("Discriminación y representatividad de %s", co),
        media_estabilidad  = sprintf("Media y estabilidad de %s", f),
        ranking_ambientes  = sprintf("Ranking de %s respecto al ideal", co),
        ranking_genotipos  = sprintf("Ranking de %s respecto al ideal", f),
        "Tabla de la herramienta"))
    })
    output$nota_tabla <- shiny::renderUI({
      v <- vista()
      if (is.null(v$tabla)) {
        return(shiny::div(class = "bl-info", shiny::icon("circle-info"),
                          "Elige una herramienta biplot (por ejemplo «Examinar por …») para ver su tabla."))
      }
      shiny::p(class = "bl-help", texto_tabla_gge(herramienta(), res_ok(), ejes(), nombre_filas(), nombre_columnas()))
    })
    output$tabla_herramienta <- DT::renderDT({
      v <- vista()
      shiny::req(v$tabla)
      tabla_resultados(v$tabla, 3, 25)
    })
    output$titulo_tabla2 <- shiny::renderUI({
      v <- vista()
      if (is.null(v$tabla2)) return(NULL)
      shiny::h4(switch(herramienta(),
                       relacion_ambientes = sprintf("Correlación real entre %s (tabla analizada)", tolower(nombre_columnas())),
                       quien_gano_donde = "Sectores (variables con el mismo ganador)",
                       "Tabla complementaria"))
    })
    output$tabla_herramienta2 <- DT::renderDT({
      v <- vista()
      shiny::req(v$tabla2)
      tabla_resultados(v$tabla2, 3, 25)
    })

    output$inercia <- shiny::renderTable({
      t <- tablas()$inercia
      t$Eje <- as.integer(t$Eje)
      con_decimales(traducir_tabla(t))
    }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")

    output$sedimentacion <- shiny::renderPlot({
      inr <- tablas()$inercia
      ggplot2::ggplot(inr, ggplot2::aes(factor(.data$Eje), .data$`% Inercia`)) +
        ggplot2::geom_col(fill = COLOR_FILAS, width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = format(round(.data$`% Inercia`, 1), decimal.mark = marca_decimal())),
                           vjust = -0.4, size = 3.2, colour = "#52514e") +
        ggplot2::labs(x = traducir("Eje"), y = traducir("% de inercia")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
    }, res = 96)

    nota <- function(cual) {
      txt <- switch(cual,
        calidad = "CRFE: calidad de representación (coseno², por mil), calculada con las coordenadas de la partición elegida, como GGEBiplotGUI. La columna «Plano» suma los dos ejes del gráfico.",
        contrib = "CREF: contribución del elemento a cada eje (por mil). En cada eje suman 1000.",
        coordenadas = "Coordenadas de la partición elegida, sin el ajuste visual del gráfico (signos tal como salen de la SVD).")
      shiny::p(class = "bl-help", txt)
    }
    tipo_tabla <- function(nombre) sub("_(filas|columnas)$", "", nombre)
    output$nota_filas <- shiny::renderUI(nota(tipo_tabla(input$tabla_filas)))
    output$nota_columnas <- shiny::renderUI(nota(tipo_tabla(input$tabla_columnas)))
    decimales <- function(nombre) if (grepl("^coordenadas", nombre)) 3 else 0
    output$tabla_f <- DT::renderDT(tabla_resultados(tablas()[[input$tabla_filas]],
                                                    decimales(input$tabla_filas)))
    output$tabla_c <- DT::renderDT(tabla_resultados(tablas()[[input$tabla_columnas]],
                                                    decimales(input$tabla_columnas), 25))

    # --- Sintaxis R ----------------------------------------------------------------
    sintaxis <- shiny::reactive({
      r <- res_ok()
      cadena <- function(x) encodeString(x, quote = "\"")
      og <- opciones_grafico()
      extras <- c(
        if (!identical(og$color_filas, COLOR_FILAS)) sprintf("color_filas = \"%s\"", og$color_filas),
        if (!identical(og$color_columnas, COLOR_VECTOR)) sprintf("color_columnas = \"%s\"", og$color_columnas),
        if (og$tam_texto != 3) sprintf("tam_texto = %s", og$tam_texto),
        if (og$tam_puntos != 1.5) sprintf("tam_puntos = %s", og$tam_puntos))
      ej <- paste(c(sprintf("ejes = c(%d, %d)", ejes()[1], ejes()[2]), extras), collapse = ", ")
      h <- herramienta()
      volteados <- c(if (isTRUE(input$voltear_h)) ejes()[1], if (isTRUE(input$voltear_v)) ejes()[2])
      mostrar <- input$mostrar %||% "ambos"
      cal_f <- if (h == "biplot" && mostrar != "columnas") input$cal_filas %||% 0 else 0
      cal_c <- if (h == "biplot" && mostrar != "filas") input$cal_columnas %||% 0 else 0
      ver_g <- if (mostrar == "columnas") ", ver_genotipos = character(0)" else
        if (cal_f > 0) ", ver_genotipos = ver_filas" else ""
      ver_a <- if (mostrar == "filas") ", ver_ambientes = character(0)" else
        if (cal_c > 0) ", ver_ambientes = ver_columnas" else ""
      llamada <- switch(h,
        biplot = c(
          if (cal_f > 0) c(sprintf("# Individuos con calidad en el plano de al menos %s por mil", cal_f),
            "calidad <- round(resultados$calidad_filas[, \"Plano\"])",
            sprintf("ver_filas <- names(calidad)[calidad >= %s]", cal_f)),
          if (cal_c > 0) c(sprintf("# Variables con calidad en el plano de al menos %s por mil", cal_c),
            "calidad <- round(resultados$calidad_columnas[, \"Plano\"])",
            sprintf("ver_columnas <- names(calidad)[calidad >= %s]", cal_c)),
          sprintf("grafico <- gge_biplot_basico(desc, %s%s%s)", ej, ver_g, ver_a)),
        examinar_ambiente = sprintf("grafico <- gge_examinar_ambiente(desc, %s, %s)", cadena(input$ambiente), ej),
        examinar_genotipo = sprintf("grafico <- gge_examinar_genotipo(desc, %s, %s)", cadena(input$genotipo), ej),
        relacion_ambientes = sprintf("grafico <- gge_relacion_ambientes(desc, %s)", ej),
        comparar_genotipos = sprintf("grafico <- gge_comparar_genotipos(desc, %s, %s, %s)",
                                     cadena(input$genotipo1), cadena(input$genotipo2), ej),
        quien_gano_donde = c(
          "# Encuadre con los puntos: las rectas de los sectores se cortan en el borde",
          sprintf("coord <- gge_coordenadas_grafico(desc, ejes = c(%d, %d))", ejes()[1], ejes()[2]),
          "puntos <- rbind(coord$genotipos[, c(\"x\", \"y\")], coord$ambientes[, c(\"x\", \"y\")])",
          sprintf("grafico <- gge_quien_gano_donde(desc, %s) +", ej),
          "  ggplot2::coord_equal(xlim = range(puntos$x), ylim = range(puntos$y))"),
        discriminacion = sprintf("grafico <- gge_dis_rep(desc, %s)", ej),
        media_estabilidad = sprintf("grafico <- gge_media_estabilidad(desc, %s)", ej),
        ranking_ambientes = sprintf("grafico <- gge_ranking_ambientes(desc, %s)", ej),
        ranking_genotipos = sprintf("grafico <- gge_ranking_genotipos(desc, %s)", ej))
      lineas <- c(
        sprintf("# GGE-Biplot: %s",
                names(gge_menu_herramientas(nombre_filas(), nombre_columnas()))[GGE_HERRAMIENTAS == h]),
        sintaxis_paquete(),
        "",
        sintaxis_lectura(datos()$origen),
        if (length(input$excluir)) c(
          "",
          "# Individuos excluidos del análisis",
          sprintf("datos <- datos[!rownames(datos) %%in%% %s, ]", vector_a_r(input$excluir, 2))),
        "",
        sprintf("variables <- %s", vector_a_r(input$variables, 13)),
        "tabla <- as.matrix(datos[, variables])",
        sprintf("desc <- gge_descomposicion(gge_escalado(gge_centrado(tabla, \"%s\"), \"%s\"), \"%s\")",
                r$centrado, r$escalado, r$particion),
        if (length(volteados)) c(
          "# Ejes volteados (no cambia ningún producto escalar)",
          sprintf("desc$filas[, %s] <- -desc$filas[, %s]", vector_a_r(volteados), vector_a_r(volteados)),
          sprintf("desc$columnas[, %s] <- -desc$columnas[, %s]", vector_a_r(volteados), vector_a_r(volteados))),
        sprintf("resultados <- gge_resultados(desc, eje = c(%d, %d))", ejes()[1], ejes()[2]),
        "resultados$inercia                       # valores propios e inercia",
        "round(resultados$calidad_filas)          # CRFE de los individuos (por mil)",
        "round(resultados$calidad_columnas)       # CRFE de las variables (por mil)",
        "",
        llamada,
        "grafico"
      )
      paste(lineas, collapse = "\n")
    })
    output$sintaxis <- shiny::renderText(sintaxis())
    output$descargar_r <- shiny::downloadHandler(
      filename = function() "gge_biplot.R",
      content = function(file) writeLines(enc2utf8(sintaxis()), file, useBytes = TRUE)
    )

    # --- Exportación a Excel ---------------------------------------------------------
    output$excel <- shiny::downloadHandler(
      filename = function() sprintf("resultados_gge-biplot_%s.xlsx", tolower(res_ok()$particion)),
      content = function(file) {
        t <- tablas()
        hojas <- list(
          "Inercia"              = t$inercia,
          "Coordenadas individuos" = t$coordenadas_filas,
          "Coordenadas variables"  = t$coordenadas_columnas,
          "CRFE individuos"        = t$calidad_filas,
          "CRFE variables"         = t$calidad_columnas,
          "CREF individuos"        = t$contrib_filas,
          "CREF variables"         = t$contrib_columnas
        )
        v <- vista()
        nombre <- substr(names(gge_menu_herramientas(nombre_filas(), nombre_columnas()))[GGE_HERRAMIENTAS == herramienta()], 1, 31)
        if (!is.null(v$tabla)) hojas[[nombre]] <- v$tabla
        if (!is.null(v$tabla2)) hojas[[substr(paste(nombre, "(2)"), 1, 31)]] <- v$tabla2
        guardar_xlsx(hojas, file)
      }
    )

    res_ok
  })
}

# --- Textos de ayuda ----------------------------------------------------------------

texto_como_leer_gge <- function(herramienta, input, filas, columnas) {
  f <- tolower(filas); co <- tolower(columnas)
  switch(herramienta,
    biplot = sprintf("El valor aproximado de cada celda es el producto escalar entre el marcador del individuo (%s) y el de la variable (%s) (Ec. 1 de Yan y Tinker, 2006). Con GH, JK y SQ vale lo mismo; con HJ no.", f, co),
    examinar_ambiente = sprintf(
      "La línea continua es el eje de la variable %s (pasa por el origen y por su marcador). Cada individuo (%s) se proyecta en perpendicular sobre ese eje: cuanto más avanza en la dirección de la flecha, mayor es su valor en %s. La perpendicular por el origen separa a los que quedan por encima y por debajo del promedio (Yan y Tinker, 2006, Fig. 6).",
      input$ambiente, f, input$ambiente),
    examinar_genotipo = sprintf(
      "La línea continua es el eje del individuo %s. Cada variable (%s) se proyecta sobre él: %s queda por encima del promedio en las que avanzan en la dirección de la flecha y por debajo en las del otro lado de la perpendicular (Yan y Tinker, 2006, Fig. 7).",
      input$genotipo, co, input$genotipo),
    relacion_ambientes = sprintf("El coseno del ángulo entre dos variables (%s) aproxima su correlación: agudo = correlación positiva, recto = sin correlación, obtuso = negativa. Los vectores largos discriminan más (Yan y Tinker, 2006, Fig. 2).", co),
    comparar_genotipos = sprintf(
      "El segmento rojo une %s y %s; la recta roja por el origen, perpendicular al segmento, es la línea de igualdad. En las variables (%s) que quedan del lado de un individuo, ese individuo tiene el valor más alto (Yan y Tinker, 2006, Fig. 11).",
      input$genotipo1, input$genotipo2, co),
    quien_gano_donde = sprintf("El polígono une los individuos (%s) más alejados del origen. Las rectas rojas, perpendiculares a cada lado, dividen el plano en sectores: el individuo del vértice gana en las variables (%s) de su sector (Yan y Tinker, 2006, Fig. 12).", f, co),
    discriminacion = sprintf("La flecha apunta a la variable media y la recta es su eje (AEA). Vector largo = variable discriminante; ángulo pequeño con el AEA = variable representativa (%s) (Yan y Tinker, 2006, Fig. 3).", co),
    media_estabilidad = sprintf("La recta con flecha es el eje de la variable media (AEA): la proyección de cada individuo (%s) sobre ella estima su media (más hacia la flecha, mayor). La distancia de cada individuo al AEA (punteadas) mide su inestabilidad (Yan y Tinker, 2006, Fig. 8).", f),
    ranking_ambientes = sprintf("La variable ideal (punta de la flecha, centro de los círculos) está sobre el AEA a la distancia de la variable más larga: máxima discriminación y representatividad. Cuanto más cerca del centro, mejor es la variable (%s) (Yan y Tinker, 2006, Fig. 4).", co),
    ranking_genotipos = sprintf("El individuo ideal (punta de la flecha, centro de los círculos) está sobre el AEA, del lado positivo: media alta y máxima estabilidad. Cuanto más cerca del centro, más deseable es el individuo (%s) (Yan y Tinker, 2006, Fig. 9).", f),
    "")
}

texto_tabla_gge <- function(herramienta, res, ejes, filas = "Individuos", columnas = "Variables") {
  f <- tolower(filas); co <- tolower(columnas)
  base <- sprintf("Calculada con las coordenadas %s de los ejes %d y %d (sin el ajuste visual). ",
                  res$particion, ejes[1], ejes[2])
  tabla <- if (identical(res$escalado, "std")) "centrada y escalada" else "analizada"
  paste0(base, switch(herramienta,
    examinar_ambiente = sprintf("En GH, JK y SQ el producto escalar es la celda aproximada; «Valor en la tabla» es el dato de la tabla %s, para comparar.", tabla),
    examinar_genotipo = sprintf("En GH, JK y SQ el producto escalar es la celda aproximada; «Valor en la tabla» es el dato de la tabla %s.", tabla),
    relacion_ambientes = sprintf("Coseno de los ángulos entre las variables (%s) en el plano; debajo, la correlación real, para comparar.", co),
    comparar_genotipos = sprintf("Diferencia individuo 1 − individuo 2 en cada variable (%s): según el biplot (producto escalar) y en la tabla %s.", co, tabla),
    quien_gano_donde = sprintf("Ganador de cada variable (%s) según el biplot (mayor producto escalar) y en la tabla; debajo, los sectores.", co),
    discriminacion = "Ordenada de más a menos discriminante. La desviación estándar de la tabla sirve para comparar con la longitud del vector.",
    media_estabilidad = "Ordenada por la proyección sobre el AEA (media). «Media estimada» es el promedio de las celdas aproximadas; «Media en la tabla», el real.",
    ranking_ambientes = sprintf("Ordenada por la distancia a la variable ideal (menor = mejor variable; %s).", co),
    ranking_genotipos = sprintf("Ordenada por la distancia al individuo ideal (menor = más deseable; %s).", f),
    ""))
}

# --- Ayudas internas ------------------------------------------------------------------

# Texto vacío -> valor por defecto
`%|v|%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a

# Nombres con calidad en el plano (por mil, redondeada como en la tabla) de al
# menos `minimo`; NULL = todos
gge_con_calidad <- function(calidad, minimo) {
  if (is.null(minimo) || minimo <= 0) return(NULL)
  q <- round(calidad[, "Plano"])
  names(q)[q >= minimo]
}

# Gráfico de cada vista con las funciones de calculo_gge.R, más el título y el
# subtítulo de la app. cal_filas y cal_columnas (solo en el biplot): calidad
# mínima en el plano para dibujar un individuo o una variable.
gge_grafico_herramienta <- function(res, herramienta, ejes = c(1, 2), ambiente = NULL,
                                    genotipo = NULL, genotipo1 = NULL, genotipo2 = NULL,
                                    mostrar = "ambos", cal_filas = 0, cal_columnas = 0,
                                    titulo = NULL, ...) {
  if (herramienta == "biplot") calidad <- gge_resultados(res, ejes)
  grafico <- suppressMessages(switch(herramienta,
    biplot = gge_biplot_basico(res, ejes,
                               ver_genotipos = if (mostrar == "columnas") character(0) else
                                 gge_con_calidad(calidad$calidad_filas, cal_filas),
                               ver_ambientes = if (mostrar == "filas") character(0) else
                                 gge_con_calidad(calidad$calidad_columnas, cal_columnas), ...),
    examinar_ambiente  = gge_examinar_ambiente(res, ambiente, ejes, ...),
    examinar_genotipo  = gge_examinar_genotipo(res, genotipo, ejes, ...),
    relacion_ambientes = gge_relacion_ambientes(res, ejes, ...),
    comparar_genotipos = gge_comparar_genotipos(res, genotipo1, genotipo2, ejes, ...),
    quien_gano_donde   = gge_quien_gano_donde(res, ejes, ...),
    discriminacion     = gge_dis_rep(res, ejes, ...),
    media_estabilidad  = gge_media_estabilidad(res, ejes, ...),
    ranking_ambientes  = gge_ranking_ambientes(res, ejes, ...),
    ranking_genotipos  = gge_ranking_genotipos(res, ejes, ...),
    stop("Herramienta no reconocida. Usa: ", paste(GGE_HERRAMIENTAS, collapse = ", "), call. = FALSE)))

  # Quién ganó dónde: encuadre con los puntos; las rectas de los sectores se
  # cortan en el borde (si no, agrandan el plano y la figura queda pequeña)
  if (herramienta == "quien_gano_donde") {
    co <- gge_coordenadas_grafico(res, ejes)
    puntos <- rbind(co$genotipos[, c("x", "y")], co$ambientes[, c("x", "y")])
    grafico <- suppressMessages(grafico + ggplot2::coord_equal(xlim = range(puntos$x),
                                                               ylim = range(puntos$y)))
  }

  centrado <- names(GGE_OPCIONES_CENTRADO)[GGE_OPCIONES_CENTRADO == (res$centrado %||% "tester")]
  subtitulo <- paste0("GGE-Biplot - ", centrado, " - SVP ", res$particion,
                      if (identical(res$escalado, "std")) " - escalado por desviación estándar")
  grafico +
    ggplot2::labs(title = titulo, subtitle = subtitulo) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"),
                   plot.subtitle = ggplot2::element_text(colour = "#6b6a65", size = 10),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
