# =============================================================================
# MÓDULO: HJ-Biplot (Galindo-Villardón, 1986)
#
# Este archivo sirve también de MODELO para los demás módulos: tiene las tres
# piezas que pide el registro (opciones_ui, resultados_ui y server).
# El cálculo vive en R/calculo_hj_biplot.R para poder usarlo sin la app.
#
# Comparte su configuración y su resultado con los módulos que dependen del
# núcleo HJ (Bootstrap y Clúster) a través de `compartido`.
# =============================================================================

#' Entrada del HJ-Biplot en el registro de análisis
#' @export
analisis_hj_biplot <- function() {
  list(
    id = "hj", titulo = "HJ-Biplot", grupo = "Biplot", icono = "arrows-to-dot",
    descripcion = "Representación simultánea de filas y columnas con la misma calidad",
    listo = TRUE,
    opciones_ui = mod_hj_opciones_ui,
    resultados_ui = mod_hj_resultados_ui,
    server = mod_hj_server
  )
}

N_BLOQUES <- 3

# ---------------------------------------------------------------------------
# Panel de opciones (izquierda)
# ---------------------------------------------------------------------------
mod_hj_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("HJ-Biplot", "Galindo-Villardón (1986) - Gabriel (1971)"),

    seccion_opciones(
      "Variables",
      shiny::selectizeInput(ns("variables"), "Variables activas (escala; construyen los ejes)",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"),
                                           placeholder = "Elige al menos 2 variables")),
      shiny::div(class = "bl-links",
                 shiny::actionLink(ns("todas"), "Todas las de escala"),
                 shiny::actionLink(ns("ninguna"), "Quitar todas")),
      shiny::p(class = "bl-help",
               "Las columnas nominales (de agrupación) no aparecen aquí: se marcan en el panel Datos."),
      shiny::selectInput(ns("grupo"), "Variable de grupo (colorea; no construye ejes)",
                         choices = c("(ninguna)" = "")),
      shiny::selectizeInput(ns("excluir"), "Excluir individuos (atípicos o errores)",
                            choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"),
                                           placeholder = "Ninguno"))
    ),

    seccion_opciones(
      "Preparación de la matriz",
      shiny::selectInput(ns("transformacion"), "Transformación", TRANSFORMACIONES,
                         selected = "estandarizar"),
      shiny::uiOutput(ns("ayuda_transf")),
      shiny::checkboxInput(ns("log"), "Aplicar logaritmo antes (solo valores > 0)", FALSE)
    ),

    seccion_opciones(
      "Modelo y orientación",
      shiny::radioButtons(ns("tipo"), "Tipo de biplot",
                          c("HJ" = "HJ", "GH (columnas)" = "GH", "JK (filas)" = "JK"),
                          selected = "HJ", inline = TRUE),
      shiny::numericInput(ns("dims"), "Número de ejes a calcular", value = 3, min = 2,
                          max = 10, step = 1),
      shiny::selectInput(ns("referencia"), "Convenio de signos",
                         c("Signo de la SVD (como MultBiplotR)" = "")),
      shiny::p(class = "bl-help",
               "Para dar la vuelta al gráfico usa «Voltear» en la sección Gráfico.")
    ),

    seccion_opciones(
      "Gráfico",
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("eje_x"), "Eje horizontal", 1:3, selected = 1)),
        shiny::column(6, shiny::selectInput(ns("eje_y"), "Eje vertical", 1:3, selected = 2))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("voltear_h"), "Voltear de izquierda a derecha", FALSE)),
        shiny::column(6, shiny::checkboxInput(ns("voltear_v"), "Voltear de arriba a abajo", FALSE))
      ),
      shiny::p(class = "bl-help",
               "El signo de cada eje es arbitrario: voltear el gráfico no cambia inercia, calidades, ángulos ni distancias. Se aplica también a las tablas de coordenadas, al Clúster y al Bootstrap."),
      shiny::radioButtons(ns("mostrar"), "Mostrar",
                          c("Individuos y variables" = "ambos", "Solo individuos" = "filas",
                            "Solo variables" = "columnas"), inline = TRUE),
      shiny::fluidRow(
        shiny::column(6, shiny::checkboxInput(ns("etiquetas"), "Nombres de individuos", TRUE)),
        shiny::column(6, shiny::checkboxInput(ns("etiquetas_c"), "Nombres de variables", TRUE))
      ),
      shiny::checkboxInput(ns("envolventes"), "Envolvente de cada grupo", FALSE),
      shiny::sliderInput(ns("escala"), "Longitud de los vectores (solo visual)",
                         min = 0.2, max = 3, value = 1, step = 0.1),
      shiny::fluidRow(
        shiny::column(6, shiny::sliderInput(ns("tam_texto"), "Texto", min = 2, max = 5,
                                            value = 3.2, step = 0.2)),
        shiny::column(6, shiny::sliderInput(ns("tam_puntos"), "Puntos", min = 0.5, max = 5,
                                            value = 2.2, step = 0.1))
      )
    ),

    seccion_opciones(
      "Calidad de representación (CRFE)",
      shiny::p(class = "bl-help",
               "CRFE acumulada en el plano del gráfico, de 0 a 1000 (500 = 50 %). Lo que no llega al umbral no debe interpretarse en este plano."),
      shiny::sliderInput(ns("cal_filas"), "Mínimo para individuos", min = 0, max = 1000,
                         value = 0, step = 50),
      shiny::sliderInput(ns("cal_columnas"), "Mínimo para variables", min = 0, max = 1000,
                         value = 0, step = 50),
      shiny::radioButtons(ns("mal"), "Lo que no llega al mínimo",
                          c("Atenuar (gris, sin nombre)" = "atenuar", "Ocultar" = "ocultar"),
                          inline = TRUE),
      shiny::uiOutput(ns("conteo_calidad"))
    ),

    seccion_opciones(
      "Colores",
      shiny::uiOutput(ns("colores_grupos_ui")),
      colourpicker::colourInput(ns("color_vectores"), "Vectores (variables)", COLOR_VECTOR),
      shiny::p(class = "bl-help", "Bloques de variables: los vectores de cada bloque toman su color."),
      lapply(seq_len(N_BLOQUES), function(b) {
        shiny::fluidRow(
          shiny::column(8, shiny::selectizeInput(
            ns(paste0("bloque_", b)), paste("Bloque", b), choices = NULL, multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Variables"))),
          shiny::column(4, colourpicker::colourInput(
            ns(paste0("bloque_col_", b)), "Color", PALETA_GRUPOS[b + 1]))
        )
      }),
      shiny::actionLink(ns("restablecer"), "Restablecer colores"),
      abierta = FALSE
    ),

    seccion_opciones(
      "Tablas",
      shiny::radioButtons(ns("unidad"), "CRFE y CREF en",
                          c("Por mil (0-1000)" = "milesima", "Porcentaje (%)" = "porcentaje"),
                          inline = TRUE),
      shiny::sliderInput(ns("umbral"), "CRFE mínima para la lectura guiada (0-1000)",
                         min = 300, max = 800, value = 500, step = 50),
      abierta = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# Panel de resultados (derecha)
# ---------------------------------------------------------------------------
mod_hj_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("HJ-Biplot", ns("subtitulo")),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        id = ns("pestanas"), type = "tabs",
        shiny::tabPanel(
          "Biplot",
          shiny::plotOutput(ns("biplot"), height = "auto"),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("pdf"), "PDF (vectorial)", class = "btn-sm"),
                     if (requireNamespace("svglite", quietly = TRUE))
                       shiny::downloadButton(ns("svg"), "SVG (editable)", class = "btn-sm"))
        ),
        shiny::tabPanel("Lectura guiada", shiny::uiOutput(ns("lectura"))),
        shiny::tabPanel(
          "Inercia",
          shiny::div(class = "bl-two",
                     shiny::div(shiny::h4("Valores propios e inercia"),
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
                                "Coordenadas" = "coordenadas_columnas",
                                "Correlaciones" = "correlaciones")),
          shiny::uiOutput(ns("nota_columnas")),
          DT::DTOutput(ns("tabla_c"))
        ),
        shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Fundamentos"), teoria_ui("hj")),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::p(class = "bl-muted",
                   "Este código reproduce exactamente el resultado en R o RStudio."),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))),
          shiny::downloadButton(ns("descargar_r"), "Descargar script .R", class = "btn-sm")
        )
      )
    ),
    shiny::div(class = "bl-actions",
               shiny::downloadButton(ns("excel"), "Exportar todas las tablas a Excel"))
  )
}

# ---------------------------------------------------------------------------
# Lógica
# ---------------------------------------------------------------------------
# `datos`: reactive con list(df, origen, etiqueta, nominales).
# `compartido`: reactiveValues donde se publica el HJ para Bootstrap y Clúster.
mod_hj_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    escala_de <- function(d) {
      df <- d$df
      setdiff(names(df)[vapply(df, is.numeric, logical(1))], d$nominales)
    }

    # --- Al cambiar de base o de columnas nominales, proponer variables ------
    shiny::observeEvent(datos(), {
      d <- datos()
      df <- d$df
      escala <- escala_de(d)
      sel <- intersect(shiny::isolate(input$variables), escala)
      if (length(sel) < 2) sel <- escala
      shiny::updateSelectizeInput(session, "variables", choices = escala, selected = sel)

      candidatos <- setdiff(names(df), escala)
      g_actual <- shiny::isolate(input$grupo)
      g_sel <- if (!is.null(g_actual) && g_actual %in% candidatos) g_actual else {
        nom <- intersect(d$nominales, candidatos)
        texto <- nom[vapply(df[nom], function(v) length(unique(v)) <= 12, logical(1))]
        if (length(texto)) texto[1] else ""
      }
      shiny::updateSelectInput(session, "grupo", choices = c("(ninguna)" = "", candidatos),
                               selected = g_sel)
      shiny::updateSelectizeInput(session, "excluir", choices = rownames(df),
                                  selected = intersect(shiny::isolate(input$excluir), rownames(df)),
                                  server = TRUE)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$todas, {
      shiny::req(datos())
      shiny::updateSelectizeInput(session, "variables", selected = escala_de(datos()))
    })
    shiny::observeEvent(input$ninguna, {
      shiny::updateSelectizeInput(session, "variables", selected = character())
    })

    # Opciones que dependen de las variables activas
    shiny::observeEvent(input$variables, {
      v <- input$variables %||% character()
      ref <- shiny::isolate(input$referencia)
      shiny::updateSelectInput(session, "referencia",
                               choices = c("Signo de la SVD (como MultBiplotR)" = "",
                                           if (length(v)) stats::setNames(v, paste("Que", v, "quede en positivo"))),
                               selected = if (!is.null(ref) && ref %in% v) ref else "")
      for (b in seq_len(N_BLOQUES)) {
        sel <- intersect(shiny::isolate(input[[paste0("bloque_", b)]]), v)
        shiny::updateSelectizeInput(session, paste0("bloque_", b), choices = v, selected = sel)
      }
    }, ignoreNULL = FALSE)

    output$ayuda_transf <- shiny::renderUI({
      txt <- switch(input$transformacion,
        estandarizar = "Cada variable con media 0 y varianza 1: todas pesan igual (análisis de correlaciones). Úsala si las escalas o unidades son distintas.",
        centrar = "Resta la media de cada variable: conserva las varianzas (análisis de covarianzas). Las variables más dispersas pesan más.",
        doble_centrado = "Resta medias de filas y columnas: estudia la interacción, como en la tabla genotipo × ambiente.",
        media_global = "Resta la media de toda la matriz.",
        ninguna = "Usa los datos tal cual; el primer eje suele recoger la media.")
      shiny::p(class = "bl-help", txt)
    })

    # --- Datos que entran al análisis ---------------------------------------
    datos_analisis <- shiny::reactive({
      df <- datos()$df
      if (length(input$excluir)) df <- df[!rownames(df) %in% input$excluir, , drop = FALSE]
      df
    })

    # Voltear el gráfico = cambiar el signo del eje horizontal y/o vertical que se dibuja
    invertir_ejes <- shiny::reactive({
      e <- as.integer(c(input$eje_x %||% 1, input$eje_y %||% 2))
      if (e[1] == e[2]) e <- c(1L, 2L)
      sort(unique(c(if (isTRUE(input$voltear_h)) e[1], if (isTRUE(input$voltear_v)) e[2])))
    })

    resultado <- shiny::reactive({
      if (is.null(datos())) {
        return(simpleError("Primero carga una base de datos: botón «Datos» de la cinta superior."))
      }
      if (length(input$variables) < 2) {
        return(simpleError("Elige al menos 2 variables activas en el panel de opciones."))
      }
      tryCatch(
        hj_biplot(datos_analisis(), variables = input$variables,
                  transformacion = input$transformacion, log = input$log,
                  dimensiones = input$dims %||% 3, tipo = input$tipo,
                  referencia = if (nzchar(input$referencia %||% "")) input$referencia else NULL,
                  invertir_ejes = invertir_ejes()),
        error = function(e) e
      )
    })
    res_ok <- shiny::reactive({
      r <- resultado()
      shiny::validate(shiny::need(!inherits(r, "error"), ""))
      r
    })

    # Opciones de ejes según lo calculado
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

    grupos <- shiny::reactive({
      g <- input$grupo
      if (is.null(g) || !nzchar(g) || !g %in% names(datos_analisis())) return(NULL)
      datos_analisis()[[g]]
    })
    niveles_grupo <- shiny::reactive({
      g <- grupos()
      if (is.null(g)) return(character())
      g <- as.character(g); g[is.na(g) | g == ""] <- "(sin dato)"
      sort(unique(g))
    })

    # --- Colores --------------------------------------------------------------
    output$colores_grupos_ui <- shiny::renderUI({
      niv <- niveles_grupo()
      if (!length(niv)) {
        return(colourpicker::colourInput(session$ns("color_filas"), "Individuos", COLOR_FILAS))
      }
      pal <- colores_por_defecto(niv)
      shiny::tagList(
        shiny::tags$label(class = "form-label", paste("Grupos:", input$grupo)),
        lapply(seq_along(niv)[seq_len(min(length(niv), 15))], function(i) {
          colourpicker::colourInput(session$ns(paste0("col_g_", i)), niv[i], pal[[i]])
        })
      )
    })
    colores_grupos <- shiny::reactive({
      niv <- niveles_grupo()
      if (!length(niv)) return(NULL)
      pal <- colores_por_defecto(niv)
      for (i in seq_len(min(length(niv), 15))) {
        v <- input[[paste0("col_g_", i)]]
        if (!is.null(v) && nzchar(v)) pal[[i]] <- v
      }
      pal
    })
    colores_variables <- shiny::reactive({
      out <- character()
      for (b in seq_len(N_BLOQUES)) {
        v <- input[[paste0("bloque_", b)]]
        if (length(v)) out[v] <- input[[paste0("bloque_col_", b)]] %||% PALETA_GRUPOS[b + 1]
      }
      out
    })
    shiny::observeEvent(input$restablecer, {
      colourpicker::updateColourInput(session, "color_vectores", value = COLOR_VECTOR)
      colourpicker::updateColourInput(session, "color_filas", value = COLOR_FILAS)
      pal <- colores_por_defecto(niveles_grupo())
      for (i in seq_along(pal)) colourpicker::updateColourInput(session, paste0("col_g_", i), value = pal[[i]])
      for (b in seq_len(N_BLOQUES)) {
        colourpicker::updateColourInput(session, paste0("bloque_col_", b), value = PALETA_GRUPOS[b + 1])
      }
    })

    tablas <- shiny::reactive(tablas_biplot(res_ok(), ejes(), input$unidad))

    # --- Compartir con Bootstrap y Clúster -------------------------------------
    if (!is.null(compartido)) {
      compartido$hj <- res_ok
      compartido$hj_grupos <- grupos
      compartido$hj_ejes <- ejes
      compartido$hj_config <- shiny::reactive(list(
        datos = datos_analisis(), variables = input$variables,
        transformacion = input$transformacion, log = isTRUE(input$log),
        tipo = input$tipo, dimensiones = input$dims %||% 3,
        referencia = if (nzchar(input$referencia %||% "")) input$referencia else NULL,
        invertir_ejes = invertir_ejes(), excluir = input$excluir,
        origen = datos()$origen, grupo = input$grupo
      ))
    }

    # --- Salidas ------------------------------------------------------------
    output$error <- shiny::renderUI({
      r <- resultado()
      if (inherits(r, "error")) caja_error(conditionMessage(r))
    })

    output$subtitulo <- shiny::renderUI({
      r <- res_ok()
      shiny::span(class = "bl-sub", sprintf(
        "%s - %d individuos × %d variables - %s%s",
        datos()$etiqueta, r$n, r$p,
        names(TRANSFORMACIONES)[TRANSFORMACIONES == r$transformacion],
        if (length(input$excluir)) sprintf(" - %d excluidos", length(input$excluir)) else ""))
    })

    output$conteo_calidad <- shiny::renderUI({
      r <- res_ok(); e <- ejes()
      qf <- 1000 * (r$calidad_filas[, e[1]] + r$calidad_filas[, e[2]])
      qc <- 1000 * (r$calidad_columnas[, e[1]] + r$calidad_columnas[, e[2]])
      shiny::p(class = "bl-help", sprintf(
        "Cumplen el mínimo: %d de %d individuos y %d de %d variables.",
        sum(qf >= input$cal_filas), length(qf), sum(qc >= input$cal_columnas), length(qc)))
    })

    grafico <- shiny::reactive({
      grafico_biplot(res_ok(), ejes = ejes(), grupos = grupos(),
                     etiquetas_filas = input$etiquetas, etiquetas_columnas = input$etiquetas_c,
                     calidad_min_filas = input$cal_filas, calidad_min_columnas = input$cal_columnas,
                     mal_representados = input$mal,
                     escala_vectores = input$escala, mostrar = input$mostrar,
                     colores_grupos = colores_grupos(),
                     color_filas = input$color_filas %||% COLOR_FILAS,
                     color_vectores = input$color_vectores %||% COLOR_VECTOR,
                     colores_variables = colores_variables(),
                     envolventes = isTRUE(input$envolventes),
                     tam_texto = input$tam_texto, tam_puntos = input$tam_puntos)
    })
    # Encuadre rectangular: mismo cálculo en pantalla y en las descargas
    output$biplot <- render_biplot(grafico, session)
    nombre_archivo <- function(ext) function() sprintf("%s-biplot_ejes%d-%d.%s",
      tolower(res_ok()$tipo), ejes()[1], ejes()[2], ext)
    guardar <- function(ext, ...) descarga_biplot(grafico, nombre_archivo(ext), ext, ...)
    output$png <- guardar("png", dpi = 300, bg = "white")
    output$pdf <- guardar("pdf")
    output$svg <- guardar("svg")

    output$lectura <- shiny::renderUI({
      frases <- lectura_biplot(res_ok(), ejes(), input$umbral)
      shiny::tagList(
        shiny::p(class = "bl-muted",
                 "Orden de lectura recomendado: inercia, calidad (CRFE), variables, individuos y atípicos (CREF). ",
                 "Es una ayuda automática: confirma siempre con el gráfico y las tablas."),
        shiny::tags$ol(class = "bl-lectura", lapply(frases, function(f) {
          shiny::tags$li(shiny::HTML(gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>",
                                          htmltools::htmlEscape(f))))
        }))
      )
    })

    output$inercia <- shiny::renderTable({
      t <- tablas()$inercia
      t$Eje <- as.integer(t$Eje)
      con_decimales(traducir_tabla(t))
    }, striped = TRUE, hover = TRUE, spacing = "s", align = "r")

    output$sedimentacion <- shiny::renderPlot({
      inr <- res_ok()$inercia
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
      u <- if (input$unidad == "porcentaje") "%" else "por mil"
      txt <- switch(cual,
        calidad = sprintf("CRFE: contribución relativa del factor al elemento (coseno², %s). Indica qué parte del elemento recoge cada eje; la columna del plano es la suma de los dos ejes del gráfico. Solo se interpreta lo bien representado.", u),
        contrib = sprintf("CREF: contribución relativa del elemento al factor (%s). En cada eje suman el total: sirven para nombrar los ejes y detectar elementos que «capturan» un eje.", u),
        coordenadas = "Coordenadas factoriales, con el convenio de signos elegido en «Modelo y orientación».",
        correlaciones = "Matriz de correlaciones de las variables activas (datos originales).")
      shiny::p(class = "bl-help", txt)
    }
    tipo_tabla <- function(nombre) sub("_(filas|columnas)$", "", sub("^coordenadas.*", "coordenadas", nombre))
    output$nota_filas <- shiny::renderUI(nota(tipo_tabla(input$tabla_filas)))
    output$nota_columnas <- shiny::renderUI(nota(tipo_tabla(input$tabla_columnas)))

    decimales_tabla <- function(nombre) if (grepl("^(calidad|contrib)", nombre)) {
      if (input$unidad == "porcentaje") 1 else 0
    } else 3
    output$tabla_f <- DT::renderDT(tabla_resultados(tablas()[[input$tabla_filas]],
                                                    decimales_tabla(input$tabla_filas)))
    output$tabla_c <- DT::renderDT(tabla_resultados(tablas()[[input$tabla_columnas]],
                                                    decimales_tabla(input$tabla_columnas), 25))

    # --- Sintaxis R -----------------------------------------------------------
    sintaxis <- shiny::reactive({
      r <- res_ok()
      cg <- colores_grupos(); cv <- colores_variables()
      lineas <- c(
        sprintf("# %s-Biplot", r$tipo),
        sintaxis_paquete(),
        "",
        sintaxis_lectura(datos()$origen),
        if (length(input$excluir)) c(
          "",
          "# Individuos excluidos del análisis",
          sprintf("datos <- datos[!rownames(datos) %%in%% %s, ]", vector_a_r(input$excluir, 2))),
        "",
        "res <- hj_biplot(",
        "  datos,",
        sprintf("  variables = %s,", vector_a_r(input$variables, 14)),
        sprintf("  transformacion = \"%s\",", r$transformacion),
        if (r$log) "  log = TRUE,",
        sprintf("  tipo = \"%s\",", r$tipo),
        if (!is.null(r$referencia)) sprintf("  referencia = \"%s\",", r$referencia),
        if (length(invertir_ejes())) sprintf("  invertir_ejes = %s,", vector_a_r(invertir_ejes())),
        sprintf("  dimensiones = %d", r$dimensiones),
        ")",
        "res                                  # valores propios e inercia",
        "",
        sprintf("tablas <- tablas_biplot(res, ejes = c(%d, %d), unidad = \"%s\")",
                ejes()[1], ejes()[2], input$unidad),
        "tablas$calidad_columnas              # CRFE de las variables",
        "tablas$calidad_filas                 # CRFE de los individuos",
        "tablas$contrib_columnas              # CREF de las variables",
        "tablas$contrib_filas                 # CREF de los individuos",
        "",
        "grafico_biplot(",
        "  res,",
        sprintf("  ejes = c(%d, %d),", ejes()[1], ejes()[2]),
        if (!is.null(grupos())) sprintf("  grupos = datos[[%s]],", encodeString(input$grupo, quote = "\"")),
        if (!is.null(cg)) sprintf("  colores_grupos = %s,", vector_a_r(cg, 2)),
        if (is.null(cg) && !identical(input$color_filas, COLOR_FILAS) && !is.null(input$color_filas))
          sprintf("  color_filas = \"%s\",", input$color_filas),
        if (!identical(input$color_vectores, COLOR_VECTOR))
          sprintf("  color_vectores = \"%s\",", input$color_vectores),
        if (length(cv)) sprintf("  colores_variables = %s,",
                                paste0("c(", paste0("\"", names(cv), "\" = \"", cv, "\"", collapse = ", "), ")")),
        if (isTRUE(input$envolventes)) "  envolventes = TRUE,",
        if (!input$etiquetas) "  etiquetas_filas = FALSE,",
        if (!input$etiquetas_c) "  etiquetas_columnas = FALSE,",
        if (input$cal_filas > 0) sprintf("  calidad_min_filas = %s,", input$cal_filas),
        if (input$cal_columnas > 0) sprintf("  calidad_min_columnas = %s,", input$cal_columnas),
        if (input$mal != "atenuar") sprintf("  mal_representados = \"%s\",", input$mal),
        if (input$escala != 1) sprintf("  escala_vectores = %s,", input$escala),
        if (input$mostrar != "ambos") sprintf("  mostrar = \"%s\",", input$mostrar),
        if (input$tam_puntos != 2.2) sprintf("  tam_puntos = %s,", input$tam_puntos),
        sprintf("  tam_texto = %s", input$tam_texto),
        ")",
        "",
        sprintf("lectura_biplot(res, ejes = c(%d, %d), umbral = %d)   # lectura guiada",
                ejes()[1], ejes()[2], input$umbral)
      )
      paste(lineas, collapse = "\n")
    })
    output$sintaxis <- shiny::renderText(sintaxis())
    output$descargar_r <- shiny::downloadHandler(
      filename = function() "hj_biplot.R",
      content = function(file) writeLines(enc2utf8(sintaxis()), file, useBytes = TRUE)
    )

    # --- Exportación a Excel ----------------------------------------------------
    output$excel <- shiny::downloadHandler(
      filename = function() sprintf("resultados_%s-biplot.xlsx", tolower(res_ok()$tipo)),
      content = function(file) {
        t <- tablas()
        guardar_xlsx(list(
          "Inercia"                = t$inercia,
          "Coordenadas individuos" = t$coordenadas_filas,
          "Coordenadas variables"  = t$coordenadas_columnas,
          "CRFE individuos"        = t$calidad_filas,
          "CRFE variables"         = t$calidad_columnas,
          "CREF individuos"        = t$contrib_filas,
          "CREF variables"         = t$contrib_columnas,
          "Correlaciones"          = t$correlaciones
        ), file)
      }
    )

    res_ok
  })
}
