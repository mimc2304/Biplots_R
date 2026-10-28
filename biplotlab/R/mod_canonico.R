# =============================================================================
# MÓDULO: Biplot canónico / MANOVA biplot
#
# Pantalla del Biplot canónico dentro de BiplotLab. El cálculo, el gráfico y
# las tablas vienen de la aplicación del grupo (R/calculo_canonico.R,
# R/grafico_canonico.R y R/tablas_canonico.R). Aquí solo se adapta la interfaz
# al formato de BiplotLab: los datos llegan del panel Datos, las columnas
# nominales sirven de factores y el gráfico usa el encuadre rectangular.
# =============================================================================

#' Entrada del Biplot canónico en el registro de análisis
#' @export
analisis_canonico <- function() {
  list(
    id = "canonico", titulo = "Canónico / MANOVA", grupo = "Biplot", icono = "layer-group",
    descripcion = "Diferencias entre grupos con MANOVA y regiones de confianza",
    listo = TRUE,
    opciones_ui = mod_canonico_opciones_ui,
    resultados_ui = mod_canonico_resultados_ui,
    server = mod_canonico_server
  )
}

REGIONES_CANONICO <- c(
  "Elipses de Krzanowski, escala W (recomendado)" = "ellipse",
  "Elipses bootstrap" = "boot",
  "Elipses de Amaro (2008) tal como se publicaron (no coherentes)" = "ellipse_amaro",
  "Círculos chi²" = "chi2", "Círculos t" = "t", "Círculos Bonferroni" = "bonferroni"
)

mod_canonico_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  shiny::tagList(
    encabezado_opciones("Biplot canónico / MANOVA",
                        "Gabriel (1972) - Amaro, Vicente-Villardón y Galindo-Villardón (2004, 2008)"),
    seccion_opciones(
      "Variables y diseño",
      shiny::selectizeInput(ns("vars"), "Variables dependientes (escala)", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button"))),
      shiny::radioButtons(ns("diseno"), "Diseño", c("Un factor" = "uno", "Dos factores" = "dos"),
                          inline = TRUE),
      shiny::selectInput(ns("grupo"), "Factor de agrupación", choices = NULL),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'dos'", ns("diseno")),
        shiny::selectInput(ns("grupo2"), "Segundo factor", choices = NULL),
        shiny::radioButtons(ns("efecto"), "Biplot a representar",
                            c("Total (todas las celdas)" = "total", "Filas (1.er factor)" = "filas",
                              "Columnas (2.º factor)" = "columnas", "Interacción" = "interaccion")),
        ayuda("Amaro, Vicente-Villardón y Galindo-Villardón (2004). Las sumas de cuadrados son secuenciales (tipo I);",
              "en diseños balanceados no depende del orden.")),
      ayuda("Los factores son las columnas nominales (de agrupación) marcadas en el panel Datos.")
    ),
    seccion_opciones(
      "Modelo",
      shiny::radioButtons(ns("transform"), "Transformación",
                          c("Centrar columnas" = "center", "Estandarizar columnas" = "standardize"),
                          inline = TRUE),
      shiny::radioButtons(ns("na_action"), "Datos faltantes",
                          c("Eliminar filas con NA" = "omit", "Imputar (SVD iterativa)" = "impute"),
                          inline = TRUE),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'impute'", ns("na_action")),
        shiny::numericInput(ns("impute_rank"), "Rango de la imputación", 2, min = 1, max = 10)),
      shiny::checkboxInput(ns("log_vars"), "Aplicar logaritmo natural a las variables", FALSE),
      shiny::selectInput(ns("composicional"), "Datos composicionales (proporciones)",
                         c("Sin transformar" = "ninguna",
                           "clr: log-cociente centrado (+ inversa generalizada)" = "clr",
                           "alr: log-cociente respecto a una variable" = "alr")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'alr'", ns("composicional")),
        shiny::selectInput(ns("alr_ref"), "Variable de referencia (denominador)", choices = NULL)),
      shiny::checkboxInput(ns("ginv"), "Permitir W singular (inversa generalizada)", FALSE),
      abierta = FALSE
    ),
    seccion_opciones(
      "Regiones de confianza",
      shiny::selectInput(ns("region"), "Tipo", REGIONES_CANONICO, selected = "ellipse"),
      shiny::sliderInput(ns("conf"), "Nivel de confianza", 0.80, 0.99, 0.95, 0.01),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'ellipse'", ns("region")),
        shiny::radioButtons(ns("theta_est"), "Estimador de θ (Krzanowski)",
                            c("1 + λ/(K-1)  (Amaro et al., 2008)" = "uno_mas",
                              "λ/(K-1)  (cuadrado medio)" = "ms")),
        ayuda("Centros, varianzas y covarianzas en la escala de W. Cobertura simulada al 95 %:",
              "Iris 98,5-99 %; cerámica romana 93-97 %.")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'ellipse_amaro'", ns("region")),
        ayuda("Reproduce las tablas 4-7 del artículo, pero mezcla escalas. Úsala solo para reproducir el artículo.")),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'boot'", ns("region")),
        ayuda("Usa las réplicas y la semilla de la sección Bootstrap; se calcula automáticamente.")),
      shiny::conditionalPanel(
        sprintf("['chi2','t','bonferroni'].indexOf(input['%s']) >= 0", ns("region")),
        ayuda("Suponen ejes fijos: sirven para comparar pares de grupos, pero subestiman la incertidumbre",
              "de la posición de cada media."))
    ),
    seccion_opciones(
      "Tablas",
      shiny::selectInput(ns("ajuste_p"), "Ajuste de p en las comparaciones por pares",
                         c("Bonferroni" = "bonferroni", "Holm" = "holm", "Ninguno" = "none")),
      shiny::checkboxInput(ns("inercia_amaro"), "Añadir la inercia con θ² (convención de Amaro et al., 2004)", FALSE),
      abierta = FALSE
    ),
    seccion_opciones(
      "Bootstrap",
      shiny::checkboxInput(ns("boot_on"), "Activar bootstrap", FALSE),
      shiny::conditionalPanel(
        sprintf("input['%s']", ns("boot_on")),
        shiny::numericInput(ns("boot_B"), "Réplicas", 1000, min = 100, max = 10000, step = 100),
        shiny::numericInput(ns("boot_seed"), "Semilla", 2026),
        shiny::radioButtons(ns("boot_ci"), "Intervalos", c("Percentil" = "percentile", "Normal" = "normal"),
                            inline = TRUE),
        shiny::sliderInput(ns("boot_conf"), "Nivel", 0.80, 0.99, 0.95, 0.01),
        shiny::actionButton(ns("boot_run"), "Ejecutar bootstrap", icon = shiny::icon("play"),
                            class = "btn-primary btn-sm"),
        shiny::tags$hr(),
        shiny::checkboxInput(ns("boot_medias"), "Elipses bootstrap de las medias", TRUE),
        shiny::checkboxInput(ns("boot_variables"), "Elipses bootstrap de las flechas", TRUE),
        shiny::checkboxInput(ns("boot_nubes"), "Mostrar las réplicas (nubes)", FALSE),
        colourpicker::colourInput(ns("boot_color"), "Color de las elipses de las flechas", "#C2410C")),
      abierta = FALSE
    ),
    seccion_opciones(
      "Gráfico: plano y elementos",
      shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("eje_x"), "Eje X", 1, min = 1, max = 10)),
                      shiny::column(6, shiny::numericInput(ns("eje_y"), "Eje Y", 2, min = 1, max = 10))),
      shiny::fluidRow(shiny::column(6, shiny::checkboxInput(ns("invertir_x"), "Voltear de izquierda a derecha", FALSE)),
                      shiny::column(6, shiny::checkboxInput(ns("invertir_y"), "Voltear de arriba a abajo", FALSE))),
      shiny::checkboxInput(ns("ver_individuos"), "Individuos", TRUE),
      shiny::checkboxInput(ns("ver_medias"), "Medias de grupo", TRUE),
      shiny::checkboxInput(ns("ver_regiones"), "Regiones de confianza", TRUE),
      shiny::checkboxInput(ns("ver_flechas"), "Flechas de las variables", TRUE),
      abierta = FALSE
    ),
    seccion_opciones(
      "Gráfico: variables (flechas)",
      shiny::div(class = "bl-cols2",
                 shiny::checkboxGroupInput(ns("vars_mostrar"), "Variables a mostrar", choices = NULL)),
      shiny::radioButtons(ns("calidad_def"), "Calidad de las variables",
                          c("Correlación² dentro de los grupos (Amaro et al., 2008)" = "corr",
                            "Coordenadas principales (Amaro et al., 2004)" = "principal")),
      shiny::sliderInput(ns("min_calidad"), "Calidad mínima en el plano (%)", 0, 100, 0, 5),
      shiny::radioButtons(ns("flecha_escala"), "Escala de las flechas",
                          c("Automática" = "auto", "Original" = "orig"), inline = TRUE),
      shiny::sliderInput(ns("flecha_mult"), "Multiplicador de longitud", 0.1, 3, 1, 0.05),
      colourpicker::colourInput(ns("flecha_color"), "Color de las flechas", "#333333"),
      shiny::checkboxInput(ns("colores_por_var"), "Un color por variable", FALSE),
      shiny::uiOutput(ns("colores_var_ui")),
      shiny::sliderInput(ns("flecha_grosor"), "Grosor", 0.2, 3, 0.7, 0.1),
      shiny::sliderInput(ns("flecha_punta"), "Tamaño de la punta (cm)", 0, 0.6, 0.2, 0.02),
      abierta = FALSE
    ),
    seccion_opciones(
      "Gráfico: grupos",
      shiny::div(class = "bl-cols2",
                 shiny::checkboxGroupInput(ns("grupos_mostrar"), "Grupos a mostrar", choices = NULL)),
      shiny::selectInput(ns("paleta"), "Paleta", paletas_disponibles),
      shiny::checkboxInput(ns("colores_manual"), "Elegir el color de cada grupo", FALSE),
      shiny::uiOutput(ns("colores_grupo_ui")),
      shiny::sliderInput(ns("ind_tam"), "Tamaño de los individuos", 0.2, 6, 1.6, 0.1),
      shiny::sliderInput(ns("ind_alfa"), "Opacidad de los individuos", 0.05, 1, 0.45, 0.05),
      shiny::selectInput(ns("ind_forma"), "Forma de los individuos", formas, selected = 16),
      shiny::checkboxInput(ns("ind_forma_por_grupo"), "Una forma por grupo", FALSE),
      shiny::sliderInput(ns("media_tam"), "Tamaño de las medias", 1, 10, 4, 0.5),
      shiny::selectInput(ns("media_forma"), "Forma de las medias", formas, selected = 15),
      shiny::sliderInput(ns("reg_grosor"), "Grosor de las regiones", 0.2, 3, 0.9, 0.1),
      shiny::selectInput(ns("reg_linea"), "Tipo de línea",
                         c("Continua" = "solid", "Discontinua" = "dashed", "Punteada" = "dotted",
                           "Raya-punto" = "dotdash")),
      shiny::checkboxInput(ns("reg_relleno"), "Rellenar las regiones", FALSE),
      shiny::sliderInput(ns("reg_alfa"), "Opacidad del relleno", 0.02, 0.6, 0.12, 0.02),
      abierta = FALSE
    ),
    seccion_opciones(
      "Gráfico: etiquetas",
      shiny::checkboxInput(ns("etiq_medias"), "Nombres de los grupos", TRUE),
      shiny::checkboxInput(ns("etiq_variables"), "Nombres de las variables", TRUE),
      shiny::checkboxInput(ns("etiq_individuos"), "Nombres de los individuos", FALSE),
      shiny::checkboxInput(ns("evitar_solape"), "Evitar que se superpongan", TRUE),
      shiny::sliderInput(ns("etiq_media_tam"), "Tamaño (grupos)", 1, 10, 4, 0.2),
      shiny::sliderInput(ns("etiq_var_tam"), "Tamaño (variables)", 1, 10, 3.4, 0.2),
      shiny::sliderInput(ns("etiq_ind_tam"), "Tamaño (individuos)", 1, 8, 2.4, 0.2),
      shiny::checkboxInput(ns("etiq_var_mismo"), "Etiquetas de variables del color de su flecha", TRUE),
      shiny::conditionalPanel(
        sprintf("!input['%s']", ns("etiq_var_mismo")),
        colourpicker::colourInput(ns("etiq_var_color"), "Color de las etiquetas de variables", "#000000")),
      abierta = FALSE
    ),
    seccion_opciones(
      "Gráfico: textos y estilo",
      shiny::textInput(ns("titulo"), "Título", "Biplot canónico"),
      shiny::textInput(ns("subtitulo"), "Subtítulo", ""),
      shiny::textInput(ns("xlab"), "Eje X (vacío = automático)", ""),
      shiny::textInput(ns("ylab"), "Eje Y (vacío = automático)", ""),
      shiny::textInput(ns("pie"), "Nota al pie", ""),
      shiny::textInput(ns("titulo_leyenda"), "Título de la leyenda", "Grupo"),
      shiny::selectInput(ns("tema"), "Tema", temas),
      shiny::sliderInput(ns("tam_base"), "Tamaño de letra", 8, 24, 13, 1),
      shiny::selectInput(ns("fuente"), "Fuente", c("Sans" = "sans", "Serif" = "serif", "Mono" = "mono")),
      shiny::selectInput(ns("leyenda"), "Leyenda", selected = "bottom",
                         c("Abajo" = "bottom", "Derecha" = "right", "Arriba" = "top",
                           "Izquierda" = "left", "Sin leyenda" = "none")),
      shiny::checkboxInput(ns("cuadricula"), "Cuadrícula", FALSE),
      shiny::checkboxInput(ns("lineas_cero"), "Líneas en el origen", TRUE),
      colourpicker::colourInput(ns("fondo"), "Color de fondo", "#FFFFFF"),
      shiny::tags$label("Límites (vacío = automático, con el encuadre rectangular)"),
      shiny::fluidRow(shiny::column(3, shiny::numericInput(ns("xmin"), "x mín", NA)),
                      shiny::column(3, shiny::numericInput(ns("xmax"), "x máx", NA)),
                      shiny::column(3, shiny::numericInput(ns("ymin"), "y mín", NA)),
                      shiny::column(3, shiny::numericInput(ns("ymax"), "y máx", NA))),
      abierta = FALSE
    ),
    seccion_opciones(
      "Exportar",
      shiny::fluidRow(shiny::column(4, shiny::numericInput(ns("exp_ancho"), "Ancho máx. (cm)", 25, 5, 60)),
                      shiny::column(4, shiny::numericInput(ns("exp_alto"), "Alto máx. (cm)", 20, 5, 60)),
                      shiny::column(4, shiny::numericInput(ns("exp_dpi"), "DPI", 300, 72, 1200))),
      ayuda("El gráfico se ajusta dentro de ese espacio con el encuadre rectangular (ancho/alto = Δx/Δy)."),
      abierta = FALSE
    )
  )
}

mod_canonico_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("Biplot canónico / MANOVA biplot", ns("subtitulo")),
    shiny::uiOutput(ns("error")),
    shiny::uiOutput(ns("avisos")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Biplot",
          shiny::plotOutput(ns("grafico"), height = "auto"),
          shiny::uiOutput(ns("nota_grafico")),
          shiny::div(class = "bl-actions",
                     shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                     shiny::downloadButton(ns("pdf"), "PDF (vectorial)", class = "btn-sm"),
                     if (requireNamespace("svglite", quietly = TRUE))
                       shiny::downloadButton(ns("svg"), "SVG (editable)", class = "btn-sm"),
                     shiny::downloadButton(ns("coords"), "Coordenadas (CSV)", class = "btn-sm"))
        ),
        shiny::tabPanel("MANOVA", shiny::uiOutput(ns("tab_manova"))),
        shiny::tabPanel("Valores propios", shiny::uiOutput(ns("tab_eigen"))),
        shiny::tabPanel("Medias y regiones", shiny::uiOutput(ns("tab_medias"))),
        shiny::tabPanel("Variables", shiny::uiOutput(ns("tab_variables"))),
        shiny::tabPanel("Bootstrap", shiny::uiOutput(ns("tab_boot"))),
        shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Fundamentos"), teoria_ui("canonico")),
        shiny::tabPanel(
          shiny::span(shiny::icon("code"), "Sintaxis R"),
          shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))),
          shiny::downloadButton(ns("descargar_r"), "Descargar script .R", class = "btn-sm")
        )
      )
    ),
    shiny::div(class = "bl-actions",
               shiny::downloadButton(ns("excel"), "Exportar todas las tablas a Excel")),
    shiny::div(class = "res-ref", shiny::HTML(paste(
      "Referencias: Gabriel (1972); Amaro, Vicente-Villardón y Galindo-Villardón (2004), <i>Interciencia</i> 29(1), 26-32;",
      "Amaro, Vicente-Villardón y Galindo-Villardón (2008), <i>Investigación Operacional</i> 29(3), 231-241;",
      "Krzanowski (1989); Aitchison (1986); Nieto, Galindo-Villardón, Leiva y Vicente-Galindo (2014).")))
  )
}

mod_canonico_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Selectores: variables de escala y factores (columnas nominales) ----
    shiny::observeEvent(datos(), {
      d <- datos()
      df <- d$df
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      escala <- setdiff(num, d$nominales)
      factores <- unique(c(intersect(d$nominales, names(df)),
                           names(df)[!vapply(df, is.numeric, logical(1))]))
      sel_v <- intersect(shiny::isolate(input$vars), escala)
      if (length(sel_v) < 2) sel_v <- escala
      sel_g <- shiny::isolate(input$grupo)
      if (is.null(sel_g) || !sel_g %in% factores) sel_g <- factores[1]
      sel_g2 <- shiny::isolate(input$grupo2)
      if (is.null(sel_g2) || !sel_g2 %in% factores || identical(sel_g2, sel_g)) sel_g2 <- setdiff(factores, sel_g)[1]
      shiny::updateSelectizeInput(session, "vars", choices = escala, selected = sel_v)
      shiny::updateSelectInput(session, "grupo", choices = factores, selected = sel_g %||% character())
      shiny::updateSelectInput(session, "grupo2", choices = factores,
                               selected = if (is.na(sel_g2 %||% NA)) character() else sel_g2)
    })
    shiny::observe({
      v <- input$vars
      ref <- shiny::isolate(input$alr_ref)
      shiny::updateSelectInput(session, "alr_ref", choices = v,
                               selected = if (!is.null(ref) && ref %in% v) ref else v[length(v)])
    })

    # Título del gráfico según el diseño (solo si el usuario no lo cambió)
    titulos_auto <- c("Biplot canónico", "Biplot total", "Biplot de filas", "Biplot de columnas",
                      "Biplot de interacción")
    shiny::observeEvent(list(input$diseno, input$efecto), {
      if (shiny::isolate(input$titulo) %in% titulos_auto) {
        nuevo <- if (identical(input$diseno, "dos")) c(total = "Biplot total", filas = "Biplot de filas",
                                                       columnas = "Biplot de columnas",
                                                       interaccion = "Biplot de interacción")[[input$efecto]]
                 else "Biplot canónico"
        shiny::updateTextInput(session, "titulo", value = nuevo)
      }
    }, ignoreInit = TRUE)

    # ---- Datos del modelo ----
    datos_modelo <- shiny::reactive({
      shiny::validate(shiny::need(!is.null(datos()),
                                  "Primero carga una base de datos: botón «Datos» de la cinta superior."))
      d <- datos()$df
      dos <- identical(input$diseno, "dos")
      shiny::validate(
        shiny::need(length(input$vars) >= 2, "Elige al menos dos variables dependientes de escala."),
        shiny::need(!is.null(input$grupo) && nzchar(input$grupo) && input$grupo %in% names(d),
                    "Elige un factor de agrupación. Si no aparece, márcalo como nominal en el panel Datos."))
      if (dos) shiny::validate(
        shiny::need(!is.null(input$grupo2) && input$grupo2 %in% names(d), "Elige el segundo factor."),
        shiny::need(input$grupo2 != input$grupo, "Los dos factores deben ser distintos."))
      vars <- intersect(setdiff(input$vars, c(input$grupo, if (dos) input$grupo2)), names(d))
      shiny::validate(shiny::need(length(vars) >= 2, "Elige al menos dos variables dependientes de escala."))
      X <- d[, vars, drop = FALSE]
      g <- d[[input$grupo]]
      g2 <- if (dos) d[[input$grupo2]] else NULL
      ok <- !is.na(g) & (if (dos) !is.na(g2) else TRUE)
      if (input$na_action == "omit") ok <- ok & stats::complete.cases(X)
      X <- X[ok, , drop = FALSE]
      comp <- es_composicional(X)
      if (isTRUE(input$log_vars) && input$composicional == "ninguna") {
        shiny::validate(shiny::need(all(as.matrix(X) > 0, na.rm = TRUE),
                                    "El logaritmo necesita valores positivos en todas las variables."))
        X[] <- lapply(X, log)
      }
      if (input$composicional != "ninguna") {
        shiny::validate(
          shiny::need(!anyNA(X), "La transformación log-cociente necesita datos completos: elimina o imputa los NA."),
          shiny::need(all(as.matrix(X) >= 0), "La transformación log-cociente necesita valores no negativos."))
        rn <- rownames(X)
        X <- transformar_composicional(X, input$composicional, input$alr_ref)
        rownames(X) <- rn
      }
      list(X = X, g = droplevels(as.factor(g[ok])), g2 = if (dos) droplevels(as.factor(g2[ok])),
           dos = dos, composicional = comp, vars = vars, n_fuera = sum(!ok))
    })

    singular <- shiny::reactive(if (isTRUE(input$ginv) || input$composicional == "clr") "ginv" else "stop")

    # ---- Ajuste (uno o dos factores) ----
    ajuste_base <- shiny::reactive({
      dm <- datos_modelo()
      na <- if (input$na_action == "impute") "impute" else "fail"
      tryCatch(withCallingHandlers({
        cb <- if (dm$dos) {
          bigarx_manova2(dm$X, dm$g, dm$g2, efecto = input$efecto, transform = input$transform,
                         theta_est = input$theta_est, conf = input$conf, region = input$region, ndim = 10,
                         na_action = na, impute_rank = input$impute_rank, singular = singular(),
                         nombre_A = input$grupo, nombre_B = input$grupo2)
        } else {
          bigarx_canonical(dm$X, dm$g, transform = input$transform, conf = input$conf, region = input$region,
                           theta_est = input$theta_est, ndim = 10, na_action = na,
                           impute_rank = input$impute_rank, singular = singular())
        }
        cb$.composicional <- dm$composicional
        cb
      },
      warning = function(w) {
        shiny::showNotification(conditionMessage(w), type = "warning", duration = 8)
        invokeRestart("muffleWarning")
      }),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "error_ajuste"))
    })
    ejes <- shiny::reactive(c(input$eje_x %||% 1, input$eje_y %||% 2))
    ejes_invertidos <- shiny::reactive(unique(c(if (isTRUE(input$invertir_x)) input$eje_x,
                                                if (isTRUE(input$invertir_y)) input$eje_y)))
    ajuste <- shiny::reactive({
      cb <- ajuste_base()
      if (inherits(cb, "error_ajuste")) return(cb)
      if (identical(input$calidad_def, "principal")) cb$quality_cols <- cb$quality_cols_principal
      invertir_ejes(cb, ejes_invertidos())
    })
    cb_ok <- shiny::reactive({
      cb <- ajuste()
      shiny::validate(shiny::need(!inherits(cb, "error_ajuste"), if (inherits(cb, "error_ajuste")) cb$msg else ""))
      cb
    })

    output$error <- shiny::renderUI({
      cb <- tryCatch(ajuste(), error = function(e) {
        if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) NULL else e
      })
      if (inherits(cb, "error_ajuste")) caja_error(cb$msg) else NULL
    })

    # Listas de variables y grupos que se muestran
    vars_modelo <- shiny::reactive({
      cb <- ajuste_base(); if (inherits(cb, "error_ajuste")) character() else rownames(cb$cols)
    })
    prev_vars <- shiny::reactiveVal(character())
    shiny::observe({
      v <- vars_modelo()
      old <- shiny::isolate(input$vars_mostrar)
      sel <- if (is.null(old)) v else union(intersect(old, v), setdiff(v, shiny::isolate(prev_vars())))
      shiny::updateCheckboxGroupInput(session, "vars_mostrar", choices = v, selected = sel)
      prev_vars(v)
    })
    niveles <- shiny::reactive({
      cb <- ajuste_base()
      if (!inherits(cb, "error_ajuste")) return(levels(cb$groups))
      dm <- datos_modelo(); levels(dm$g)
    })
    shiny::observe({
      g <- niveles()
      shiny::updateCheckboxGroupInput(session, "grupos_mostrar", choices = g, selected = g)
    })

    # ---- Bootstrap ----
    boot_raw <- shiny::reactiveVal(NULL)
    boot_disponible <- shiny::reactive({
      cb <- ajuste_base()
      !inherits(cb, "error_ajuste") && (is.null(cb$diseno) || identical(cb$diseno$efecto, "total"))
    })
    run_boot <- function() {
      cb <- ajuste_base(); shiny::req(!inherits(cb, "error_ajuste"))
      if (!boot_disponible()) {
        shiny::showNotification("El bootstrap está disponible para un factor y para el biplot total de dos factores.",
                                type = "warning", duration = 8)
        boot_raw(NULL); return(invisible())
      }
      dm <- datos_modelo()
      g <- if (dm$dos) cb$diseno$celda else dm$g
      res <- shiny::withProgress(message = "Bootstrap en curso", value = 0, {
        tryCatch(boot_canonical(dm$X, g, B = input$boot_B, transform = input$transform, axes = ejes(),
                                conf = if (input$region == "boot") input$conf else input$boot_conf,
                                ci = input$boot_ci, seed = input$boot_seed,
                                na_action = if (input$na_action == "impute") "impute" else "fail",
                                impute_rank = input$impute_rank, singular = singular(),
                                progress = function(p) shiny::setProgress(p)),
                 error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      })
      boot_raw(res)
    }
    shiny::observeEvent(input$boot_run, run_boot())
    shiny::observeEvent(list(ajuste_base(), input$eje_x, input$eje_y, input$boot_conf, input$region), {
      cb <- ajuste_base()
      if (!inherits(cb, "error_ajuste") && identical(input$region, "boot") && input$eje_x != input$eje_y &&
          max(input$eje_x, input$eje_y) <= cb$ndim) run_boot() else boot_raw(NULL)
    })
    boot <- shiny::reactive({
      b <- boot_raw(); if (is.null(b)) return(NULL)
      invertir_boot(b, ejes_invertidos())
    })

    # ---- Colores ----
    output$colores_grupo_ui <- shiny::renderUI({
      shiny::req(input$colores_manual)
      g <- niveles(); cols <- colores_paleta(length(g), input$paleta)
      lapply(seq_along(g), function(i) colourpicker::colourInput(ns(paste0("col_g_", i)), g[i], cols[i]))
    })
    output$colores_var_ui <- shiny::renderUI({
      shiny::req(input$colores_por_var)
      v <- vars_modelo(); cols <- colores_paleta(length(v), "Dark 3")
      lapply(seq_along(v), function(i) colourpicker::colourInput(ns(paste0("col_v_", i)), v[i], cols[i]))
    })

    opciones <- shiny::reactive({
      cb <- cb_ok()
      g <- levels(cb$groups)
      cols_g <- if (isTRUE(input$colores_manual)) {
        vals <- vapply(seq_along(niveles()), function(i) {
          v <- input[[paste0("col_g_", i)]]; if (is.null(v)) NA_character_ else v
        }, character(1))
        if (anyNA(vals)) NULL else stats::setNames(vals, niveles())[g]
      } else NULL
      cols_v <- if (isTRUE(input$colores_por_var)) {
        vm <- vars_modelo()
        vals <- vapply(seq_along(vm), function(i) {
          v <- input[[paste0("col_v_", i)]]; if (is.null(v)) NA_character_ else v
        }, character(1))
        stats::setNames(vals, vm)[!is.na(vals)]
      } else NULL
      opciones_grafico(cb,
        axes = ejes(),
        grupos = input$grupos_mostrar %||% character(),
        variables = input$vars_mostrar %||% character(),
        min_calidad = input$min_calidad,
        ver_individuos = input$ver_individuos, ver_medias = input$ver_medias,
        ver_regiones = input$ver_regiones, ver_flechas = input$ver_flechas,
        etiq_medias = input$etiq_medias, etiq_variables = input$etiq_variables,
        etiq_individuos = input$etiq_individuos,
        paleta = input$paleta, colores = cols_g,
        ind_tam = input$ind_tam, ind_alfa = input$ind_alfa, ind_forma = as.numeric(input$ind_forma),
        ind_forma_por_grupo = input$ind_forma_por_grupo,
        media_tam = input$media_tam, media_forma = as.numeric(input$media_forma),
        reg_grosor = input$reg_grosor, reg_linea = input$reg_linea, reg_relleno = input$reg_relleno,
        reg_alfa = input$reg_alfa,
        flecha_color = input$flecha_color, flecha_colores = cols_v, flecha_grosor = input$flecha_grosor,
        flecha_punta = input$flecha_punta, flecha_escala = if (input$flecha_escala == "auto") "auto" else "orig",
        flecha_mult = input$flecha_mult,
        etiq_var_tam = input$etiq_var_tam, etiq_media_tam = input$etiq_media_tam,
        etiq_ind_tam = input$etiq_ind_tam,
        etiq_var_color = if (isTRUE(input$etiq_var_mismo)) NULL else input$etiq_var_color,
        evitar_solape = input$evitar_solape,
        titulo = input$titulo, subtitulo = input$subtitulo, xlab = input$xlab, ylab = input$ylab,
        pie = input$pie, titulo_leyenda = input$titulo_leyenda,
        tema = input$tema, tam_base = input$tam_base, fuente = input$fuente, leyenda = input$leyenda,
        lineas_cero = input$lineas_cero, cuadricula = input$cuadricula, fondo = input$fondo,
        xlim = c(input$xmin, input$xmax), ylim = c(input$ymin, input$ymax),
        boot_medias = input$boot_medias, boot_variables = input$boot_variables,
        boot_nubes = input$boot_nubes, boot_color = input$boot_color)
    })

    grafico <- shiny::reactive({
      cb <- cb_ok()
      shiny::validate(shiny::need(input$eje_x != input$eje_y, "Elige dos ejes distintos."),
                      shiny::need(max(ejes()) <= cb$ndim,
                                  sprintf("Este análisis tiene %d ejes canónicos.", cb$ndim)))
      suppressWarnings(grafico_canonico(cb, opciones(), boot = boot()))
    })
    # Encuadre rectangular: el mismo cálculo que el HJ-Biplot
    output$grafico <- render_biplot(grafico, session, salida = "grafico")

    guardar <- function(ext) shiny::downloadHandler(
      filename = function() paste0("biplot_canonico.", ext),
      content = function(file) {
        W <- (input$exp_ancho %||% 25) / 2.54; H <- (input$exp_alto %||% 20) / 2.54
        e <- encuadre_biplot(grafico(), W = W, H = H, unidad = "in")
        args <- list(filename = file, plot = traducir_grafico(grafico()), width = e$ancho, height = e$alto, units = "in",
                     bg = input$fondo)
        if (ext == "png") args$dpi <- input$exp_dpi %||% 300
        suppressWarnings(do.call(ggplot2::ggsave, args))
      })
    output$png <- guardar("png")
    output$pdf <- guardar("pdf")
    output$svg <- guardar("svg")
    output$coords <- shiny::downloadHandler(
      filename = function() "coordenadas_biplot_canonico.csv",
      content = function(file) utils::write.csv(canonical_tidy(cb_ok()), file, row.names = FALSE,
                                                fileEncoding = "UTF-8"))

    # ---- Tablas ----
    tablas_res <- shiny::reactive({
      cb <- cb_ok()
      dm <- datos_modelo()
      ax <- ejes(); ax <- ax[ax <= cb$ndim]; if (length(ax) < 2) ax <- c(1, 2)
      out <- list()
      Xd <- if (!is.null(cb$imputation)) cb$imputation$X else as.matrix(dm$X)
      gdesc <- if (is.null(cb$diseno)) cb$groups else cb$diseno$celda
      out$desc <- descriptivos_grupo(Xd, gdesc)
      if (is.null(cb$diseno)) {
        mt <- manova_tests(cb)
        m <- cb$manova
        out$manova <- data.frame(Contraste = rownames(mt), Valor = mt$estadistico, F = mt$F_aprox,
                                 `gl num.` = mt$gl1, `gl den.` = mt$gl2, p = mt$p_valor, check.names = FALSE)
        out$bartlett <- data.frame(Contraste = "Wilks (aprox. χ² de Bartlett)", Lambda = m$wilks,
                                   `χ²` = m$chi2, gl = m$df, p = m$p_value, check.names = FALSE)
        ua <- univariate_anovas(cb)
        out$anova <- data.frame(Variable = rownames(ua), `SC entre` = ua$SC_entre, `SC dentro` = ua$SC_dentro,
                                F = ua$F, `gl num.` = ua$gl1, `gl den.` = ua$gl2, p = ua$p_valor,
                                check.names = FALSE)
      } else {
        pr <- cb$diseno$pruebas
        out$manova <- data.frame(Efecto = pr$efecto, Contraste = pr$contraste, Valor = pr$estadistico,
                                 F = pr$F_aprox, `gl num.` = pr$gl1, `gl den.` = pr$gl2, p = pr$p_valor,
                                 check.names = FALSE)
        w <- pr[pr$contraste == "Wilks", ]
        out$bartlett <- data.frame(Efecto = w$efecto, Lambda = w$estadistico, `χ²` = w$chi2_bartlett,
                                   gl = w$gl_chi2, p = w$p_chi2, check.names = FALSE)
        an <- cb$diseno$anova
        out$anova <- data.frame(Variable = an$variable, Efecto = an$efecto, SC = an$SC, F = an$F,
                                `gl num.` = an$gl1, `gl den.` = an$gl2, p = an$p_valor, check.names = FALSE)
      }
      th <- cb$canonical$valor_propio
      out$eigen <- data.frame(Eje = cb$canonical$eje, `Valor propio (HE⁻¹)` = th,
                              `Valor singular (escala E)` = sqrt(th),
                              `Correlación canónica` = cb$canonical$correlacion_canonica,
                              `Inercia (%)` = cb$inertia$inercia, `Acumulada (%)` = cb$inertia$acumulada,
                              check.names = FALSE)
      if (isTRUE(input$inercia_amaro)) out$eigen$`Inercia θ² (%)` <- 100 * th^2 / sum(th^2)
      out$medias <- data.frame(Grupo = rownames(cb$rows), n = cb$n_groups, cb$rows[, ax, drop = FALSE],
                               `Calidad (%)` = rowSums(cb$quality_rows[, ax, drop = FALSE]), check.names = FALSE)
      names(out$medias)[3:4] <- paste("Eje", ax)
      out$variables <- data.frame(Variable = rownames(cb$cols), cb$cols[, ax, drop = FALSE],
                                  `Calidad (%)` = rowSums(cb$quality_cols[, ax, drop = FALSE]),
                                  cb$structure[, ax, drop = FALSE], check.names = FALSE)
      names(out$variables)[c(2, 3, 5, 6)] <- c(paste("Eje", ax), paste("r dentro, Eje", ax))
      tot <- cb$structure_total[, ax, drop = FALSE]
      out$corr_total <- data.frame(Variable = rownames(tot), tot, tot^2 * 100, check.names = FALSE)
      names(out$corr_total)[-1] <- c(paste("r total, Eje", ax), paste("r² × 100, Eje", ax))
      b <- boot()
      if (identical(cb$region, "boot")) {
        if (!is.null(b)) {
          rg <- regiones_boot(b)
          out$regiones <- data.frame(Grupo = rg$grupo, `Centro X` = rg$centro1, `Centro Y` = rg$centro2,
                                     `Var X` = rg$var1, `Var Y` = rg$var2, Cov = rg$cov,
                                     `Semieje mayor` = rg$semieje_mayor, `Semieje menor` = rg$semieje_menor,
                                     `Ángulo (°)` = rg$angulo, `Área` = rg$area, check.names = FALSE)
        }
      } else {
        rg <- canonical_regions(cb, ax)
        out$regiones <- if (cb$region %in% c("ellipse", "ellipse_amaro")) {
          data.frame(Grupo = rg$grupo, `Centro X` = rg$centro1, `Centro Y` = rg$centro2, `Var X` = rg$var1,
                     `Var Y` = rg$var2, Cov = rg$cov, `Semieje mayor` = rg$semieje_mayor,
                     `Semieje menor` = rg$semieje_menor, `Orientación eje mayor (°)` = rg$direccion_eje_mayor,
                     `Área` = rg$area, `Definida positiva` = ifelse(rg$definida_positiva, "sí", "no"),
                     check.names = FALSE)
        } else {
          data.frame(Grupo = rg$grupo, `Centro X` = rg$centro1, `Centro Y` = rg$centro2,
                     Radio = rg$semieje_mayor, check.names = FALSE)
        }
      }
      if (nrow(cb$.M) >= 2) {
        hp <- hotelling_pares(cb, input$ajuste_p)
        out$pares <- data.frame(`Grupo 1` = hp$grupo1, `Grupo 2` = hp$grupo2, `D² Mahalanobis` = hp$D2_Mahalanobis,
                                `T²` = hp$T2, F = hp$F, `gl num.` = hp$gl1, `gl den.` = hp$gl2, p = hp$p_valor,
                                `p ajustado` = hp$p_ajustado, check.names = FALSE)
      }
      if (!is.null(cb$imputation)) {
        v <- cb$imputation$values
        out$imput <- data.frame(Fila = v$fila, Variable = v$columna, `Valor imputado` = v$valor,
                                check.names = FALSE)
      }
      if (!is.null(b)) {
        t <- b$table
        nom <- c(inercia = "Inercia (%)", valor_propio = "Valor propio",
                 correlacion_canonica = "Correlación canónica", media = "Media de grupo",
                 variable = "Flecha (variable)", calidad_variable = "Calidad de variable (%)",
                 correlacion_variable = "r variable-eje")
        out$boot <- data.frame(`Parámetro` = nom[t$tipo], Elemento = t$elemento, Eje = t$eje,
                               `Estimación` = t$estimacion, EE = t$ee, Sesgo = t$sesgo,
                               Inferior = t$inferior, Superior = t$superior, check.names = FALSE)
      }
      out
    })

    bloque <- function(tb, clave, titulo, nota = NULL, digits = 3, formatos = NULL) {
      if (is.null(tb[[clave]])) return(NULL)
      tabla_apa(tb[[clave]], titulo, nota, digits = digits, formatos = formatos, rownames = FALSE)
    }
    es_dos <- function(cb) !is.null(cb$diseno)
    region_txt <- function(cb) c(ellipse = "elipses de Krzanowski, escala W",
                                 ellipse_amaro = "elipses de Amaro (2008) publicadas",
                                 chi2 = "círculos chi²", t = "círculos t",
                                 bonferroni = "círculos Bonferroni", boot = "elipses bootstrap")[[cb$region]]

    output$subtitulo <- shiny::renderUI({
      cb <- cb_ok(); dos <- es_dos(cb)
      tfm <- c(ninguna = "", clr = " - clr", alr = paste0(" - alr (ref.: ", input$alr_ref, ")"))[[input$composicional]]
      if (isTRUE(input$log_vars) && input$composicional == "ninguna") tfm <- " - logaritmo natural"
      efectos <- c(total = "biplot total (celdas)", filas = paste("biplot de filas:", input$grupo),
                   columnas = paste("biplot de columnas:", input$grupo2), interaccion = "biplot de interacción")
      shiny::span(class = "bl-sub", sprintf(
        "%s%d individuos - %d %s - %d variables - transformación: %s%s%s",
        if (dos) paste0("Dos factores, ", efectos[[cb$diseno$efecto]], " - ") else "",
        nrow(cb$individuals), nrow(cb$rows),
        if (dos && cb$diseno$efecto %in% c("total", "interaccion")) "celdas" else "grupos",
        nrow(cb$cols), if (cb$preprocessing$transform == "center") "centrar" else "estandarizar", tfm,
        if (!is.null(cb$imputation)) sprintf(" - %d datos imputados", nrow(cb$imputation$missing)) else ""))
    })

    output$avisos <- shiny::renderUI({
      cb <- tryCatch(cb_ok(), error = function(e) NULL)
      if (is.null(cb)) return(NULL)
      dm <- datos_modelo()
      aviso <- function(...) shiny::div(class = "res-aviso", shiny::icon("circle-info"), ...)
      shiny::tagList(
        if (dm$n_fuera > 0) aviso(sprintf("Se dejaron fuera %d filas con datos faltantes.", dm$n_fuera)),
        if (es_dos(cb) && isTRUE(cb$diseno$proyectado)) aviso(
          "Este efecto tiene un solo grado de libertad (dos niveles) y no tiene plano propio: sus medias se",
          "proyectan sobre el biplot total, como en Amaro et al. (2004, figura 5)."),
        if (isTRUE(cb$.composicional) && input$composicional == "ninguna") aviso(
          "Las variables suman una constante en cada fila (datos composicionales): conviene usar la",
          "transformación clr o alr en «Modelo»."),
        if (isTRUE(cb$singular)) aviso(sprintf(
          "W es singular: se usó la inversa generalizada (rango efectivo %d de %d variables).",
          cb$.krz$p_eff, nrow(cb$cols))),
        if (identical(cb$region, "boot") && is.null(boot())) aviso(
          if (boot_disponible()) "Calculando las elipses bootstrap..." else
            "Las elipses bootstrap solo están disponibles para un factor y para el biplot total.")
      )
    })

    output$nota_grafico <- shiny::renderUI({
      cb <- cb_ok(); b <- boot()
      shiny::tagList(
        shiny::div(class = "res-nota", sprintf("Regiones de confianza al %s %%: %s.", format(100 * cb$conf),
                                               region_txt(cb))),
        if (!is.null(b) && !identical(cb$region, "boot")) shiny::div(class = "res-nota", sprintf(
          "Líneas discontinuas: elipses bootstrap al %s %% de las medias (%d réplicas estratificadas).",
          format(100 * b$conf), b$B)))
    })

    output$tab_manova <- shiny::renderUI({
      cb <- cb_ok(); tb <- tablas_res(); dos <- es_dos(cb)
      shiny::tagList(
        bloque(tb, "manova", if (dos) "Contrastes multivariantes por efecto (MANOVA de dos factores)" else
                 "Contrastes multivariantes (MANOVA)",
               nota = "Aproximaciones F de <i>summary.manova</i> de R.",
               formatos = if (dos) c("txt", "txt", "num", "num", "int", "num", "p") else
                 c("txt", "num", "num", "int", "num", "p")),
        bloque(tb, "bartlett", "Lambda de Wilks con la aproximación χ² de Bartlett",
               nota = "χ² = -[gl<sub>E</sub> - (p - gl<sub>H</sub> + 1)/2]·ln Λ.",
               formatos = c("txt", "num", "num", "int", "p"), digits = 4),
        bloque(tb, "anova", if (dos) "ANOVA de dos factores por variable" else "ANOVA de una vía por variable",
               formatos = if (dos) c("txt", "txt", "num", "num", "int", "int", "p") else
                 c("txt", "num", "num", "num", "int", "int", "p")),
        bloque(tb, "desc", if (dos) "Descriptivos por celda" else "Descriptivos por grupo",
               formatos = c("txt", "txt", "int", "num", "num")),
        bloque(tb, "imput", "Valores imputados (SVD iterativa)"))
    })
    output$tab_eigen <- shiny::renderUI({
      tb <- tablas_res()
      bloque(tb, "eigen", "Valores propios, correlaciones canónicas e inercia",
             nota = paste("El «valor singular (escala E)» es lo que Amaro et al. (2008) llaman valor propio (5,67 en Iris).",
                          if (isTRUE(input$inercia_amaro)) "La inercia θ² reproduce las tablas II, IV y VI de Amaro et al. (2004)." else ""))
    })
    output$tab_medias <- shiny::renderUI({
      cb <- cb_ok(); tb <- tablas_res()
      shiny::tagList(
        bloque(tb, "medias", "Medias de grupo: coordenadas y calidad de representación"),
        bloque(tb, "regiones", sprintf("Regiones de confianza al %s %% (%s)", format(100 * cb$conf), region_txt(cb)),
               digits = 4,
               nota = switch(cb$region,
                 ellipse = paste0("Centros y covarianzas en la escala de W (varianza dentro de los grupos = 1). θ estimado con ",
                                  if (identical(input$theta_est, "ms")) "λ/(K-1)" else "1 + λ/(K-1)",
                                  ", λ = autovalores de SCH·W⁻¹. Orientación: ángulo del eje mayor desde el eje X."),
                 ellipse_amaro = paste("Versión publicada: centros en escala W y covarianzas con coordenadas divididas",
                                       "entre √(n-K); no son coherentes. Solo para reproducir el artículo."),
                 boot = "Elipse normal que contiene el nivel indicado de las réplicas bootstrap de cada media.",
                 "Radio en la escala de W.")),
        bloque(tb, "pares", "Comparaciones por pares: T² de Hotelling",
               nota = sprintf("W combinada del modelo (gl = %d). Ajuste de p: %s.", cb$.krz$dfE,
                              c(bonferroni = "Bonferroni", holm = "Holm", none = "ninguno")[[input$ajuste_p]]),
               formatos = c("txt", "txt", "num", "num", "num", "int", "int", "p", "p")))
    })
    output$tab_variables <- shiny::renderUI({
      tb <- tablas_res()
      calidad_txt <- if (identical(input$calidad_def, "principal"))
        "Calidad: coseno² sobre las coordenadas principales (Amaro et al., 2004)." else
        "Calidad: correlación al cuadrado (dentro de los grupos) entre la variable y el plano (Amaro et al., 2008)."
      shiny::tagList(
        bloque(tb, "variables", "Variables: coordenadas, calidad y correlación con los ejes", nota = calidad_txt),
        bloque(tb, "corr_total", "Correlaciones totales entre las variables y los ejes",
               nota = "Correlación sobre todos los individuos (sin quitar el efecto de grupo), como en la tabla 9 de Amaro et al. (2008)."))
    })
    output$tab_boot <- shiny::renderUI({
      tb <- tablas_res(); b <- boot()
      if (!is.null(tb$boot)) {
        tabla_apa(tb$boot, sprintf("Intervalos bootstrap al %s %% (%s, %d réplicas)", format(100 * b$conf),
                                   if (b$ci == "percentile") "percentil" else "normal", b$B), rownames = FALSE)
      } else {
        shiny::div(class = "res-nota",
                   "Activa el bootstrap en las opciones (sección Bootstrap) y pulsa «Ejecutar bootstrap».")
      }
    })

    output$excel <- shiny::downloadHandler(
      filename = function() "resultados_biplot_canonico.xlsx",
      content = function(file) {
        tb <- tablas_res()
        nombres <- c(desc = "Descriptivos", manova = "MANOVA", bartlett = "Bartlett", anova = "ANOVA",
                     eigen = "Valores propios", medias = "Medias", variables = "Variables",
                     corr_total = "Correlaciones totales", regiones = "Regiones", pares = "Pares Hotelling",
                     imput = "Imputacion", boot = "Bootstrap")
        tb <- tb[intersect(names(nombres), names(tb))]
        names(tb) <- nombres[names(tb)]
        guardar_xlsx(tb, file)
      })

    # ---- Sintaxis R ----
    sintaxis <- shiny::reactive({
      cb <- cb_ok(); dm <- datos_modelo(); d <- datos()
      dos <- dm$dos
      na <- if (input$na_action == "impute") "impute" else "fail"
      c0 <- c(
        "# Biplot canónico / MANOVA-Biplot",
        sintaxis_paquete(),
        sintaxis_lectura(d$origen),
        sprintf("vars <- %s", vector_a_r(dm$vars)),
        if (input$na_action == "omit") "datos <- datos[stats::complete.cases(datos[, vars]), ]",
        "X <- datos[, vars]",
        if (isTRUE(input$log_vars) && input$composicional == "ninguna") "X[] <- lapply(X, log)",
        if (input$composicional != "ninguna")
          sprintf("X <- transformar_composicional(X, \"%s\", %s)", input$composicional,
                  encodeString(input$alr_ref %||% "", quote = "\"")),
        "")
      ajuste_txt <- if (dos) c(
        sprintf("cb <- bigarx_manova2(X, datos[[\"%s\"]], datos[[\"%s\"]], efecto = \"%s\",",
                input$grupo, input$grupo2, input$efecto),
        sprintf("  transform = \"%s\", conf = %s, region = \"%s\", theta_est = \"%s\",",
                input$transform, input$conf, input$region, input$theta_est),
        sprintf("  ndim = 10, na_action = \"%s\", singular = \"%s\", nombre_A = \"%s\", nombre_B = \"%s\")",
                na, singular(), input$grupo, input$grupo2))
      else c(
        sprintf("cb <- bigarx_canonical(X, datos[[\"%s\"]], transform = \"%s\", conf = %s,",
                input$grupo, input$transform, input$conf),
        sprintf("  region = \"%s\", theta_est = \"%s\", ndim = 10, na_action = \"%s\", singular = \"%s\")",
                input$region, input$theta_est, na, singular()))
      ax <- vector_a_r(ejes())
      paste(c(c0, ajuste_txt,
              if (length(ejes_invertidos())) sprintf("cb <- invertir_ejes(cb, %s)", vector_a_r(ejes_invertidos())),
              "",
              "cb                                   # MANOVA e inercia",
              sprintf("summary(cb, axes = %s)", ax),
              if (!dos) "manova_tests(cb)                     # Pillai, Wilks, Hotelling-Lawley y Roy",
              if (!identical(input$region, "boot")) sprintf("canonical_regions(cb, %s)", ax),
              sprintf("hotelling_pares(cb, \"%s\")", input$ajuste_p),
              "",
              sprintf("grafico_canonico(cb, opciones_grafico(cb, axes = %s))", ax)),
            collapse = "\n")
    })
    output$sintaxis <- shiny::renderText(sintaxis())
    output$descargar_r <- shiny::downloadHandler(
      filename = function() "biplot_canonico.R",
      content = function(file) writeLines(enc2utf8(sintaxis()), file, useBytes = TRUE))

    ajuste
  })
}
