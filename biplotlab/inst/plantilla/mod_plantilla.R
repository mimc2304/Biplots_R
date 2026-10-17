# =============================================================================
# PLANTILLA DE MÓDULO PARA BiplotLab
#
# 1. Copia este archivo a  R/mod_<metodo>.R   (ej.: R/mod_gge.R)
# 2. Reemplaza en TODO el archivo la palabra  METODO  por el nombre corto de tu
#    método, sin espacios ni tildes (ej.: gge, canonico, logistico, dpca).
# 3. Completa las partes marcadas con  TODO.
# 4. En R/registro_analisis.R cambia tu línea  proximamente("...")  por
#    analisis_METODO()
# 5. Si usas paquetes nuevos, agrégalos en DESCRIPTION (Imports) y llama sus
#    funciones con  paquete::funcion()  (no uses library() dentro del paquete).
#
# Tal como está, la plantilla FUNCIONA: hace un ACP con prcomp() para que veas
# el flujo completo. Reemplaza ese cálculo por el tuyo.
#
# IMPORTANTE: no uses paquetes que abren ventanas propias (tcltk), como
# biplotbootGUI::biplotboot() o GGEBiplotGUI::GGEBiplot(): no funcionan en una
# app web. Usa sus funciones de cálculo o el cálculo directo.
# =============================================================================

# -----------------------------------------------------------------------------
# A) CÁLCULO: función normal de R, sin nada de Shiny.
#    Recibe los datos y las opciones; devuelve una lista con los resultados.
#    Así cualquiera puede usarla desde R (es lo que muestra "Sintaxis R").
# -----------------------------------------------------------------------------

#' TODO: título de tu método
#'
#' @param datos data.frame con los individuos en filas (nombres de fila = etiquetas).
#' @param variables nombres de las variables que entran al análisis.
#' @param estandarizar TODO: cambia o agrega las opciones de tu método.
#' @export
calcular_METODO <- function(datos, variables, estandarizar = TRUE) {
  X <- as.matrix(datos[, variables, drop = FALSE])
  if (anyNA(X)) stop("Hay datos faltantes en las variables elegidas.", call. = FALSE)

  # ---- TODO: aquí va TU código de R (el que te funcionó en tu práctica) ----
  acp <- stats::prcomp(X, center = TRUE, scale. = estandarizar)
  inercia <- data.frame(
    Eje = seq_along(acp$sdev),
    `% Inercia` = 100 * acp$sdev^2 / sum(acp$sdev^2),
    check.names = FALSE
  )
  # --------------------------------------------------------------------------

  # Devuelve SIEMPRE una lista con nombre. Si tu método produce coordenadas de
  # filas y columnas, llámalas `filas` y `columnas` (matrices con nombres) para
  # poder reutilizar grafico_biplot().
  list(inercia = inercia, filas = acp$x, columnas = acp$rotation)
}

# -----------------------------------------------------------------------------
# B) REGISTRO: cómo aparece tu módulo en la cinta del aplicativo.
# -----------------------------------------------------------------------------
#' @export
analisis_METODO <- function() {
  list(
    id = "METODO",                         # TODO: igual que el nombre corto
    titulo = "TODO Título corto",          # lo que se lee en el botón
    grupo = "Biplot",
    icono = "seedling",                    # TODO: ícono de https://fontawesome.com/search?o=r&m=free
    descripcion = "TODO Una línea sobre qué hace el método",
    listo = TRUE,
    opciones_ui = mod_METODO_opciones_ui,
    resultados_ui = mod_METODO_resultados_ui,
    server = mod_METODO_server
  )
}

# -----------------------------------------------------------------------------
# C) PANEL DE OPCIONES (izquierda). Cada control lleva ns("nombre").
# -----------------------------------------------------------------------------
mod_METODO_opciones_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_opciones("TODO Título", "TODO Autor del método (año)"),
    seccion_opciones(
      "Variables",
      shiny::selectizeInput(ns("variables"), "Variables", choices = NULL, multiple = TRUE,
                            options = list(plugins = list("remove_button")))
      # TODO: si tu método necesita una variable de grupo, factor, etc., agrégala aquí
    ),
    seccion_opciones(
      "Opciones",
      shiny::checkboxInput(ns("estandarizar"), "Estandarizar", TRUE)
      # TODO: más opciones de tu método
    )
  )
}

# -----------------------------------------------------------------------------
# D) PANEL DE RESULTADOS (derecha): pestañas con tablas, gráficos y sintaxis.
# -----------------------------------------------------------------------------
mod_METODO_resultados_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    encabezado_resultados("TODO Título"),
    shiny::uiOutput(ns("error")),
    shiny::div(
      class = "bl-card",
      shiny::tabsetPanel(
        shiny::tabPanel("Gráfico", shiny::plotOutput(ns("grafico"), height = "560px")),
        shiny::tabPanel("Tablas", DT::DTOutput(ns("tabla"))),
        shiny::tabPanel(shiny::span(shiny::icon("code"), "Sintaxis R"),
                        shiny::div(class = "bl-code", shiny::verbatimTextOutput(ns("sintaxis"))))
      )
    )
  )
}

# -----------------------------------------------------------------------------
# E) LÓGICA. `datos()` devuelve list(df, origen, etiqueta, nominales):
#    `nominales` son las columnas de agrupación (no usarlas como cuantitativas).
#    `compartido` trae lo que publica el HJ-Biplot (compartido$hj(),
#    compartido$hj_config()) por si tu método se apoya en él.
# -----------------------------------------------------------------------------
mod_METODO_server <- function(id, datos, compartido = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Cuando cambia la base, ofrecer sus variables numéricas
    shiny::observeEvent(datos(), {
      df <- datos()$df
      num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], datos()$nominales)
      shiny::updateSelectizeInput(session, "variables", choices = num, selected = num)
    })

    # Cálculo: llama a tu función del punto A
    resultado <- shiny::reactive({
      if (is.null(datos())) return(simpleError("Primero carga una base de datos."))
      if (length(input$variables) < 2) return(simpleError("Elige al menos 2 variables."))
      tryCatch(calcular_METODO(datos()$df, input$variables, input$estandarizar),
               error = function(e) e)
    })
    res_ok <- shiny::reactive({
      r <- resultado()
      shiny::validate(shiny::need(!inherits(r, "error"), ""))
      r
    })

    output$error <- shiny::renderUI({
      r <- resultado()
      if (inherits(r, "error")) caja_error(conditionMessage(r))
    })

    # TODO: tu gráfico (ggplot2 recomendado)
    output$grafico <- shiny::renderPlot({
      inr <- res_ok()$inercia
      ggplot2::ggplot(inr, ggplot2::aes(factor(.data$Eje), .data$`% Inercia`)) +
        ggplot2::geom_col(fill = "#2a78d6", width = 0.6) +
        ggplot2::labs(x = "Eje", y = "% de inercia") +
        ggplot2::theme_minimal()
    })

    # TODO: tus tablas
    output$tabla <- DT::renderDT(tabla_resultados(res_ok()$inercia))

    # Sintaxis R: el código que reproduce el resultado (como en jamovi)
    output$sintaxis <- shiny::renderText(paste(
      "library(biplotlab)",
      sintaxis_lectura(datos()$origen),
      sprintf("res <- calcular_METODO(datos, variables = %s, estandarizar = %s)",
              vector_a_r(input$variables), input$estandarizar),
      "res$inercia",
      sep = "\n"
    ))

    res_ok
  })
}
