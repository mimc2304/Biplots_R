# =============================================================================
# Aplicativo BiplotLab: interfaz al estilo jamovi.
#   - Cinta superior con el menú de análisis (uno por integrante).
#   - Izquierda: la hoja de datos o, al abrir un análisis, sus opciones.
#   - Derecha: los resultados, con su "Sintaxis R" para quien quiera el código.
# =============================================================================

#' Abrir el aplicativo
#'
#' @param ... argumentos para [shiny::shinyApp()] (por ejemplo `options =
#'   list(launch.browser = TRUE)`).
#' @export
run_app <- function(...) {
  # Archivos de hasta 200 MB (Shiny acepta solo 5 MB si no se indica)
  options(shiny.maxRequestSize = LIMITE_ARCHIVO_MB * 1024^2)
  shiny::addResourcePath("biplotlab", app_www())
  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}

#' Tamaño máximo de archivo que se puede subir (MB)
#' @export
LIMITE_ARCHIVO_MB <- 200

app_www <- function() {
  ruta <- system.file("app", "www", package = "biplotlab")
  if (!nzchar(ruta)) ruta <- normalizePath(file.path("inst", "app", "www"))
  ruta
}

boton_analisis <- function(a) {
  if (isTRUE(a$listo)) {
    shiny::actionButton(paste0("abrir_", a$id), label = shiny::span(a$titulo),
                        icon = shiny::icon(a$icono), class = "bl-ribbon-btn",
                        title = a$descripcion)
  } else {
    shiny::tags$button(class = "btn bl-ribbon-btn bl-disabled", type = "button", disabled = NA,
                       title = paste0(a$descripcion, " - Próximamente"),
                       shiny::icon(a$icono), shiny::span(a$titulo))
  }
}

app_ui <- function(request) {
  analisis <- registro_analisis()
  listos <- Filter(function(a) isTRUE(a$listo), analisis)

  panel_datos <- shiny::tabPanelBody(
    "datos",
    shiny::div(
      class = "bl-opt-header",
      shiny::div(shiny::h2("Datos"),
                 shiny::span(class = "bl-ref", "Carga tu matriz: individuos en filas, variables en columnas"))
    ),
    seccion_opciones(
      "Abrir",
      shiny::fileInput("archivo", NULL,
                       accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx", ".sav", ".zsav",
                                  ".por", ".dta", ".parquet"),
                       buttonLabel = "Elegir archivo",
                       placeholder = "CSV, Excel, SPSS (.sav), Stata o Parquet"),
      shiny::uiOutput("selector_hoja"),
      shiny::uiOutput("opcion_etiquetas"),
      shiny::checkboxInput("nombres_filas", "La primera columna tiene los nombres de los individuos", TRUE),
      shiny::selectInput("ejemplo", "O usa una base de ejemplo",
                         c("Elegir una base de ejemplo..." = "", EJEMPLOS_BIPLOT)),
      shiny::p(class = "bl-help",
               "Las bases de ejemplo vienen con el aplicativo y sirven para probar cada método sin preparar un archivo.")
    ),
    shiny::uiOutput("info_datos"),
    seccion_opciones(
      "Tipo de columnas",
      shiny::selectizeInput("nominales", "Columnas nominales (de agrupación)", choices = NULL,
                            multiple = TRUE,
                            options = list(plugins = list("remove_button"),
                                           placeholder = "Ninguna")),
      shiny::p(class = "bl-help",
               "Las nominales solo sirven para colorear, agrupar o filtrar: nunca entran como variables cuantitativas en los ejes. Se proponen solas (texto, nombres como Group/Grupo/Código, o códigos enteros con pocos valores); revisa y corrige si hace falta.")
    ),
    seccion_opciones("Variables", DT::DTOutput("resumen_vars")),
    seccion_opciones(
      "Hoja de datos",
      shiny::conditionalPanel(
        "output.hay_datos",
        shiny::div(
          class = "bl-edit-bar",
          shiny::actionButton("fila_mas", "Fila", icon = shiny::icon("plus"),
                              class = "btn-sm", title = "Agregar una fila debajo de la seleccionada (o al final)"),
          shiny::actionButton("fila_menos", "Fila", icon = shiny::icon("minus"),
                              class = "btn-sm", title = "Eliminar las filas seleccionadas"),
          shiny::actionButton("col_mas", "Columna", icon = shiny::icon("plus"),
                              class = "btn-sm", title = "Agregar una columna"),
          shiny::actionButton("col_menos", "Columna", icon = shiny::icon("minus"),
                              class = "btn-sm", title = "Eliminar una columna"),
          shiny::actionButton("restaurar", "Restaurar", icon = shiny::icon("rotate-left"),
                              class = "btn-sm", title = "Volver a los datos del archivo, sin cambios"),
          shiny::downloadButton("bajar_csv", "CSV", class = "btn-sm",
                                title = "Descargar los datos (con los cambios) en CSV"),
          shiny::downloadButton("bajar_xlsx", "Excel", class = "btn-sm",
                                title = "Descargar los datos (con los cambios) en Excel")
        ),
        shiny::p(class = "bl-help",
                 "Doble clic en una celda para editarla (también el nombre del individuo) y Enter para guardar. Un clic sobre una fila la selecciona. Los cambios se aplican a todos los análisis; el archivo original no se modifica.")
      ),
      DT::DTOutput("tabla_datos")
    )
  )

  paneles_izq <- c(list(panel_datos), lapply(listos, function(a) {
    shiny::tabPanelBody(a$id, a$opciones_ui(a$id))
  }))
  paneles_der <- c(
    list(shiny::tabPanelBody("bienvenida", pantalla_bienvenida(analisis))),
    lapply(listos, function(a) shiny::tabPanelBody(a$id, a$resultados_ui(a$id)))
  )

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "biplotlab/estilo.css"),
      shiny::tags$script(src = "biplotlab/app.js"),
      shiny::tags$link(rel = "stylesheet", href = "biplotlab/katex/katex.min.css"),
      shiny::tags$script(src = "biplotlab/katex/katex.min.js"),
      shiny::tags$script(src = "biplotlab/katex/auto-render.min.js"),
      shiny::tags$script(src = "biplotlab/teoria.js"),
      shiny::tags$title("BiplotLab")
    ),
    bslib::page(
      theme = bslib::bs_theme(version = 5, primary = "#2a5d9f",
                              base_font = bslib::font_collection("Segoe UI", "system-ui", "sans-serif")),
      shiny::div(
        class = "bl-app",
        shiny::tags$header(
          class = "bl-top",
          shiny::div(class = "bl-brand", shiny::icon("compass-drafting"), shiny::span("BiplotLab")),
          shiny::span(class = "bl-top-sub", "Análisis multivariante con métodos Biplot"),
          selector_idioma()
        ),
        shiny::div(
          class = "bl-ribbon",
          shiny::div(class = "bl-ribbon-group",
                     shiny::actionButton("ir_datos", shiny::span("Datos"), icon = shiny::icon("table"),
                                         class = "bl-ribbon-btn"),
                     shiny::span(class = "bl-ribbon-label", "Datos")),
          shiny::div(class = "bl-ribbon-sep"),
          shiny::div(class = "bl-ribbon-group",
                     shiny::div(class = "bl-ribbon-row", lapply(analisis, boton_analisis)),
                     shiny::span(class = "bl-ribbon-label", "Análisis Biplot"))
        ),
        shiny::div(
          class = "bl-main",
          shiny::div(class = "bl-left",
                     do.call(shiny::tabsetPanel, c(list(id = "izq", type = "hidden"), paneles_izq))),
          shiny::div(class = "bl-results",
                     do.call(shiny::tabsetPanel, c(list(id = "der", type = "hidden"), paneles_der)))
        )
      )
    )
  )
}

pantalla_bienvenida <- function(analisis) {
  shiny::div(
    class = "bl-welcome",
    shiny::h2("Bienvenida a BiplotLab"),
    shiny::p("1. Carga tu base en el panel de la izquierda (CSV, Excel, SPSS, Stata o Parquet).",
             shiny::br(), "2. Elige un análisis en la cinta superior.",
             shiny::br(), "3. Ajusta las opciones; los resultados se actualizan solos.",
             shiny::br(), "4. En la pestaña ", shiny::strong("Sintaxis R"),
             " de cada análisis está el código para reproducirlo en R."),
    shiny::h4("Módulos"),
    shiny::tags$table(
      class = "table table-sm bl-modulos",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Método"), shiny::tags$th("Descripción"),
                                       shiny::tags$th("Estado"))),
      shiny::tags$tbody(lapply(analisis, function(a) shiny::tags$tr(
        shiny::tags$td(shiny::icon(a$icono), " ", a$titulo), shiny::tags$td(a$descripcion),
        shiny::tags$td(if (isTRUE(a$listo)) shiny::span(class = "bl-ok", "Disponible")
                       else shiny::span(class = "bl-pend", "En desarrollo")))))
    )
  )
}

app_server <- function(input, output, session) {
  analisis <- Filter(function(a) isTRUE(a$listo), registro_analisis())
  # Idioma de los resultados (selector ES | EN de la barra superior, ver idioma.R)
  session$userData$idioma <- shiny::reactive(input$idioma %||% "es")

  # ---------------------------------------------------------------- Datos ----
  # La app empieza sin datos: el usuario carga su archivo
  datos <- shiny::reactiveVal(NULL)
  original <- shiny::reactiveVal(NULL)     # tal como se leyó del archivo
  ediciones <- shiny::reactiveVal(0L)      # cambios hechos en la hoja
  version_tabla <- shiny::reactiveVal(0L)  # redibujar la hoja (carga o columnas nuevas)
  carga <- 0L                              # cuenta las cargas de archivo
  fuente <- shiny::reactiveVal("archivo")  # "archivo" o "ejemplo": lo último que se cargó

  hojas <- shiny::reactive({
    shiny::req(input$archivo, fuente() == "archivo")
    ext <- tolower(tools::file_ext(input$archivo$name))
    if (ext %in% c("xls", "xlsx")) readxl::excel_sheets(input$archivo$datapath) else NULL
  })
  output$selector_hoja <- shiny::renderUI({
    h <- hojas()
    if (length(h) > 1) shiny::selectInput("hoja", "Hoja de Excel", h)
  })
  # SPSS y Stata: leer las etiquetas de valor (texto) o los códigos numéricos
  con_etiquetas <- shiny::reactive({
    shiny::req(input$archivo, fuente() == "archivo")
    tolower(tools::file_ext(input$archivo$name)) %in% c("sav", "zsav", "por", "dta")
  })
  output$opcion_etiquetas <- shiny::renderUI({
    if (!con_etiquetas()) return(NULL)
    shiny::tagList(
      shiny::checkboxInput("etiquetas", "Usar las etiquetas de valor (p. ej. 1 = Hombre, 2 = Mujer)", TRUE),
      shiny::p(class = "bl-help",
               "Las variables con todos sus valores etiquetados se leen como texto y quedan como nominales (de agrupación). Desmárcalo para leer los códigos numéricos, por ejemplo si una escala Likert etiquetada debe entrar como variable.")
    )
  })

  cargar_archivo <- function() {
    shiny::req(input$archivo)
    hoja <- if (length(hojas()) > 1) input$hoja %||% hojas()[1] else NULL
    df <- tryCatch(
      leer_datos(input$archivo$datapath, hoja = hoja, nombres_filas = input$nombres_filas,
                 nombre_original = input$archivo$name, etiquetas = input$etiquetas %||% TRUE),
      error = function(e) {
        shiny::showNotification(paste("No se pudo leer el archivo:", conditionMessage(e)),
                                type = "error", duration = 8)
        NULL
      })
    if (is.null(df)) return()
    poner_datos(df, list(tipo = "archivo", nombre = input$archivo$name, hoja = hoja,
                         nombres_filas = input$nombres_filas,
                         etiquetas = if (con_etiquetas()) input$etiquetas %||% TRUE),
                input$archivo$name)
  }
  poner_datos <- function(df, origen, etiqueta) {
    carga <<- carga + 1L
    d <- list(df = df, origen = origen, etiqueta = etiqueta, carga = carga)
    original(d)
    ediciones(0L)
    datos(d)
    version_tabla(version_tabla() + 1L)
  }
  shiny::observeEvent(input$archivo, {
    fuente("archivo")
    shiny::updateSelectInput(session, "ejemplo", selected = "")
    cargar_archivo()
  })
  shiny::observeEvent(list(input$hoja, input$nombres_filas, input$etiquetas), {
    if (!is.null(input$archivo) && fuente() == "archivo") cargar_archivo()
  }, ignoreInit = TRUE)
  # Bases de ejemplo: se leen con ejemplo_biplot(), así la Sintaxis R las reproduce
  shiny::observeEvent(input$ejemplo, {
    shiny::req(nzchar(input$ejemplo))
    fuente("ejemplo")
    df <- ejemplo_biplot(input$ejemplo)
    poner_datos(df, list(tipo = "ejemplo", nombre = input$ejemplo),
                names(EJEMPLOS_BIPLOT)[EJEMPLOS_BIPLOT == input$ejemplo])
  })

  # Guardar un cambio de la hoja: los análisis reciben la copia editada y su
  # Sintaxis R lee el archivo editado que se descarga con el botón CSV
  aplicar_edicion <- function(df, redibujar = FALSE) {
    d <- datos()
    d$df <- df
    d$origen <- utils::modifyList(original()$origen,
                                  list(tipo = "archivo", hoja = NULL, nombres_filas = TRUE,
                                       etiquetas = NULL,
                                       nombre = nombre_editado(original()$origen$nombre),
                                       editado = TRUE))
    datos(d)
    ediciones(ediciones() + 1L)
    if (redibujar) version_tabla(version_tabla() + 1L) else refrescar_tabla(df)
  }
  intentar <- function(expr) {
    tryCatch(expr, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "warning", duration = 6)
      NULL
    })
  }

  output$hay_datos <- shiny::reactive(!is.null(datos()))
  shiny::outputOptions(output, "hay_datos", suspendWhenHidden = FALSE)

  output$info_datos <- shiny::renderUI({
    d <- datos()
    if (is.null(d)) {
      return(shiny::div(class = "bl-info bl-vacio", shiny::icon("circle-info"),
                        "Aún no hay datos. Elige un archivo o una base de ejemplo para empezar."))
    }
    n <- ediciones()
    shiny::div(class = "bl-info",
               shiny::icon("database"), shiny::strong(d$etiqueta),
               sprintf(" - %d filas × %d columnas", nrow(d$df), ncol(d$df)),
               if (n > 0) shiny::span(class = "bl-editado",
                                      sprintf(" - editado (%d %s)", n, if (n == 1) "cambio" else "cambios")))
  })
  # Columnas nominales: se proponen al cargar y el usuario puede corregirlas.
  # Al editar se conserva su elección; solo se proponen las columnas nuevas.
  nominales <- shiny::reactiveVal(character())
  ultima <- list(carga = -1L, columnas = character())
  shiny::observeEvent(datos(), {
    d <- datos()
    df <- d$df
    if (!identical(d$carga, ultima$carga)) {
      sel <- detectar_nominales(df)
    } else if (!identical(names(df), ultima$columnas)) {
      nuevas <- setdiff(names(df), ultima$columnas)
      sel <- union(intersect(shiny::isolate(nominales()), names(df)),
                   intersect(detectar_nominales(df), nuevas))
    } else {
      return()
    }
    ultima <<- list(carga = d$carga, columnas = names(df))
    nominales(sel)
    shiny::updateSelectizeInput(session, "nominales", choices = names(df), selected = sel)
  })
  shiny::observeEvent(input$nominales, {
    shiny::req(datos())
    nominales(intersect(input$nominales %||% character(), names(datos()$df)))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Lo que reciben los módulos: datos + columnas nominales
  datos_app <- shiny::reactive({
    d <- datos()
    if (is.null(d)) return(NULL)
    d$nominales <- nominales()
    d
  })

  output$resumen_vars <- DT::renderDT({
    shiny::req(datos())
    tabla_resultados(resumen_variables(datos()$df, nominales()), 0, 20)
  })

  # La hoja se dibuja al cargar o al cambiar las columnas; los cambios de celdas
  # y de filas se envían sin redibujar, para no perder la página en que se está
  output$tabla_datos <- DT::renderDT({
    version_tabla()
    df <- shiny::isolate(datos()$df)
    shiny::req(df)
    num <- names(df)[vapply(df, is.numeric, logical(1))]
    dt <- DT::datatable(df, class = "compact stripe hover nowrap",
                        editable = list(target = "cell", numeric = "none"), selection = "multiple",
                        # Enter guarda la celda (DT, por defecto, solo guarda al salir de ella)
                        callback = DT::JS("table.on('keydown', 'tbody td input', function(e) {",
                                          "  if (e.keyCode === 13) $(this).blur();",
                                          "});"),
                        options = list(pageLength = 20, scrollX = TRUE, dom = "tip",
                                       language = list(info = "_START_ a _END_ de _TOTAL_",
                                                       paginate = list(previous = "Anterior",
                                                                       `next` = "Siguiente"))))
    if (length(num)) dt <- DT::formatRound(dt, num, digits = 3, dec.mark = ",", mark = "")
    dt
  })
  proxy_tabla <- DT::dataTableProxy("tabla_datos")
  refrescar_tabla <- function(df) {
    DT::replaceData(proxy_tabla, df, resetPaging = FALSE, rownames = TRUE)
  }

  shiny::observeEvent(input$tabla_datos_cell_edit, {
    e <- input$tabla_datos_cell_edit
    df <- intentar(editar_celda(datos()$df, e$row, e$col, e$value))
    if (is.null(df)) refrescar_tabla(datos()$df)
    else if (!identical(df, datos()$df)) aplicar_edicion(df)
  })

  shiny::observeEvent(input$fila_mas, {
    shiny::req(datos())
    sel <- input$tabla_datos_rows_selected
    pos <- if (length(sel)) max(sel) else nrow(datos()$df)
    df <- agregar_fila(datos()$df, despues = pos)
    aplicar_edicion(df)
    DT::selectRows(proxy_tabla, pos + 1L)
    DT::selectPage(proxy_tabla, ceiling((pos + 1L) / 20))
    shiny::showNotification("Fila agregada. Doble clic en sus celdas para escribir los valores; mientras tenga celdas vacías, los análisis lo avisarán.",
                            type = "message", duration = 6)
  })
  shiny::observeEvent(input$fila_menos, {
    shiny::req(datos())
    sel <- input$tabla_datos_rows_selected
    if (!length(sel)) {
      shiny::showNotification("Primero selecciona con un clic las filas que quieres eliminar.",
                              type = "warning", duration = 5)
      return()
    }
    df <- intentar(eliminar_filas(datos()$df, sel))
    if (!is.null(df)) {
      aplicar_edicion(df)
      DT::selectRows(proxy_tabla, NULL)
    }
  })

  shiny::observeEvent(input$col_mas, {
    shiny::req(datos())
    shiny::showModal(shiny::modalDialog(
      title = "Agregar columna", easyClose = TRUE,
      shiny::textInput("col_nombre", "Nombre"),
      shiny::radioButtons("col_tipo", "Tipo",
                          c("Numérica (variable de escala)" = "numerica",
                            "Texto (nominal, de agrupación)" = "texto")),
      shiny::textInput("col_valor", "Valor inicial de todas las celdas (opcional)"),
      footer = shiny::tagList(shiny::modalButton("Cancelar"),
                              shiny::actionButton("col_ok", "Agregar", class = "btn-primary"))
    ))
  })
  shiny::observeEvent(input$col_ok, {
    df <- intentar(agregar_columna(datos()$df, input$col_nombre, input$col_tipo, input$col_valor))
    if (is.null(df)) return()
    shiny::removeModal()
    aplicar_edicion(df, redibujar = TRUE)
  })

  shiny::observeEvent(input$col_menos, {
    shiny::req(datos())
    shiny::showModal(shiny::modalDialog(
      title = "Eliminar columna", easyClose = TRUE,
      shiny::selectInput("col_quitar", "Columna", names(datos()$df)),
      footer = shiny::tagList(shiny::modalButton("Cancelar"),
                              shiny::actionButton("col_quitar_ok", "Eliminar", class = "btn-danger"))
    ))
  })
  shiny::observeEvent(input$col_quitar_ok, {
    df <- intentar(eliminar_columna(datos()$df, input$col_quitar))
    if (is.null(df)) return()
    shiny::removeModal()
    aplicar_edicion(df, redibujar = TRUE)
  })

  shiny::observeEvent(input$restaurar, {
    shiny::req(original())
    if (ediciones() == 0) return()
    datos(original())
    ediciones(0L)
    version_tabla(version_tabla() + 1L)
    shiny::showNotification("Se restauraron los datos del archivo.", type = "message", duration = 4)
  })

  output$bajar_csv <- shiny::downloadHandler(
    filename = function() nombre_editado(original()$origen$nombre, "csv"),
    content = function(file) {
      utils::write.csv(datos_para_guardar(datos()$df), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    })
  output$bajar_xlsx <- shiny::downloadHandler(
    filename = function() nombre_editado(original()$origen$nombre, "xlsx"),
    content = function(file) writexl::write_xlsx(datos_para_guardar(datos()$df), file))

  # ------------------------------------------------------------ Navegación ---
  activo <- shiny::reactiveVal("datos")
  marcar <- function(id) {
    activo(id)
    session$sendCustomMessage("bl-activo", id)
  }
  shiny::observeEvent(input$ir_datos, {
    shiny::updateTabsetPanel(session, "izq", selected = "datos"); marcar("datos")
  })
  shiny::observeEvent(input$volver_datos, {
    shiny::updateTabsetPanel(session, "izq", selected = "datos"); marcar("datos")
  })

  # Resultados compartidos entre módulos (el HJ publica aquí su configuración y
  # su resultado para Bootstrap y Clúster)
  compartido <- shiny::reactiveValues()

  for (a in analisis) local({
    a <- a
    shiny::observeEvent(input[[paste0("abrir_", a$id)]], {
      shiny::updateTabsetPanel(session, "izq", selected = a$id)
      shiny::updateTabsetPanel(session, "der", selected = a$id)
      marcar(a$id)
    })
    a$server(a$id, datos_app, compartido)
  })
}
