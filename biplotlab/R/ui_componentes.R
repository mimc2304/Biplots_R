# =============================================================================
# Piezas de interfaz comunes. Úsalas en tu módulo para que todos los análisis
# se vean igual (estilo jamovi).
# =============================================================================

#' Encabezado del panel de opciones de un análisis
#' @param titulo nombre del análisis.
#' @param referencia autor/año del método (texto pequeño).
#' @param autor (sin uso; se conserva por compatibilidad).
#' @export
encabezado_opciones <- function(titulo, referencia = NULL, autor = NULL) {
  shiny::div(
    class = "bl-opt-header",
    shiny::tags$a(href = "#", class = "bl-back", title = "Volver a los datos",
                  onclick = "Shiny.setInputValue('volver_datos', Date.now(), {priority: 'event'}); return false;",
                  shiny::icon("arrow-left")),
    shiny::div(
      shiny::h2(titulo),
      if (!is.null(referencia)) shiny::span(class = "bl-ref", referencia)
    )
  )
}

#' Sección plegable del panel de opciones
#' @param titulo título de la sección.
#' @param ... controles de Shiny.
#' @param abierta si la sección empieza abierta.
#' @export
seccion_opciones <- function(titulo, ..., abierta = TRUE) {
  shiny::tags$details(
    class = "bl-section", open = if (abierta) NA else NULL,
    shiny::tags$summary(titulo),
    shiny::div(class = "bl-section-body", ...)
  )
}

#' Encabezado del panel de resultados
#' @param titulo título.
#' @param subtitulo_id id de un `uiOutput` para el subtítulo dinámico.
#' @export
encabezado_resultados <- function(titulo, subtitulo_id = NULL) {
  shiny::div(class = "bl-res-header",
             shiny::h2(titulo),
             if (!is.null(subtitulo_id)) shiny::uiOutput(subtitulo_id, inline = TRUE))
}

#' Mensaje de error amigable en el panel de resultados
#' @param mensaje texto.
#' @export
caja_error <- function(mensaje) {
  shiny::div(class = "bl-alert", shiny::icon("triangle-exclamation"), shiny::span(mensaje))
}

#' Tabla de resultados interactiva (DT) con formato uniforme
#' @param df data.frame.
#' @param decimales decimales para las columnas numéricas.
#' @param filas filas por página.
#' @export
tabla_resultados <- function(df, decimales = 3, filas = 15) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  dt <- DT::datatable(
    df, rownames = FALSE, class = "compact stripe hover",
    extensions = character(),
    options = list(pageLength = filas, scrollX = TRUE, dom = if (nrow(df) > filas) "ftip" else "t",
                   language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_",
                                   paginate = list(previous = "Anterior", `next` = "Siguiente"),
                                   zeroRecords = "Sin coincidencias", infoEmpty = "Sin datos",
                                   infoFiltered = "(de _MAX_)")))
  if (length(num)) dt <- DT::formatRound(dt, num, digits = decimales, mark = "", dec.mark = ",")
  dt
}
