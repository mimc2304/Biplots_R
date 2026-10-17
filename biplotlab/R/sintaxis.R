# =============================================================================
# Ayudantes para generar la "Sintaxis R" de cada análisis (como el modo
# sintaxis de jamovi): el código que reproduce el resultado fuera de la app.
# =============================================================================

#' Convierte un vector en código R legible, partiendo líneas largas
#' @param v vector de caracteres o números.
#' @param sangria espacios de sangría para las líneas siguientes.
#' @export
vector_a_r <- function(v, sangria = 4) {
  if (is.null(v) || !length(v)) return("NULL")
  elem <- if (is.character(v)) encodeString(v, quote = "\"") else format(v, trim = TRUE)
  if (length(elem) == 1) return(elem)
  lineas <- character(); actual <- "c("
  for (i in seq_along(elem)) {
    pieza <- paste0(elem[i], if (i < length(elem)) ", " else ")")
    if (nchar(actual) + nchar(pieza) > 72 && actual != "c(") {
      lineas <- c(lineas, sub(" $", "", actual))
      actual <- strrep(" ", sangria + 2)
    }
    actual <- paste0(actual, pieza)
  }
  paste(c(lineas, actual), collapse = "\n")
}

#' Cabecera de la Sintaxis R: cómo cargar (o instalar) BiplotLab
#'
#' BiplotLab es el paquete del grupo y no está en CRAN. La sintaxis lo carga
#' con `library(biplotlab)` y explica cómo instalarlo una sola vez desde el
#' archivo `biplotlab_0.1.0.tar.gz` (o la carpeta `biplotlab`) si R responde
#' "there is no package called 'biplotlab'".
#' @export
sintaxis_paquete <- function() {
  c("# BiplotLab es el paquete del grupo: NO está en CRAN, por eso install.packages(\"biplotlab\")",
    "# no lo encuentra. Si R dice \"there is no package called 'biplotlab'\", instálalo UNA vez:",
    "#   install.packages(c(\"shiny\", \"bslib\", \"DT\", \"ggplot2\", \"ggrepel\", \"htmltools\",",
    "#                      \"colourpicker\", \"cluster\", \"readxl\", \"writexl\", \"haven\", \"nanoparquet\"))",
    "#   install.packages(\"C:/ruta/biplotlab_0.1.0.tar.gz\", repos = NULL, type = \"source\")",
    "# (también sirve la carpeta: install.packages(\"C:/ruta/biplotlab\", repos = NULL, type = \"source\"))",
    "library(biplotlab)")
}

#' Código R para leer los datos según su origen
#' @param origen lista con `tipo` ("ejemplo" o "archivo"), `nombre`, `hoja` y
#'   `nombres_filas`.
#' @export
sintaxis_lectura <- function(origen) {
  if (identical(origen$tipo, "ejemplo")) {
    return(sprintf("datos <- ejemplo_biplot(%s)", encodeString(origen$nombre, quote = "\"")))
  }
  args <- encodeString(origen$nombre, quote = "\"")
  if (!is.null(origen$hoja)) args <- paste0(args, ", hoja = ", encodeString(origen$hoja, quote = "\""))
  if (!isTRUE(origen$nombres_filas)) args <- paste0(args, ", nombres_filas = FALSE")
  if (isFALSE(origen$etiquetas)) args <- paste0(args, ", etiquetas = FALSE")
  if (isTRUE(origen$editado)) {
    return(paste0("# Los datos se editaron en la aplicación: descárgalos con el botón CSV\n",
                  "# del panel Datos y cambia la ruta por la ubicación de ese archivo\n",
                  "datos <- leer_datos(", args, ")"))
  }
  paste0("# Cambia la ruta por la ubicación del archivo en tu computador\n",
         "datos <- leer_datos(", args, ")")
}

#' Código R que reproduce la lectura de datos y el HJ-Biplot configurado
#'
#' Lo usan los módulos que trabajan sobre el núcleo HJ (Bootstrap, Clúster).
#' @param cfg lista de configuración publicada por el módulo HJ-Biplot.
#' @param llamar_hj si se incluye la llamada a [hj_biplot()].
#' @export
sintaxis_hj_base <- function(cfg, llamar_hj = TRUE) {
  c(
    sintaxis_paquete(),
    "",
    sintaxis_lectura(cfg$origen),
    if (length(cfg$excluir)) c(
      "",
      "# Individuos excluidos del análisis",
      sprintf("datos <- datos[!rownames(datos) %%in%% %s, ]", vector_a_r(cfg$excluir, 2))),
    if (llamar_hj) c(
      "",
      "res <- hj_biplot(",
      "  datos,",
      sprintf("  variables = %s,", vector_a_r(cfg$variables, 14)),
      sprintf("  transformacion = \"%s\",", cfg$transformacion),
      if (isTRUE(cfg$log)) "  log = TRUE,",
      sprintf("  tipo = \"%s\",", cfg$tipo),
      if (!is.null(cfg$referencia)) sprintf("  referencia = \"%s\",", cfg$referencia),
      if (length(cfg$invertir_ejes)) sprintf("  invertir_ejes = %s,", vector_a_r(cfg$invertir_ejes)),
      sprintf("  dimensiones = %d", as.integer(cfg$dimensiones)),
      ")")
  )
}

# Caja que indica de dónde salen los datos de un módulo que depende del HJ
caja_dependencia_hj <- function(texto) {
  shiny::div(
    class = "bl-info",
    shiny::icon("link"),
    shiny::span(texto, " "),
    shiny::tags$a(href = "#", onclick = "$('#abrir_hj').click(); return false;",
                  "Ir al HJ-Biplot")
  )
}
