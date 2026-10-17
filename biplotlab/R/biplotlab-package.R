#' biplotlab: aplicativo de métodos Biplot al estilo jamovi
#'
#' Abre el aplicativo con [run_app()]. Cada método vive en su propio módulo
#' (archivo `R/mod_<metodo>.R`) y se registra en [registro_analisis()].
#'
#' @keywords internal
#' @importFrom ggplot2 .data
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices chull
#' @importFrom graphics abline arrows legend mtext plot points text
#' @importFrom stats setNames qlogis runif
#' @importFrom utils write.csv
"_PACKAGE"
