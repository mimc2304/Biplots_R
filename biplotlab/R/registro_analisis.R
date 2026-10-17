# =============================================================================
# REGISTRO DE ANÁLISIS
#
# Aquí se "enchufa" cada método al aplicativo. Para agregar el método de un
# compañero:
#   1. Copiar inst/plantilla/mod_plantilla.R a R/mod_<metodo>.R y completarlo.
#   2. Cambiar su entrada de `proximamente(...)` por la función del módulo,
#      por ejemplo `analisis_gge()`.
# Nada más: el menú, el panel de opciones y el de resultados se generan solos.
# =============================================================================

#' Lista de análisis del aplicativo
#'
#' Cada elemento es una lista con: `id`, `titulo`, `grupo`, `icono`
#' (Font Awesome), `descripcion`, `listo` y, si está listo,
#' `opciones_ui(id)`, `resultados_ui(id)` y `server(id, datos, compartido)`.
#' @export
registro_analisis <- function() {
  list(
    analisis_hj_biplot(),     # Evelyn: núcleo HJ, Bootstrap y Clúster
    analisis_bootstrap(),
    analisis_cluster(),
    analisis_canonico(),      # Roxi y Kleber
    analisis_logistico(),     # app del compañero: Logístico, DPCA, Disjoint y Cenet
    analisis_dpca(),
    analisis_disjoint(),
    analisis_cenet(),
    analisis_neutrosofico(),  # HJ-Biplot neutrosófico
    analisis_gge()            # GGE-Biplot
  )
}

proximamente <- function(id, titulo, icono, descripcion) {
  list(id = id, titulo = titulo, grupo = "Biplot", icono = icono,
       descripcion = descripcion, listo = FALSE)
}
