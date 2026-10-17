# =============================================================================
# BiplotLab: abrir el aplicativo desde RStudio
#
# 1. Abre el archivo biplotlab.Rproj (doble clic): RStudio se ubica en la carpeta.
# 2. Abre este archivo (iniciar.R) y pulsa "Source" (o Ctrl + Shift + S).
# =============================================================================

# Paquetes necesarios (se instalan solo la primera vez)
paquetes <- c("shiny", "bslib", "DT", "ggplot2", "ggrepel", "htmltools", "colourpicker", "cluster",
              "readxl", "writexl", "haven", "nanoparquet", "MASS", "svglite", "pkgload", "roxygen2", "testthat")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)

# Carga el código de la carpeta (no hace falta instalar el paquete) y abre la app
pkgload::load_all(".")
run_app(options = list(launch.browser = TRUE))

# -----------------------------------------------------------------------------
# Otras tareas útiles (ejecuta la línea que necesites):
#
# Comprobar que los cálculos siguen coincidiendo con MULTBIPLOT:
#   testthat::test_local(".")
#
# Actualizar la documentación y el NAMESPACE después de agregar un módulo:
#   roxygen2::roxygenise()
#
# Instalar el paquete en tu R (para usarlo con library(biplotlab)):
#   install.packages(".", repos = NULL, type = "source")
# -----------------------------------------------------------------------------
