# Punto de entrada para publicar en shinyapps.io / Posit Connect o para abrir
# con el botón "Run App" de RStudio sin instalar el paquete.
# Forzar UTF-8 para que los textos con tildes se vean bien en el servidor
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))

pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
biplotlab::run_app()
