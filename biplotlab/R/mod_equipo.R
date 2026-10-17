# =============================================================================
# Métodos del compañero: DPCA, Disjoint Biplot (CDBiplot), CenetBiplot y
# Biplot logístico. Los cálculos están en los archivos R/equipo_*.R, tal como
# los entregó. Aquí solo se adapta su pantalla al aplicativo:
#   - los datos vienen del panel Datos (con sus columnas nominales), y en cada
#     método se eligen las variables, el grupo y la preparación de la matriz;
#   - las opciones van a la izquierda y los resultados a la derecha;
#   - el plano usa el encuadre rectangular de la Dra. (mínimo 4:3) y se puede
#     voltear, como en el resto de biplots;
#   - cada método tiene su pestaña Teoría y su Sintaxis R.
# Su PCA/HJ-Biplot no se integra porque el aplicativo ya tiene el HJ-Biplot.
# =============================================================================

#' @rawNamespace export(prep_matrix, svd_fact, make_fact, biplot_coords, eigen_table, rows_table, cols_table)
#' @rawNamespace export(dpca_greedy, dpca_pso, cdpca, cenet_svd, cenet_bic, cenet_cv_alpha)
#' @rawNamespace export(elb_fit, elb_stats, elb_select, elb_cluster, logistic_biplot)
#' @rawNamespace export(plot_biplot, plot_logistic)
NULL

METODOS_EQUIPO <- list(
  dpca = list(titulo = "DPCA", icono = "puzzle-piece",
              descripcion = "Componentes principales disjuntas: cada variable en un solo eje",
              referencia = "Vichi y Saporta (2009)"),
  disjoint = list(titulo = "Disjoint Biplot", icono = "object-ungroup",
                  descripcion = "CDBiplot: grupos de individuos y componentes disjuntas de variables",
                  referencia = "Nieto-Librero et al. (2017)"),
  cenet = list(titulo = "CenetBiplot", icono = "filter",
               descripcion = "Biplot con cargas dispersas (elastic net) y ejes ortogonales",
               referencia = "González-García et al."),
  logistic = list(titulo = "Logístico", icono = "toggle-on",
                  descripcion = "Biplot logístico externo para datos binarios (0/1)",
                  referencia = "Demey et al. (2008); Vicente-Villardón et al. (2006)")
)

analisis_equipo <- function(metodo) {
  m <- METODOS_EQUIPO[[metodo]]
  list(id = metodo, titulo = m$titulo, grupo = "Biplot", icono = m$icono,
       descripcion = m$descripcion, listo = TRUE,
       opciones_ui = function(id) mod_equipo_opciones_ui(id, metodo),
       resultados_ui = function(id) mod_equipo_resultados_ui(id, metodo),
       server = function(id, datos, compartido) mod_equipo_server(id, metodo, datos))
}

#' @rdname registro_analisis
analisis_dpca <- function() analisis_equipo("dpca")
#' @rdname registro_analisis
analisis_disjoint <- function() analisis_equipo("disjoint")
#' @rdname registro_analisis
analisis_cenet <- function() analisis_equipo("cenet")
#' @rdname registro_analisis
analisis_logistico <- function() analisis_equipo("logistic")

# ----------------------------------------------------------------- Opciones --
mod_equipo_opciones_ui <- function(id, method) {
  ns <- shiny::NS(id)
  m <- METODOS_EQUIPO[[method]]
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  is_fact <- method != "logistic"

  variables <- seccion_opciones(
    "Variables y preparación",
    shiny::selectizeInput(ns("vars"),
                          if (is_fact) "Variables activas (escala)" else "Variables binarias (0/1)",
                          choices = NULL, multiple = TRUE,
                          options = list(plugins = list("remove_button"))),
    shiny::selectInput(ns("grp"), "Variable de grupo (colorea; no construye ejes)", choices = NULL),
    if (is_fact) shiny::tagList(
      shiny::checkboxInput(ns("center"), "Centrar columnas", TRUE),
      shiny::checkboxInput(ns("scale"), "Estandarizar columnas (dividir por la desviación típica)", TRUE),
      shiny::checkboxInput(ns("logt"), "Transformar log(x + 1) antes (abundancias muy asimétricas)", FALSE)
    ) else ayuda("Se proponen las columnas con solo 0 y 1. Las que no sean binarias se dicotomizan o se excluyen según la opción del modelo."),
    ayuda("Las filas con datos faltantes en las variables elegidas se dejan fuera del análisis.")
  )

  model_controls <- switch(method,
    dpca = shiny::tagList(
      shiny::numericInput(ns("Q"), "Número de componentes (grupos de variables)", 3, 2, 20),
      shiny::radioButtons(ns("algo"), "Algoritmo", c("Greedy (DPCA)" = "greedy", "CBPSO-PCA (enjambre)" = "pso")),
      shiny::conditionalPanel("input.algo == 'greedy'", ns = ns,
                              shiny::numericInput(ns("nstart"), "Arranques aleatorios", 30, 1, 500)),
      shiny::conditionalPanel("input.algo == 'pso'", ns = ns,
                              shiny::numericInput(ns("P"), "Partículas", 40, 5, 300),
                              shiny::numericInput(ns("nIter"), "Iteraciones", 40, 5, 500)),
      shiny::numericInput(ns("seed"), "Semilla", 1, 1, 99999)),
    disjoint = shiny::tagList(
      shiny::numericInput(ns("Q"), "Número de componentes Q (grupos de variables)", 3, 2, 20),
      shiny::numericInput(ns("Pcl"), "Número de clusters de individuos P", 3, 2, 50),
      shiny::numericInput(ns("nstart"), "Arranques aleatorios", 20, 1, 500),
      shiny::numericInput(ns("seed"), "Semilla", 1, 1, 99999)),
    cenet = shiny::tagList(
      shiny::numericInput(ns("Q"), "Número de ejes", 3, 2, 20),
      shiny::sliderInput(ns("alpha"), "α (0 = Lasso, 1 = Ridge)", 0, 0.95, 0.5, step = 0.05),
      shiny::sliderInput(ns("lv"), "Nivel de sparsity en variables (1 = sin restricción)", 0.05, 1, 0.5, step = 0.05),
      shiny::checkboxInput(ns("sparse_u"), "Sparsity también en individuos", FALSE),
      shiny::conditionalPanel("input.sparse_u", ns = ns,
                              shiny::sliderInput(ns("lu"), "Nivel de sparsity en individuos", 0.05, 1, 0.8, step = 0.05)),
      shiny::numericInput(ns("eps"), "Tolerancia POCS", 1e-7, 1e-12, 1e-3)),
    logistic = shiny::tagList(
      shiny::radioButtons(ns("lmode"), "Método",
                          c("Externo: PCoA + regresión logística (clase)" = "ext",
                            "Interno: regresiones alternadas" = "int")),
      shiny::numericInput(ns("Q"), "Dimensiones", 2, 2, 6),
      shiny::conditionalPanel("input.lmode == 'ext'", ns = ns,
        shiny::selectInput(ns("sim"), "Coeficiente de similaridad", similarity_choices, "simple_matching"),
        shiny::checkboxInput(ns("sqrt_d"), "Distancia = √(1 − s) (Gower-Legendre)", FALSE),
        shiny::numericInput(ns("alpha_p"), "Nivel de significación (p-valor)", 0.05, 0.001, 0.5, step = 0.01),
        shiny::checkboxInput(ns("bonf"), "Corrección de Bonferroni", TRUE),
        shiny::sliderInput(ns("r2min"), "R² (Nagelkerke) mínimo para dibujar", 0, 0.95, 0, step = 0.05),
        shiny::numericInput(ns("ncl"), "Clusters (Ward) sobre las coordenadas; 0 = ninguno", 0, 0, 12)),
      shiny::conditionalPanel("input.lmode == 'int'", ns = ns,
        shiny::numericInput(ns("lambda"), "Penalización ridge", 0.1, 0, 10, step = 0.05),
        shiny::numericInput(ns("maxit"), "Iteraciones máximas", 60, 5, 500)),
      shiny::radioButtons(ns("dich"), "Variables no binarias",
                          c("Dicotomizar por la mediana" = "median", "Dicotomizar por la media" = "mean",
                            "Excluirlas" = "drop")),
      shiny::checkboxInput(ns("drop_rows"), "Quitar individuos con todo 0 o todo 1", TRUE)))

  plot_controls <- shiny::tagList(
    shiny::fluidRow(shiny::column(6, shiny::numericInput(ns("dx"), "Eje X", 1, 1, 20)),
                    shiny::column(6, shiny::numericInput(ns("dy"), "Eje Y", 2, 1, 20))),
    shiny::fluidRow(shiny::column(6, shiny::checkboxInput(ns("voltear_h"), "Voltear de izquierda a derecha", FALSE)),
                    shiny::column(6, shiny::checkboxInput(ns("voltear_v"), "Voltear de arriba a abajo", FALSE))),
    if (is_fact) shiny::radioButtons(ns("type"), "Marcadores", c("HJ" = "HJ", "JK" = "JK", "GH" = "GH"), inline = TRUE),
    if (!is_fact) shiny::sliderInput(ns("prob"), "Probabilidad de la punta de flecha", 0.55, 0.95, 0.75, step = 0.05),
    if (method %in% c("disjoint", "logistic"))
      shiny::radioButtons(ns("color_by"), "Colorear individuos por",
                          c("Cluster" = "cluster", "Grupo de los datos" = "group"), inline = TRUE),
    if (method == "logistic")
      shiny::selectInput(ns("cl_style"), "Dibujo de clusters",
                         c("Ninguno" = "none", "Envolvente convexa" = "hull", "Elipse" = "ellipse", "Estrella" = "star"), "hull"),
    shiny::checkboxInput(ns("show_rows"), "Mostrar individuos", TRUE),
    shiny::checkboxInput(ns("row_labels"), "Etiquetas de individuos", FALSE),
    shiny::checkboxInput(ns("col_labels"), "Etiquetas de variables", TRUE),
    if (is_fact) shiny::checkboxInput(ns("fit_arrows"), "Ajustar escala de flechas (solo visual)", TRUE),
    if (!is_fact) shiny::checkboxInput(ns("full_lines"), "Dibujar rectas completas", FALSE),
    shiny::checkboxInput(ns("use_group"), "Colorear por grupo (si hay)", TRUE),
    shiny::sliderInput(ns("psize"), "Tamaño de puntos", 0.5, 6, 2.4, step = 0.1),
    shiny::sliderInput(ns("lsize"), "Tamaño de etiquetas", 2, 6, 3.4, step = 0.1))

  shiny::tagList(
    encabezado_opciones(m$titulo, m$referencia),
    variables,
    seccion_opciones("Modelo", model_controls,
                     shiny::actionButton(ns("run"), "Calcular", class = "btn-primary w-100")),
    seccion_opciones("Gráfico", plot_controls),
    seccion_opciones("Tablas",
                     shiny::radioButtons(ns("unit"), "Contribuciones en",
                                         c("‰ (por mil)" = "1000", "%" = "100"), inline = TRUE),
                     abierta = FALSE),
    seccion_opciones("Exportar",
                     shiny::downloadButton(ns("dl_xlsx"), "Todas las tablas (.xlsx)", class = "btn-sm"),
                     abierta = FALSE)
  )
}

# --------------------------------------------------------------- Resultados --
mod_equipo_resultados_ui <- function(id, method) {
  ns <- shiny::NS(id)
  m <- METODOS_EQUIPO[[method]]
  ayuda <- function(...) shiny::p(class = "bl-help", ...)
  tabs <- list(
    shiny::tabPanel("Biplot",
                    shiny::div(class = "bl-descargas",
                               shiny::downloadButton(ns("dl_png"), "PNG", class = "btn-sm"),
                               shiny::downloadButton(ns("dl_pdf"), "PDF (vectorial)", class = "btn-sm")),
                    shiny::plotOutput(ns("biplot"), height = "auto")),
    shiny::tabPanel("Autovalores", shiny::fluidRow(
      shiny::column(6, DT::DTOutput(ns("eig"))),
      shiny::column(6, shiny::plotOutput(ns("scree"), height = "340px")))),
    shiny::tabPanel("Individuos",
                    ayuda(if (method == "logistic") "Coordenadas principales, contribución absoluta (CA) y calidad de representación (QR, Demey et al. 2008)."
                          else "Coordenadas, contribución absoluta CA (el elemento al eje) y contribución relativa CR = calidad de representación (el eje al elemento)."),
                    DT::DTOutput(ns("rows"))),
    shiny::tabPanel("Variables",
                    ayuda(if (method == "logistic") "Coeficientes logísticos: b0 y dirección b_j de cada variable."
                          else "Coordenadas, cargas, contribución absoluta CA y contribución relativa CR (= r² con el eje)."),
                    DT::DTOutput(ns("cols"))),
    if (method == "disjoint")
      shiny::tabPanel("Clusters", ayuda("Partición de individuos (U), centroides en el espacio reducido y devianza entre/intra clases."),
                      DT::DTOutput(ns("cltab")), shiny::br(), DT::DTOutput(ns("clmem"))),
    if (method == "logistic")
      shiny::tabPanel("Clusters", ayuda("Ward sobre las coordenadas principales (solo método externo). Calidad del cluster: distancia² del centroide en el plano / en todo el espacio."),
                      DT::DTOutput(ns("cltab"))),
    if (method %in% c("dpca", "disjoint"))
      shiny::tabPanel("Partición y convergencia", shiny::fluidRow(
        shiny::column(5, DT::DTOutput(ns("part")), shiny::br(), shiny::verbatimTextOutput(ns("fcmp"))),
        shiny::column(7, shiny::plotOutput(ns("conv"), height = "300px"), DT::DTOutput(ns("starts"))))),
    if (method == "cenet")
      shiny::tabPanel("Sparsity", DT::DTOutput(ns("spars")), shiny::br(), shiny::plotOutput(ns("conv"), height = "300px")),
    if (method == "cenet")
      shiny::tabPanel("Selección de parámetros", shiny::fluidRow(
        shiny::column(6, shiny::h5("τ por BIC (ec. 9)"),
                      shiny::actionButton(ns("run_bic"), "Calcular BIC", class = "btn-primary btn-sm"),
                      shiny::actionButton(ns("apply_bic"), "Usar nivel óptimo", class = "btn-sm"),
                      shiny::plotOutput(ns("bic_plot"), height = "260px"), DT::DTOutput(ns("bic_tab"))),
        shiny::column(6, shiny::h5("α por validación cruzada (ec. 8)"),
                      shiny::numericInput(ns("folds"), "Folds", 5, 2, 10),
                      shiny::actionButton(ns("run_cv"), "Calcular CV", class = "btn-primary btn-sm"),
                      shiny::actionButton(ns("apply_cv"), "Usar α óptimo", class = "btn-sm"),
                      shiny::plotOutput(ns("cv_plot"), height = "260px"), DT::DTOutput(ns("cv_tab"))))),
    if (method == "logistic")
      shiny::tabPanel("Ajuste logístico", shiny::fluidRow(
        shiny::column(8, shiny::h5("Estadísticos por variable"), DT::DTOutput(ns("lstats"))),
        shiny::column(4, shiny::h5("Clasificación global"), shiny::verbatimTextOutput(ns("lglob")),
                      shiny::plotOutput(ns("conv"), height = "260px")))),
    shiny::tabPanel(shiny::span(shiny::icon("book-open"), "Teoría"), teoria_ui(method)),
    shiny::tabPanel(shiny::span(shiny::icon("code"), "Sintaxis R"), shiny::verbatimTextOutput(ns("sintaxis")))
  )
  tabs <- Filter(Negate(is.null), tabs)
  shiny::tagList(
    encabezado_resultados(m$titulo, ns("subtitulo")),
    shiny::uiOutput(ns("aviso")),
    shiny::div(class = "bl-card", do.call(shiny::tabsetPanel, c(list(id = ns("tabs")), tabs)))
  )
}

# Cambia el signo de un eje en puntos y vectores a la vez (no cambia ningún
# resultado: solo la orientación del dibujo)
voltear_equipo <- function(r, ejes) {
  if (!length(ejes)) return(r)
  cambiar <- function(M) { if (is.null(M)) return(M); k <- intersect(ejes, seq_len(ncol(M))); M[, k] <- -M[, k]; M }
  if (!is.null(r$fact)) {
    r$fact$U <- cambiar(r$fact$U); r$fact$V <- cambiar(r$fact$V); r$fact$S <- cambiar(r$fact$S)
    if (!is.null(r$centroids)) r$centroids <- cambiar(r$centroids)
  } else {
    r$A <- cambiar(r$A); r$B <- cambiar(r$B)
    if (!is.null(r$Yfull)) r$Yfull <- cambiar(r$Yfull)
  }
  r
}

# Encuadre rectangular (ecuaciones de la Dra.) para un gráfico con coord_equal()
encuadre_ggplot <- function(p) {
  r <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  lim <- limites_encuadre(r$x.range, r$y.range, incluir_origen = TRUE, margen = 0)
  suppressMessages(p + ggplot2::coord_equal(xlim = lim$x, ylim = lim$y, expand = FALSE, clip = "off"))
}

# ------------------------------------------------------------------ Servidor --
mod_equipo_server <- function(id, method, datos) {
  shiny::moduleServer(id, function(input, output, session) {
    is_fact <- method != "logistic"

    # --- Variables y grupo a partir del panel Datos -------------------------
    shiny::observeEvent(datos(), {
      d <- datos(); df <- d$df
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      if (is_fact) {
        choices <- setdiff(num, d$nominales)
        defecto <- choices
      } else {
        choices <- num
        bin <- num[vapply(df[num], function(z) all(stats::na.omit(z) %in% c(0, 1)), logical(1))]
        defecto <- if (length(bin) >= 3) bin else setdiff(num, d$nominales)
      }
      sel <- intersect(shiny::isolate(input$vars), choices)
      if (length(sel) < 2) sel <- defecto
      shiny::updateSelectizeInput(session, "vars", choices = choices, selected = sel)
      # Primero las columnas de texto; luego las nominales numéricas que no son variables del análisis
      grupos <- unique(c(names(df)[!vapply(df, is.numeric, logical(1))],
                         setdiff(intersect(d$nominales, names(df)), defecto)))
      g <- shiny::isolate(input$grp)
      if (is.null(g) || !g %in% grupos) g <- if (length(grupos)) grupos[1] else ""
      shiny::updateSelectInput(session, "grp", choices = c("(ninguna)" = "", grupos), selected = g)
    })

    sel <- shiny::reactive({
      d <- datos(); shiny::req(d, input$vars)
      v <- intersect(input$vars, names(d$df)); shiny::req(length(v) > 0)
      df <- d$df
      ok <- stats::complete.cases(df[, v, drop = FALSE])
      df <- df[ok, , drop = FALSE]
      M <- as.matrix(df[, v, drop = FALSE]); storage.mode(M) <- "double"
      rownames(M) <- make.unique(rownames(df))
      g <- if (nzchar(input$grp %||% "") && input$grp %in% names(df)) factor(df[[input$grp]]) else NULL
      list(M = M, g = g, dropped = sum(!ok))
    })
    dat <- list(
      Xraw = shiny::reactive(sel()$M),
      X = shiny::reactive({
        M <- sel()$M
        if (isTRUE(input$logt)) {
          shiny::validate(shiny::need(all(M >= 0), "log(x+1) requiere valores >= 0."))
          M <- log1p(M)
        }
        prep_matrix(M, isTRUE(input$center), isTRUE(input$scale))
      }),
      groups = shiny::reactive(sel()$g)
    )

    output$subtitulo <- shiny::renderUI({
      s <- sel()
      shiny::span(class = "bl-sub", sprintf("%s - %d individuos × %d variables%s", datos()$etiqueta,
                                            nrow(s$M), ncol(s$M),
                                            if (s$dropped) sprintf(" (%d filas con faltantes fuera)", s$dropped) else ""))
    })

    Xbin <- shiny::reactive({
      M <- dat$Xraw(); shiny::validate(shiny::need(ncol(M) >= 2, "Selecciona al menos 2 variables."))
      isbin <- apply(M, 2, function(z) all(z %in% c(0, 1)))
      if (input$dich == "drop") M <- M[, isbin, drop = FALSE]
      else {
        f <- if (input$dich == "mean") mean else stats::median
        for (j in which(!isbin)) M[, j] <- as.numeric(M[, j] > f(M[, j]))
      }
      shiny::validate(shiny::need(ncol(M) >= 3, "Hacen falta al menos 3 variables binarias."))
      M <- M[, apply(M, 2, function(z) length(unique(z)) == 2), drop = FALSE]
      if (isTRUE(input$drop_rows)) M <- M[rowSums(M) > 0 & rowSums(M) < ncol(M), , drop = FALSE]
      M
    })

    # --- Cálculo (motores del compañero, sin cambios) ------------------------
    fit0 <- shiny::eventReactive(list(input$run, dat$Xraw(), dat$X()), {
      if (method == "logistic") {
        X <- Xbin(); shiny::validate(shiny::need(nrow(X) >= 10, "Muy pocos individuos."))
        r <- shiny::withProgress(message = "Ajustando biplot logístico", value = 0, {
          if (input$lmode == "ext") elb_fit(X, input$Q, input$sim, isTRUE(input$sqrt_d), progress = function(v) shiny::setProgress(v))
          else logistic_biplot(X, input$Q, maxit = input$maxit, lambda = input$lambda, progress = function(v) shiny::setProgress(v))
        })
        return(r)
      }
      X <- dat$X()
      shiny::validate(shiny::need(ncol(X) >= 3 && nrow(X) >= 5, "Selecciona al menos 3 variables y 5 individuos."))
      Q <- min(input$Q, ncol(X) - (method == "cenet"), nrow(X) - 1)
      switch(method,
        disjoint = {
          shiny::validate(shiny::need(Q <= ncol(X), "Q no puede superar el número de variables."))
          shiny::validate(shiny::need(input$Pcl < nrow(X), "P debe ser menor que el número de individuos."))
          shiny::withProgress(message = "CDBiplot (ALS: k-means + partición de variables)", value = 0,
                              cdpca(X, input$Pcl, Q, nstart = input$nstart, seed = input$seed, progress = function(v) shiny::setProgress(v)))
        },
        dpca = {
          shiny::validate(shiny::need(Q <= ncol(X), "Q no puede superar el número de variables."))
          shiny::withProgress(message = "Buscando partición disjunta", value = 0, {
            if (input$algo == "greedy")
              dpca_greedy(X, Q, nstart = input$nstart, seed = input$seed, progress = function(v) shiny::setProgress(v))
            else dpca_pso(X, Q, P = input$P, nIter = input$nIter, seed = input$seed, progress = function(v) shiny::setProgress(v))
          })
        },
        cenet = shiny::withProgress(message = "CenetSVD (POCS)", value = 0, {
          cenet_svd(X, Q, alpha = input$alpha, level_v = input$lv,
                    level_u = if (isTRUE(input$sparse_u)) input$lu else NULL, eps = input$eps,
                    progress = function(v) shiny::setProgress(v))
        }))
    }, ignoreNULL = FALSE)

    # Volteo: mismo resultado, otra orientación
    ejes_volteados <- shiny::reactive(sort(unique(c(if (isTRUE(input$voltear_h)) input$dx,
                                                    if (isTRUE(input$voltear_v)) input$dy))))
    fit <- shiny::reactive(voltear_equipo(fit0(), ejes_volteados()))

    fact <- shiny::reactive(fit()$fact)
    Xm   <- shiny::reactive(if (method == "logistic") fit()$X else dat$X())
    ext  <- shiny::reactive(method == "logistic" && identical(fit()$type, "external"))
    clus <- shiny::reactive({
      if (method == "disjoint") fit()$u
      else if (ext() && isTRUE(input$ncl >= 2)) elb_cluster(fit(), input$ncl)$cl
      else NULL
    })
    groups <- shiny::reactive({
      if (!isTRUE(input$use_group)) return(NULL)
      if (method %in% c("disjoint", "logistic") && identical(input$color_by, "cluster") && !is.null(clus())) return(clus())
      g <- dat$groups(); X <- Xm()
      if (is.null(g) || length(g) != nrow(X)) NULL
      else if (method == "logistic") factor(g[match(rownames(X), rownames(dat$Xraw()))]) else g
    })
    unit <- shiny::reactive(as.numeric(input$unit))
    dims <- shiny::reactive({
      Q <- if (method == "logistic") fit()$k else ncol(fact()$V)
      shiny::validate(shiny::need(input$dx <= Q && input$dy <= Q && input$dx != input$dy,
                                  paste0("Elige dos ejes distintos entre 1 y ", Q, " (los calculados).")))
      c(input$dx, input$dy)
    })

    titulo <- c(dpca = "DPCA (componentes disjuntos)", disjoint = "Disjoint Biplot (CDBiplot)",
                cenet = "CenetBiplot (sparse + ortogonal)", logistic = "Biplot logístico")
    the_plot <- shiny::reactive({
      p <- if (method == "logistic") {
        keep <- if (ext()) elb_select(fit(), input$alpha_p, isTRUE(input$bonf), input$r2min) else NULL
        shiny::validate(shiny::need(is.null(keep) || any(keep), "Ninguna variable cumple el filtro (p-valor / R²). Relaja los criterios."))
        ev <- if (ext()) 100 * fit()$eig / sum(fit()$eig) else NULL
        plot_logistic(fit(), dims(), groups(), prob = input$prob, show_rows = input$show_rows,
                      row_labels = input$row_labels, point_size = input$psize, label_size = input$lsize,
                      full_lines = input$full_lines,
                      title = if (ext()) "Biplot logístico externo (PCoA + RL)" else "Biplot logístico interno",
                      keep = keep, clusters = if (ext() && isTRUE(input$ncl >= 2)) clus() else NULL,
                      cl_style = if (is.null(input$cl_style)) "none" else input$cl_style,
                      xlab = if (ext()) sprintf("Eje %d (%.1f%%)", dims()[1], ev[dims()[1]]),
                      ylab = if (ext()) sprintf("Eje %d (%.1f%%)", dims()[2], ev[dims()[2]]))
      } else {
        plot_biplot(Xm(), fact(), input$type, dims(), groups(), input$show_rows, input$row_labels,
                    input$col_labels, input$fit_arrows, input$psize, input$lsize, title = titulo[[method]],
                    centroids = if (method == "disjoint") fit()$centroids else NULL)
      }
      encuadre_ggplot(p)
    })
    output$biplot <- render_biplot(the_plot, session)
    output$dl_png <- descarga_biplot(the_plot, paste0(method, "_biplot.png"), "png", dpi = 300, bg = "white")
    output$dl_pdf <- descarga_biplot(the_plot, paste0(method, "_biplot.pdf"), "pdf")

    dt_table <- function(df, zero_cols = NULL, page = 10) {
      d <- DT::datatable(df, rownames = FALSE, class = "compact stripe hover",
                         options = list(scrollX = TRUE, pageLength = page, dom = "frtip"))
      if (!is.null(zero_cols) && length(zero_cols))
        d <- DT::formatStyle(d, zero_cols, color = DT::styleEqual(0, "#c0c7cc"))
      d
    }

    # --- Tablas ---------------------------------------------------------------
    tab_eig <- shiny::reactive(if (method == "logistic") {
      shiny::validate(shiny::need(ext(), "La tabla de autovalores (PCoA) solo existe en el método externo."))
      elb_eigen_table(fit())
    } else eigen_table(Xm(), fact()))
    tab_rows <- shiny::reactive(if (method == "logistic") { if (ext()) elb_rows_table(fit()) else logistic_rows_table(fit()) }
                                else rows_table(Xm(), fact(), input$type, unit = unit()))
    tab_cols <- shiny::reactive(if (method == "logistic") logistic_cols_table(fit())
                                else cols_table(Xm(), fact(), input$type, unit = unit()))
    output$eig <- DT::renderDT({
      d <- tab_eig(); d[["Valor propio"]] <- round(d[["Valor propio"]], 4); d[3:4] <- round(d[3:4], 2)
      dt_table(d, page = 20)
    })
    output$scree <- shiny::renderPlot(plot_scree(tab_eig()), res = 110)
    zc <- shiny::reactive(if (method %in% c("dpca", "disjoint", "cenet")) grep("^Carga", names(tab_cols()), value = TRUE) else NULL)
    output$rows <- DT::renderDT(dt_table(tab_rows()))
    output$cols <- DT::renderDT(dt_table(tab_cols(), zero_cols = zc()))

    # --- DPCA / Disjoint --------------------------------------------------------
    if (method %in% c("dpca", "disjoint")) {
      output$part <- DT::renderDT(dt_table(partition_table(Xm(), fit()), page = 25))
      output$fcmp <- shiny::renderPrint({
        r <- fit()
        if (method == "disjoint") {
          cat(sprintf("CDBiplot (P = %d clusters, Q = %d componentes)\nError relativo F = ||X - U Ybar B'||^2/||X||^2 : %.4f\nDevianza entre clases explicada      : %.2f %%\nError de PCA sin restricciones (cota) : %.4f\n\nCorrelación entre las puntuaciones de los ejes (no son ortogonales, Nieto-Librero et al. 2017):\n",
                      r$P, r$Q, r$F, r$between, r$F_pca))
          print(round(stats::cor(r$fact$S), 3)); return(invisible())
        }
        cat(sprintf("Algoritmo: %s\nError relativo DPCA : %.4f  (varianza explicada %.2f%%)\nError relativo PCA  : %.4f  (cota inferior, sin restricción)\nPérdida por imponer la estructura disjunta: %.2f puntos porcentuales\n",
                    r$algo, r$F, 100 * (1 - r$F), r$F_pca, 100 * (r$F - r$F_pca)))
      })
      output$conv <- shiny::renderPlot(plot_convergence(fit()$hist, "Error relativo F",
        if (fit()$algo == "pso") "Mejor F global por iteración (PSO)" else if (fit()$algo == "cdpca") "F por iteración ALS (mejor arranque)" else "F por barrido (mejor arranque)"), res = 110)
      if (method == "disjoint") {
        output$cltab <- DT::renderDT(dt_table(cluster_table(Xm(), fit()), page = 12))
        output$clmem <- DT::renderDT(dt_table(data.frame(Individuo = rownames(Xm()), Cluster = as.character(fit()$u)), page = 10))
      }
      output$starts <- DT::renderDT({
        Fs <- fit()$Fstarts
        dt_table(data.frame(Arranque = seq_along(Fs), F = round(Fs, 5), Mejor = Fs == min(Fs)), page = 6)
      })
    }

    # --- Cenet ----------------------------------------------------------------
    if (method == "cenet") {
      output$spars <- DT::renderDT(dt_table(sparsity_table(fit()), page = 20))
      output$conv <- shiny::renderPlot({
        h <- fit()$conv
        d <- do.call(rbind, lapply(seq_along(h), function(q) data.frame(it = seq_along(h[[q]]) - 1, d = h[[q]], Eje = paste("Eje", q))))
        ggplot2::ggplot(d, ggplot2::aes(.data$it, .data$d, colour = .data$Eje)) + ggplot2::geom_line(linewidth = 0.9) +
          ggplot2::scale_colour_manual(values = pal_groups) +
          ggplot2::labs(x = "Iteración POCS", y = "d = u'Xv", title = "Convergencia del algoritmo POCS") + theme_biplot()
      }, res = 110)
      bic <- shiny::reactiveVal(NULL); cvres <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$run_bic, {
        X <- dat$X(); Q <- min(input$Q, ncol(X) - 1)
        shiny::withProgress(message = "BIC(τ)", value = 0, bic(cenet_bic(X, Q, input$alpha, progress = function(v) shiny::setProgress(v))))
      })
      shiny::observeEvent(input$run_cv, {
        X <- dat$X(); Q <- min(input$Q, ncol(X) - 1)
        shiny::withProgress(message = "Validación cruzada de α", value = 0,
                            cvres(cenet_cv_alpha(X, Q, input$lv, folds = input$folds, progress = function(v) shiny::setProgress(v))))
      })
      shiny::observeEvent(input$apply_bic, { b <- bic(); shiny::req(b); shiny::updateSliderInput(session, "lv", value = b$nivel[which.min(b$BIC)]) })
      shiny::observeEvent(input$apply_cv, { b <- cvres(); shiny::req(b); shiny::updateSliderInput(session, "alpha", value = min(b$alpha[which.min(b$MSE)], 0.95)) })
      grafico_seleccion <- function(b, x, y, xlab, ylab) {
        ggplot2::ggplot(b, ggplot2::aes(.data[[x]], .data[[y]])) + ggplot2::geom_line(colour = "#1f6f8b") +
          ggplot2::geom_point(ggplot2::aes(colour = .data$optimo), size = 3) +
          ggplot2::scale_colour_manual(values = c("FALSE" = "#1f6f8b", "TRUE" = "#e07a1f"), guide = "none") +
          ggplot2::labs(x = xlab, y = ylab) + theme_biplot()
      }
      output$bic_plot <- shiny::renderPlot(grafico_seleccion(shiny::req(bic()), "nivel", "BIC", "Nivel de sparsity (1 = sin restricción)", "BIC(τ)"), res = 110)
      output$bic_tab <- DT::renderDT({ b <- shiny::req(bic()); b$tau <- round(b$tau, 3); b$BIC <- round(b$BIC, 4); dt_table(b, page = 8) })
      output$cv_plot <- shiny::renderPlot(grafico_seleccion(shiny::req(cvres()), "alpha", "MSE", "α", "MSE de reconstrucción (test)"), res = 110)
      output$cv_tab <- DT::renderDT({ b <- shiny::req(cvres()); b$MSE <- round(b$MSE, 5); dt_table(b, page = 8) })
    }

    # --- Logístico -------------------------------------------------------------
    if (method == "logistic") {
      lst <- shiny::reactive(if (ext()) elb_stats(fit(), input$alpha_p, isTRUE(input$bonf), input$r2min) else logistic_stats(fit()))
      output$lstats <- DT::renderDT(dt_table(lst(), page = 15))
      output$lglob <- shiny::renderPrint({
        g <- logistic_global(fit())
        if (ext()) cat(sprintf("Método externo: similaridad %s\nBondad de ajuste global (%% correcto): %.2f%%\nVariables dibujadas: %d de %d\n\n",
                               fit()$sim, g$ccr, sum(elb_select(fit(), input$alpha_p, isTRUE(input$bonf), input$r2min)), ncol(fit()$X)))
        else cat(sprintf("Correctamente clasificados: %.2f%%\nIteraciones: %d\nDevianza: %.2f\n\n", g$ccr, fit()$iterations, fit()$deviance))
        print(g$tabla)
      })
      output$conv <- shiny::renderPlot(if (ext()) plot_scree(elb_eigen_table(fit())) else plot_convergence(fit()$dev_hist, "Devianza", "Convergencia (devianza)"), res = 110)
      output$cltab <- DT::renderDT({
        shiny::validate(shiny::need(ext() && isTRUE(input$ncl >= 2), "Indica un número de clusters >= 2 (método externo)."))
        dt_table(elb_cluster(fit(), input$ncl)$tabla, page = 12)
      })
    }

    # --- Exportar todas las tablas ----------------------------------------------
    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() paste0(method, "_tablas.xlsx"),
      content = function(f) {
        L <- list(Individuos = tab_rows(), Variables = tab_cols())
        if (method != "logistic" || ext()) L <- c(list(Autovalores = tab_eig()), L)
        if (method %in% c("dpca", "disjoint")) L$Particion <- partition_table(Xm(), fit())
        if (method == "cenet") L$Sparsity <- sparsity_table(fit())
        if (method == "logistic") L$Ajuste <- lst()
        if (method == "disjoint") { L$Clusters <- cluster_table(Xm(), fit()); L$Pertenencia <- data.frame(Individuo = rownames(Xm()), Cluster = as.character(fit()$u)) }
        if (method == "logistic" && ext() && isTRUE(input$ncl >= 2)) L$Clusters <- elb_cluster(fit(), input$ncl)$tabla
        writexl::write_xlsx(L, f)
      })

    # --- Sintaxis R ---------------------------------------------------------------
    output$sintaxis <- shiny::renderText({
      d <- datos(); v <- input$vars
      lectura <- sintaxis_lectura(d$origen)
      sel_txt <- c(sprintf("variables <- %s", vector_a_r(v, 13)),
                   "M <- as.matrix(datos[stats::complete.cases(datos[variables]), variables])")
      calculo <- switch(method,
        dpca = c(if (isTRUE(input$logt)) "M <- log1p(M)",
                 sprintf("X <- prep_matrix(M, center = %s, scale = %s)", isTRUE(input$center), isTRUE(input$scale)),
                 if (identical(input$algo, "pso"))
                   sprintf("res <- dpca_pso(X, Q = %d, P = %d, nIter = %d, seed = %d)", input$Q, input$P, input$nIter, input$seed)
                 else sprintf("res <- dpca_greedy(X, Q = %d, nstart = %d, seed = %d)", input$Q, input$nstart, input$seed)),
        disjoint = c(if (isTRUE(input$logt)) "M <- log1p(M)",
                     sprintf("X <- prep_matrix(M, center = %s, scale = %s)", isTRUE(input$center), isTRUE(input$scale)),
                     sprintf("res <- cdpca(X, P = %d, Q = %d, nstart = %d, seed = %d)", input$Pcl, input$Q, input$nstart, input$seed)),
        cenet = c(if (isTRUE(input$logt)) "M <- log1p(M)",
                  sprintf("X <- prep_matrix(M, center = %s, scale = %s)", isTRUE(input$center), isTRUE(input$scale)),
                  sprintf("res <- cenet_svd(X, Q = %d, alpha = %s, level_v = %s%s, eps = %s)", input$Q, input$alpha, input$lv,
                          if (isTRUE(input$sparse_u)) paste0(", level_u = ", input$lu) else "", format(input$eps))),
        logistic = c("# Variables binarias 0/1 (la app dicotomiza las demás según la opción elegida)",
                     if (identical(input$lmode, "int"))
                       sprintf("res <- logistic_biplot(M, k = %d, maxit = %d, lambda = %s)", input$Q, input$maxit, input$lambda)
                     else sprintf("res <- elb_fit(M, k = %d, sim = \"%s\", sqrt_d = %s)", input$Q, input$sim, isTRUE(input$sqrt_d))))
      grafico <- if (is_fact)
        sprintf("plot_biplot(X, res$fact, type = \"%s\", dims = c(%d, %d))", input$type, input$dx, input$dy)
      else sprintf("plot_logistic(res, dims = c(%d, %d), prob = %s)", input$dx, input$dy, input$prob)
      paste(c(sprintf("# %s generado con BiplotLab", titulo[[method]]), sintaxis_paquete(), "",
              lectura, "", sel_txt, calculo, "", if (is_fact) "eigen_table(X, res$fact)", grafico),
            collapse = "\n")
    })
  })
}
