# =============================================================================
# Idioma de los resultados (español / inglés)
#
# La Dra. pidió poder llevar los resultados a artículos y a otras aplicaciones
# en inglés o en castellano. El selector «ES | EN» de la barra superior cambia
# los nombres de las tablas, los títulos y ejes de los gráficos, las hojas de
# Excel y el separador decimal (coma en español, punto en inglés).
#
# Los cálculos no cambian: solo se traducen las etiquetas al mostrarlas, en
# tabla_resultados(), render_biplot(), descarga_biplot() y guardar_xlsx().
# Los nombres de individuos y variables (los datos del usuario) no se tocan.
# =============================================================================

# Etiquetas completas (la tabla o el gráfico usan exactamente este texto)
IDIOMA_EN <- c(
  # Inercia y ejes
  "Eje" = "Axis", "Plano" = "Plane", "Valor propio" = "Eigenvalue",
  "Valor singular" = "Singular value", "Inercia" = "Inertia",
  "% Inercia" = "% Inertia", "% Acumulado" = "Cumulative %",
  "% de inercia" = "% of inertia", "% acumulado" = "Cumulative %",
  "Varianza explicada (%)" = "Explained variance (%)", "Acumulada (%)" = "Cumulative (%)",
  "Sedimentación" = "Scree plot",
  # Filas y columnas
  "Individuo" = "Individual", "Individuos" = "Individuals", "Variable" = "Variable",
  "Variables" = "Variables", "Observación" = "Observation", "Observaciones" = "Observations",
  "Genotipo" = "Genotype", "Genotipos" = "Genotypes", "Ambiente" = "Environment",
  "Ambientes" = "Environments", "Grupo" = "Group", "Grupos" = "Groups",
  # Hojas de Excel
  "Coordenadas" = "Coordinates", "Coordenadas individuos" = "Individual coordinates",
  "Coordenadas variables" = "Variable coordinates",
  "Coordenadas de las variables" = "Variable coordinates",
  "CRFE individuos" = "CRFE individuals", "CRFE variables" = "CRFE variables",
  "CREF individuos" = "CREF individuals", "CREF variables" = "CREF variables",
  "Correlaciones" = "Correlations", "Intervalos" = "Intervals", "Replicas" = "Replicates",
  "Centroides" = "Centroids", "Asignacion" = "Assignment", "Recodificacion" = "Recoding",
  "Perfiles" = "Profiles",
  # Clúster
  "Tamaño" = "Size", "Silueta" = "Silhouette", "Silueta media" = "Mean silhouette",
  "Miembros" = "Members", "SC intra" = "Within SS", "Centroide" = "Centroid",
  "Número" = "Number", "Numero de grupos" = "Number of clusters",
  # Bootstrap
  "Estadístico" = "Statistic", "Media bootstrap" = "Bootstrap mean",
  "Error estándar" = "Standard error", "LI percentil" = "Percentile LL",
  "LS percentil" = "Percentile UL", "LI normal" = "Normal LL", "LS normal" = "Normal UL",
  "Desviación estándar" = "Standard deviation",
  # GGE
  "Posición" = "Position", "Encima del promedio" = "Above average",
  "Debajo del promedio" = "Below average", "Producto escalar" = "Scalar product",
  "Valor en la tabla" = "Value in the table", "Media en la tabla" = "Mean in the table",
  "Desv. estándar en la tabla" = "SD in the table",
  "Media estimada (producto escalar)" = "Estimated mean (scalar product)",
  "Diferencia (producto escalar)" = "Difference (scalar product)",
  "Diferencia en la tabla" = "Difference in the table",
  "Ganador según el biplot" = "Winner according to the biplot",
  "Ganador en la tabla" = "Winner in the table", "Mejor según el biplot" = "Best according to the biplot",
  "Longitud del vector" = "Vector length",
  "Longitud del vector (discriminación)" = "Vector length (discrimination)",
  "Coseno con el AEA (representatividad)" = "Cosine with the AEA (representativeness)",
  "Ángulo con el AEA (°)" = "Angle with the AEA (°)",
  "Proyección sobre el AEA (media)" = "Projection on the AEA (mean)",
  "Distancia al AEA (inestabilidad)" = "Distance to the AEA (instability)",
  "Distancia al ideal" = "Distance to the ideal", "Dibujada" = "Drawn",
  # Neutrosófico y equipo
  "Media de I" = "Mean of I", "Calidad (%)" = "Quality (%)", "% ceros" = "% zeros",
  "Coef. no nulos" = "Non-zero coef.", "Coef. nulos" = "Zero coef.",
  "Devianza intra" = "Within deviance", "Separación" = "Separation",
  "Intercepto b0" = "Intercept b0", "% Correcto" = "% Correct",
  "Sensibilidad (%)" = "Sensitivity (%)", "Especificidad (%)" = "Specificity (%)",
  "p-valor" = "p-value", "Norma v" = "Norm v", "Cluster" = "Cluster",
  "Silueta media (más alto = mejor)" = "Mean silhouette (higher = better)",
  "Suma de cuadrados intra (buscar el «codo»)" = "Within sum of squares (look for the elbow)",
  "Número de grupos" = "Number of clusters", "Elemento" = "Element",
  # Hojas de Excel de los demás métodos
  "Descriptivos" = "Descriptives", "Valores propios" = "Eigenvalues", "Autovalores" = "Eigenvalues",
  "Medias" = "Means", "Correlaciones totales" = "Total correlations", "Regiones" = "Regions",
  "Pares Hotelling" = "Hotelling pairs", "Imputacion" = "Imputation", "Particion" = "Partition",
  "Ajuste" = "Fit", "Pertenencia" = "Membership", "Clusters" = "Clusters", "Sparsity" = "Sparsity",
  # Canónico / MANOVA: columnas
  "Media" = "Mean", "Contraste" = "Test", "Valor" = "Value", "gl num." = "num. df",
  "gl den." = "den. df", "gl" = "df", "SC entre" = "SS between", "SC dentro" = "SS within",
  "Valor singular (escala E)" = "Singular value (E scale)", "Correlación canónica" = "Canonical correlation",
  "Wilks (aprox. χ² de Bartlett)" = "Wilks (Bartlett χ² approx.)",
  "Centro X" = "Center X", "Centro Y" = "Center Y", "Semieje mayor" = "Major semi-axis",
  "Semieje menor" = "Minor semi-axis", "Orientación eje mayor (°)" = "Major axis orientation (°)",
  "Ángulo (°)" = "Angle (°)", "Área" = "Area", "Radio" = "Radius",
  "Definida positiva" = "Positive definite", "sí" = "yes", "no" = "no",
  "Grupo 1" = "Group 1", "Grupo 2" = "Group 2", "D² Mahalanobis" = "Mahalanobis D²",
  "p ajustado" = "adjusted p", "Fila" = "Row", "Valor imputado" = "Imputed value",
  "Parámetro" = "Parameter", "Estimación" = "Estimate", "EE" = "SE", "Sesgo" = "Bias",
  "Inferior" = "Lower", "Superior" = "Upper", "Inercia (%)" = "Inertia (%)",
  "Media de grupo" = "Group mean", "Flecha (variable)" = "Arrow (variable)",
  "Calidad de variable (%)" = "Variable quality (%)", "r variable-eje" = "r variable-axis",
  "Var X" = "Var X", "Var Y" = "Var Y", "Cov" = "Cov",
  # Canónico / MANOVA: títulos de las tablas
  "Contrastes multivariantes (MANOVA)" = "Multivariate tests (MANOVA)",
  "Valores propios, correlaciones canónicas e inercia" = "Eigenvalues, canonical correlations and inertia",
  "Contrastes multivariantes por efecto (MANOVA de dos factores)" = "Multivariate tests by effect (two-way MANOVA)",
  "Lambda de Wilks con la aproximación χ² de Bartlett" = "Wilks' lambda with Bartlett's χ² approximation",
  "ANOVA de dos factores por variable" = "Two-way ANOVA by variable",
  "ANOVA de una vía por variable" = "One-way ANOVA by variable",
  "Descriptivos por celda" = "Descriptives by cell", "Descriptivos por grupo" = "Descriptives by group",
  "Valores imputados (SVD iterativa)" = "Imputed values (iterative SVD)",
  "Medias de grupo: coordenadas y calidad de representación" = "Group means: coordinates and quality of representation",
  "Comparaciones por pares: T² de Hotelling" = "Pairwise comparisons: Hotelling's T²",
  "Variables: coordenadas, calidad y correlación con los ejes" = "Variables: coordinates, quality and correlation with the axes",
  "Correlaciones totales entre las variables y los ejes" = "Total correlations between the variables and the axes",
  # Métodos del equipo: columnas y títulos de los gráficos
  "Componente" = "Component", "Iteraciones" = "Iterations",
  "Biplot canónico" = "Canonical biplot", "DPCA (componentes disjuntos)" = "DPCA (disjoint components)",
  "Disjoint Biplot (CDBiplot)" = "Disjoint Biplot (CDBiplot)",
  "CenetBiplot (sparse + ortogonal)" = "CenetBiplot (sparse + orthogonal)",
  "Biplot logístico externo (PCoA + RL)" = "External logistic biplot (PCoA + LR)",
  "Biplot logístico interno" = "Internal logistic biplot",
  "Círculo = punto con P(x=1)=0.5; flecha = dirección hasta P(x=1)=0.75" =
    "Circle = point with P(x=1)=0.5; arrow = direction to P(x=1)=0.75",
  "Error relativo F" = "Relative error F", "Convergencia" = "Convergence",
  "% de varianza" = "% of variance", "Varianza explicada" = "Explained variance",
  "Iteración" = "Iteration", "Iteración POCS" = "POCS iteration",
  "Convergencia del algoritmo POCS" = "Convergence of the POCS algorithm",
  "Devianza" = "Deviance", "Convergencia (devianza)" = "Convergence (deviance)",
  "Nivel de sparsity (1 = sin restricción)" = "Sparsity level (1 = unconstrained)",
  "MSE de reconstrucción (test)" = "Reconstruction MSE (test)",
  "Mejor F global por iteración (PSO)" = "Best global F per iteration (PSO)",
  "F por iteración ALS (mejor arranque)" = "F per ALS iteration (best start)",
  "F por barrido (mejor arranque)" = "F per sweep (best start)"
)

# Partes de una etiqueta: solo patrones seguros (ejes numerados, porcentajes,
# títulos de los gráficos); una frase libre se deja entera en español antes
# que traducirla a medias
IDIOMA_EN_PARTES <- c(
  "^Eje\\b" = "Axis", "^Eje(\\d)" = "Axis \\1","\\beje (\\d)" = "axis \\1", "\\bejes (\\d)" = "axes \\1",
  ", Eje (\\d)" = ", Axis \\1", "^Plano\\b" = "Plane", "\\bplano (\\d)" = "plane \\1",
  "\\bplano:" = "plane:", "^Valor propio\\b" = "Eigenvalue", "^% Inercia\\b" = "% Inertia",
  "^Calidad\\b" = "Quality", "^Coordenada\\b" = "Coordinate", "^Carga\\b" = "Loading",
  "^Ángulo con eje\\b" = "Angle with axis", "^Ángulo\\b" = "Angle",
  "^Longitud\\b" = "Length", "^Centroide\\b" = "Centroid", "^Cluster\\b" = "Cluster",
  "^Grupo (\\d)" = "Group \\1", "^r dentro," = "r within,", "^r total," = "r total,",
  "^Regiones de confianza al ([0-9.,]+) %" = "\\1 % confidence regions",
  "^Intervalos bootstrap al ([0-9.,]+) %" = "\\1 % bootstrap intervals",
  "réplicas\\)" = "replicates)", "\\(percentil," = "(percentile,",
  "\\(elipses de Krzanowski, escala W\\)" = "(Krzanowski ellipses, W scale)",
  "\\(elipses de Amaro \\(2008\\) publicadas\\)" = "(published Amaro (2008) ellipses)",
  "\\(círculos chi²\\)" = "(chi² circles)", "\\(círculos t\\)" = "(t circles)",
  "\\(círculos Bonferroni\\)" = "(Bonferroni circles)", "\\(elipses bootstrap\\)" = "(bootstrap ellipses)",
  " - Estandarizar columnas" = " - Column standardization", " - Centrar columnas" = " - Column centering",
  " - Sin transformar" = " - No transformation", " - Doble centrado" = " - Double centering"
)

#' Idioma de los resultados en la sesión actual
#'
#' Lo fija el selector «ES | EN» de la app; fuera de Shiny es siempre "es".
#' @return "es" o "en".
#' @noRd
idioma_actual <- function() {
  s <- shiny::getDefaultReactiveDomain()
  f <- if (!is.null(s)) s$userData$idioma
  if (!is.function(f)) return("es")
  tryCatch(f(), error = function(e) shiny::isolate(f())) %||% "es"
}

#' Traducir etiquetas de resultados al inglés
#'
#' @param x vector de texto.
#' @param idioma "es" (no cambia nada) o "en".
#' @return el mismo vector, traducido cuando la etiqueta es conocida.
#' @noRd
traducir <- function(x, idioma = idioma_actual()) {
  if (!identical(idioma, "en") || !length(x) || !is.character(x)) return(x)
  vapply(x, function(s) {
    if (is.na(s)) return(s)
    if (s %in% names(IDIOMA_EN)) return(unname(IDIOMA_EN[s]))
    t <- s
    for (p in names(IDIOMA_EN_PARTES)) t <- gsub(p, IDIOMA_EN_PARTES[[p]], t, perl = TRUE)
    gsub("(\\d),(\\d)", "\\1.\\2", t)        # 28,37 % -> 28.37 %
  }, character(1), USE.NAMES = FALSE)
}

# Tabla con columnas y textos fijos traducidos (los nombres de filas no cambian)
traducir_tabla <- function(df, idioma = idioma_actual()) {
  if (!identical(idioma, "en") || !is.data.frame(df)) return(df)
  names(df) <- traducir(names(df), idioma)
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.factor(v)) levels(v) <- traducir_celdas(levels(v)) else if (is.character(v)) v <- traducir_celdas(v)
    df[[j]] <- v
  }
  df
}

# Textos que el aplicativo escribe dentro de las celdas. Una palabra como
# «Media» o «Grupo 1» puede ser también un valor de los datos, por eso no se
# traduce en las celdas.
IDIOMA_CELDAS <- c("Eje", "Encima del promedio", "Debajo del promedio", "sí",
                   "Wilks (aprox. χ² de Bartlett)", "Inercia (%)", "Valor propio",
                   "Correlación canónica", "Media de grupo", "Flecha (variable)",
                   "Calidad de variable (%)", "r variable-eje")

# En las celdas solo se traducen los textos fijos del aplicativo («Eje 1»,
# «Encima del promedio»...), nunca los nombres que vienen de los datos
traducir_celdas <- function(v) {
  fijo <- !is.na(v) & (v %in% IDIOMA_CELDAS | grepl("^(Eje|Plano) \\d", v))
  v[fijo] <- traducir(v[fijo], "en")
  v
}

# Gráfico ggplot con título, ejes y leyendas traducidos
traducir_grafico <- function(g, idioma = idioma_actual()) {
  if (!identical(idioma, "en") || !inherits(g, "ggplot")) return(g)
  textos <- Filter(is.character, as.list(g$labels))
  if (!length(textos)) return(g)
  g + do.call(ggplot2::labs, lapply(textos, traducir, idioma = idioma))
}

# Excel con hojas y columnas en el idioma elegido
guardar_xlsx <- function(hojas, archivo, idioma = idioma_actual()) {
  if (identical(idioma, "en")) {
    hojas <- lapply(hojas, traducir_tabla, idioma = idioma)
    names(hojas) <- traducir(names(hojas), idioma)
  }
  writexl::write_xlsx(hojas, archivo)
}

# Separador decimal de las tablas en pantalla
marca_decimal <- function(idioma = idioma_actual()) if (identical(idioma, "en")) "." else ","

# Selector de la barra superior
selector_idioma <- function(id = "idioma") {
  shiny::div(class = "bl-idioma",
             shiny::radioButtons(id, NULL, choices = c("ES" = "es", "EN" = "en"),
                                 selected = "es", inline = TRUE))
}

# Tablas de renderTable: números ya escritos con el separador del idioma
# (format.args de renderTable se fija una sola vez y no sigue al selector)
con_decimales <- function(df, digitos = 3, idioma = idioma_actual()) {
  for (j in seq_along(df)) if (is.double(df[[j]])) {
    df[[j]] <- formatC(df[[j]], format = "f", digits = digitos, decimal.mark = marca_decimal(idioma))
  }
  df
}

# Tabla de autovalores con «Eje 1, Eje 2...» traducidos en la columna Eje (las
# etiquetas del eje X del gráfico de sedimentación del equipo salen de ahí)
con_ejes_traducidos <- function(tab, idioma = idioma_actual()) {
  if ("Eje" %in% names(tab)) tab$Eje <- traducir(as.character(tab$Eje), idioma)
  tab
}
