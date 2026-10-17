# =============================================================================
# Pestaña "Teoría" de cada biplot.
# Idea del grupo: una hoja breve en cada método que diga para qué sirve, qué
# datos espera (numéricos, binarios, categóricos), qué datos no usar, qué
# calcula (fórmulas), cómo leer el gráfico (flechas, distancias, ángulos),
# dónde se hace el cálculo en el aplicativo y las referencias.
# Todas las hojas tienen las mismas secciones y empiezan con una tabla
# resumen (para qué sirve, datos de entrada, no usar con, cómo se lee).
# Referencias: las del material del Módulo 3 (`modulo`) y la obra original del
# método (`original`); en la pantalla salen juntas, en orden alfabético. Todas
# se comprobaron una por una (revista, volumen, páginas y DOI).
# Las fórmulas se escriben en LaTeX entre \( \) o \[ \] y se dibujan con KaTeX (incluido en inst/app/www,
# sin conexión a internet), como en la app del compañero de DPCA y Cenet, de
# donde vienen también las fórmulas de esos cuatro métodos.
# =============================================================================

#' Contenido de la pestaña Teoría de un método
#' @param metodo id del análisis.
#' @return lista con titulo, subtitulo, para_que, entrada, no_usar, calculos,
#'   lectura, donde (texto HTML con fórmulas en LaTeX) y referencias
#'   (lista con `modulo`, las del material del módulo, y `original`).
#' @export
teoria_metodo <- function(metodo) {
  T <- TEORIA[[metodo]]
  if (is.null(T)) stop("No hay teoría para el método ", metodo, call. = FALSE)
  T
}

#' Interfaz de la pestaña Teoría
#' @param metodo id del análisis.
#' @export
teoria_ui <- function(metodo) {
  t <- teoria_metodo(metodo)
  html <- function(x) shiny::HTML(x)
  lista <- function(x) shiny::tags$ul(lapply(x, function(i) shiny::tags$li(html(i))))
  seccion <- function(n, titulo, ...) shiny::tagList(shiny::h4(paste0(n, ". ", titulo)), ...)
  fila <- function(aspecto, texto) shiny::tags$tr(shiny::tags$th(aspecto), shiny::tags$td(html(texto)))
  refs <- sort(unique(unlist(t$referencias, use.names = FALSE)))
  shiny::div(
    class = "bl-teoria",
    shiny::h3(t$titulo), shiny::p(class = "bl-teoria-sub", t$subtitulo),
    shiny::tags$table(class = "bl-resumen",
                      shiny::tags$tbody(
                        fila("Para qué sirve", t$resumen$para_que),
                        fila("Datos de entrada", t$resumen$entrada),
                        fila("No usar con", t$resumen$no_usar),
                        fila("Cómo se lee", t$resumen$lectura))),
    seccion(1, "Para qué sirve", lapply(t$para_que, function(x) shiny::p(html(x)))),
    seccion(2, "Qué datos necesita", lista(t$entrada)),
    seccion(3, "Qué datos no debe usar", lista(t$no_usar)),
    seccion(4, "Qué calcula", lapply(t$calculos, function(x) shiny::div(class = "bl-teoria-p", html(x)))),
    seccion(5, "Cómo leer el gráfico", lista(t$lectura)),
    seccion(6, "Dónde se calcula en el aplicativo", lista(t$donde)),
    shiny::div(class = "bl-teoria-refs", shiny::tags$b("Referencias"),
               lapply(refs, function(r) shiny::p(class = "bl-ref", html(r))))
  )
}

TEORIA <- list(

  # ---------------------------------------------------------------- HJ ------
  hj = list(
    titulo = "HJ-Biplot",
    subtitulo = "Representación simultánea de individuos y variables con la misma calidad (Galindo, 1986)",
    resumen = list(
      para_que = "Ver en un solo plano cómo se parecen los individuos, cómo se relacionan las variables y qué variables caracterizan a cada individuo o grupo.",
      entrada = "Matriz <b>individuos × variables numéricas</b> (de escala). Las columnas de grupo solo colorean.",
      no_usar = "Variables categóricas o códigos de grupo como si fueran números; matrices con datos faltantes; menos de 3 individuos.",
      lectura = "Ángulo entre flechas ≈ correlación; proyección de un punto sobre una flecha ≈ su valor; distancia entre puntos ≈ semejanza. Solo lo bien representado (CRFE alta)."),
    para_que = c(
      "Es un análisis exploratorio que resume una matriz de datos en pocos ejes (normalmente un plano) y dibuja a la vez a los <b>individuos</b> (puntos) y a las <b>variables</b> (flechas). A diferencia de los biplots GH y JK de Gabriel (1971), el HJ-Biplot (Galindo, 1986) da la <b>misma calidad de representación</b> a filas y columnas, por eso ambos se pueden interpretar en el mismo gráfico.",
      "Se usa para describir patrones: qué individuos son parecidos, qué variables van juntas o se oponen, y qué variables explican la posición de cada individuo o grupo."),
    entrada = c(
      "Una fila por <b>individuo</b> (país, muestra, persona…) y una columna por <b>variable cuantitativa</b> (de escala). La primera columna del archivo da el nombre de cada individuo.",
      "Variables numéricas continuas o discretas con suficiente variación. Si están en unidades o escalas distintas, se <b>estandarizan</b> por columnas (opción por defecto); si están en la misma unidad, se puede solo centrar.",
      "Columnas de agrupación (región, semana, tratamiento…) marcadas como <b>nominales</b> en el panel Datos: sirven para colorear, nunca construyen los ejes.",
      "Tamaño: al menos 3 individuos y 2 variables. El cálculo es rápido incluso con decenas de miles de filas; el gráfico se vuelve ilegible con miles de etiquetas."),
    no_usar = c(
      "Códigos numéricos de grupo (1, 2, 3…) como variables: cambian los ejes y la inercia (es el error 26,40 % + 17,14 % que señaló la Dra.).",
      "Variables binarias (0/1) o categóricas: para ellas está el <b>Biplot logístico</b>.",
      "Datos faltantes: el HJ no los imputa; hay que resolverlos antes (o excluir esos individuos).",
      "Variables constantes (sin variación) o casi idénticas a otra: no aportan y pueden dar ejes degenerados.",
      "Atípicos muy extremos sin revisar: pueden \"robarse\" un eje (se ven con la contribución CREF y se pueden excluir)."),
    calculos = c(
      "<b>1. Preparación.</b> Sea \\(X\\) la matriz \\(n\\times p\\). Por defecto se estandariza cada columna: \\(z_{ij}=(x_{ij}-\\bar x_j)/s_j\\), con \\(s_j\\) la desviación típica con \\(n-1\\).",
      "<b>2. Descomposición en valores singulares (SVD).</b> \\[ Z = U D V^{\\top},\\qquad U^{\\top}U=V^{\\top}V=I,\\quad D=\\operatorname{diag}(d_1\\ge d_2\\ge\\dots\\ge 0). \\] Por el teorema de Eckart y Young, quedarse con los \\(q\\) primeros ejes da la mejor aproximación de rango \\(q\\) de \\(Z\\).",
      "<b>3. Marcadores HJ.</b> \\[ J = U D\\ \\ (\\text{individuos}),\\qquad H = V D\\ \\ (\\text{variables}). \\] (GH: \\(G=U,\\ H=VD\\); JK: \\(J=UD,\\ K=V\\).)",
      "<b>4. Inercia.</b> Valor propio \\(\\lambda_k=d_k^2\\); % de inercia del eje \\(k\\): \\(100\\,d_k^2/\\sum_s d_s^2\\).",
      "<b>5. Calidad de representación (CRFE)</b>, contribución relativa del factor al elemento, en ‰: \\[ \\mathrm{CRFE}_{ik}=1000\\,\\frac{j_{ik}^2}{\\sum_s j_{is}^2}=1000\\cos^2\\theta_{ik}. \\] La calidad en el plano es la suma de las CRFE de sus dos ejes.",
      "<b>6. Contribución (CREF)</b>, del elemento al factor, en ‰: \\(\\mathrm{CREF}_{ik}=1000\\,u_{ik}^2\\) (individuos) y \\(1000\\,v_{jk}^2\\) (variables); cada eje suma 1000."),
    lectura = c(
      "<b>Primero la inercia</b>: el % de cada eje dice cuánta información recoge el plano.",
      "<b>Solo se interpreta lo bien representado</b>: CRFE alta en el plano (por ejemplo ≥ 500 ‰). Lo gris o punteado no se interpreta.",
      "<b>Longitud de la flecha</b>: con datos estandarizados, cuanto más larga, mejor representada está la variable (y más variabilidad recoge el plano).",
      "<b>Ángulo entre dos flechas</b>: su coseno aproxima la correlación. Agudo = correlación positiva; recto = sin relación; obtuso = negativa (solo entre variables bien representadas).",
      "<b>Ángulo entre una flecha y un eje</b>: cuanto menor, más se relaciona la variable con ese eje.",
      "<b>Proyección de un punto sobre una flecha</b>: ordena a los individuos según esa variable; cuanto más avanza en el sentido de la flecha, mayor su valor. El origen es la media.",
      "<b>Distancia entre dos puntos</b>: individuos cercanos tienen perfiles parecidos.",
      "<b>El signo de cada eje es arbitrario</b>: voltear el gráfico no cambia ninguna conclusión."),
    donde = c(
      "<code>R/calculo_hj_biplot.R</code>: <code>transformar_matriz()</code> (preparación) y <code>hj_biplot()</code> (SVD con <code>svd()</code>, marcadores, CRFE, CREF e inercia).",
      "<code>R/grafico_biplot.R</code>: <code>grafico_biplot()</code> (dibujo, calidad mínima, colores) y el encuadre rectangular (<code>limites_encuadre()</code>, <code>encuadre_biplot()</code>).",
      "<code>R/mod_hj_biplot.R</code>: la pantalla. Publica el resultado para Bootstrap y Clúster.",
      "Validado con MULTBIPLOT (PISA), biplotbootGUI (cantones) y el material del fitoplancton (pruebas en <code>tests/testthat</code>)."),
    referencias = list(
      modulo = c(
        "Galindo, M. P. (1986). Una alternativa de representación simultánea: HJ-Biplot. <i>Qüestiió, 10</i>(1), 13-23. http://hdl.handle.net/2099/4523",
        "Gabriel, K. R. (1971). The biplot graphic display of matrices with application to principal component analysis. <i>Biometrika, 58</i>(3), 453-467. https://doi.org/10.1093/biomet/58.3.453",
        "Gabriel, K. R. y Odoroff, C. L. (1990). Biplots in biomedical research. <i>Statistics in Medicine, 9</i>(5), 469-485. https://doi.org/10.1002/sim.4780090502",
        "Cascante-Yarlequé, R., Galindo-Villardón, P., Guevara-Viejó, F., Vicente-Villardón, J. L. y Vicente-Galindo, P. (2025). HJ-BIPLOT: A theoretical and empirical systematic review of its 38 years of history, using text mining and LLMs. <i>Mathematics, 13</i>(12), 1913. https://doi.org/10.3390/math13121913"),
      original = c(
        "Eckart, C. y Young, G. (1936). The approximation of one matrix by another of lower rank. <i>Psychometrika, 1</i>(3), 211-218. https://doi.org/10.1007/BF02288367"))),

  # --------------------------------------------------------- Bootstrap ------
  bootstrap = list(
    titulo = "Bootstrap del HJ-Biplot",
    subtitulo = "Intervalos de confianza para lo que muestra el biplot (Nieto et al., 2014)",
    resumen = list(
      para_que = "Saber cuánto se puede confiar en el biplot: si la inercia, las calidades y la posición de las variables son estables o dependen de la muestra.",
      entrada = "La misma matriz y configuración del HJ-Biplot (se toman de ese módulo). Individuos que sean una muestra de una población.",
      no_usar = "Muestras muy pequeñas (menos de unos 20 individuos); datos con dependencia (series de tiempo) sin adaptar el remuestreo.",
      lectura = "Elipse pequeña = variable estable; elipse grande o que contiene al origen = posición poco fiable. Intervalo estrecho = estimación precisa."),
    para_que = c(
      "El biplot es una estimación hecha con una muestra. El bootstrap (Efron y Tibshirani, 1993) repite el análisis con muchas muestras obtenidas de la original y mide cuánto varían los resultados. Así se obtienen error estándar, sesgo e intervalos de confianza para la inercia, las calidades (CRFE), las contribuciones (CREF), las coordenadas, longitudes y ángulos de las variables, como en la metodología de Nieto et al. (2014) y el paquete biplotbootGUI."),
    entrada = c(
      "La matriz del HJ-Biplot, con las mismas variables, transformación e individuos excluidos (el módulo los lee del HJ).",
      "Individuos que representen a una población: son ellos los que se remuestrean.",
      "Número de réplicas \\(B\\) (500 por defecto; el artículo usa 1000), nivel de confianza y semilla para que el resultado sea reproducible."),
    no_usar = c(
      "Muestras muy pequeñas: el remuestreo no puede imitar a la población.",
      "Datos con dependencia entre filas (tiempo, espacio) con el remuestreo simple de individuos.",
      "Interpretar los intervalos de un eje cuando dos ejes tienen inercia casi igual: pueden intercambiarse entre réplicas; la CRFE del plano no tiene ese problema."),
    calculos = c(
      "<b>1. Remuestreo.</b> Se sortean \\(n\\) individuos con reemplazo y se forma \\(X^{*b}\\), para \\(b=1,\\dots,B\\).",
      "<b>2. Mismo HJ-Biplot</b> en cada réplica: \\(Z^{*b}=U^{*b}D^{*b}V^{*b\\top}\\).",
      "<b>3. Alineación de Procrustes.</b> La SVD puede cambiar el signo o el orden de ejes parecidos. Cada réplica se rota hacia la original: con \\(H_0\\) y \\(H_b\\) los marcadores de las variables, \\[ H_b^{\\top}H_0=P\\Sigma Q^{\\top},\\qquad R=PQ^{\\top},\\qquad H_b\\leftarrow H_bR \\]. En el biplot JK se alinea con \\(V\\).",
      "<b>4. Estadísticos.</b> Para cada estadístico \\(\\hat\\theta\\): error estándar \\(\\mathrm{EE}=\\operatorname{sd}(\\hat\\theta^{*b})\\), sesgo \\(\\bar{\\hat\\theta}^{*}-\\hat\\theta\\), intervalo percentil \\([\\hat\\theta^{*}_{(\\alpha/2)},\\hat\\theta^{*}_{(1-\\alpha/2)}]\\) e intervalo normal \\(\\hat\\theta\\pm z_{1-\\alpha/2}\\,\\mathrm{EE}\\).",
      "<b>5. Regiones de confianza.</b> Para cada variable, una elipse que contiene el \\(100(1-\\alpha)\\%\\) de las posiciones alineadas de su flecha."),
    lectura = c(
      "<b>Elipse pequeña</b>: la posición de la variable casi no cambia con la muestra; es estable.",
      "<b>Elipse grande, o que contiene al origen</b>: la dirección de la variable no es fiable; no conviene interpretarla.",
      "Las variables con CRFE alta suelen tener elipses pequeñas: el bootstrap confirma lo que dice la calidad.",
      "<b>Intervalo percentil</b>: no supone normalidad; es el recomendado. El normal sirve si el histograma es simétrico y el gráfico Q-Q es una recta.",
      "El primer valor propio suele salir sesgado hacia arriba: es un efecto conocido y por eso se informa el sesgo."),
    donde = c(
      "<code>R/calculo_bootstrap.R</code>: <code>bootstrap_biplot()</code> (remuestreo, Procrustes, intervalos) y los gráficos de regiones, histograma y Q-Q.",
      "<code>R/mod_bootstrap.R</code>: la pantalla; toma la configuración publicada por el HJ-Biplot.",
      "Validado con biplotbootGUI (práctica de cantones)."),
    referencias = list(
      modulo = c(
        "Nieto, A. B., Galindo, M. P., Leiva, V. y Vicente-Galindo, P. (2014). A methodology for biplots based on bootstrapping with R. <i>Revista Colombiana de Estadística, 37</i>(2), 367-397. https://doi.org/10.15446/rce.v37n2spe.47944",
        "Galindo, M. P. (1986). Una alternativa de representación simultánea: HJ-Biplot. <i>Qüestiió, 10</i>(1), 13-23. http://hdl.handle.net/2099/4523"),
      original = c(
        "Efron, B. y Tibshirani, R. J. (1993). <i>An introduction to the bootstrap</i>. Chapman & Hall."))),

  # ------------------------------------------------------------ Clúster ------
  cluster = list(
    titulo = "Clúster sobre las coordenadas del biplot",
    subtitulo = "Agrupar individuos (o variables) con las coordenadas de los primeros ejes",
    resumen = list(
      para_que = "Formar grupos de individuos parecidos y verlos dibujados sobre el mismo biplot, con su perfil.",
      entrada = "Las coordenadas del HJ-Biplot (filas o columnas) en los ejes elegidos.",
      no_usar = "Más de unos 5000 individuos con métodos jerárquicos (la memoria crece con n²); ejes con muy poca inercia.",
      lectura = "Cada envolvente es un grupo; se desplaza hacia las variables que lo caracterizan. Altura del dendrograma = distancia de fusión."),
    para_que = c(
      "En lugar de agrupar los datos originales, se agrupan las <b>coordenadas del biplot</b> en los primeros ejes, que recogen la estructura principal y dejan fuera el ruido de los ejes residuales. Los grupos se dibujan como envolventes sobre el mismo biplot, de modo que se ve a la vez a qué grupo pertenece cada individuo y qué variables lo caracterizan. Es el procedimiento del estudio del fitoplancton del embalse de Riaño visto en clase."),
    entrada = c(
      "El resultado del HJ-Biplot (individuos o variables) y los ejes que se usan (por ejemplo 1 y 2).",
      "Método de agrupamiento (Ward por defecto) y número de grupos \\(k\\).",
      "Opcional: una variable nominal para cruzarla con los grupos (por ejemplo, profundidad)."),
    no_usar = c(
      "Bases muy grandes con métodos jerárquicos: la matriz de distancias ocupa \\(n^2\\) celdas (con 10 000 individuos, unos 4 GB). Para más de 5000 individuos, usar k-medias o una muestra.",
      "Ejes con muy poca inercia: agrupar ruido no da grupos con sentido.",
      "Decidir \\(k\\) solo por un índice: también debe tener interpretación."),
    calculos = c(
      "<b>1. Distancias</b> euclídeas entre las coordenadas \\(\\mathbf j_i\\) de los ejes elegidos.",
      "<b>2. Ward</b> (<code>ward.D2</code>): en cada paso se unen los dos grupos que menos aumentan la suma de cuadrados dentro de los grupos, \\[ \\Delta(A,B)=\\frac{n_An_B}{n_A+n_B}\\,\\|\\bar{\\mathbf j}_A-\\bar{\\mathbf j}_B\\|^2 . \\] También se ofrecen enlace completo, promedio, simple, centroide y k-medias.",
      "<b>3. Correlación cofenética</b>: correlación entre las distancias reales y las alturas a las que el dendrograma une cada par (cuanto más cerca de 1, mejor respeta el árbol las distancias).",
      "<b>4. Silueta</b> de cada elemento: \\(s_i=(b_i-a_i)/\\max(a_i,b_i)\\), con \\(a_i\\) la distancia media a su grupo y \\(b_i\\) al grupo vecino más cercano. Se calcula para \\(k=2,\\dots,10\\) junto con la suma de cuadrados intra-grupo (codo).",
      "<b>5. Perfiles</b>: medias de las variables originales en cada grupo."),
    lectura = c(
      "Cada <b>envolvente</b> encierra a los miembros de un grupo; el grupo se caracteriza por las flechas hacia las que se desplaza.",
      "En el <b>dendrograma</b>, la altura de cada unión es la distancia de Ward a la que se fusionan; un salto grande indica dónde cortar.",
      "<b>Silueta</b>: cerca de 1, bien asignado; cerca de 0, en la frontera; negativa, quizá en el grupo equivocado (más de 0,5 = estructura razonable; 0,25-0,5 = débil).",
      "La <b>tabla cruzada</b> con una variable nominal muestra si los grupos coinciden con categorías conocidas."),
    donde = c(
      "<code>R/calculo_cluster.R</code>: <code>cluster_biplot()</code> (distancias, <code>hclust()</code>, <code>kmeans()</code>, cofenética, silueta, elección de k, perfiles) y <code>dendrograma_cluster()</code>.",
      "<code>R/mod_cluster.R</code>: la pantalla; usa el resultado publicado por el HJ-Biplot.",
      "Validado con el material del fitoplancton: cofenética 0,67941 con Ward en el plano 1-2."),
    referencias = list(
      modulo = c(
        "Tejedor-Flores, N., Vicente-Galindo, P. y Galindo-Villardón, P. (2017). Multivariate analysis of the energy consumption of Ecuador using MuSIASEM and BIPLOT approach. <i>Sustainability, 9</i>(6), 984. https://doi.org/10.3390/su9060984",
        "Sabando-Álvarez, M. C., Tallana-Chimarro, D. X., Amaro, I. R. y Camacho, F. (2021). Statistical analysis of the impact of COVID-19: A study case on several countries. <i>Bulletin of Computational Applied Mathematics, 9</i>(1), 63-83.",
        "Demey, J. R., Vicente-Villardón, J. L., Galindo-Villardón, M. P. y Zambrano, A. Y. (2008). Identifying molecular markers associated with classification of genotypes by External Logistic Biplots. <i>Bioinformatics, 24</i>(24), 2832-2838. https://doi.org/10.1093/bioinformatics/btn552"),
      original = c(
        "Ward, J. H., Jr. (1963). Hierarchical grouping to optimize an objective function. <i>Journal of the American Statistical Association, 58</i>(301), 236-244. https://doi.org/10.1080/01621459.1963.10500845"))),

  # ----------------------------------------------------------- Canónico ------
  canonico = list(
    titulo = "Biplot canónico / MANOVA-Biplot",
    subtitulo = "Diferencias entre grupos con varias variables a la vez (Gabriel, 1972; Amaro et al., 2004)",
    resumen = list(
      para_que = "Ver qué grupos difieren y en qué variables, con contraste MANOVA y regiones de confianza para las medias.",
      entrada = "Variables numéricas de respuesta y uno o dos factores (columnas nominales) que definen los grupos.",
      no_usar = "Grupos con muy pocas observaciones (menos individuos que variables); un solo grupo; variables categóricas como respuesta.",
      lectura = "Distancia entre medias = separación de grupos (en unidades de Mahalanobis). Regiones que no se cruzan = grupos distintos. Flechas = variables que separan."),
    para_que = c(
      "Es la versión del biplot para comparar <b>grupos</b>. En lugar de la matriz de individuos, representa las <b>medias de los grupos</b> en los ejes que mejor las separan respecto a la variabilidad dentro de los grupos (variables canónicas). Junto con el MANOVA (Wilks, Pillai, Hotelling-Lawley, Roy) dice si los grupos difieren y, con las flechas, en qué variables."),
    entrada = c(
      "Variables de respuesta <b>numéricas</b> (de escala).",
      "Uno o dos <b>factores</b>: columnas nominales del panel Datos (por ejemplo especie, tratamiento, temperatura).",
      "Cada grupo con varias observaciones; en total más individuos que variables.",
      "Opcional: transformaciones log o composicionales (clr, alr) y bootstrap."),
    no_usar = c(
      "Grupos con una sola observación o con menos observaciones que variables: la matriz W no se puede invertir bien.",
      "Respuestas categóricas o binarias.",
      "Interpretarlo sin revisar los supuestos del MANOVA (normalidad multivariante, covarianzas parecidas entre grupos)."),
    calculos = c(
      "<b>1. Matrices de sumas de cuadrados</b> entre grupos \\(B\\) y dentro de los grupos \\(W\\).",
      "<b>2. Problema canónico</b>: \\[ W^{-1}B\\,\\mathbf a=\\theta\\,\\mathbf a , \\] los valores propios \\(\\theta_k\\) miden cuánto separa cada eje; la inercia es \\(100\\,\\theta_k/\\sum\\theta\\).",
      "<b>3. MANOVA</b>: \\(\\Lambda_{\\text{Wilks}}=|W|/|W+B|=\\prod_k 1/(1+\\theta_k)\\); Pillai \\(=\\sum\\theta_k/(1+\\theta_k)\\); Hotelling-Lawley \\(=\\sum\\theta_k\\); Roy \\(=\\theta_1\\).",
      "<b>4. Biplot</b>: medias de los grupos en los ejes canónicos (distancia de Mahalanobis) y variables como flechas (correlaciones estructurales).",
      "<b>5. Regiones de confianza</b> de cada media: elipses de Krzanowski en la escala de W (recomendado), círculos \\(\\chi^2\\), t o Bonferroni, o elipses bootstrap. Comparaciones por pares con \\(T^2\\) de Hotelling."),
    lectura = c(
      "<b>Distancia entre las medias</b>: separación entre grupos ajustada por la variabilidad interna (Mahalanobis).",
      "<b>Regiones que no se solapan</b>: los grupos difieren significativamente en el plano.",
      "<b>Proyección de una media sobre una flecha</b>: ese grupo tiene valores altos (o bajos) de la variable.",
      "<b>Flechas largas en la dirección que separa grupos</b>: variables que más discriminan.",
      "Mirar primero el MANOVA: si no es significativo, no se interpretan diferencias."),
    donde = c(
      "<code>R/calculo_canonico.R</code>: <code>bigarx_canonical()</code> (una vía), <code>bigarx_manova2()</code> (dos factores) y <code>boot_canonical()</code> (código de Roxi y Kleber, sin cambios).",
      "<code>R/grafico_canonico.R</code> y <code>R/tablas_canonico.R</code>: gráfico (con el encuadre rectangular) y tablas APA. <code>R/mod_canonico.R</code>: la pantalla.",
      "Validado con iris (Wilks igual a <code>stats::manova</code>, individuos iguales a <code>MASS::lda</code>) y el césped de Seber (dos factores)."),
    referencias = list(
      modulo = c(
        "Amaro, I. R., Vicente-Villardón, J. L. y Galindo-Villardón, M. P. (2004). MANOVA Biplot para arreglos de tratamientos con dos factores basado en modelos lineales generales multivariantes. <i>Interciencia, 29</i>(1), 26-32.",
        "Amaro, I. R., Vicente-Villardón, J. L. y Galindo-Villardón, M. P. (2008). Contribuciones al Manova-Biplot: regiones de confianza alternativas. <i>Revista Investigación Operacional, 29</i>(3), 231-241.",
        "Amaro, I. R., Ponsot Balaguer, E. A., Castillo, Z. N. y Machado, W. (2019). Aspectos teóricos del MANOVA-Biplot y su aplicación a los usos del suelo en la ESPAC-2016. <i>Revista Bases de la Ciencia, 4</i>(2), 51-72. https://doi.org/10.33936/rev_bas_de_la_ciencia.v4i2.4236"),
      original = c(
        "Gabriel, K. R. (1972). Analysis of meteorological data by means of canonical decomposition and biplots. <i>Journal of Applied Meteorology, 11</i>, 1071-1077.",
        "Krzanowski, W. J. (1989). On confidence regions in canonical variate analysis. <i>Biometrika, 76</i>(1), 107-116. https://doi.org/10.1093/biomet/76.1.107"))),

  # --------------------------------------------------------------- DPCA ------
  dpca = list(
    titulo = "DPCA",
    subtitulo = "Componentes principales disjuntas: cada variable en un solo componente (Vichi y Saporta, 2009)",
    resumen = list(
      para_que = "Obtener componentes fáciles de interpretar: las variables se reparten en grupos y cada componente usa solo las de su grupo.",
      entrada = "Matriz individuos × variables numéricas (estandarizadas si tienen escalas distintas).",
      no_usar = "Variables binarias o categóricas; pocas variables (menos de 3); querer la máxima varianza explicada (eso es el PCA/HJ).",
      lectura = "Cada flecha tiene carga en un solo eje. Las variables de un mismo eje forman un bloque; la pérdida frente al PCA dice cuánto cuesta la simplicidad."),
    para_que = c(
      "En el PCA o el HJ cada eje mezcla todas las variables, lo que a veces dificulta interpretarlo. El DPCA impone que cada variable contribuya a <b>un solo componente</b>: el resultado es una partición de las variables en \\(Q\\) bloques, cada uno con su componente. Se pierde algo de varianza explicada a cambio de componentes más claros."),
    entrada = c(
      "Matriz <b>individuos × variables numéricas</b>; se centra y, por defecto, se estandariza (opción log(x+1) para abundancias).",
      "Número de componentes \\(Q\\) (grupos de variables), con \\(Q\\) menor o igual que el número de variables.",
      "Algoritmo: <i>greedy</i> con varios arranques o enjambre de partículas (CBPSO-PCA) y una semilla."),
    no_usar = c(
      "Variables binarias o categóricas.",
      "Menos de 3 variables o \\(Q\\) igual al número de variables (cada variable sola: no hay nada que agrupar).",
      "Si lo importante es la máxima varianza explicada: el HJ-Biplot sin restricciones siempre explica más."),
    calculos = c(
      "<b>Modelo</b>: \\[ \\min_{B}\\frac{\\|X-XBB^{\\top}\\|^{2}}{\\|X\\|^{2}}\\quad\\text{s.a. una sola entrada no nula por fila de }B,\\ \\|\\mathbf b_q\\|=1 . \\] Como los soportes son disjuntos, \\(B^{\\top}B=I_Q\\).",
      "<b>Reducción a una partición</b>: con \\(G=X^{\\top}X\\), para un bloque fijo \\(\\mathcal I_q\\) la mejor \\(\\mathbf b_q\\) es el primer autovector de \\(G[\\mathcal I_q,\\mathcal I_q]\\) y \\(\\|X\\mathbf b_q\\|^2=\\lambda_{\\max}(G[\\mathcal I_q,\\mathcal I_q])\\). Se busca la partición que maximiza \\(\\sum_q\\lambda_{\\max}\\).",
      "<b>Greedy</b>: cada variable se mueve al bloque que más aumenta el criterio, hasta que no mejora; varios arranques evitan óptimos locales. <b>CBPSO-PCA</b>: enjambre de partículas binarias (Ramírez-Figueroa et al., 2021).",
      "<b>Error relativo</b> \\(F\\) del DPCA frente al del PCA sin restricción (cota inferior): la diferencia es la pérdida por imponer la estructura disjunta."),
    lectura = c(
      "Cada <b>flecha</b> tiene carga distinta de cero en un solo eje: las variables de un mismo eje forman un bloque interpretable.",
      "Los <b>puntos</b> se leen como en el HJ: proyección sobre la flecha ≈ valor de la variable.",
      "Los ejes son ortogonales en las cargas, pero las <b>puntuaciones</b> de los individuos pueden estar correlacionadas: no se interpreta el ángulo entre ejes como en el PCA.",
      "La pestaña <i>Partición y convergencia</i> muestra qué variables van a cada componente y cuánto se pierde frente al PCA.",
      "La opción «Ajustar escala de flechas» solo cambia el largo dibujado de las flechas."),
    donde = c(
      "<code>R/equipo_dpca.R</code>: <code>dpca_greedy()</code>, <code>dpca_pso()</code> y <code>partition_table()</code> (código del compañero, sin cambios).",
      "<code>R/equipo_nucleo.R</code>: preparación, marcadores HJ/JK/GH y tablas; <code>R/equipo_graficos.R</code>: biplot.",
      "<code>R/mod_equipo.R</code>: la pantalla dentro del aplicativo (variables desde el panel Datos, encuadre 4:3, voltear, Teoría y Sintaxis R)."),
    referencias = list(
      modulo = c(
        "Ramírez-Figueroa, J. A., Martín-Barreiro, C., Nieto-Librero, A. B., Leiva, V. y Galindo-Villardón, M. P. (2021). A new principal component analysis by particle swarm optimization with an environmental application for data science. <i>Stochastic Environmental Research and Risk Assessment, 35</i>(10), 1969-1984. https://doi.org/10.1007/s00477-020-01961-3"),
      original = c(
        "Vichi, M. y Saporta, G. (2009). Clustering and disjoint principal component analysis. <i>Computational Statistics & Data Analysis, 53</i>(8), 3194-3208."))),

  # ----------------------------------------------------------- Disjoint ------
  disjoint = list(
    titulo = "Disjoint Biplot (CDBiplot)",
    subtitulo = "Clustering Disjoint HJ-Biplot: grupos de individuos y componentes disjuntas de variables (Nieto-Librero et al., 2017)",
    resumen = list(
      para_que = "Encontrar a la vez grupos de individuos (clusters) y grupos de variables (componentes disjuntas) que los separan.",
      entrada = "Matriz individuos × variables numéricas; número de clusters P y de componentes Q.",
      no_usar = "Variables binarias o categóricas; P mayor o igual que el número de individuos; pocas variables.",
      lectura = "Rombos = centroides de los clusters; cada flecha pertenece a un solo componente. Los ejes no son ortogonales."),
    para_que = c(
      "Combina el análisis de conglomerados de individuos con el DPCA de variables: los individuos se agrupan en \\(P\\) clusters y las variables en \\(Q\\) componentes disjuntas, de modo que los componentes son los que mejor <b>separan los clusters</b>. Se dibuja con marcadores HJ. Se propuso para identificar patrones de contaminación en estudios geoquímicos."),
    entrada = c(
      "Matriz <b>individuos × variables numéricas</b> (centrada y, por defecto, estandarizada).",
      "Número de clusters de individuos \\(P\\) (menor que el número de individuos) y de componentes \\(Q\\).",
      "Arranques aleatorios y semilla (el algoritmo converge a óptimos locales)."),
    no_usar = c(
      "Variables binarias o categóricas.",
      "\\(P\\) mayor o igual que el número de individuos (con \\(P=n\\) se recupera el DPCA).",
      "Interpretar ángulos entre ejes como en el PCA: aquí los ejes no son ortogonales."),
    calculos = c(
      "<b>Modelo</b>: \\[ X=U\\bar YB^{\\top}+E,\\qquad U\\in\\{0,1\\}^{n\\times P},\\quad B\\ \\text{disjunta},\\ B^{\\top}B=I , \\] con \\(\\bar Y\\) los centroides en el espacio reducido. Minimizar \\(\\|X-U\\bar YB^{\\top}\\|^2\\) equivale a maximizar la devianza <i>entre</i> clases \\(\\|U\\bar Y\\|^2\\).",
      "<b>Mínimos cuadrados alternados</b>: (1) con \\(U\\) fijo se forman los centroides \\(Z=U\\bar X\\) y se reparten las variables con el greedy del DPCA; (2) con \\(B\\) fijo se proyecta \\(Y=XB\\) y cada individuo va al centroide más cercano (k-means). Ningún paso empeora el criterio.",
      "<b>Marcadores HJ</b>: individuos \\(A=XB\\); variables \\(B\\Lambda\\), con \\(\\Lambda_q=\\|Z\\mathbf b_q\\|\\) y \\(\\Lambda_q^2\\) la devianza entre clases del eje \\(q\\)."),
    lectura = c(
      "Los <b>rombos</b> son los centroides de los clusters; los puntos se colorean por cluster o por la variable de grupo.",
      "Cada <b>flecha</b> pertenece a un solo componente: las variables de un eje son las que separan los clusters en esa dirección.",
      "<b>Proyección</b> de un individuo o centroide sobre una flecha ≈ valor de la variable.",
      "Los ejes <b>no son ortogonales</b>: la pestaña de partición muestra la correlación entre ellos."),
    donde = c(
      "<code>R/equipo_cdpca.R</code>: <code>cdpca()</code> y <code>cluster_table()</code> (código del compañero, sin cambios).",
      "<code>R/equipo_dpca.R</code> (greedy de variables), <code>R/equipo_nucleo.R</code> (marcadores y tablas) y <code>R/equipo_graficos.R</code>.",
      "<code>R/mod_equipo.R</code>: la pantalla dentro del aplicativo."),
    referencias = list(
      modulo = c(
        "Cubilla-Montilla, M., Nieto-Librero, A. B., Galindo-Villardón, M. P. y Torres-Cubilla, C. A. (2021). Sparse HJ biplot: A new methodology via elastic net. <i>Mathematics, 9</i>(11), 1298. https://doi.org/10.3390/math9111298",
        "Ramírez-Figueroa, J. A., Martín-Barreiro, C., Nieto-Librero, A. B., Leiva, V. y Galindo-Villardón, M. P. (2021). A new principal component analysis by particle swarm optimization with an environmental application for data science. <i>Stochastic Environmental Research and Risk Assessment, 35</i>(10), 1969-1984. https://doi.org/10.1007/s00477-020-01961-3"),
      original = c(
        "Nieto-Librero, A. B., Sierra, C., Vicente-Galindo, M. P., Ruíz-Barzola, O. y Galindo-Villardón, M. P. (2017). Clustering disjoint HJ-biplot: A new tool for identifying pollution patterns in geochemical studies. <i>Chemosphere, 176</i>, 389-396. https://doi.org/10.1016/j.chemosphere.2017.02.125",
        "Vichi, M. y Saporta, G. (2009). Clustering and disjoint principal component analysis. <i>Computational Statistics & Data Analysis, 53</i>(8), 3194-3208."))),

  # ------------------------------------------------------------- Cenet -------
  cenet = list(
    titulo = "CenetBiplot",
    subtitulo = "Biplot disperso y ortogonal mediante CSVD con elastic net (González-García et al., 2023)",
    resumen = list(
      para_que = "Biplot con cargas dispersas: muchas variables quedan con carga cero en cada eje, lo que facilita interpretar con muchas variables.",
      entrada = "Matriz individuos × variables numéricas, idealmente con muchas variables; α (mezcla Lasso/Ridge) y nivel de dispersión.",
      no_usar = "Variables binarias o categóricas; pocas variables (la dispersión no aporta); interpretar las longitudes como en el HJ clásico.",
      lectura = "Solo dibuja con fuerza las variables que el eje conserva; las de carga cero no lo construyen. Ejes ortogonales."),
    para_que = c(
      "Cuando hay muchas variables, cada eje del HJ mezcla todas y cuesta interpretarlo. CenetBiplot obtiene vectores singulares <b>dispersos</b> (muchas cargas exactamente cero) y <b>ortogonales</b>, con una penalización elastic net que combina Lasso (selecciona variables) y Ridge (estabiliza variables correlacionadas)."),
    entrada = c(
      "Matriz <b>individuos × variables numéricas</b> (centrada y, por defecto, estandarizada); funciona mejor con muchas variables.",
      "Número de ejes, \\(\\alpha\\in[0,1]\\) (0 = Lasso, 1 = Ridge) y nivel de dispersión (1 = SVD clásica).",
      "Opcional: dispersión también en los individuos. Los parámetros se pueden elegir con BIC (\\(\\tau\\)) y validación cruzada (\\(\\alpha\\))."),
    no_usar = c(
      "Variables binarias o categóricas.",
      "Muy pocas variables: no hay nada que \"dispersar\" y el resultado es casi el HJ clásico.",
      "Comparar longitudes de flechas con el HJ: con elastic net los vectores se encogen (la tabla de sparsity informa la norma)."),
    calculos = c(
      "<b>Modelo</b>, para cada eje \\(q\\): \\[ \\max_{\\mathbf u,\\mathbf v}\\ \\mathbf u^{\\top}X\\mathbf v\\ \\ \\text{s.a.}\\ \\ \\|\\mathbf u\\|_2\\le1,\\ (1-\\alpha)\\|\\mathbf v\\|_1+\\alpha\\|\\mathbf v\\|_2^2\\le\\tau,\\ \\ \\mathbf u\\perp\\mathbf u_{1..q-1},\\ \\mathbf v\\perp\\mathbf v_{1..q-1}. \\]",
      "<b>Resolución</b> (POCS): \\(\\mathbf u\\leftarrow\\mathcal P_{\\mathcal C_u}(X\\mathbf v)\\), \\(\\mathbf v\\leftarrow\\mathcal P_{\\mathcal C_v}(X^{\\top}\\mathbf u)\\). La proyección sobre la bola elastic net es un umbral suave reescalado, \\(\\mathcal P(z)_j=\\operatorname{sign}(z_j)(|z_j|-\\mu)_+/(1+2\\mu\\alpha)\\), y la intersección con la ortogonalidad se resuelve con el algoritmo de Dykstra.",
      "<b>Selección de parámetros</b>: \\(\\tau\\) por BIC y \\(\\alpha\\) por validación cruzada del error de reconstrucción (ecuaciones 9 y 8 del artículo)."),
    lectura = c(
      "Las variables con <b>carga cero</b> en un eje no lo construyen (en la tabla aparecen en gris).",
      "Las <b>flechas</b> que quedan señalan qué variables definen cada eje; el ángulo entre ellas aproxima la correlación solo entre variables bien representadas.",
      "Los <b>puntos</b> se leen como en el HJ: proyección sobre la flecha ≈ valor.",
      "La pestaña <i>Sparsity</i> dice cuántas cargas son cero por eje y la convergencia del algoritmo."),
    donde = c(
      "<code>R/equipo_cenet.R</code>: <code>cenet_svd()</code>, <code>cenet_bic()</code>, <code>cenet_cv_alpha()</code> y <code>sparsity_table()</code> (código del compañero, sin cambios).",
      "<code>R/equipo_nucleo.R</code> y <code>R/equipo_graficos.R</code>: marcadores, tablas y biplot.",
      "<code>R/mod_equipo.R</code>: la pantalla dentro del aplicativo."),
    referencias = list(
      modulo = c(
        "Cubilla-Montilla, M., Nieto-Librero, A. B., Galindo-Villardón, M. P. y Torres-Cubilla, C. A. (2021). Sparse HJ biplot: A new methodology via elastic net. <i>Mathematics, 9</i>(11), 1298. https://doi.org/10.3390/math9111298"),
      original = c(
        "González-García, N., Nieto-Librero, A. B. y Galindo-Villardón, P. (2023). CenetBiplot: A new proposal of sparse and orthogonal biplots methods by means of elastic net CSVD. <i>Advances in Data Analysis and Classification, 17</i>(1), 5-19. https://doi.org/10.1007/s11634-021-00468-1",
        "Zou, H. y Hastie, T. (2005). Regularization and variable selection via the elastic net. <i>Journal of the Royal Statistical Society: Series B, 67</i>(2), 301-320. https://doi.org/10.1111/j.1467-9868.2005.00503.x"))),

  # ---------------------------------------------------------- Logístico ------
  logistic = list(
    titulo = "Biplot logístico",
    subtitulo = "Biplot para datos binarios: presencia/ausencia, sí/no (Vicente-Villardón et al., 2006; Demey et al., 2008)",
    resumen = list(
      para_que = "Representar individuos y variables binarias (0/1), mostrando la probabilidad de cada respuesta.",
      entrada = "Matriz individuos × variables binarias (0/1). Las no binarias se dicotomizan o se excluyen.",
      no_usar = "Variables continuas sin una razón para dicotomizarlas (se pierde información; usar HJ); variables casi constantes (todo 0 o todo 1).",
      lectura = "Círculo = probabilidad 0,5; flecha hasta P = 0,75. Flecha corta = variable muy discriminante. Individuos más allá del círculo: probable presencia."),
    para_que = c(
      "Con variables binarias (presencia/ausencia de marcadores, respuestas sí/no, ítems acertados/errados) el HJ no es adecuado, porque supone variables continuas. El biplot logístico dibuja a los individuos y, para cada variable, la dirección en la que <b>aumenta la probabilidad</b> de que valga 1, ajustando una regresión logística por variable. La versión <b>externa</b> es la que se usa en clase con MultBiplot."),
    entrada = c(
      "Matriz <b>individuos × variables binarias (0/1)</b>. Se proponen solas las columnas con solo 0 y 1.",
      "Las variables no binarias se dicotomizan por la mediana o la media, o se excluyen.",
      "Se quitan, si se desea, los individuos con todo 0 o todo 1 (no aportan información).",
      "Coeficiente de similaridad (Simple Matching, Jaccard, Dice, Russell-Rao, Rogers-Tanimoto…), número de dimensiones y criterios para dibujar (p-valor, Bonferroni, R² mínimo)."),
    no_usar = c(
      "Variables continuas sin justificación para dicotomizarlas: se pierde información; para ellas está el HJ-Biplot.",
      "Variables casi constantes (casi todo 0 o todo 1): la regresión logística se vuelve inestable (separación).",
      "Muy pocos individuos (menos de 10)."),
    calculos = c(
      "<b>Externo, paso 1</b>: similaridad \\(s_{ij}\\) entre individuos y disimilaridad \\(\\delta_{ij}=1-s_{ij}\\) (o \\(\\sqrt{1-s_{ij}}\\)).",
      "<b>Paso 2, coordenadas principales</b> (Gower, 1966): \\[ B_\\delta=-\\tfrac12\\,J\\,\\Delta^{(2)}J=V\\Lambda V^{\\top},\\qquad Y=V\\Lambda^{1/2},\\qquad J=I-\\tfrac1n\\mathbf 1\\mathbf 1^{\\top}. \\]",
      "<b>Paso 3, una regresión logística por variable</b> con las coordenadas como regresoras: \\[ \\operatorname{logit}\\pi_{ij}=b_{j0}+\\sum_{s=1}^{k}b_{js}\\,y_{is}. \\]",
      "<b>Geometría</b>: el punto de la dirección \\(\\mathbf b_j\\) que predice la probabilidad \\(p\\) es \\(\\mathbf y_j(p)=\\dfrac{\\operatorname{logit}(p)-b_{j0}}{\\|\\mathbf b_j\\|^2}\\,\\mathbf b_j\\).",
      "<b>Ajuste</b>: por variable, devianza, p-valor, \\(R^2\\) de Nagelkerke y % de aciertos; por individuo, calidad = distancia² en el plano / distancia² total. Interno (opcional): \\(A\\), \\(B\\) y \\(b_0\\) a la vez por regresiones alternadas con penalización ridge."),
    lectura = c(
      "El <b>círculo</b> de cada variable marca dónde la probabilidad de 1 es 0,5; la <b>flecha</b> llega hasta 0,75 (u otra probabilidad elegida).",
      "<b>Flecha corta</b> = la probabilidad cambia rápido = variable muy <b>discriminante</b>. Flecha larga = discrimina poco.",
      "La recta perpendicular a la flecha por el círculo separa la zona donde lo esperado es 1 (en el sentido de la flecha) de la zona donde lo esperado es 0.",
      "<b>Individuos cercanos</b> tienen perfiles binarios parecidos (según la similaridad elegida).",
      "Solo se dibujan las variables con buen ajuste (p-valor y R² elegidos)."),
    donde = c(
      "<code>R/equipo_logistico_externo.R</code>: <code>elb_fit()</code>, <code>elb_stats()</code>, <code>elb_select()</code>, <code>elb_cluster()</code> (código del compañero, sin cambios).",
      "<code>R/equipo_logistico_interno.R</code>: <code>logistic_biplot()</code> (versión interna).",
      "<code>R/equipo_graficos.R</code>: <code>plot_logistic()</code>; <code>R/mod_equipo.R</code>: la pantalla dentro del aplicativo."),
    referencias = list(
      modulo = c(
        "Demey, J. R., Vicente-Villardón, J. L., Galindo-Villardón, M. P. y Zambrano, A. Y. (2008). Identifying molecular markers associated with classification of genotypes by External Logistic Biplots. <i>Bioinformatics, 24</i>(24), 2832-2838. https://doi.org/10.1093/bioinformatics/btn552",
        "Vicente-Galindo, P., de Noronha Vaz, T. y Nijkamp, P. (2011). Institutional capacity to dynamically innovate: An application to the Portuguese case. <i>Technological Forecasting and Social Change, 78</i>(1), 3-12."),
      original = c(
        "Vicente-Villardón, J. L., Galindo-Villardón, M. P. y Blázquez-Zaballos, A. (2006). Logistic biplots. En M. Greenacre y J. Blasius (Eds.), <i>Multiple correspondence analysis and related methods</i> (pp. 491-509). Chapman & Hall/CRC.",
        "Gower, J. C. (1966). Some distance properties of latent root and vector methods used in multivariate analysis. <i>Biometrika, 53</i>(3-4), 325-338. https://doi.org/10.1093/biomet/53.3-4.325"))),

  # ------------------------------------------------------- Neutrosófico ------
  neutro = list(
    titulo = "HJ-Biplot neutrosófico",
    subtitulo = "HJ-Biplot de puntuaciones neutrosóficas: verdad, indeterminación y falsedad (Galindo-Villardón, 2025)",
    resumen = list(
      para_que = "Analizar respuestas con incertidumbre (acuerdo, duda, desacuerdo) sin descartar la indeterminación.",
      entrada = "Para cada individuo y variable, una terna (T, I, F) entre 0 y 1: ya calculada o obtenida recodificando respuestas categóricas.",
      no_usar = "Mediciones continuas sin una regla justificada para pasarlas a T, I y F; faltantes tratados como «no sabe».",
      lectura = "Como el HJ, sobre la puntuación S = T - I - F. Punto más grande = más indeterminación."),
    para_que = c(
      "La lógica neutrosófica (Smarandache) describe cada respuesta con tres grados independientes: <b>verdad</b> \\(T\\), <b>indeterminación</b> \\(I\\) y <b>falsedad</b> \\(F\\), entre 0 y 1, sin exigir que sumen 1. Así la duda o el \"no sabe\" no se pierden. Como la SVD no está definida para una matriz de ternas, el enfoque pragmático (Galindo-Villardón, 2025) convierte cada terna en un número con una función de puntuación y aplica el HJ-Biplot a esa matriz."),
    entrada = c(
      "<b>Ternas ya calculadas</b>: tres columnas por variable, <code>nombre_T</code>, <code>nombre_I</code> y <code>nombre_F</code>, con valores entre 0 y 1 (por ejemplo, de una escala neutrosófica deslizante).",
      "<b>Respuestas categóricas</b> (por ejemplo, un .sav de SPSS con etiquetas «Muy seguro … Muy inseguro, No sabe»): cada respuesta se asigna a T, I, F, neutral (0, 0, 0) o excluir, con una regla que hay que justificar.",
      "Al menos 3 observaciones, 2 variables y variación después de puntuar y centrar (al menos dos ejes no nulos para dibujar un plano)."),
    no_usar = c(
      "Mediciones continuas (abundancias, puntajes) sin una regla externa y justificada para asignar T, I y F: para ellas está el HJ-Biplot cuantitativo.",
      "Un dato ausente como si fuera indeterminación: los faltantes se excluyen.",
      "Concluir sobre la diferencia entre duda y desacuerdo: con \\(S=T-I-F\\) aumentar \\(I\\) o \\(F\\) baja \\(S\\) en la misma cantidad, así que el plano no los distingue (la media de \\(I\\) se informa aparte y se ve en el tamaño de los puntos)."),
    calculos = c(
      "<b>1. Puntuación</b> de cada terna: \\[ s_{ij}=t_{ij}-i_{ij}-f_{ij}. \\] Es equivalente, tras centrar, a la puntuación \\((2+t-i-f)/3\\) del artículo: sumar una constante o multiplicar por \\(1/3\\) no cambia los porcentajes de inercia ni las calidades.",
      "<b>2. Centrado</b> por columnas, sin estandarizar (las puntuaciones ya están en la misma escala): \\(z_{ij}=s_{ij}-\\bar s_j\\).",
      "<b>3. SVD y marcadores HJ</b>: \\(Z=UDV^{\\top}\\), \\(J=UD\\) (observaciones), \\(H=VD\\) (variables). Se comprueba que \\(UDV^{\\top}\\) reconstruye \\(Z\\).",
      "<b>4. Inercia, calidad y contribución</b> como en el HJ: inercia \\(100\\,d_k^2/\\sum d^2\\); calidad (CRFE) \\(a_{ik}^2/\\sum_k a_{ik}^2\\); contribución (CREF) \\(a_{ik}^2/\\sum_i a_{ik}^2\\). Además, la media de \\(I\\) por observación y por variable."),
    lectura = c(
      "Se lee como un <b>HJ-Biplot</b>: el ángulo entre flechas aproxima la correlación entre las puntuaciones; la proyección de un punto sobre una flecha, su puntuación en esa variable.",
      "Una observación en el sentido de la flecha tiene <b>alta verdad</b> (acuerdo, seguridad…) en esa variable; en el sentido contrario, falsedad o indeterminación.",
      "El <b>tamaño del punto</b> crece con la indeterminación media de la observación.",
      "Revisar la tabla <i>Recodificación</i>: cuántas respuestas fueron T, I, F o neutrales y cuántas filas se excluyeron.",
      "La escala visual de las flechas solo cambia su largo dibujado."),
    donde = c(
      "<code>R/neutrosofico_calculo.R</code>: <code>puntuar_neutrosofico()</code>, <code>hj_neutrosofico()</code>, <code>indicadores_por_eje()</code> y <code>calidades_hj()</code> (código del compañero, sin cambios).",
      "<code>R/mod_neutrosofico.R</code>: obtención de las ternas (<code>ternas_desde_respuestas()</code>, <code>ternas_desde_columnas()</code>), gráfico con encuadre 4:3 y la pantalla.",
      "Validado con matrices pequeñas de resultado conocido y con la base de victimización del INEC (pruebas en <code>tests/testthat/test-neutrosofico.R</code>)."),
    referencias = list(
      modulo = c(
        "Galindo, M. P. (1986). Una alternativa de representación simultánea: HJ-Biplot. <i>Qüestiió, 10</i>(1), 13-23. http://hdl.handle.net/2099/4523"),
      original = c(
        "Galindo-Villardón, P. (2025). Analysis of customer perception of a new product using the Neutrosophic Biplot method. <i>Neutrosophic Sets and Systems, 89</i>, 34-49. https://doi.org/10.5281/zenodo.16420847",
        "Smarandache, F. (2014). <i>Introduction to neutrosophic statistics</i>. Sitech & Education Publishing."))),

  # ---------------------------------------------------------------- GGE -------
  gge = list(
    titulo = "GGE-Biplot",
    subtitulo = "Efecto del genotipo más la interacción genotipo × ambiente: quién gana dónde (Yan et al., 2000; Yan y Tinker, 2006)",
    resumen = list(
      para_que = "Evaluar genotipos (o cualquier individuo) en varios ambientes (o variables): quién gana dónde, cuál es estable y qué ambiente discrimina mejor.",
      entrada = "Tabla de dos vías completa: genotipos en filas, ambientes en columnas, una media (rendimiento…) en cada celda.",
      no_usar = "Datos con faltantes; observaciones individuales sin promediar; columnas de escalas distintas sin escalar; menos de 3 genotipos.",
      lectura = "Producto escalar genotipo·ambiente ≈ valor de la celda. Polígono = quién ganó dónde; flecha media = media y estabilidad."),
    para_que = c(
      "En los ensayos multiambiente el efecto del ambiente suele ser el mayor, pero no sirve para elegir cultivares. El GGE-Biplot quita ese efecto (centrando por ambiente) y representa solo lo que importa para la selección: el efecto del <b>genotipo (G)</b> más la <b>interacción genotipo × ambiente (GE)</b>. Con herramientas gráficas responde preguntas concretas: qué genotipo gana en cada ambiente (mega-ambientes), cuáles tienen media alta y son estables, y qué ambientes discriminan y representan mejor. Funciona con cualquier tabla individuo × variable de dos vías."),
    entrada = c(
      "Una <b>tabla de dos vías</b>: una fila por genotipo (individuo) y una columna por ambiente (variable), con un valor en cada celda, por ejemplo la media de rendimiento del genotipo en ese ambiente.",
      "Sin faltantes: el GGE necesita la tabla completa.",
      "Los nombres de los genotipos en la primera columna (deben ser únicos).",
      "Opciones: centrado (por ambiente, G + GE, el habitual; global; doble; sin centrar), escalado por desviación típica si los ambientes tienen variabilidad muy distinta, y partición de los valores singulares (GH, JK, HJ o SQ)."),
    no_usar = c(
      "Tablas con datos faltantes o con repeticiones sin promediar (primero se calculan las medias por genotipo y ambiente).",
      "Columnas de naturaleza distinta (rendimiento, altura, precio…) sin escalar: entonces es mejor un HJ-Biplot estandarizado.",
      "Interpretar productos escalares con la partición HJ: con HJ \\(J H^{\\top}=U D^2 V^{\\top}\\) no reconstruye la tabla; para leer valores usar GH, JK o SQ.",
      "Sacar conclusiones si los dos primeros ejes explican poco de G + GE."),
    calculos = c(
      "<b>1. Centrado por ambiente</b> (G + GE): \\[ y_{ij}-\\bar y_{\\cdot j}=\\sum_{k=1}^{r}\\lambda_k\\,\\xi_{ik}\\,\\eta_{jk}+\\varepsilon_{ij}, \\] con \\(i\\) el genotipo y \\(j\\) el ambiente. Opcionalmente se divide cada ambiente por su desviación típica.",
      "<b>2. SVD</b> de la tabla centrada \\(Z=UDV^{\\top}\\) y <b>partición</b> de los valores singulares: GH \\((G=U,\\ H=VD)\\) conserva la métrica de los ambientes; JK \\((J=UD,\\ K=V)\\) la de los genotipos; SQ \\((UD^{1/2},\\ VD^{1/2})\\) es simétrica; HJ \\((UD,\\ VD)\\) representa ambos con la misma calidad.",
      "<b>3. Aproximación</b>: con GH, JK y SQ, \\(z_{ij}\\approx \\mathbf g_i^{\\top}\\mathbf e_j\\) (producto escalar del marcador del genotipo y el del ambiente en el plano).",
      "<b>4. Inercia</b> \\(100\\,d_k^2/\\sum d^2\\), calidad (CRFE, coseno² en ‰) y contribución (CREF) con las coordenadas de la partición elegida, como GGEBiplotGUI.",
      "<b>5. Herramientas</b>: examinar un ambiente o un genotipo (proyecciones), relación entre ambientes (ángulos), comparar dos genotipos, quién ganó dónde (polígono y sectores), discriminación vs representatividad y media vs estabilidad (eje del ambiente medio, AEA), y rankings respecto al ambiente o genotipo ideal."),
    lectura = c(
      "<b>Producto escalar</b>: proyectar un genotipo sobre el eje de un ambiente ordena a los genotipos en ese ambiente (más hacia la flecha = mayor valor).",
      "<b>Ángulo entre dos ambientes</b>: agudo = clasifican igual a los genotipos (correlación positiva); obtuso = los clasifican al revés (interacción cruzada).",
      "<b>Largo del vector de un ambiente</b>: capacidad de discriminar genotipos.",
      "<b>Quién ganó dónde</b>: el polígono une los genotipos más alejados; las rectas perpendiculares a sus lados forman sectores; el genotipo del vértice gana en los ambientes de su sector (mega-ambientes).",
      "<b>Media vs estabilidad</b>: la proyección sobre el eje del ambiente medio estima la media del genotipo; la distancia a ese eje mide su inestabilidad.",
      "<b>Ideal</b>: el genotipo (o ambiente) más deseable es el más cercano al centro de los círculos.",
      "El signo de los ejes es arbitrario: voltear no cambia los productos escalares."),
    donde = c(
      "<code>R/calculo_gge.R</code>: centrado, escalado, partición (<code>gge_descomposicion()</code>), resultados y los diez gráficos (código de la compañera).",
      "<code>R/tablas_gge.R</code>: <code>calcular_gge()</code> y las tablas de cada herramienta.",
      "<code>R/mod_gge.R</code>: la pantalla, la Sintaxis R autónoma y la exportación; al integrarlo se agregó el mínimo 4:3, «Voltear» y esta pestaña.",
      "Validado con los datos de Ontario (18 genotipos × 9 ambientes) frente a la práctica de clase y <code>metan::gge()</code> (pruebas en <code>tests/testthat/test-gge.R</code>)."),
    referencias = list(
      modulo = c(
        "Frutos, E., Galindo, M. P. y Leiva, V. (2014). An interactive biplot implementation in R for modeling genotype-by-environment interaction. <i>Stochastic Environmental Research and Risk Assessment, 28</i>(7), 1629-1641. https://doi.org/10.1007/s00477-013-0821-z"),
      original = c(
        "Yan, W., Hunt, L. A., Sheng, Q. y Szlavnics, Z. (2000). Cultivar evaluation and mega-environment investigation based on the GGE biplot. <i>Crop Science, 40</i>(3), 597-605. https://doi.org/10.2135/cropsci2000.403597x",
        "Yan, W. y Kang, M. S. (2003). <i>GGE biplot analysis: A graphical tool for breeders, geneticists, and agronomists</i>. CRC Press.",
        "Yan, W. y Tinker, N. A. (2006). Biplot analysis of multi-environment trial data: Principles and applications. <i>Canadian Journal of Plant Science, 86</i>(3), 623-645. https://doi.org/10.4141/P05-169")))
)
