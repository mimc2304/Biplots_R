#FUNCION AUXILIAR: RECTA POR EL ORIGEN EN LA DIRECCIÓN (a, b)
# Si a = 0 la recta es vertical (no tiene pendiente) → geom_vline.
gge_recta <- function(a, b, ...) {
  if (abs(a) < 1e-12) {
    ggplot2::geom_vline(xintercept = 0, ...)
  } else {
    ggplot2::geom_abline(intercept = 0, slope = b / a, ...)
  }
}


#FUNCION 1: CENTRADO

GGE_CENTRADOS <- c(
  "No centering"           = "ninguno",
  "Global-centered E+G+GE" = "global",
  "Tester-centered G+GE"   = "tester",
  "Double-centered GE"     = "doble"
)



gge_centrado <- function(datos, tipo_centrado = "tester") {
  datos <- as.matrix(datos)
  
  if (tipo_centrado == "ninguno") {
    datos_tipo_centrado <- datos
    
  } else if (tipo_centrado == "global") {
    promedio <- mean(datos)
    datos_tipo_centrado <- datos - promedio
    
  } else if (tipo_centrado == "tester") {
    mean_columna <- colMeans(datos)
    datos_tipo_centrado <- sweep(datos, 2, mean_columna, "-")
    
  } else if (tipo_centrado == "doble") {
    mean_columna <- colMeans(datos)
    mean_filas <- rowMeans(datos)
    promedio <- mean(datos)
    datos_tipo_centrado <- sweep(datos, 2, mean_columna, "-")
    datos_tipo_centrado <- sweep(datos_tipo_centrado, 1, mean_filas, "-")
    datos_tipo_centrado <- datos_tipo_centrado + promedio
    
  } else {
    stop("Centrado no reconocido. Usa: ",
         paste(GGE_CENTRADOS, collapse = ", "),
         call. = FALSE)
  }
  return(datos_tipo_centrado)
}


#FUNCION 2: ESCALAR
GGE_ESCALADOS <- c("No scaling"="no",
                   "Std Deviation (SD)"="std")

gge_escalado <- function(datos, tipo_escalado="no"){
  datos <- as.matrix(datos)
  
  if (tipo_escalado=="no"){
    datos_escalados <- datos
    
  } else if (tipo_escalado=="std"){
    
    desviaciones <- apply(datos,2,stats::sd)
    
    constantes <- names(desviaciones)[desviaciones == 0]
    if (length(constantes) > 0) {
      stop("Estas columnas tienen todos sus valores iguales y no se pueden escalar: ",
           paste(constantes, collapse = ", "), call. = FALSE)
    }
    
    datos_escalados <- sweep(datos,2,desviaciones,"/")
    
  } else {stop("Escalado no reconocido. Usa: ",
               paste(GGE_ESCALADOS, collapse = ", "),
               call. = FALSE)}
  return(datos_escalados)
}


#FUNCION 3: PARTICION (SVD)

GGE_PARTICIONES <- c("JK-Row metric preserving"="JK",
                     "GH-Col metric preserving"="GH",
                     "HJ-Dual metric preserving"="HJ",
                     "SQ-Symmetrical"="SQ")

gge_descomposicion <- function(datos, tipo_particion="GH"){
  descomp <- svd(datos)
  u <- descomp$u
  v <- descomp$v
  d <- descomp$d
  
  if(tipo_particion=="JK"){
    filas <- u %*% diag(d)
    columnas <- v
    
  } else if (tipo_particion=="GH"){
    filas <- u
    columnas <- v %*% diag(d)
    
  }else if (tipo_particion=="HJ"){
    filas <- u%*% diag(d)
    columnas <- v %*% diag(d)
    
  }else if (tipo_particion=="SQ"){
    filas <- u%*% diag(sqrt(d))
    columnas <- v %*% diag(sqrt(d))
    
  } else {
    stop("Partición no reconocida. Usa: ",
         paste(GGE_PARTICIONES, collapse = ", "), call. = FALSE)
  }
  
  ejes <- paste("Eje", seq_along(d))
  rownames(filas)    <- rownames(datos)
  colnames(filas)    <- ejes
  rownames(columnas) <- colnames(datos)
  colnames(columnas) <- ejes
  
  return(list(filas=filas,columnas=columnas,valor_singular=d,valor_propio=d*d,particion=tipo_particion,datos_centrados=datos))
  
}


#FUNCION 4: RESULTADOS

gge_resultados <- function(lista, eje=c(1,2)){
  inercia <- (lista$valor_propio/sum(lista$valor_propio))*100
  acumulado <- cumsum(inercia)
  varianza <- data.frame(
    "Eje"            = seq_along(lista$valor_propio),
    "Valor singular" = lista$valor_singular,
    "Valor propio"   = lista$valor_propio,
    "% Inercia"      = inercia,
    "% Acumulado"    = acumulado,
    check.names = FALSE
  )
  calidad_filas <- lista$filas^2/rowSums(lista$filas^2)*1000
  calidad_columnas <- lista$columnas^2/rowSums(lista$columnas^2)*1000
  
  contrib_filas <- sweep(lista$filas^2,2,colSums(lista$filas^2),"/")*1000
  contrib_columnas <- sweep(lista$columnas^2,2,colSums(lista$columnas^2),"/")*1000
  
  calidad_filas    <- cbind(calidad_filas,    Plano = rowSums(calidad_filas[, eje]))
  calidad_columnas <- cbind(calidad_columnas, Plano = rowSums(calidad_columnas[, eje]))
  
  return(list(inercia=varianza,calidad_filas=calidad_filas,calidad_columnas=calidad_columnas,
              contrib_filas=contrib_filas,contrib_columnas=contrib_columnas))
}


#FUNCION 5: ADAPTAR COORDENADAS PARA GRAFICOS

gge_coordenadas_grafico <- function(datos, ejes=c(1,2)){
  
  rango <- function(v){max(v) - min(v)}
  
  genotipos <- data.frame(
    x      = datos$filas[, ejes[1]],
    y      = datos$filas[, ejes[2]],
    nombre = rownames(datos$filas))
  
  ambientes <- data.frame(
    x      = datos$columnas[, ejes[1]],
    y      = datos$columnas[, ejes[2]],
    nombre = rownames(datos$columnas))
  
  if (datos$particion=="JK"){
    r_arriba <- c(rango(datos$columnas[,ejes[1]]), rango(datos$columnas[,ejes[2]]))
    r_abajo  <- c(rango(datos$filas[,ejes[1]]),    rango(datos$filas[,ejes[2]]))
    d  = max((r_arriba / r_abajo)[r_abajo > 1e-12])
    
    ambientes$x <- ambientes$x/d
    ambientes$y <- ambientes$y/d
    
  } else if (datos$particion=="GH")
  {
    r_arriba <- c(rango(datos$filas[,ejes[1]]),    rango(datos$filas[,ejes[2]]))
    r_abajo  <- c(rango(datos$columnas[,ejes[1]]), rango(datos$columnas[,ejes[2]]))
    d  = max((r_arriba / r_abajo)[r_abajo > 1e-12])
    
    genotipos$x <- genotipos$x/d
    genotipos$y <- genotipos$y/d
    
  }
  
  return(list(genotipos=genotipos, ambientes=ambientes))
}

#FUNCION 6: GRAFICO BÁSICO
gge_biplot_basico <- function(datos,ejes=c(1,2),ver_ambientes = NULL,ver_genotipos=NULL,flechas=TRUE, color_filas = "#2a78d6", color_columnas = "#9a3412", tam_texto = 3, tam_puntos = 1.5){
  
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos   <- coordenadas$genotipos
  ambientes   <- coordenadas$ambientes
  
  if (!is.null(ver_ambientes)){
    ambientes <- ambientes[ver_ambientes,]
    
  }
  
  if (!is.null(ver_genotipos)){
    genotipos <- genotipos[ver_genotipos,]
    
  }
  
  
  porcentaje <- 100 * datos$valor_propio / sum(datos$valor_propio)
  titulo_x <- paste("Eje", ejes[1], round(porcentaje[ejes[1]], 2), "%")
  titulo_y <- paste("Eje", ejes[2], round(porcentaje[ejes[2]], 2), "%")
  
  
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted", colour = "grey60") +
    ggplot2::geom_point(data =genotipos, ggplot2::aes(x,y),colour = color_filas, size = tam_puntos) +
    ggrepel::geom_text_repel(data = genotipos, ggplot2::aes(x, y, label = nombre),
                             colour = color_filas, size = tam_texto,
                             seed = 1,
                             max.overlaps = Inf,
                             segment.colour = "grey70") +
    (if (flechas) ggplot2::geom_segment(data = ambientes,          # flechas de los ambientes (opcionales)
                                        ggplot2::aes(x = 0, y = 0,
                                                     xend = x, yend = y),
                                        colour = color_columnas,
                                        arrow = grid::arrow(length = grid::unit(0.2, "cm")))) +
    (if (!flechas) ggplot2::geom_point(data =ambientes, ggplot2::aes(x,y),colour = color_columnas, size = tam_puntos))+
    ggrepel::geom_text_repel(data = ambientes, ggplot2::aes(x, y, label = nombre),
                             colour = "black", size = tam_texto, fontface = "bold",
                             seed = 1,
                             max.overlaps = Inf,
                             segment.colour = "grey70") +
    ggplot2::labs(x = titulo_x, y = titulo_y) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()
  
}



#GRÁFICO 2: EXAMINAR POR AMBIENTES

gge_examinar_ambiente <- function(datos, ambiente, ejes = c(1,2), ...){
  
  # 1. Coordenadas tal como se dibujan
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos   <- coordenadas$genotipos
  ambientes   <- coordenadas$ambientes
  
  # 2. Comprobar que el ambiente existe
  if (!(ambiente %in% ambientes$nombre)) {
    stop("Ambiente no encontrado. Usa: ", paste(ambientes[,3], collapse = ", "), call. = FALSE)
  }
  
  # 3. Sacar e = (x, y) del ambiente elegido
  e <- c(ambientes$x[ambientes$nombre == ambiente],
         ambientes$y[ambientes$nombre == ambiente])
  
  # 4. Cuentas del Paso 1, para todos los genotipos
  g <- as.matrix(genotipos[, c("x", "y")])
  producto_escalar <- g %*%e
  modulo           <- sqrt(sum(e^2))
  pie              <-  (producto_escalar/modulo^2) %*%e
  
  # 5. Guardar el pie como columnas nuevas de genotipos
  genotipos$pie_x <- pie[, 1]
  genotipos$pie_y <- pie[, 2]
  
  # 6. Dibujo: biplot básico + 4 capas
  grafico <- gge_biplot_basico(datos, ejes,ver_ambientes=ambiente, ...) +
    gge_recta( e[1], e[2],                     # a) eje del ambiente
                         colour = "#9a3412") +
    gge_recta(-e[2], e[1],                     # b) perpendicular
                         colour = "#9a3412") +
    ggplot2::geom_segment(data = genotipos,                              # c) punteadas al pie
                          ggplot2::aes(x = x, y = y, xend = pie_x, yend = pie_y),
                          colour = "grey50", linetype = "dotted") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = e[1], yend = e[2],   # d) flecha del ambiente
                      colour = "#9a3412", linewidth = 1,
                      arrow = grid::arrow(length = grid::unit(0.3, "cm")))
  
  return(grafico)
}



#GRÁFICO 3: EXAMINAR POR GENOTIPOS

gge_examinar_genotipo <- function(datos, genotipo, ejes = c(1,2),flechas = FALSE, ...){
  
  # 1. Coordenadas tal como se dibujan
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos   <- coordenadas$genotipos
  ambientes   <- coordenadas$ambientes
  
  # 2. Comprobar que el ambiente existe
  if (!(genotipo %in% genotipos$nombre)) {
    stop("Genotipo no encontrado. Usa: ", paste(genotipos[,3], collapse = ", "), call. = FALSE)
  }
  
  # 3. Sacar g = (x, y) del genotipo elegido
  g <- c(genotipos$x[genotipos$nombre == genotipo],
         genotipos$y[genotipos$nombre == genotipo])
  
  # 4. Cuentas del Paso 1, para todos los ambientes
  e <- as.matrix(ambientes[, c("x", "y")])
  producto_escalar <- e %*% g
  modulo           <- sqrt(sum(g^2))
  pie              <-  (producto_escalar/modulo^2) %*%g
  
  # 5. Guardar el pie como columnas nuevas de ambientes
  ambientes$pie_x <- pie[, 1]
  ambientes$pie_y <- pie[, 2]
  
  # 6. Dibujo: biplot básico + 4 capas
  grafico <- gge_biplot_basico(datos, ejes,ver_genotipos=genotipo,flechas = FALSE, ...) +
    gge_recta( g[1], g[2],                    # a) eje del ambiente
                         colour = "#9a3412") +
    gge_recta(-g[2], g[1],                     # b) perpendicular
                         colour = "#9a3412") +
    ggplot2::geom_segment(data = ambientes,                              # c) punteadas al pie
                          ggplot2::aes(x = x, y = y, xend = pie_x, yend = pie_y),
                          colour = "grey50", linetype = "dotted") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = g[1], yend = g[2],   # d) flecha del ambiente
                      colour = "#9a3412", linewidth = 1,
                      arrow = grid::arrow(length = grid::unit(0.3, "cm")))
  
  return(grafico)
}


#GRAFICO 4: RELACION ENTRE AMBIENTES


gge_relacion_ambientes <- function(datos, ejes = c(1,2), circulos = "gui", radio_manual = NULL,
                                   mostrar_genotipos = FALSE, ...){
  
  # 1. Coordenadas tal como se dibujan (con el ajuste d de JK / GH)
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  ambientes   <- coordenadas$ambientes
  
  # 2. Radio de los círculos
  
  rango <- function(v){max(v) - min(v)}
  
  
  if (circulos == "gui") {
    # Fórmula LITERAL de GGEBiplotGUI: usa las FILAS ejes[1] y ejes[2] de la
    # matriz de ambientes (dos ambientes, en todos sus ejes), no las columnas.
    # La GUI usa las coordenadas ya ajustadas por d, así que aplicamos el mismo
    # factor que gge_coordenadas_grafico() aplicó a los ambientes (1 si no hubo ajuste).
    factor <- sum(abs(ambientes$x)) / sum(abs(datos$columnas[, ejes[1]]))
    E      <- datos$columnas * factor
    radio  <- max(rango(E[ejes[1], ]), rango(E[ejes[2], ])) / 10
    
  } else if (circulos == "corregido") {
    # Versión corregida: rango de los ambientes en los dos ejes dibujados
    radio <- max(rango(ambientes$x), rango(ambientes$y)) / 10
    
  } else {
    stop("Círculos no reconocidos. Usa: gui, corregido", call. = FALSE)
  }
  
  if (!is.null(radio_manual)) {
    radio <- radio_manual
  }
  
  # 3. Puntos de los 5 círculos (ggplot no tiene círculos: se dibujan como
  #    100 puntos unidos por una línea que se cierra)
  angulo  <- seq(0, 2*pi, length.out = 100)
  lista   <- list()
  for (k in 1:5) {
    lista[[k]] <- data.frame(x       = k * radio * cos(angulo),
                             y       = k * radio * sin(angulo),
                             circulo = k)
  }
  circulos_df <- do.call(rbind, lista)          # pega los 5 data.frames uno debajo de otro
  
  # 4. Dibujo: biplot sin genotipos (solo flechas de ambientes) + círculos
  genotipos_a_ver <- if (mostrar_genotipos) NULL else character(0)
  grafico <- gge_biplot_basico(datos, ejes, ver_genotipos = genotipos_a_ver, ...) +
    ggplot2::geom_path(data = circulos_df,
                       ggplot2::aes(x = x, y = y, group = circulo),
                       colour = "grey70")
  
  return(grafico)
}



#GRÁFICO 4: COMPARAR GENOTIPOS

gge_comparar_genotipos <- function(datos, genotipo1, genotipo2, ejes = c(1,2), ...){
  
  # 1. Coordenadas tal como se dibujan
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos   <- coordenadas$genotipos
  
  # 2. Comprobar que existen los dos genotipos
  for (gen in c(genotipo1, genotipo2)) {
    if (!(gen %in% genotipos$nombre)) {
      stop("Genotipo no encontrado: ", gen, ". Usa: ",
           paste(genotipos$nombre, collapse = ", "), call. = FALSE)
    }
  }
  if (genotipo1 == genotipo2) {
    stop("Elige dos genotipos distintos", call. = FALSE)
  }
  
  # 3. Sacar g1 y g2 = (x, y)
  g1 <- c(genotipos$x[genotipos$nombre == genotipo1],
          genotipos$y[genotipos$nombre == genotipo1])
  g2 <- c(genotipos$x[genotipos$nombre == genotipo2],
          genotipos$y[genotipos$nombre == genotipo2])
  
  # 4. Diferencia: flecha que va de g2 a g1
  d <- g1 - g2

  # 5. Recta de empate (d · e = 0): perpendicular al segmento, por el origen
  recta <- gge_recta(-d[2], d[1], colour = "red", linewidth = 0.5)
  
  # 6. Dibujo: biplot básico + segmento + recta + círculos
  grafico <- gge_biplot_basico(datos, ejes, flechas = FALSE, ...) +
    ggplot2::annotate("segment", x = g1[1], y = g1[2], xend = g2[1], yend = g2[2],   # a) segmento
                      colour = "red", linewidth = 0.5) +
    recta
  
  return(grafico)
}


#GRÁFICO 6: ¿QUIÉN GANÓ DÓNDE?

gge_quien_gano_donde <- function(datos, ejes = c(1,2), ...){
  
  # 1. Coordenadas tal como se dibujan
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos   <- coordenadas$genotipos
  ambientes   <- coordenadas$ambientes
  
  # 2. Polígono: los genotipos de las esquinas (la "goma elástica")
  idx      <- grDevices::chull(genotipos$x, genotipos$y)
  poligono <- genotipos[idx, ]
  n        <- nrow(poligono)
  
  # 3. Largo de las rectas: un poco más que el punto más lejano del gráfico
  todos  <- rbind(genotipos[, c("x", "y")], ambientes[, c("x", "y")])
  largo  <- 1.1 * max(sqrt(todos$x^2 + todos$y^2))
  
  # 4. Una recta por cada lado (el último vértice se une con el primero)
  rectas <- data.frame(x = numeric(n), y = numeric(n))
  centro <- colMeans(poligono[, c("x", "y")])
  for (k in 1:n) {
    siguiente <- ifelse(k == n, 1, k + 1)
    a  <- c(poligono$x[k],         poligono$y[k])
    b  <- c(poligono$x[siguiente], poligono$y[siguiente])
    dd <- b - a                                   # dirección del lado
    t  <- -sum(a * dd) / sum(dd * dd)             # proyección del origen sobre el lado
    pie <- a + t * dd                             # pie de la perpendicular
    if (sqrt(sum(pie^2)) < 1e-10) {               # el origen está sobre el lado
      pie <- c(dd[2], -dd[1])                     # perpendicular al lado
      if (sum(pie * (a - centro)) < 0) pie <- -pie  # que apunte hacia fuera
    }
    rectas[k, ] <- pie / sqrt(sum(pie^2)) * largo # misma dirección, estirada hasta el borde
  }
  
  # 5. Dibujo: biplot básico + polígono + rectas
  grafico <- gge_biplot_basico(datos, ejes, flechas = FALSE, ...) +
    ggplot2::geom_polygon(data = poligono, ggplot2::aes(x = x, y = y),             # a) polígono
                          fill = NA, colour = "black") +
    ggplot2::geom_segment(data = rectas, ggplot2::aes(x = 0, y = 0, xend = x, yend = y),  # b) rectas
                          colour = "red")
  
  return(grafico)
}



#GRAFICO 7: DISCRIMINACION VS REPRESENTATIVIDAD

gge_dis_rep <- function(datos,ejes=c(1,2),mostrar_genotipos = TRUE, ...){
  
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  ambientes <- coordenadas$ambientes[,c(1,2)]
  promedio <- colMeans(ambientes)
  
  gge_relacion_ambientes(datos,ejes,mostrar_genotipos = TRUE, ...)+
    gge_recta(promedio[1], promedio[2])+
    ggplot2::annotate("segment", x=0, y=0, xend =promedio[1], yend=promedio[2],arrow=grid::arrow(length = grid::unit(0.3, "cm")),
                      colour = "red", linewidth = 1)
}


#GRAFICO 8 MEDIA VS ESTABILIDAD

gge_media_estabilidad <- function(datos, ejes = c(1,2), ...){
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  ambientes <- coordenadas$ambientes[,c(1,2)]
  genotipos   <- coordenadas$genotipos
  e <- colMeans(ambientes)
  
  
  g <- as.matrix(genotipos[, c("x", "y")])
  producto_escalar <- g %*%e
  modulo           <- sqrt(sum(e^2))
  pie              <-  (producto_escalar/modulo^2) %*%e
  
  genotipos$pie_x <- pie[, 1]
  genotipos$pie_y <- pie[, 2]
  
  grafico <- gge_biplot_basico(datos, ejes,flechas = FALSE, ...) +
    gge_recta( e[1], e[2],
                         colour = "#9a3412") +
    gge_recta(-e[2], e[1],
                         colour = "#9a3412") +
    ggplot2::geom_segment(data = genotipos,
                          ggplot2::aes(x = x, y = y, xend = pie_x, yend = pie_y),
                          colour = "grey50", linetype = "dotted") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = e[1], yend = e[2],
                      colour = "#9a3412", linewidth = 1,
                      arrow = grid::arrow(length = grid::unit(0.3, "cm")))
  
  return(grafico)
  
}



#GRAFICO 9: RANKING DE AMBIENTES
gge_ranking_ambientes <- function(datos,ejes=c(1,2), ...){
  
  # 1. Coordenadas, promedio, largos, ideal y radio
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  ambientes <- coordenadas$ambientes[,c(1,2)]
  promedio <- colMeans(ambientes)
  largos <- sqrt(ambientes$x^2 + ambientes$y^2)
  ideal <- promedio / sqrt(sum(promedio^2)) * max(largos)
  radio <- sqrt(sum((ideal - promedio)^2)) / 3
  
  # 2. Los 8 círculos de la diana, centrados en el ambiente ideal
  angulo <- seq(0, 2*pi, length.out = 100)
  lista  <- list()
  for (k in 1:8) {
    lista[[k]] <- data.frame(x       = ideal[1] + k * radio * cos(angulo),
                             y       = ideal[2] + k * radio * sin(angulo),
                             circulo = k)
  }
  circulos_df <- do.call(rbind, lista)
  
  # 3. Círculo pequeño (radio 0.1) en el ambiente promedio, como la GUI
  promedio_df <- data.frame(x = promedio[1] + 0.1 * cos(angulo),
                            y = promedio[2] + 0.1 * sin(angulo))
  
  # 4. Dibujo
  grafico <- gge_biplot_basico(datos, ejes, flechas = FALSE, ver_genotipos = character(0), ...) +
    ggplot2::geom_path(data = circulos_df,                                      # diana
                       ggplot2::aes(x = x, y = y, group = circulo),
                       colour = "grey70") +
    ggplot2::geom_path(data = promedio_df, ggplot2::aes(x = x, y = y),                   # círculo del promedio
                       colour = "#9a3412") +
    gge_recta( promedio[1], promedio[2],       # AEA
                         colour = "#9a3412") +
    gge_recta(-promedio[2], promedio[1],       # perpendicular
                         colour = "#9a3412") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = ideal[1], yend = ideal[2],   # flecha al ideal
                      colour = "#9a3412", linewidth = 1,
                      arrow = grid::arrow(length = grid::unit(0.3, "cm")))
  
  # 5. Encuadre como la GUI: el marco lo deciden los puntos, no los círculos
  
  todos <- rbind(coordenadas$genotipos[, c("x", "y")],
                 coordenadas$ambientes[, c("x", "y")])
  lado    <- max(diff(range(todos$x)), diff(range(todos$y)))  # el lado más largo 
  centro_x <- mean(range(todos$x))                            # mitad del ancho
  centro_y <- mean(range(todos$y))                            # mitad del alto
  grafico <- grafico + ggplot2::coord_equal(
    xlim = centro_x + c(-1, 1) * lado / 2,   # mismo largo en los dos ejes
    ylim = centro_y + c(-1, 1) * lado / 2)
  return(grafico)
}



#GRAFICO 10: RANKING DE GENOTIPOS (genotipo ideal, criterio de la GUI)
gge_ranking_genotipos <- function(datos,ejes=c(1,2), ...){
  coordenadas <- gge_coordenadas_grafico(datos, ejes)
  genotipos <- coordenadas$genotipos
  ambientes <- coordenadas$ambientes[,c(1,2)]
  promedio <- colMeans(ambientes)
  
  # 2. Dirección del AEA con largo 1 y cuánto avanza cada genotipo sobre ella
  u <- promedio / sqrt(sum(promedio^2))
  t <- as.matrix(genotipos[, c("x", "y")]) %*% u
  
  # 3. Genotipo ideal (GUI): sobre el AEA, tan lejos como el pie más avanzado
  ideal <- max(t,0) * u
  radio <- sqrt(sum((ideal - promedio)^2)) / 3
  
  # 4. Los 10 círculos de la diana, centrados en el genotipo ideal
  angulo <- seq(0, 2*pi, length.out = 100)
  lista  <- list()
  for (k in 1:10) {
    lista[[k]] <- data.frame(x       = ideal[1] + k * radio * cos(angulo),
                             y       = ideal[2] + k * radio * sin(angulo),
                             circulo = k)
  }
  circulos_df <- do.call(rbind, lista)
  
  # 5. Dibujo
  grafico <- gge_biplot_basico(datos, ejes, flechas = FALSE, ...) +
    ggplot2::geom_path(data = circulos_df,                                      # diana
                       ggplot2::aes(x = x, y = y, group = circulo),
                       colour = "grey70") +
    gge_recta( promedio[1], promedio[2],       # AEA
                         colour = "#2a78d6") +
    gge_recta(-promedio[2], promedio[1],       # perpendicular
                         colour = "#2a78d6") +
    ggplot2::annotate("segment", x = 0, y = 0, xend = ideal[1], yend = ideal[2],   # flecha al ideal
                      colour = "#2a78d6", linewidth = 1,
                      arrow = grid::arrow(length = grid::unit(0.3, "cm")))
  
  
  todos <- rbind(coordenadas$genotipos[, c("x", "y")],
                 coordenadas$ambientes[, c("x", "y")])
  lado    <- max(diff(range(todos$x)), diff(range(todos$y)))  # el lado más largo
  centro_x <- mean(range(todos$x))                            # mitad del ancho
  centro_y <- mean(range(todos$y))                            # mitad del alto
  grafico <- grafico + ggplot2::coord_equal(
    xlim = centro_x + c(-1, 1) * lado / 2,   # mismo largo en los dos ejes
    ylim = centro_y + c(-1, 1) * lado / 2)
  
  return(grafico)
}



