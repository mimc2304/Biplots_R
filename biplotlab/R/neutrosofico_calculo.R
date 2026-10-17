# =============================================================================
# HJ-Biplot de puntuaciones neutrosóficas: código del compañero (archivo
# original modulo_hj_neutrosofico.R). Se integra sin cambios en los cálculos;
# solo se quitó el ejemplo que se ejecutaba al final del archivo.
# =============================================================================

# Modulo de ejecución
# Enfoque: puntuacion T-I-F, centrado y SVD ordinaria con coordenadas HJ.

puntuar_neutrosofico <- function(T, I, F) {
  
  # Comprobar que las tres entradas sean matrices numéricas
  entradas <- list(T = T, I = I, F = F)
  
  for (nombre in names(entradas)) {
    matriz <- entradas[[nombre]]
    
    if (!is.matrix(matriz) || !is.numeric(matriz)) {
      stop(nombre, " debe ser una matriz numérica.")
    }
    
    if (length(matriz) == 0L ||
        any(!is.finite(matriz))) {
      stop(nombre, " debe contener datos finitos, sin NA.")
    }
    
    if (any(matriz < 0 | matriz > 1)) {
      stop(nombre, " debe contener valores entre 0 y 1.")
    }
  }
  
  # Comprobar dimensiones y etiquetas
  if (!identical(dim(T), dim(I)) ||
      !identical(dim(T), dim(F))) {
    stop("T, I y F deben tener las mismas dimensiones.")
  }
  
  if (!identical(dimnames(T), dimnames(I)) ||
      !identical(dimnames(T), dimnames(F))) {
    stop("T, I y F deben tener las mismas etiquetas y orden.")
  }
  
  # Puntuación para el análisis centrado
  S <- T - I - F
  
  return(S)
}


hj_neutrosofico <- function(T, I, F) {
  
  # 1. Validar las entradas y calcular la puntuación
  S <- puntuar_neutrosofico(T, I, F)
  
  if (nrow(S) < 3 || ncol(S) < 2) {
    stop("Se necesitan al menos 3 observaciones y 2 variables.")
  }
  
  # Crear etiquetas cuando no existan
  
  if (is.null(rownames(S))) {
    rownames(S) <- paste0("Obs_", seq_len(nrow(S)))
  }
  
  if (is.null(colnames(S))) {
    colnames(S) <- paste0("Var_", seq_len(ncol(S)))
  }
  
  # 2. Centrar cada columna, sin estandarizar
  
  Z <- scale(S, center = TRUE, scale = FALSE)
  
  # 3. Descomposición en valores singulares
  
  des <- svd(Z)
  
  if (max(des$d) == 0) {
    stop("La matriz centrada no tiene variación.")
  }
  
  tolerancia <- max(dim(Z)) * max(des$d) *
    .Machine$double.eps
  
  conservar <- which(des$d > tolerancia)
  d <- des$d[conservar]
  
  # 4. Coordenadas HJ: J = U D y H = V D
  
  J <- sweep(
    des$u[, conservar, drop = FALSE],
    2, d, "*"
  )
  
  H <- sweep(
    des$v[, conservar, drop = FALSE],
    2, d, "*"
  )
  
  nombres_ejes <- paste0("Eje_", seq_along(d))
  
  dimnames(J) <- list(rownames(S), nombres_ejes)
  dimnames(H) <- list(colnames(S), nombres_ejes)
  
  # 5. Inercia explicada por cada eje
  
  porcentaje <- 100 * d^2 / sum(des$d^2)
  
  inercia <- data.frame(
    Eje = seq_along(d),
    Porcentaje = porcentaje,
    Acumulado = cumsum(porcentaje)
  )
  
  # 6. Reconstruir Z usando U D V', no J H'
  
  reconstruida <- sweep(des$u, 2, des$d, "*") %*%
    t(des$v)
  
  error <- max(abs(Z - reconstruida))
  
  # 7. Devolver resultados para tablas y gráficos
  
  return(list(
    puntuaciones = S,
    matriz_centrada = Z,
    coordenadas_observaciones = J,
    coordenadas_variables = H,
    inercia = inercia,
    indeterminacion_observaciones = rowMeans(I),
    indeterminacion_variables = colMeans(I),
    error_reconstruccion = error,
    convenio = "HJ: J = U D; H = V D",
    regla_puntuacion = "S = T - I - F"
  ))
}


indicadores_por_eje <- function(resultado) {
  
  calcular <- function(A) {
    
    cuadrados <- A^2
  
# Calidad: dividir cada fila por su total
    
    total_fila <- rowSums(cuadrados)
    total_fila[total_fila == 0] <- NA_real_
    
    calidad <- 100 * sweep(
      cuadrados, 1, total_fila, "/"
    )
    
# Contribución: dividir cada columna por su total
    
    total_eje <- colSums(cuadrados)
    total_eje[total_eje == 0] <- NA_real_
    
    contribucion <- 100 * sweep(
      cuadrados, 2, total_eje, "/"
    )
    
    list(
      calidad_por_eje = calidad,
      contribucion_al_eje = contribucion
    )
  }
  
  list(
    observaciones = calcular(
      resultado$coordenadas_observaciones
    ),
    variables = calcular(
      resultado$coordenadas_variables
    )
  )
}


.validar_ejes_hj <- function(resultado, ejes) {
  if (!is.numeric(ejes) || length(ejes) != 2L ||
      any(!is.finite(ejes)) || anyDuplicated(ejes) ||
      any(ejes != floor(ejes)) ||
      any(ejes < 1 | ejes > ncol(resultado$coordenadas_observaciones))) {
    stop("Selecciona dos ejes distintos que existan.")
  }
}

calidades_hj <- function(resultado, ejes = c(1, 2)) {
  .validar_ejes_hj(resultado, ejes)
  calcular <- function(A) {
    total <- rowSums(A^2)
    total[total == 0] <- NA_real_
    100 * rowSums(A[, ejes, drop = FALSE]^2) / total
  }
  J <- resultado$coordenadas_observaciones
  H <- resultado$coordenadas_variables
  list(observaciones = data.frame(Observacion = rownames(J),
         Calidad_pct = calcular(J), row.names = NULL),
       variables = data.frame(Variable = rownames(H),
         Calidad_pct = calcular(H), row.names = NULL), ejes = ejes)
}

graficar_hj_neutrosofico <- function(resultado, ejes = c(1, 2),
    calidad_minima = 0, calidad_minima_puntos = calidad_minima,
    calidad_minima_vectores = calidad_minima,
    ocultar_puntos = character(), ocultar_variables = character(),
    grupos = NULL, colores_grupos = NULL,
    color_puntos = "steelblue", color_vectores = "firebrick",
    etiquetas_puntos = FALSE, etiquetas_variables = TRUE,
    escala_vectores = 1, titulo = "HJ-Biplot de puntuaciones neutrosoficas") {
  .validar_ejes_hj(resultado, ejes)
  for (q in list(calidad_minima, calidad_minima_puntos, calidad_minima_vectores)) {
    if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 || q > 100)
      stop("La calidad minima debe estar entre 0 y 100.")
  }
  if (!is.numeric(escala_vectores) || length(escala_vectores) != 1L ||
      !is.finite(escala_vectores) || escala_vectores <= 0)
    stop("escala_vectores debe ser positiva y finita.")
  J <- resultado$coordenadas_observaciones
  H <- resultado$coordenadas_variables
  if (length(color_puntos) != 1L || length(color_vectores) != 1L)
    stop("Usa un color de puntos y uno de vectores; para grupos usa colores_grupos.")
  if (!all(ocultar_puntos %in% rownames(J)) ||
      !all(ocultar_variables %in% rownames(H)))
    stop("Los nombres a ocultar deben existir en las coordenadas.")
  cal <- calidades_hj(resultado, ejes)
  filas <- which(!is.na(cal$observaciones$Calidad_pct) &
    cal$observaciones$Calidad_pct >= calidad_minima_puntos &
    !(rownames(J) %in% ocultar_puntos))
  vars <- which(!is.na(cal$variables$Calidad_pct) &
    cal$variables$Calidad_pct >= calidad_minima_vectores &
    !(rownames(H) %in% ocultar_variables))
  if (length(filas) + length(vars) == 0L)
    stop("No hay elementos visibles con estas opciones.")
  colores <- rep(color_puntos, nrow(J))
  if (!is.null(grupos)) {
    if (!is.atomic(grupos) || length(grupos) != nrow(J) || anyNA(grupos))
      stop("grupos debe tener una etiqueta no ausente por observacion.")
    grupos <- as.character(grupos)
    niveles <- unique(grupos)
    if (is.null(colores_grupos)) {
      colores_grupos <- setNames(grDevices::hcl.colors(length(niveles), "Dark 3"), niveles)
    } else if (is.null(names(colores_grupos))) {
      if (length(colores_grupos) != length(niveles))
        stop("Indica un color por grupo o un vector de colores con nombres.")
      names(colores_grupos) <- niveles
    }
    if (!all(niveles %in% names(colores_grupos)) || anyNA(colores_grupos[niveles]))
      stop("Faltan colores para algunos grupos.")
    colores <- unname(colores_grupos[grupos])
  }
  grDevices::col2rgb(c(colores, color_vectores))
  puntos <- J[filas, ejes, drop = FALSE]
  vectores <- H[vars, ejes, drop = FALSE] * escala_vectores
  if (any(!is.finite(vectores))) stop("La escala de vectores es demasiado grande.")
  ampliar <- function(x) {
    r <- range(c(0, x)); margen <- max(diff(r) * 0.2, 0.1)
    r + c(-margen, margen)
  }
  plot(0, 0, type = "n", asp = 1,
       xlim = ampliar(c(puntos[, 1], vectores[, 1])),
       ylim = ampliar(c(puntos[, 2], vectores[, 2])),
       xlab = sprintf("Eje %d (%.2f%%)", ejes[1], resultado$inercia$Porcentaje[ejes[1]]),
       ylab = sprintf("Eje %d (%.2f%%)", ejes[2], resultado$inercia$Porcentaje[ejes[2]]),
       main = titulo)
  abline(h = 0, v = 0, col = "gray80", lty = 2)
  if (length(filas)) {
    points(puntos, pch = 21, bg = colores[filas],
           cex = 1 + 4 * resultado$indeterminacion_observaciones[filas])
    if (etiquetas_puntos)
      text(puntos, labels = rownames(puntos), pos = 3, cex = 0.65)
  }
  if (length(vars)) {
    no_nulos <- rowSums(vectores^2) > 0
    arrows(0, 0, vectores[no_nulos, 1], vectores[no_nulos, 2],
           col = color_vectores, length = 0.08)
    if (etiquetas_variables)
      text(vectores, labels = rownames(vectores), pos = 3,
           col = color_vectores, cex = 0.8)
  }
  if (!is.null(grupos))
    legend("topright", legend = niveles, pt.bg = colores_grupos[niveles],
           pch = 21, bty = "n", cex = 0.8)
  if (escala_vectores != 1)
    mtext(paste("Flechas multiplicadas por", escala_vectores), side = 3, cex = 0.7)
  invisible(list(calidades = cal, puntos_visibles = rownames(puntos),
                 variables_visibles = rownames(vectores), escala_vectores = escala_vectores))
}

exportar_hj <- function(resultado, carpeta = "resultados_hj", ejes = c(1, 2)) {
  .validar_ejes_hj(resultado, ejes)
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  guardar <- function(x, nombre) {
    ruta <- file.path(carpeta, nombre)
    if (file.exists(ruta)) stop("Ya existe ", ruta, "; elige una carpeta nueva.")
    write.csv(x, ruta, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  
  # Comprobar
  
  nombres <- c("inercia.csv", "coordenadas_observaciones.csv", "coordenadas_variables.csv",
    "calidades_observaciones.csv", "calidades_variables.csv", "calidad_por_eje_observaciones.csv",
    "calidad_por_eje_variables.csv", "contribucion_al_eje_observaciones.csv",
    "contribucion_al_eje_variables.csv", "indeterminacion_variables.csv", "resultado.rds", "biplot.pdf")
  
  if (any(file.exists(file.path(carpeta, nombres))))
    stop("La carpeta ya contiene resultados; elige otra carpeta para conservarlos.")
  guardar(resultado$inercia, "inercia.csv")
  J <- resultado$coordenadas_observaciones; H <- resultado$coordenadas_variables
  guardar(data.frame(Observacion = rownames(J), J, row.names = NULL), "coordenadas_observaciones.csv")
  guardar(data.frame(Variable = rownames(H), H, row.names = NULL), "coordenadas_variables.csv")
  cal <- calidades_hj(resultado, ejes)
  guardar(cal$observaciones, "calidades_observaciones.csv")
  guardar(cal$variables, "calidades_variables.csv")
  ind <- indicadores_por_eje(resultado)
  for (tipo in names(ind)) for (medida in names(ind[[tipo]])) {
    A <- ind[[tipo]][[medida]]
    guardar(data.frame(Elemento = rownames(A), A, row.names = NULL),
            paste0(medida, "_", tipo, ".csv"))
  }
  
  guardar(data.frame(Variable = rownames(H), Media_I = resultado$indeterminacion_variables),
          "indeterminacion_variables.csv")
 
   saveRDS(resultado, file.path(carpeta, "resultado.rds"))
  grDevices::pdf(file.path(carpeta, "biplot.pdf"), width = 10, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  graficar_hj_neutrosofico(resultado, ejes = ejes)
  invisible(normalizePath(carpeta))
}

