# =============================================================================
# Clúster sobre las coordenadas del biplot
# Clasifica individuos (filas) o variables (columnas) a partir de sus
# coordenadas factoriales y valida el resultado con la correlación cofenética,
# la silueta y los perfiles de cada grupo. Los grupos se dibujan como
# envolventes convexas sobre el biplot.
#
# Validado con la matriz de fitoplancton del embalse de Riaño (material de
# clase): Ward sobre las coordenadas de los 70 lugares en el plano 1-2 da una
# correlación cofenética de 0,67941, la misma que reporta el material.
# =============================================================================

#' Métodos de agrupamiento disponibles
#' @format vector de caracteres con nombre.
#' @export
METODOS_CLUSTER <- c(
  "Ward (varianza mínima)"   = "ward.D2",
  "Enlace completo"          = "complete",
  "Enlace promedio (UPGMA)"  = "average",
  "Enlace simple"            = "single",
  "Centroide"                = "centroid",
  "K-medias"                 = "kmeans"
)
#' Número máximo de elementos para el clúster (memoria de la matriz de distancias)
#' @export
LIMITE_CLUSTER <- 5000

#' Clúster sobre las coordenadas de un biplot
#'
#' @param res objeto devuelto por [hj_biplot()].
#' @param sobre `"filas"` (individuos) o `"columnas"` (variables).
#' @param ejes ejes cuyas coordenadas se usan. Por defecto, los dos primeros.
#' @param metodo uno de [METODOS_CLUSTER].
#' @param k número de grupos.
#' @param semilla semilla para k-medias.
#' @return objeto de clase `cluster_biplot`.
#' @export
#' @examples
#' pisa <- ejemplo_biplot("pisa")
#' res <- hj_biplot(pisa, variables = names(pisa)[3:12])
#' cl <- cluster_biplot(res, k = 4)
#' cl
#' grafico_biplot(res, grupos = cl$grupos, envolventes = TRUE)
cluster_biplot <- function(res, sobre = c("filas", "columnas"), ejes = 1:2,
                           metodo = "ward.D2", k = 3, semilla = 123) {
  sobre <- match.arg(sobre)
  if (!metodo %in% METODOS_CLUSTER) stop("Método no reconocido: ", metodo, call. = FALSE)
  M0 <- if (sobre == "filas") res$filas else res$columnas
  ejes <- as.integer(ejes)
  if (any(ejes > ncol(M0))) {
    stop("Solo hay ", ncol(M0), " ejes calculados en el biplot.", call. = FALSE)
  }
  M <- M0[, ejes, drop = FALSE]
  n <- nrow(M)
  k <- as.integer(k)
  if (k < 2 || k >= n) stop("El número de grupos debe estar entre 2 y ", n - 1, ".", call. = FALSE)
  # La matriz de distancias ocupa n² celdas: con 10 000 individuos son unos 4 GB,
  # la memoria completa de una app gratuita en Posit Connect Cloud
  if (n > LIMITE_CLUSTER) {
    stop("El clúster admite hasta ", format(LIMITE_CLUSTER, big.mark = "."), " elementos (hay ",
         format(n, big.mark = "."), "): la matriz de distancias crece con n² y no cabe en memoria. ",
         "Agrupa una muestra o excluye individuos.", call. = FALSE)
  }

  d <- stats::dist(M)
  hc <- NULL
  cofenetica <- NA_real_
  if (metodo == "kmeans") {
    set.seed(semilla)
    km <- stats::kmeans(M, centers = k, nstart = 25)
    grupos_num <- km$cluster
  } else {
    hc <- stats::hclust(d, method = metodo)
    grupos_num <- stats::cutree(hc, k = k)
    cofenetica <- stats::cor(stats::cophenetic(hc), d)
  }
  # Numerar los grupos por orden de aparición en el dendrograma (o en los datos)
  orden <- if (!is.null(hc)) unique(grupos_num[hc$order]) else unique(grupos_num)
  grupos_num <- match(grupos_num, orden)
  grupos <- factor(paste0("C", grupos_num), levels = paste0("C", seq_len(k)))
  names(grupos) <- rownames(M)

  sil <- cluster::silhouette(grupos_num, d)
  silueta <- if (k > 1) mean(sil[, "sil_width"]) else NA_real_

  # Elección del número de grupos: silueta media y suma de cuadrados intra
  kmax <- min(10, n - 1)
  eleccion <- do.call(rbind, lapply(2:kmax, function(kk) {
    g <- if (metodo == "kmeans") {
      set.seed(semilla); stats::kmeans(M, centers = kk, nstart = 25)$cluster
    } else stats::cutree(hc, k = kk)
    wss <- sum(vapply(split(as.data.frame(M), g), function(x) {
      sum(scale(as.matrix(x), scale = FALSE)^2)
    }, numeric(1)))
    data.frame(Grupos = kk, `Silueta media` = mean(cluster::silhouette(g, d)[, "sil_width"]),
               `SC intra` = wss, check.names = FALSE)
  }))

  tam <- as.data.frame(table(Grupo = grupos), responseName = "Tamaño")
  tam$Silueta <- tapply(sil[, "sil_width"], grupos, mean)[as.character(tam$Grupo)]
  tam$Miembros <- vapply(levels(grupos), function(g) {
    paste(names(grupos)[grupos == g], collapse = ", ")
  }, character(1))

  centro <- function(X) {
    out <- stats::aggregate(as.data.frame(X), by = list(Grupo = grupos), FUN = mean)
    rbind(out, cbind(Grupo = "Total", as.data.frame(t(colMeans(X)))))
  }
  perfiles <- if (sobre == "filas") centro(res$original) else NULL
  centroides <- centro(M)

  structure(list(
    sobre = sobre, ejes = ejes, metodo = metodo, k = k, grupos = grupos,
    hclust = hc, cofenetica = cofenetica, silueta = silueta, silueta_detalle = sil,
    eleccion_k = eleccion, tamanos = tam, perfiles = perfiles, centroides = centroides,
    coordenadas = M
  ), class = "cluster_biplot")
}

#' @export
print.cluster_biplot <- function(x, ...) {
  etiqueta <- names(METODOS_CLUSTER)[METODOS_CLUSTER == x$metodo]
  cat(sprintf("Clúster sobre las coordenadas de las %s (ejes %s) - %s - %d grupos\n",
              x$sobre, paste(x$ejes, collapse = ", "), etiqueta, x$k))
  if (!is.na(x$cofenetica)) cat(sprintf("Correlación cofenética: %.5f\n", x$cofenetica))
  cat(sprintf("Silueta media: %.3f\n\n", x$silueta))
  print(x$tamanos[, c("Grupo", "Tamaño", "Silueta")], row.names = FALSE)
  invisible(x)
}

#' Dendrograma de un clúster sobre coordenadas
#'
#' Dibuja el dendrograma (gráficos base de R) con los grupos recuadrados en
#' los mismos colores que el biplot.
#' @param cl objeto devuelto por [cluster_biplot()].
#' @param colores colores de los grupos (con nombre C1, C2...).
#' @param cex tamaño de las etiquetas.
#' @export
dendrograma_cluster <- function(cl, colores = colores_por_defecto(levels(cl$grupos)), cex = 0.7) {
  if (is.null(cl$hclust)) stop("K-medias no produce dendrograma.", call. = FALSE)
  hc <- cl$hclust
  op <- graphics::par(mar = c(2, 4, 3, 1), family = "sans")
  on.exit(graphics::par(op))
  plot(hc, hang = -1, cex = cex, main = "", sub = "", xlab = "",
       ylab = "Altura (distancia de fusión)", col = "#52514e", col.axis = "#8a8984")
  graphics::title(main = sprintf("Dendrograma - correlación cofenética %s",
                                 format(round(cl$cofenetica, 4), decimal.mark = ",")),
                  font.main = 1, cex.main = 1, col.main = "#1f1f1d")
  # Recuadros por grupo, en el orden del dendrograma
  grupos_orden <- unique(as.character(cl$grupos[hc$order]))
  stats::rect.hclust(hc, k = cl$k, border = colores[grupos_orden])
  invisible(NULL)
}
