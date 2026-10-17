# =============================================================================
# Lectura de datos (CSV, TXT, TSV, XLS, XLSX) y bases de ejemplo.
# =============================================================================

#' Leer una base de datos para el análisis
#'
#' Detecta el formato por la extensión:
#' - CSV, TXT, TSV: detecta solo el separador (coma, punto y coma o tabulador)
#'   y el separador decimal; acepta UTF-8 y Latin-1 (los típicos de Excel en
#'   Windows).
#' - XLS, XLSX: Excel (la hoja que se indique).
#' - SAV, ZSAV, POR: SPSS. DTA: Stata. Los valores perdidos definidos por el
#'   usuario pasan a `NA`.
#' - PARQUET: Apache Parquet.
#'
#' @param ruta ruta del archivo.
#' @param hoja hoja de Excel (nombre o número). Por defecto, la primera.
#' @param nombres_filas si es `TRUE`, la primera columna pasa a ser el nombre de
#'   cada individuo (como "File includes row names" en MULTBIPLOT).
#' @param nombre_original nombre del archivo, si `ruta` es un temporal (Shiny).
#' @param etiquetas solo SPSS y Stata: si es `TRUE`, una variable cuyos valores
#'   tienen todos etiqueta (p. ej. 1 = "Hombre", 2 = "Mujer") se lee con las
#'   etiquetas, como texto, y queda como nominal. Si es `FALSE`, se leen los
#'   códigos numéricos.
#' @export
leer_datos <- function(ruta, hoja = NULL, nombres_filas = TRUE, nombre_original = ruta,
                       etiquetas = TRUE) {
  ext <- tolower(tools::file_ext(nombre_original))
  datos <- switch(
    ext,
    xls = , xlsx = as.data.frame(readxl::read_excel(ruta, sheet = hoja %||% 1),
                                 check.names = FALSE),
    csv = , txt = , tsv = , dat = leer_texto(ruta),
    sav = , zsav = convertir_etiquetadas(haven::read_sav(ruta), etiquetas),
    por = convertir_etiquetadas(haven::read_por(ruta), etiquetas),
    dta = convertir_etiquetadas(haven::read_dta(ruta), etiquetas),
    parquet = convertir_columnas(nanoparquet::read_parquet(ruta)),
    stop("Formato no soportado: .", ext,
         ". Usa CSV, TXT, Excel (XLS, XLSX), SPSS (SAV), Stata (DTA) o Parquet.", call. = FALSE)
  )
  if (!ncol(datos)) stop("El archivo no tiene columnas.", call. = FALSE)
  names(datos) <- trimws(names(datos))
  # Texto: quitar espacios sobrantes (p. ej. nombres de cantones con espacios al final)
  for (j in seq_along(datos)) {
    if (is.character(datos[[j]])) datos[[j]] <- trimws(datos[[j]])
  }
  if (nombres_filas) datos <- primera_columna_a_nombres(datos)
  datos
}

# Formatos con extensiones (SPSS, Stata, Parquet) -> data.frame simple de R:
# números, texto y nada más (sin atributos que estorben a las tablas y gráficos)
convertir_columnas <- function(datos) {
  datos <- as.data.frame(datos, check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(datos)) {
    x <- datos[[j]]
    if (is.factor(x) || inherits(x, c("Date", "POSIXt", "difftime", "hms"))) {
      x <- as.character(x)
    } else if (is.logical(x)) {
      x <- as.character(x)
    } else if (inherits(x, "integer64")) {
      x <- as.numeric(x)
    }
    attributes(x) <- NULL
    datos[[j]] <- x
  }
  datos
}

# SPSS / Stata: variables con etiquetas de valor
convertir_etiquetadas <- function(datos, etiquetas = TRUE) {
  datos <- as.data.frame(datos, check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(datos)) {
    x <- datos[[j]]
    if (haven::is.labelled(x)) {
      codigos <- attr(x, "labels")
      valores <- unique(stats::na.omit(as.vector(haven::zap_labels(x))))
      if (isTRUE(etiquetas) && length(codigos) && length(valores) && all(valores %in% codigos)) {
        x <- as.character(haven::as_factor(x, levels = "labels"))
      } else {
        x <- as.vector(haven::zap_labels(x))
      }
    }
    if (is.character(x)) x[!nzchar(trimws(x))] <- NA   # SPSS guarda vacíos como ""
    datos[[j]] <- x
  }
  convertir_columnas(datos)
}

leer_texto <- function(ruta) {
  lineas <- readLines(ruta, n = 5, warn = FALSE)
  codif <- if (all(validUTF8(lineas))) "UTF-8" else "latin1"
  lineas <- if (codif == "latin1") iconv(lineas, "latin1", "UTF-8") else enc2utf8(lineas)
  cab <- lineas[1]
  cuenta <- c("\t" = lengths(regmatches(cab, gregexpr("\t", cab))),
              ";"  = lengths(regmatches(cab, gregexpr(";", cab))),
              ","  = lengths(regmatches(cab, gregexpr(",", cab))))
  sep <- names(which.max(cuenta))
  if (max(cuenta) == 0) sep <- ""
  muestra <- paste(lineas[-1], collapse = "\n")
  dec <- if (sep != "," && grepl("[0-9],[0-9]", muestra) && !grepl("[0-9]\\.[0-9]", muestra)) "," else "."
  utils::read.table(ruta, header = TRUE, sep = sep, dec = dec, quote = "\"'",
                    check.names = FALSE, stringsAsFactors = FALSE,
                    fileEncoding = if (codif == "UTF-8") "UTF-8-BOM" else "latin1",
                    comment.char = "", na.strings = c("NA", "", "."), strip.white = TRUE)
}

primera_columna_a_nombres <- function(datos) {
  id <- as.character(datos[[1]])
  if (anyNA(id)) id[is.na(id)] <- paste0("Fila", which(is.na(id)))
  if (anyDuplicated(id)) {
    warning("La primera columna tiene nombres repetidos; se numeraron para distinguirlos.",
            call. = FALSE)
    id <- make.unique(id, sep = "_")
  }
  datos <- datos[-1]
  rownames(datos) <- id
  datos
}

#' Bases de ejemplo
#'
#' - `"pisa"`: 90 países × indicadores PISA 2025. `REGION` sirve como
#'   variable de grupo.
#' - `"cantones"`: 218 cantones del Ecuador × 10 variables de energía.
#'   Incluye el atípico URBINA JADO.
#'
#' @param nombre `"pisa"` o `"cantones"`.
#' @export
ejemplo_biplot <- function(nombre = c("pisa", "cantones")) {
  nombre <- match.arg(nombre)
  archivo <- switch(nombre, pisa = "pisa2025_paises.csv", cantones = "cantones_energia.csv")
  ruta <- system.file("extdata", archivo, package = "biplotlab")
  if (!nzchar(ruta)) ruta <- file.path("inst", "extdata", archivo)  # durante el desarrollo
  datos <- utils::read.csv(ruta, check.names = FALSE, stringsAsFactors = FALSE,
                           fileEncoding = "UTF-8")
  primera_columna_a_nombres(datos)
}

#' Detectar columnas de agrupación (nominales)
#'
#' Una columna se propone como nominal (sirve para colorear o agrupar, nunca
#' para construir los ejes) si:
#' - es de texto o factor;
#' - su nombre sugiere un grupo o código (group, grupo, cod, id, clase, region,
#'   tipo, cluster, fase, bloque, tratamiento...);
#' - o es numérica entera con pocos valores distintos en una base grande (por
#'   ejemplo Groupweek: 10 semanas en 70 muestras).
#'
#' Es solo una propuesta: en el aplicativo se puede corregir a mano.
#' @param datos data.frame.
#' @return nombres de las columnas propuestas como nominales.
#' @export
detectar_nominales <- function(datos) {
  # Palabras de grupo al inicio del nombre (Groupweek, Grupo_A, Region...). Las
  # cortas (id, cod, sex, tipo, nivel) solo si van solas o seguidas de _ . o
  # espacio: así un ambiente llamado ID93 no se toma por un identificador.
  patron <- paste0("^(group|grupo|grp|clase|class|region|cluster|fase|phase|bloque|block|trat|treat|",
                   "factor|semana|week|sexo|codigo|code)|^(id|cod|sex|tipo|type|nivel|level)([^a-z0-9]|$)")
  es_nominal <- vapply(names(datos), function(nm) {
    v <- datos[[nm]]
    if (!is.numeric(v)) return(TRUE)
    if (grepl(patron, tolower(nm))) return(TRUE)
    u <- unique(stats::na.omit(v))
    n <- sum(!is.na(v))
    all(u == round(u)) && length(u) <= 12 && n >= 30 && length(u) / n <= 0.2
  }, logical(1))
  names(datos)[es_nominal]
}

#' Resumen de las variables de una base
#' @param datos data.frame.
#' @param nominales columnas marcadas como nominales (de agrupación).
#' @export
resumen_variables <- function(datos, nominales = detectar_nominales(datos)) {
  data.frame(
    Variable   = names(datos),
    Tipo       = ifelse(names(datos) %in% nominales, "Nominal (agrupación)",
                        ifelse(vapply(datos, is.numeric, logical(1)), "Escala (cuantitativa)",
                               "Texto")),
    `Valores distintos` = vapply(datos, function(v) length(unique(stats::na.omit(v))), integer(1)),
    Faltantes  = vapply(datos, function(v) sum(is.na(v)), integer(1)),
    check.names = FALSE, row.names = NULL
  )
}
