# =============================================================================
# Edición de la hoja de datos dentro del aplicativo.
# Funciones puras (sin Shiny): las usa el panel Datos y las prueba testthat.
# Editar no toca el archivo original: los análisis usan la copia editada, que se
# puede descargar en CSV o Excel.
# =============================================================================

#' Convertir un texto escrito en la hoja a número
#'
#' Acepta coma o punto decimal y espacios. Una celda vacía (o "NA") es `NA`.
#' @param x texto.
#' @return número, `NA` si la celda queda vacía, o `NULL` si no es un número.
#' @noRd
texto_a_numero <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x) || toupper(x) == "NA") return(NA_real_)
  x <- gsub("\\s", "", x)
  if (grepl(",", x) && !grepl("\\.", x)) x <- sub(",", ".", x, fixed = TRUE)
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) NULL else v
}

#' Nombre que no se repite
#' @noRd
nombre_unico <- function(base, existentes) {
  if (!base %in% existentes) return(base)
  i <- 2
  while (paste0(base, "_", i) %in% existentes) i <- i + 1
  paste0(base, "_", i)
}

#' Editar una celda de la hoja de datos
#'
#' @param df data.frame (los nombres de fila son los individuos).
#' @param fila número de fila.
#' @param columna número de columna; 0 es el nombre del individuo.
#' @param valor lo que el usuario escribió.
#' @return el data.frame editado. Si el valor no es válido, se detiene con un
#'   mensaje en castellano.
#' @export
editar_celda <- function(df, fila, columna, valor) {
  stopifnot(fila >= 1, fila <= nrow(df), columna >= 0, columna <= ncol(df))
  valor <- trimws(as.character(valor))
  if (columna == 0) {
    if (!nzchar(valor)) stop("El nombre del individuo no puede quedar vacío.", call. = FALSE)
    if (valor %in% rownames(df)[-fila]) {
      stop("Ya hay un individuo llamado \"", valor, "\".", call. = FALSE)
    }
    rownames(df)[fila] <- valor
    return(df)
  }
  x <- df[[columna]]
  if (is.numeric(x)) {
    v <- texto_a_numero(valor)
    if (is.null(v)) {
      stop("La columna \"", names(df)[columna], "\" es numérica: \"", valor,
           "\" no es un número.", call. = FALSE)
    }
    if (is.integer(x) && !is.na(v) && v != round(v)) x <- as.numeric(x)
    x[fila] <- if (is.integer(x)) as.integer(v) else v
  } else {
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x)) x <- as.character(x)
    x[fila] <- if (nzchar(valor) && toupper(valor) != "NA") valor else NA
  }
  df[[columna]] <- x
  df
}

#' Agregar una fila vacía
#'
#' @param df data.frame.
#' @param despues posición después de la cual se inserta (0 = al principio).
#'   Por defecto, al final.
#' @param nombre nombre del individuo nuevo; por defecto "Nuevo", "Nuevo_2"...
#' @return el data.frame con la fila nueva (celdas en `NA`).
#' @export
agregar_fila <- function(df, despues = nrow(df), nombre = NULL) {
  despues <- max(0, min(as.integer(despues), nrow(df)))
  nombre <- nombre_unico(nombre %||% "Nuevo", rownames(df))
  nueva <- df[NA_integer_, , drop = FALSE]
  rownames(nueva) <- nombre
  orden <- append(seq_len(nrow(df)), nrow(df) + 1L, after = despues)
  res <- rbind(df, nueva)[orden, , drop = FALSE]
  res
}

#' Eliminar filas
#' @param df data.frame.
#' @param filas números de fila.
#' @export
eliminar_filas <- function(df, filas) {
  filas <- intersect(as.integer(filas), seq_len(nrow(df)))
  if (!length(filas)) return(df)
  if (length(filas) >= nrow(df)) stop("No se pueden eliminar todas las filas.", call. = FALSE)
  df[-filas, , drop = FALSE]
}

#' Agregar una columna
#'
#' @param df data.frame.
#' @param nombre nombre de la columna.
#' @param tipo `"numerica"` (de escala) o `"texto"` (nominal).
#' @param valor valor inicial de todas las celdas (vacío = `NA`).
#' @export
agregar_columna <- function(df, nombre, tipo = c("numerica", "texto"), valor = "") {
  tipo <- match.arg(tipo)
  nombre <- trimws(nombre)
  if (!nzchar(nombre)) stop("Escribe un nombre para la columna.", call. = FALSE)
  if (nombre %in% names(df)) stop("Ya existe una columna llamada \"", nombre, "\".", call. = FALSE)
  valor <- trimws(as.character(valor %||% ""))
  if (tipo == "numerica") {
    v <- texto_a_numero(valor)
    if (is.null(v)) stop("El valor inicial \"", valor, "\" no es un número.", call. = FALSE)
    df[[nombre]] <- rep(v, nrow(df))
  } else {
    df[[nombre]] <- rep(if (nzchar(valor)) valor else NA_character_, nrow(df))
  }
  df
}

#' Eliminar una columna
#' @param df data.frame.
#' @param nombre nombre de la columna.
#' @export
eliminar_columna <- function(df, nombre) {
  if (!nombre %in% names(df)) return(df)
  if (ncol(df) == 1) stop("No se puede eliminar la única columna.", call. = FALSE)
  df[[nombre]] <- NULL
  df
}

#' Tabla lista para guardar: el nombre de cada individuo como primera columna
#'
#' Así el archivo descargado se vuelve a leer con [leer_datos()] tal cual.
#' @param df data.frame.
#' @export
datos_para_guardar <- function(df) {
  cab <- nombre_unico("Individuo", names(df))
  out <- data.frame(rownames(df), check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- cab
  cbind(out, df)
}

#' Nombre del archivo editado: "iris.csv" pasa a "iris_editado.csv"
#' @noRd
nombre_editado <- function(nombre, ext = "csv") {
  paste0(tools::file_path_sans_ext(basename(nombre %||% "datos")), "_editado.", ext)
}
