# Edición de la hoja de datos (panel Datos)

base <- function() {
  d <- data.frame(x = c(1.5, 2, 3), n = c(1L, 2L, 3L), g = c("a", "b", "a"),
                  stringsAsFactors = FALSE)
  rownames(d) <- c("A", "B", "C")
  d
}

test_that("editar celdas: números con coma o punto, texto, vacío y nombres", {
  d <- editar_celda(base(), 2, 1, "4,25")
  expect_equal(d$x[2], 4.25)
  expect_equal(editar_celda(base(), 1, 1, " 7.5 ")$x[1], 7.5)
  expect_true(is.na(editar_celda(base(), 3, 1, "")$x[3]))
  expect_error(editar_celda(base(), 1, 1, "abc"), "no es un número")
  # Entera: sigue entera con un entero; pasa a decimal si hace falta
  expect_type(editar_celda(base(), 1, 2, "9")$n, "integer")
  expect_equal(editar_celda(base(), 1, 2, "2,5")$n, c(2.5, 2, 3))
  expect_equal(editar_celda(base(), 3, 3, "c")$g, c("a", "b", "c"))
  expect_equal(rownames(editar_celda(base(), 2, 0, "Bnuevo")), c("A", "Bnuevo", "C"))
  expect_error(editar_celda(base(), 2, 0, "A"), "Ya hay un individuo")
})

test_that("agregar y eliminar filas", {
  d <- agregar_fila(base(), despues = 1)
  expect_equal(rownames(d), c("A", "Nuevo", "B", "C"))
  expect_true(all(is.na(d["Nuevo", ])))
  expect_equal(rownames(agregar_fila(d)), c("A", "Nuevo", "B", "C", "Nuevo_2"))
  expect_equal(rownames(agregar_fila(base(), despues = 0))[1], "Nuevo")
  expect_equal(rownames(eliminar_filas(d, c(2, 4))), c("A", "B"))
  expect_error(eliminar_filas(base(), 1:3), "todas las filas")
})

test_that("agregar y eliminar columnas", {
  d <- agregar_columna(base(), "y", "numerica", "0,5")
  expect_equal(d$y, rep(0.5, 3))
  d <- agregar_columna(d, "zona", "texto", "Norte")
  expect_equal(d$zona, rep("Norte", 3))
  expect_true(all(is.na(agregar_columna(base(), "w", "numerica")$w)))
  expect_error(agregar_columna(base(), "x"), "Ya existe")
  expect_error(agregar_columna(base(), "w", "numerica", "uno"), "no es un número")
  expect_equal(names(eliminar_columna(d, "x")), c("n", "g", "y", "zona"))
})

test_that("los datos editados se guardan y se vuelven a leer igual", {
  d <- editar_celda(agregar_fila(base()), 4, 1, "10")
  d <- editar_celda(d, 4, 3, "b")
  f <- tempfile(fileext = ".csv")
  utils::write.csv(datos_para_guardar(d), f, row.names = FALSE, fileEncoding = "UTF-8")
  r <- leer_datos(f)
  expect_equal(rownames(r), rownames(d))
  expect_equal(r$x, d$x)
  expect_equal(r$g, d$g)
  expect_equal(nombre_editado("iris.csv"), "iris_editado.csv")
  expect_match(sintaxis_lectura(list(tipo = "archivo", nombre = "iris_editado.csv",
                                     nombres_filas = TRUE, editado = TRUE)),
               "se editaron en la aplicación")
})
