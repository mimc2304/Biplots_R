test_that("lee CSV con punto y coma y coma decimal (Excel en español)", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("Pais;A;B;Grupo", "Ecuador;1,5;2,25;x", "Perú;3,0;4,5;y", "Chile ;2,0;1,0;x"), f,
             useBytes = TRUE)
  d <- leer_datos(f)
  expect_equal(rownames(d), c("Ecuador", "Perú", "Chile"))
  expect_equal(d$A, c(1.5, 3, 2))
  expect_type(d$Grupo, "character")
})

test_that("lee CSV en Latin-1 y TXT tabulado", {
  f <- tempfile(fileext = ".csv")
  con <- file(f, open = "w", encoding = "latin1")
  writeLines(c("Cantón,X,Y", "Rumiñahui,1.5,2", "Loja,2.5,3"), con); close(con)
  d <- leer_datos(f)
  expect_equal(rownames(d)[1], "Rumiñahui")
  g <- tempfile(fileext = ".txt")
  writeLines(c("id\tX\tY", "a\t1\t2", "b\t3\t4"), g)
  expect_equal(dim(leer_datos(g)), c(2, 2))
})

test_that("nombres de fila repetidos se numeran con aviso", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("id,X,Y", "a,1,2", "a,3,4", "b,5,6"), f)
  expect_warning(d <- leer_datos(f), "repetidos")
  expect_equal(rownames(d), c("a", "a_1", "b"))
})

test_that("lee SPSS (.sav): etiquetas de valor, perdidos y nombres de fila", {
  d <- data.frame(id = c("P1", "P2", "P3", "P4"),
                  edad = c(20, 35, 41, 999),
                  sexo = haven::labelled(c(1, 2, 2, 1), c(Hombre = 1, Mujer = 2)),
                  likert = haven::labelled(c(1, 5, 3, 4), c(Nada = 1, Mucho = 5)),
                  ciudad = c("Madrid", "", "Quito", "Lima"))
  d$edad <- haven::labelled_spss(d$edad, c(`No sabe` = 999), na_values = 999)
  f <- tempfile(fileext = ".sav")
  haven::write_sav(d, f)
  r <- leer_datos(f)
  expect_equal(rownames(r), c("P1", "P2", "P3", "P4"))
  expect_equal(r$sexo, c("Hombre", "Mujer", "Mujer", "Hombre"))   # todos etiquetados -> texto
  expect_equal(r$likert, c(1, 5, 3, 4))                         # etiquetas parciales -> números
  expect_true(is.na(r$edad[4]))                                  # perdido de usuario -> NA
  expect_true(is.na(r$ciudad[2]))
  expect_null(attributes(r$sexo))
  expect_true("sexo" %in% detectar_nominales(r))
  # Sin etiquetas: códigos numéricos
  r2 <- leer_datos(f, etiquetas = FALSE)
  expect_equal(r2$sexo, c(1, 2, 2, 1))
  expect_match(sintaxis_lectura(list(tipo = "archivo", nombre = "encuesta.sav",
                                     nombres_filas = TRUE, etiquetas = FALSE)),
               "etiquetas = FALSE")
})

test_that("lee Stata (.dta) y Parquet", {
  d <- data.frame(pais = c("ECU", "ESP", "PER"), x = c(1.5, 2.5, 3.5), n = 1:3,
                  region = factor(c("A", "B", "A")))
  f1 <- tempfile(fileext = ".dta")
  haven::write_dta(d, f1)
  r1 <- leer_datos(f1)
  expect_equal(rownames(r1), c("ECU", "ESP", "PER"))
  expect_equal(r1$x, d$x)
  f2 <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(d, f2)
  r2 <- leer_datos(f2)
  expect_equal(rownames(r2), c("ECU", "ESP", "PER"))
  expect_equal(r2$x, d$x)
  expect_equal(r2$region, c("A", "B", "A"))
  expect_true(is.numeric(r2$n))
  expect_error(leer_datos(tempfile(fileext = ".json")), "Formato no soportado")
})

test_that("las bases de ejemplo del panel Datos se leen y se reproducen en la Sintaxis R", {
  for (nm in EJEMPLOS_BIPLOT) {
    d <- ejemplo_biplot(nm)
    expect_s3_class(d, "data.frame")
    expect_gt(nrow(d), 10)
    expect_false(anyDuplicated(rownames(d)) > 0)
    expect_identical(sintaxis_lectura(list(tipo = "ejemplo", nombre = nm)),
                     sprintf("datos <- ejemplo_biplot(\"%s\")", nm))
  }
  expect_equal(dim(ejemplo_biplot("ontario")), c(18, 9))
  expect_true("Species" %in% detectar_nominales(ejemplo_biplot("iris")))
  expect_true(all(c("tratamiento", "temperatura") %in% detectar_nominales(ejemplo_biplot("cesped"))))
})
