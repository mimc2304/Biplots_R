# Idioma de los resultados: el selector ES | EN solo cambia etiquetas, nunca números

test_that("en español no se traduce nada", {
  expect_equal(traducir(c("Eje 1", "Valor propio"), "es"), c("Eje 1", "Valor propio"))
  t <- tablas_biplot(hj_biplot(ejemplo_biplot("iris")))$inercia
  expect_identical(traducir_tabla(t, "es"), t)
})

test_that("en inglés se traducen columnas, ejes y decimales", {
  expect_equal(traducir(c("Eje 1 (41,76 %)", "Valor propio", "Plano 1-2", "% Inercia"), "en"),
               c("Axis 1 (41.76 %)", "Eigenvalue", "Plane 1-2", "% Inertia"))
  expect_equal(traducir("CRFE plano 1-2 (‰)", "en"), "CRFE plane 1-2 (‰)")
  expect_equal(marca_decimal("en"), ".")
  expect_equal(marca_decimal("es"), ",")
})

test_that("las tablas en inglés conservan los números y los nombres de los datos", {
  r <- hj_biplot(ejemplo_biplot("pisa"))
  t <- tablas_biplot(r)
  en <- traducir_tabla(t$coordenadas_filas, "en")
  expect_equal(names(en)[1:2], c("Individual", "Axis 1"))
  expect_equal(en[[1]], t$coordenadas_filas[[1]])            # nombres de los países sin tocar
  expect_equal(en[["Axis 1"]], t$coordenadas_filas[["Eje 1"]])
  inr <- traducir_tabla(t$inercia, "en")
  expect_equal(names(inr), c("Axis", "Eigenvalue", "Singular value", "% Inertia", "Cumulative %"))
})

test_that("los textos fijos de las celdas se traducen y los datos no", {
  df <- data.frame(Genotipo = c("Eje", "Encima del promedio"), Posición = c("Debajo del promedio", "Eje 2"))
  en <- traducir_tabla(df, "en")
  expect_equal(names(en), c("Genotype", "Position"))
  expect_equal(en$Position, c("Below average", "Axis 2"))
})

test_that("el gráfico del biplot cambia títulos de ejes en inglés", {
  g <- grafico_biplot(hj_biplot(ejemplo_biplot("iris")))
  en <- traducir_grafico(g, "en")
  expect_match(en$labels$x, "^Axis 1 [(][0-9]+[.][0-9]+ %[)]$")
  expect_identical(traducir_grafico(g, "es"), g)
})

test_that("el Excel en inglés usa hojas y columnas en inglés", {
  t <- tablas_biplot(hj_biplot(ejemplo_biplot("iris")))
  f <- tempfile(fileext = ".xlsx")
  guardar_xlsx(list("Inercia" = t$inercia, "Coordenadas individuos" = t$coordenadas_filas), f, idioma = "en")
  expect_equal(readxl::excel_sheets(f), c("Inertia", "Individual coordinates"))
  expect_equal(names(readxl::read_excel(f, "Inertia"))[2], "Eigenvalue")
})

test_that("fuera de Shiny el idioma es español", {
  expect_equal(idioma_actual(), "es")
})

test_that("los valores de los datos no se traducen aunque coincidan con una etiqueta", {
  df <- data.frame(Grupo = c("Media", "Grupo 1", "Valor"), Contraste = c("Wilks (aprox. χ² de Bartlett)", "Pillai", "sí"))
  en <- traducir_tabla(df, "en")
  expect_equal(names(en), c("Group", "Test"))
  expect_equal(en$Group, c("Media", "Grupo 1", "Valor"))
  expect_equal(en$Test, c("Wilks (Bartlett χ² approx.)", "Pillai", "yes"))
})

test_that("canónico y equipo: títulos y columnas en inglés", {
  expect_equal(traducir(c("Regiones de confianza al 95 % (elipses de Krzanowski, escala W)",
                          "r dentro, Eje 2", "Orientación eje mayor (°)", "DPCA (componentes disjuntos)"), "en"),
               c("95 % confidence regions (Krzanowski ellipses, W scale)", "r within, Axis 2",
                 "Major axis orientation (°)", "DPCA (disjoint components)"))
  expect_equal(traducir(c("Eje1", "F por barrido (mejor arranque)"), "en"), c("Axis 1", "F per sweep (best start)"))
  # una frase libre no se traduce a medias
  expect_equal(traducir("Una frase con eje y plano", "en"), "Una frase con eje y plano")
})

test_that("tabla_apa del canónico se muestra en inglés", {
  df <- data.frame(Contraste = "Wilks (aprox. χ² de Bartlett)", `gl num.` = 2, p = 0.01, check.names = FALSE)
  shiny::withReactiveDomain(list(userData = list(idioma = function() "en")), {
    html <- as.character(tabla_apa(df, "Lambda de Wilks con la aproximación χ² de Bartlett", rownames = FALSE))
  })
  expect_match(html, "lambda with Bartlett", fixed = TRUE)
  expect_match(html, "num. df", fixed = TRUE)
})
