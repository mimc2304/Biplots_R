variables_pisa <- c("MATH", "READ", "SCIE", "CMPS", "CURIO", "PERSEV",
                    "COGABIL", "GOALSET", "SELFREG", "ENPROBS")

test_that("convenio de signos: referencia e inversión no cambian la geometría", {
  pisa <- ejemplo_biplot("pisa")
  r0 <- hj_biplot(pisa, variables_pisa)
  r1 <- hj_biplot(pisa, variables_pisa, referencia = "MATH")
  expect_true(all(r1$columnas["MATH", ] > 0))
  r2 <- hj_biplot(pisa, variables_pisa, invertir_ejes = 2)
  expect_equal(r2$filas[, 2], -r0$filas[, 2])
  expect_equal(r2$calidad_filas, r0$calidad_filas)
  expect_equal(r1$inercia, r0$inercia)
})

test_that("clúster sobre coordenadas: grupos, silueta y elección de k", {
  pisa <- ejemplo_biplot("pisa")
  r <- hj_biplot(pisa, variables_pisa)
  cl <- cluster_biplot(r, k = 4)
  expect_s3_class(cl, "cluster_biplot")
  expect_length(cl$grupos, 90)
  expect_equal(nlevels(cl$grupos), 4)
  expect_true(cl$cofenetica > 0 && cl$cofenetica <= 1)
  expect_equal(nrow(cl$eleccion_k), 9)
  km <- cluster_biplot(r, k = 3, metodo = "kmeans")
  expect_true(is.na(km$cofenetica))
  clc <- cluster_biplot(r, sobre = "columnas", k = 3)
  expect_length(clc$grupos, 10)
  expect_error(cluster_biplot(r, k = 1), "entre 2")
})

test_that("bootstrap: reproducible, intervalos coherentes", {
  pisa <- ejemplo_biplot("pisa")
  b1 <- bootstrap_biplot(pisa, variables_pisa, B = 100, semilla = 7)
  b2 <- bootstrap_biplot(pisa, variables_pisa, B = 100, semilla = 7)
  expect_equal(b1$resumen, b2$resumen)
  r <- b1$resumen
  expect_true(all(r$`LI percentil` <= r$`LS percentil`))
  g <- r[r$Grupo == "Global", ]
  expect_true(all(g$`LI percentil` <= g$Estimación & g$Estimación <= g$`LS percentil`))
  expect_equal(r$Estimación[r$Estadístico == "Valor propio 1"], 408.81, tolerance = 1e-4)
  expect_s3_class(grafico_bootstrap_biplot(b1), "ggplot")
  expect_error(bootstrap_biplot(pisa, variables_pisa, B = 10), "al menos 50")
})

test_that("bootstrap con JK: réplicas en la misma escala que la estimación", {
  pisa <- ejemplo_biplot("pisa")
  for (tipo in c("HJ", "GH", "JK")) {
    bt <- bootstrap_biplot(pisa, variables_pisa, tipo = tipo, B = 100, semilla = 3)
    r <- bt$resumen[bt$resumen$Grupo == "Coordenadas de las variables", ]
    # La estimación queda dentro de su intervalo y la media bootstrap muy cerca
    expect_true(all(r$`LI percentil` <= r$Estimación & r$Estimación <= r$`LS percentil`), info = tipo)
    expect_lt(max(abs(r$Sesgo) / (abs(r$Estimación) + 1e-8)), 0.25)
  }
})
