# Validación del HJ-Biplot contra los resultados de MULTBIPLOT de la práctica
# "Guía Práctica de Análisis HJ-Biplot en MultBiplot – Caso PISA 2025"
# (estandarización por columnas, SVD, 3 ejes).

variables_pisa <- c("MATH", "READ", "SCIE", "CMPS", "CURIO", "PERSEV",
                    "COGABIL", "GOALSET", "SELFREG", "ENPROBS")

test_that("los valores propios coinciden con MULTBIPLOT (PISA 2025)", {
  res <- hj_biplot(ejemplo_biplot("pisa"), variables_pisa)
  esperado <- c(408.81, 230.989, 81.12, 72.109, 36.498, 22.521, 19.673, 13.518, 3.214, 1.548)
  expect_equal(round(res$inercia$`Valor propio`, 3), esperado, tolerance = 1e-3)
  expect_equal(round(res$inercia$`% Acumulado`[2], 3), 71.888, tolerance = 1e-4)
  expect_equal(sum(res$inercia$`Valor propio`), 89 * 10)   # (n - 1) * p
})

test_that("coordenadas de filas y columnas coinciden (ejes 1 y 2)", {
  res <- hj_biplot(ejemplo_biplot("pisa"), variables_pisa)
  expect_equal(unname(round(res$filas["ECU", 1:2], 3)), c(2.199, 1.124), tolerance = 1e-3)
  expect_equal(unname(round(res$filas["CZE", 1:2], 3)), c(-4.044, -3.005), tolerance = 1e-3)
  expect_equal(unname(round(res$columnas["MATH", 1:2], 3)), c(-8.261, 3.313), tolerance = 1e-3)
  expect_equal(unname(round(res$columnas["CURIO", 1:2], 3)), c(2.064, 7.523), tolerance = 1e-3)
  # El eje 3 puede salir con el signo contrario (propio de la SVD): se compara en valor absoluto
  expect_equal(unname(abs(round(res$columnas["COGABIL", 3], 3))), 7.178, tolerance = 1e-3)
})

test_that("la calidad de representación (por mil) coincide con MULTBIPLOT", {
  res <- hj_biplot(ejemplo_biplot("pisa"), variables_pisa)
  t <- tablas_biplot(res, unidad = "milesima")
  ecu <- unlist(t$calidad_filas[t$calidad_filas$Individuo == "ECU", 2:4])
  expect_equal(unname(ecu), c(559, 146, 21))
  cog <- unlist(t$calidad_columnas[t$calidad_columnas$Variable == "COGABIL", 2:4])
  expect_equal(unname(cog), c(215, 10, 579))
  expect_equal(t$calidad_filas$`Plano 1-2`[t$calidad_filas$Individuo == "ECU"], 705)
})

test_that("propiedades del HJ: J = UD, H = VD y X = U D V'", {
  res <- hj_biplot(ejemplo_biplot("pisa"), variables_pisa, dimensiones = 10)
  s <- res$svd
  expect_equal(unname(res$filas), unname(s$u %*% diag(s$d)))
  expect_equal(unname(res$columnas), unname(s$v %*% diag(s$d)))
  expect_equal(unname(s$u %*% diag(s$d) %*% t(s$v)), unname(res$matriz))
  # Las contribuciones al eje suman 1 y la calidad acumulada en todos los ejes vale 1
  expect_equal(unname(colSums(res$contrib_filas)), rep(1, 10))
  expect_equal(unname(rowSums(res$calidad_columnas)), rep(1, 10))
})

test_that("cantones: reproduce la inercia de la práctica con biplotbootGUI", {
  cant <- ejemplo_biplot("cantones")
  con <- hj_biplot(cant)
  expect_equal(round(con$inercia$`% Inercia`[1:2], 2), c(53.79, 39.98))
  sin <- hj_biplot(cant[rownames(cant) != "URBINA JADO", ])
  expect_equal(round(sin$inercia$`% Inercia`[1:2], 2), c(55.38, 37.64))
  # Urbina Jado aporta más del 99 % del eje 2
  expect_gt(con$contrib_filas["URBINA JADO", 2], 0.99)
  expect_true(any(grepl("URBINA JADO", lectura_biplot(con))))
})

test_that("GH y JK reparten los valores singulares como en Gabriel (1971)", {
  pisa <- ejemplo_biplot("pisa")
  gh <- hj_biplot(pisa, variables_pisa, tipo = "GH")
  jk <- hj_biplot(pisa, variables_pisa, tipo = "JK")
  hj <- hj_biplot(pisa, variables_pisa, tipo = "HJ")
  expect_equal(gh$columnas, hj$columnas)
  expect_equal(jk$filas, hj$filas)
  expect_equal(unname(colSums(gh$filas^2)), rep(1, 3))
})

test_that("mensajes de error claros", {
  pisa <- ejemplo_biplot("pisa")
  expect_error(hj_biplot(pisa, "MATH"), "al menos 2")
  expect_error(hj_biplot(pisa, c("MATH", "REGION")), "no son numéricas")
  pisa$MATH[3] <- NA
  expect_error(hj_biplot(pisa, variables_pisa), "faltantes")
})

test_that("el gráfico y la sintaxis se generan", {
  pisa <- ejemplo_biplot("pisa")
  res <- hj_biplot(pisa, variables_pisa)
  p <- grafico_biplot(res, grupos = pisa$REGION, calidad_min = 50, invertir = c(TRUE, FALSE))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_match(vector_a_r(variables_pisa), "^c\\(\"MATH\"")
  codigo <- paste(sintaxis_lectura(list(tipo = "ejemplo", nombre = "pisa")),
                  paste("res <- hj_biplot(datos, variables =", vector_a_r(variables_pisa), ")"), sep = "\n")
  expect_s3_class(eval(parse(text = codigo)), "biplot_hj")
})
