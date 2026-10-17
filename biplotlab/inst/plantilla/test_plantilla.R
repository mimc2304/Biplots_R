# =============================================================================
# PLANTILLA DE PRUEBA PARA TU MÉTODO
#
# 1. Copia este archivo a  tests/testthat/test-<metodo>.R   (ej.: test-gge.R)
# 2. Reemplaza METODO por el nombre de tu método (igual que en tu mod_<metodo>.R).
# 3. Cambia los números esperados por los que obtuviste en tu práctica
#    (con tu script original, en R, MULTBIPLOT u otro programa).
# 4. Ejecuta en la consola:   testthat::test_local(".")
#
# Si todo está bien, en tu línea aparecen solo puntos ("....").
# Si algo no coincide, R dice qué prueba falló y qué valor esperaba.
# =============================================================================

test_that("METODO: el cálculo da los mismos números que mi práctica", {
  # Base pequeña de prueba. Guárdala en inst/extdata/ o usa una de las incluidas.
  datos <- ejemplo_biplot("pisa")
  res <- calcular_METODO(datos, variables = c("MATH", "READ", "SCIE", "CURIO"))

  # TODO: cambia por los valores de tu práctica (por ejemplo, % de inercia)
  inercia_esperada <- c(73.0, 25.0)
  expect_equal(round(res$inercia$`% Inercia`[1:2], 1), inercia_esperada, tolerance = 0.1)
})

test_that("METODO: avisa con un mensaje claro si los datos no sirven", {
  datos <- ejemplo_biplot("pisa")
  datos$MATH[1] <- NA
  expect_error(calcular_METODO(datos, variables = c("MATH", "READ")), "faltantes")
})
