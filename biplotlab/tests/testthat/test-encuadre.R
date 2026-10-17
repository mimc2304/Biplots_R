# Encuadre rectangular del plano (documento "Ecuaciones_encuadre_biplot").

test_that("límites: incluyen el origen y se amplían en proporción a cada amplitud", {
  l <- limites_encuadre(c(2, 6), c(1, 3), incluir_origen = TRUE, margen = 0.1)
  # Ecuaciones 1 y 2: x en [0, 6] e y en [0, 3]  ->  amplitudes 6 y 3
  expect_equal(l$x, c(-0.6, 6.6))
  expect_equal(l$y, c(-0.3, 3.3))
  expect_equal(l$dx / l$dy, 6 / 3)          # ampliar no cambia la proporción
  sin <- limites_encuadre(c(2, 6), c(1, 3), incluir_origen = FALSE, margen = 0)
  expect_equal(c(sin$dx, sin$dy), c(4, 2))
})

test_that("ecuaciones 3 a 5: área interior con ancho/alto = dx/dy y s = min(W/dx, H/dy)", {
  pisa <- ejemplo_biplot("pisa")
  res <- hj_biplot(pisa, names(pisa)[3:12])
  for (ejes in list(c(1, 2), c(1, 3), c(2, 3))) {
    p <- grafico_biplot(res, ejes = ejes, grupos = pisa$REGION)
    for (v in list(c(900, 720), c(600, 720), c(1300, 500))) {
      e <- encuadre_biplot(p, W = v[1], H = v[2])
      expect_equal(e$ancho_panel / e$alto_panel, e$dx / e$dy, tolerance = 1e-8)
      expect_equal(e$ancho_panel, e$s * e$dx)
      expect_equal(e$alto_panel, e$s * e$dy)
      # El gráfico cabe en la ventana y toca al menos uno de sus bordes
      expect_lte(e$ancho, v[1] + 1e-6)
      expect_lte(e$alto, v[2] + 1e-6)
      expect_true(abs(e$ancho - v[1]) < 1e-6 || abs(e$alto - v[2]) < 1e-6)
    }
  }
})

test_that("los límites del gráfico son los del encuadre (sin ampliación extra de ggplot2)", {
  pisa <- ejemplo_biplot("pisa")
  res <- hj_biplot(pisa, names(pisa)[3:12])
  p <- grafico_biplot(res)
  e <- encuadre_biplot(p, W = 900, H = 700)
  F <- res$filas[, 1:2]; G <- res$columnas[, 1:2]
  l <- limites_encuadre(c(F[, 1], G[, 1]), c(F[, 2], G[, 2]))
  expect_equal(e$xlim, l$x)
  expect_equal(e$ylim, l$y)
  # El eje 1 (más inercia) es el lado largo en PISA
  expect_gt(e$dx, e$dy)
})

test_that("el encuadre funciona también para el bootstrap y para pulgadas", {
  pisa <- ejemplo_biplot("pisa")
  bt <- bootstrap_biplot(pisa, names(pisa)[3:12], B = 60, semilla = 1)
  e <- encuadre_biplot(grafico_bootstrap_biplot(bt), W = 10, H = 8, unidad = "in")
  expect_equal(e$ancho_panel / e$alto_panel, e$dx / e$dy, tolerance = 1e-8)
  expect_true(e$xlim[1] < 0 && e$xlim[2] > 0)   # incluye el origen
})

test_that("el plano nunca es más alto que ancho (mínimo 4:3, petición de la Dra.)", {
  # Nube alta y estrecha: se amplía el eje horizontal, por igual a los dos lados
  l <- limites_encuadre(c(-1, 1), c(-5, 5), incluir_origen = TRUE, margen = 0)
  expect_equal(l$dy, 10)
  expect_equal(l$dx, 10 * 4 / 3)
  expect_equal(mean(l$x), 0)
  pisa <- ejemplo_biplot("pisa")
  res <- hj_biplot(pisa, names(pisa)[3:12])
  for (ejes in list(c(1, 2), c(1, 3), c(2, 3), c(3, 1))) {
    e <- encuadre_biplot(grafico_biplot(res, ejes = ejes), W = 900, H = 720)
    expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
  }
  bt <- bootstrap_biplot(pisa, names(pisa)[3:12], B = 60, semilla = 1)
  e <- encuadre_biplot(grafico_bootstrap_biplot(bt), W = 900, H = 720)
  expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
  cb <- bigarx_canonical(iris[, 1:4], iris$Species, region = "ellipse")
  e <- encuadre_biplot(grafico_canonico(cb, opciones_grafico(cb, axes = c(2, 1))), W = 900, H = 720)
  expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
})
