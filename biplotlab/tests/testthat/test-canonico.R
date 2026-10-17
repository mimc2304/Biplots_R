# Biplot canónico / MANOVA biplot (código del grupo integrado en BiplotLab).
# Validación con Iris (Amaro, Vicente-Villardón y Galindo, 2008) y con el césped
# de Seber (dos factores; Amaro et al., 2004).

test_that("Iris: Wilks y valores propios iguales a stats::manova()", {
  cb <- bigarx_canonical(iris[, 1:4], iris$Species, region = "ellipse")
  fit <- stats::manova(as.matrix(iris[, 1:4]) ~ iris$Species)
  expect_equal(cb$manova$wilks, summary(fit, test = "Wilks")$stats[1, 2], tolerance = 1e-10)
  expect_equal(round(cb$canonical$valor_propio, 4), c(32.1919, 0.2854))
  expect_equal(round(cb$inertia$inercia, 2), c(99.12, 0.88))
  # Los individuos coinciden con las funciones discriminantes de MASS::lda (salvo el signo)
  skip_if_not_installed("MASS")
  l <- MASS::lda(iris[, 1:4], iris$Species)
  expect_equal(abs(unname(cb$individuals[, 1:2])), abs(unname(stats::predict(l)$x)), tolerance = 1e-8)
})

test_that("Iris: elipses de Krzanowski en la escala de W (corrección del 7/10)", {
  cb <- bigarx_canonical(iris[, 1:4], iris$Species, region = "ellipse")
  rg <- canonical_regions(cb, c(1, 2))
  expect_equal(round(rg$semieje_mayor, 4), c(1.3288, 0.5129, 1.0482))
  expect_equal(round(rg$semieje_menor, 4), c(1.1392, 0.4321, 0.8909))
  expect_true(all(rg$definida_positiva))
  expect_true(all(rg$error_contorno < 1e-8))
  # Los centros de las elipses son las medias del gráfico (misma escala)
  expect_equal(unname(rg$centro1), unname(cb$rows[, 1]))
})

test_that("las regiones circulares también están disponibles", {
  for (r in c("chi2", "t", "bonferroni")) {
    cb <- bigarx_canonical(iris[, 1:4], iris$Species, region = r)
    rg <- canonical_regions(cb)
    expect_equal(rg$semieje_mayor, rg$semieje_menor)
  }
  chi <- bigarx_canonical(iris[, 1:4], iris$Species, region = "chi2")
  expect_equal(unname(chi$radius), rep(sqrt(stats::qchisq(0.95, 2) / 50), 3))
})

test_that("césped (dos factores): Wilks por efecto iguales a summary.manova", {
  ruta <- system.file("extdata", "cesped_seber.csv", package = "biplotlab")
  if (!nzchar(ruta)) ruta <- file.path("..", "..", "inst", "extdata", "cesped_seber.csv")
  ces <- utils::read.csv(ruta)
  X <- log(ces[, c("X1", "X2", "X3")])
  m2 <- bigarx_manova2(X, ces$tratamiento, ces$temperatura, efecto = "total", region = "ellipse")
  A <- factor(ces$tratamiento); B <- factor(ces$temperatura)
  ref <- summary(stats::manova(scale(as.matrix(X), scale = FALSE) ~ A * B), test = "Wilks")$stats
  w <- m2$diseno$pruebas[m2$diseno$pruebas$contraste == "Wilks", "estadistico"]
  expect_equal(w, unname(ref[1:3, 2]), tolerance = 1e-10)
  expect_equal(nrow(m2$rows), 8)
  expect_s3_class(bigarx_manova2(X, A, B, efecto = "interaccion"), "bigarx_canonical")
})

test_that("bootstrap reproducible y gráfico con encuadre rectangular", {
  b1 <- boot_canonical(iris[, 1:4], iris$Species, B = 60, seed = 5)
  b2 <- boot_canonical(iris[, 1:4], iris$Species, B = 60, seed = 5)
  expect_equal(b1$table, b2$table)
  cb <- bigarx_canonical(iris[, 1:4], iris$Species, region = "ellipse")
  p <- grafico_canonico(cb, boot = b1)
  e <- encuadre_biplot(p, W = 900, H = 720)
  expect_equal(e$ancho_panel / e$alto_panel, e$dx / e$dy, tolerance = 1e-8)
  expect_gt(e$dx, e$dy)                       # el eje 1 (99 %) es el lado largo
  expect_true(e$xlim[1] < 0 && e$xlim[2] > 0)  # incluye el origen de las flechas
})
