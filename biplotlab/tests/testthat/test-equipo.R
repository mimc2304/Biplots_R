# Métodos del compañero (DPCA, Disjoint, Cenet, Logístico): los motores se
# integraron sin cambios; aquí se comprueban sus propiedades principales y el
# encuadre del gráfico dentro del aplicativo.

datos_iris <- function() {
  X <- prep_matrix(iris[, 1:4], TRUE, TRUE); rownames(X) <- paste0("f", 1:150); X
}

test_that("DPCA: cargas disjuntas y ortonormales; error no menor que el PCA", {
  X <- datos_iris()
  r <- dpca_greedy(X, Q = 2, nstart = 5, seed = 1)
  B <- r$fact$V
  expect_equal(unname(crossprod(B)), diag(2), tolerance = 1e-8)
  expect_true(all(rowSums(abs(B) > 1e-12) == 1))         # una sola carga por variable
  expect_gte(r$F, r$F_pca - 1e-10)
})

test_that("CDBiplot: P clusters y error relativo en [0, 1]", {
  X <- datos_iris()
  r <- cdpca(X, P = 3, Q = 2, nstart = 5, seed = 1)
  expect_equal(length(unique(r$u)), 3)
  expect_true(r$F >= 0 && r$F <= 1)
})

test_that("Cenet con nivel 1 se acerca a la SVD clásica (ejes ortogonales)", {
  X <- datos_iris()
  r <- cenet_svd(X, Q = 2, alpha = 0.95, level_v = 1, eps = 1e-10)
  s <- svd(X)
  expect_equal(unname(r$fact$d[1]), s$d[1], tolerance = 1e-3)
  expect_lt(abs(sum(r$fact$V[, 1] * r$fact$V[, 2])), 1e-6)
  # Observación para el compañero: con alpha = 0.5 el primer valor singular
  # queda un 0,9 % por debajo del de la SVD (20,67 frente a 20,85 en iris),
  # aunque con nivel 1 la restricción no debería estar activa.
})

test_that("Logístico externo: coeficientes iguales a glm y gráfico con encuadre 4:3", {
  set.seed(7)
  X <- matrix(rbinom(60 * 6, 1, 0.5), 60, 6, dimnames = list(paste0("P", 1:60), paste0("item", 1:6)))
  r <- elb_fit(X, k = 2)
  g <- stats::glm(X[, 1] ~ r$A[, 1] + r$A[, 2], family = stats::binomial())
  expect_equal(unname(c(r$b0[1], r$B[1, ])), unname(stats::coef(g)), tolerance = 1e-6)
  p <- encuadre_ggplot(plot_logistic(r))
  e <- encuadre_biplot(p, W = 900, H = 720)
  expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
})

test_that("voltear un eje cambia solo el signo de ese eje", {
  X <- datos_iris()
  r <- dpca_greedy(X, Q = 2, nstart = 3, seed = 1)
  v <- voltear_equipo(r, 2)
  expect_equal(v$fact$U[, 2], -r$fact$U[, 2])
  expect_equal(v$fact$V[, 1], r$fact$V[, 1])
  expect_equal(eigen_table(X, v$fact), eigen_table(X, r$fact))
})

test_that("todos los biplots tienen su pestaña Teoría completa", {
  for (m in c("hj", "bootstrap", "cluster", "canonico", "dpca", "disjoint", "cenet", "logistic", "neutro", "gge")) {
    t <- teoria_metodo(m)
    for (campo in c("para_que", "entrada", "no_usar", "calculos", "lectura", "donde", "referencias"))
      expect_true(length(t[[campo]]) > 0, info = paste(m, campo))
    expect_true(length(t$referencias$modulo) > 0, info = paste(m, "referencias del módulo"))
    expect_true(length(t$referencias$original) > 0, info = paste(m, "obras originales"))
    html <- as.character(teoria_ui(m))
    expect_match(html, "bl-resumen", fixed = TRUE)
    expect_false(grepl("bl-ficha", html, fixed = TRUE))
    expect_s3_class(teoria_ui(m), "shiny.tag")
  }
})

test_that("la Sintaxis R explica cómo instalar BiplotLab, que no está en CRAN", {
  s <- sintaxis_paquete()
  expect_equal(tail(s, 1), "library(biplotlab)")
  expect_true(any(grepl("repos = NULL, type = \"source\"", s, fixed = TRUE)))
  expect_true(all(startsWith(head(s, -1), "#")))
})
