# HJ-Biplot neutrosófico: pruebas del compañero (pruebas_modulo.R) pasadas a
# testthat, más las de la obtención de ternas dentro del aplicativo.

matriz_conocida <- function() {
  S <- rbind(c(1, 1, 1), c(1, -1, -1), c(-1, 1, -1), c(-1, -1, 1))
  dimnames(S) <- list(paste0("Obs_", 1:4), c("A", "B", "C"))
  list(S = S, T = 1 * (S == 1), F = 1 * (S == -1), I = 1 * (S == 1) * 0)
}

test_that("puntuación, inercia, calidades y contribuciones (matriz de resultado conocido)", {
  m <- matriz_conocida()
  expect_identical(puntuar_neutrosofico(m$T, m$I, m$F), m$S)
  r <- hj_neutrosofico(m$T, m$I, m$F)
  expect_lt(r$error_reconstruccion, 1e-12)
  expect_lt(max(abs(r$inercia$Porcentaje - 100 / 3)), 1e-10)
  ind <- indicadores_por_eje(r); cal <- calidades_hj(r)
  expect_lt(max(abs(colSums(ind$variables$contribucion_al_eje) - 100)), 1e-10)
  expect_lt(max(abs(rowSums(ind$variables$calidad_por_eje) - 100)), 1e-10)
  expect_lt(max(abs(rowSums(ind$variables$calidad_por_eje[, 1:2]) - cal$variables$Calidad_pct)), 1e-10)
  # Cambiar I cambia la puntuación y la inercia
  I2 <- m$I; I2[1, 1] <- 1
  r2 <- hj_neutrosofico(m$T, I2, m$F)
  expect_equal(puntuar_neutrosofico(m$T, I2, m$F)[1, 1], 0)
  expect_gt(max(abs(r2$inercia$Porcentaje - r$inercia$Porcentaje)), 1)
  expect_equal(unname(r2$indeterminacion_observaciones[1]), 1 / 3)
})

test_that("rechaza entradas inválidas", {
  m <- matriz_conocida()
  expect_error(puntuar_neutrosofico(as.data.frame(m$T), m$I, m$F))
  expect_error(puntuar_neutrosofico(m$T * 2, m$I, m$F))
  expect_error(puntuar_neutrosofico(m$T, m$I[-1, , drop = FALSE], m$F))
  mal <- m$I; mal[1, 1] <- NA_real_
  expect_error(puntuar_neutrosofico(m$T, mal, m$F))
  mal <- m$I; colnames(mal) <- rev(colnames(mal))
  expect_error(puntuar_neutrosofico(m$T, mal, m$F))
  expect_error(hj_neutrosofico(m$T * 0, m$I, m$F * 0))
  r <- hj_neutrosofico(m$T, m$I, m$F)
  expect_error(calidades_hj(r, c(1, 1)))
})

test_that("ternas desde respuestas categóricas (como la base de victimización)", {
  d <- data.frame(Casa = c("Seguro", "Muy inseguro", "No sabe", "Ni seguro ni inseguro", NA, "Muy seguro"),
                  Calle = c("Inseguro", "Seguro", "Seguro", "No sabe", "Seguro", "Inseguro"),
                  row.names = paste0("R", 1:6))
  regla <- vapply(unique(na.omit(unlist(d))), proponer_categoria, character(1))
  expect_equal(unname(regla[c("Seguro", "Muy inseguro", "No sabe", "Ni seguro ni inseguro", "Muy seguro", "Inseguro")]),
               c("T", "F", "I", "N", "T", "F"))
  t <- ternas_desde_respuestas(d, c("Casa", "Calle"), regla)
  expect_equal(sum(t$conservadas), 5)                     # la fila con NA se excluye
  expect_equal(unname(t$T["R1", ]), c(1, 0))
  expect_equal(unname(t$F["R1", ]), c(0, 1))
  expect_equal(unname(t$I["R3", ]), c(1, 0))
  expect_equal(unname(t$T["R4", ] + t$I["R4", ] + t$F["R4", ]), c(0, 1))  # neutral = (0, 0, 0)
  r <- hj_neutrosofico(t$T, t$I, t$F)
  expect_equal(sum(r$inercia$Porcentaje), 100)
})

test_that("ternas desde columnas _T, _I, _F y gráfico con encuadre", {
  set.seed(2)
  n <- 30
  d <- data.frame(id = 1:n)
  for (v in c("Precio", "Diseno", "Bateria")) {
    d[[paste0(v, "_T")]] <- runif(n); d[[paste0(v, "_I")]] <- runif(n); d[[paste0(v, "_F")]] <- runif(n)
  }
  expect_equal(variables_ternas(d), c("Precio", "Diseno", "Bateria"))
  t <- ternas_desde_columnas(d)
  r <- hj_neutrosofico(t$T, t$I, t$F)
  p <- grafico_neutrosofico(r, invertir_ejes = 2)
  e <- encuadre_biplot(p, W = 900, H = 720)
  expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
})
