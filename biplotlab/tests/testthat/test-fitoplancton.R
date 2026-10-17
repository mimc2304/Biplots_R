# Validación con la matriz de fitoplancton del embalse de Riaño (material de
# clase "HJ-Biplot de un ecosistema acuático"). El archivo no se distribuye con
# el paquete: se busca en la carpeta que contiene a biplotlab.
ruta_fito <- file.path("..", "..", "..", "familias FITOPLACTON HJ WD.xls")

leer_fito <- function() leer_datos(ruta_fito, nombre_original = ruta_fito)

test_that("las columnas de agrupación se detectan como nominales", {
  skip_if_not(file.exists(ruta_fito))
  d <- leer_fito()
  expect_setequal(detectar_nominales(d), c("Groupdeep", "Groupweek"))
})

test_that("inercia y CRFE coinciden con el material de clase", {
  skip_if_not(file.exists(ruta_fito))
  d <- leer_fito()
  r <- hj_biplot(d, setdiff(names(d), detectar_nominales(d)))
  expect_equal(round(r$inercia$`% Inercia`[1:2], 2), c(28.37, 14.89))
  crfe <- round(1000 * r$calidad_columnas[, 1:3])
  # Tabla CRFE de la diapositiva 13 (por mil); se toleran 5 unidades por el redondeo de la lámina
  esperado <- rbind(DIATOM = c(600, 30, 130), XANTOF = c(730, 63, 6), CRISOF = c(0, 580, 4),
                    EUGLEN = c(2, 39, 550), DINOF = c(560, 85, 4), ALGAS = c(500, 11, 1))
  expect_true(all(abs(crfe[rownames(esperado), ] - esperado) <= 5))
  # Con las columnas de agrupación como activas sale el error que señaló la docente
  r15 <- hj_biplot(d)
  expect_equal(round(r15$inercia$`% Inercia`[1:2], 2), c(26.40, 17.14))
})

test_that("clúster de Ward en el plano 1-2: correlación cofenética del material", {
  skip_if_not(file.exists(ruta_fito))
  d <- leer_fito()
  r <- hj_biplot(d, setdiff(names(d), detectar_nominales(d)))
  cl <- cluster_biplot(r, sobre = "filas", ejes = 1:2, metodo = "ward.D2", k = 5)
  expect_equal(round(cl$cofenetica, 5), 0.67941)
  expect_equal(sum(cl$tamanos$Tamaño), 70)
})

test_that("fitoplancton, plano 1-2: apaisado y, volteado arriba-abajo, como en el material", {
  skip_if_not(file.exists(ruta_fito))
  d <- leer_fito()
  r <- hj_biplot(d, setdiff(names(d), detectar_nominales(d)), invertir_ejes = 2)
  # En la diapositiva 11 CRISOFICEAS apunta hacia abajo y DINOFICEAS a la derecha
  expect_lt(r$columnas["CRISOF", 2], 0)
  expect_gt(r$columnas["DINOF", 1], 0)
  e <- encuadre_biplot(grafico_biplot(r), W = 900, H = 720)
  expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
})
