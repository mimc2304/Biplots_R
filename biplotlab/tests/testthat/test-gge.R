# Validación del GGE-Biplot con los datos de Ontario (18 genotipos × 9 ambientes,
# clase del 25 de septiembre). Gráficos: R/calculo_gge.R; tablas: R/tablas_gge.R.
# Valores esperados: los de la práctica propia y, si está instalado,
# metan::gge(), que replica GGEBiplotGUI.

ontario <- function() {
  ruta <- system.file("extdata", "ontario.csv", package = "biplotlab")
  if (!nzchar(ruta)) ruta <- "../../inst/extdata/ontario.csv"
  leer_datos(ruta)
}

# Datos de una capa del gráfico (para revisar lo que se dibuja)
capa <- function(grafico, geom) {
  construido <- suppressMessages(ggplot2::ggplot_build(grafico))
  cual <- which(vapply(grafico$layers, function(l) inherits(l$geom, geom), logical(1)))
  construido$data[cual]
}

# --- Cálculo -----------------------------------------------------------------------

test_that("GGE: inercia de Ontario (centrado por ambiente, GH)", {
  res <- calcular_gge(ontario(), centrado = "tester", particion = "GH")
  expect_equal(round(res$valor_singular[1:3], 3), c(5.011, 2.857, 2.063))
  expect_equal(round(res$inercia$`% Inercia`[1:3], 2), c(58.90, 19.15, 9.98))
  expect_equal(round(res$inercia$`% Acumulado`[2], 2), 78.05)
})

test_that("GGE: calidad (por mil) con las coordenadas de la partición, como la GUI", {
  res <- calcular_gge(ontario(), particion = "GH")
  t <- gge_tablas(res)$calidad_filas
  kat <- unlist(t[t$Genotipo == "kat", c("Eje 1", "Eje 2", "Plano 1-2")])
  expect_equal(unname(kat), c(528, 17, 545))
  plano <- stats::setNames(t$`Plano 1-2`, t$Genotipo)
  expect_equal(unname(plano[c("fun", "ann", "rub")]), c(402, 181, 96))
  t13 <- gge_tablas(res, ejes = c(1, 3))$calidad_filas
  expect_equal(t13$`Plano 1-3`[t13$Genotipo == "kat"], 533)
})

test_that("GGE: JK, GH y SQ reconstruyen la tabla; HJ no (U·D²·V')", {
  for (p in c("GH", "JK", "SQ")) {
    res <- calcular_gge(ontario(), particion = p)
    expect_equal(unname(res$filas %*% t(res$columnas)), unname(res$tabla))
  }
  res <- calcular_gge(ontario(), particion = "HJ")
  expect_false(isTRUE(all.equal(unname(res$filas %*% t(res$columnas)), unname(res$tabla))))
})

test_that("GGE: ajuste visual d de la GUI (GH divide los genotipos)", {
  res <- calcular_gge(ontario(), particion = "GH")
  co <- gge_coordenadas_grafico(res)
  expect_equal(unname(round(unlist(co$genotipos["zav", c("x", "y")]), 3)), c(-0.536, 0.550))
})

test_that("GGE: coincide con metan::gge() (centrados, particiones y escalado)", {
  skip_if_not_installed("metan")
  d <- ontario()
  largo <- data.frame(GEN = rep(rownames(d), ncol(d)), ENV = rep(names(d), each = nrow(d)),
                      Y = unlist(d))
  casos <- expand.grid(centrado = c("tester", "global", "doble"), particion = c("GH", "JK", "SQ"),
                       escalado = c("no", "std"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(casos))) {
    cs <- casos[i, ]
    m <- suppressMessages(metan::gge(
      largo, ENV, GEN, Y,
      centering = c(tester = "environment", global = "global", doble = "double")[[cs$centrado]],
      scaling = c(no = "none", std = "sd")[[cs$escalado]],
      svp = c(GH = "environment", JK = "genotype", SQ = "symmetrical")[[cs$particion]]))$Y
    res <- calcular_gge(d, centrado = cs$centrado, escalado = cs$escalado, particion = cs$particion)
    co <- gge_coordenadas_grafico(res)
    expect_equal(unname(m$eigenvalues[1:2]), res$valor_singular[1:2], tolerance = 1e-6)
    expect_equal(unname(m$coordgen[match(rownames(res$filas), m$labelgen), 1:2]),
                 unname(as.matrix(co$genotipos[, c("x", "y")])), tolerance = 1e-6)
    expect_equal(unname(m$coordenv[match(rownames(res$columnas), m$labelenv), 1:2]),
                 unname(as.matrix(co$ambientes[, c("x", "y")])), tolerance = 1e-6)
  }
})

test_that("GGE: avisa con mensajes claros si los datos no sirven", {
  d <- ontario()
  d$BH93[1] <- NA
  expect_error(calcular_gge(d), "faltantes")
  d <- ontario()
  d$BH93 <- 5
  expect_error(calcular_gge(d, escalado = "std"), "BH93")
  expect_error(gge_examinar_ambiente(calcular_gge(ontario()), "XX93"), "no encontrado")
  expect_error(calcular_gge(ontario(), particion = "ZZ"), "Partición")
  expect_error(gge_comparar_genotipos(calcular_gge(ontario()), "fun", "fun"), "distintos")
})

# --- Gráficos (calculo_gge.R) ------------------------------------------------------

test_that("GGE: las 10 vistas devuelven un gráfico y su tabla", {
  for (p in c("GH", "JK", "HJ", "SQ")) {
    res <- calcular_gge(ontario(), particion = p)
    for (h in GGE_HERRAMIENTAS) {
      g <- gge_grafico_herramienta(res, h, ambiente = "OA93", genotipo = "kat",
                                   genotipo1 = "fun", genotipo2 = "zav")
      expect_s3_class(g, "ggplot")
      expect_no_error(suppressMessages(ggplot2::ggplot_build(g)))
      v <- gge_tabla_herramienta(res, h, ambiente = "OA93", genotipo = "kat",
                                 genotipo1 = "fun", genotipo2 = "zav")
      if (h != "biplot") expect_s3_class(v$tabla, "data.frame")
    }
  }
  expect_error(gge_grafico_herramienta(calcular_gge(ontario()), "otra"), "no reconocida")
})

test_that("GGE: colores y tamaños llegan a los puntos de todas las vistas", {
  res <- calcular_gge(ontario(), particion = "GH")
  for (h in GGE_HERRAMIENTAS) {
    g <- gge_grafico_herramienta(res, h, ambiente = "OA93", genotipo = "kat",
                                 genotipo1 = "fun", genotipo2 = "zav",
                                 color_filas = "darkgreen", color_columnas = "purple",
                                 tam_texto = 4, tam_puntos = 3)
    capas <- Filter(nrow, capa(g, "GeomPoint"))
    puntos <- do.call(rbind, lapply(capas, function(d) d[, c("colour", "size")]))
    expect_true(all(puntos$colour %in% c("darkgreen", "purple")))
    expect_true(all(puntos$size == 3))
  }
})

test_that("GGE: radio de los círculos igual que GGEBiplotGUI", {
  res <- calcular_gge(ontario(), particion = "GH")
  E <- res$columnas
  # Línea literal de la GUI: usa las FILAS 1 y 2 (dos ambientes), no los ejes
  radio_gui <- max(max(E[1, ]) - min(E[1, ]), max(E[2, ]) - min(E[2, ])) / 10
  circulos <- capa(gge_relacion_ambientes(res), "GeomPath")[[1]]
  expect_equal(max(sqrt(circulos$x^2 + circulos$y^2)), 5 * radio_gui, tolerance = 1e-6)
})

test_that("GGE: correcciones de casos raros (ajuste d, ideal y quién ganó dónde)", {
  prueba <- data.frame(E1 = c(12, 10, 8), E2 = c(8, 4, 6), row.names = c("A", "B", "C"))
  # Ajuste d: con 2 columnas escaladas los genotipos no se aplastan en el origen
  res <- calcular_gge(prueba, escalado = "std", particion = "GH")
  co <- gge_coordenadas_grafico(res)
  expect_gt(max(abs(unlist(co$genotipos[, c("x", "y")]))), 0.1)
  # Genotipo ideal: si todos avanzan hacia atrás sobre el AEA, queda en el origen
  res <- calcular_gge(ontario(), centrado = "global", escalado = "std", particion = "GH")
  flecha <- capa(gge_ranking_genotipos(res, ejes = c(1, 3)), "GeomSegment")
  flecha <- flecha[[length(flecha)]]
  expect_equal(c(flecha$xend, flecha$yend), c(0, 0))
  # Quién ganó dónde: con el origen sobre el lado B-C, la recta sale hacia fuera (abajo)
  res <- calcular_gge(prueba, centrado = "doble", particion = "GH")
  rectas <- capa(gge_quien_gano_donde(res), "GeomSegment")[[1]]
  expect_false(anyNA(rectas$xend))
  expect_true(any(rectas$yend < 0))
})

# --- Tablas (tablas_gge.R) ---------------------------------------------------------

test_that("GGE: examinar un ambiente (OA93)", {
  res <- calcular_gge(ontario(), particion = "GH")
  t <- gge_tabla_herramienta(res, "examinar_ambiente", ambiente = "OA93")$tabla
  expect_equal(t$Genotipo[1:8], c("zav", "aug", "ham", "del", "kar", "ari", "dia", "ann"))
  expect_equal(round(t$`Producto escalar`[1], 3), 0.769)
  expect_equal(t$Genotipo[18], "luc")
  expect_equal(round(t$`Producto escalar`[18], 3), -1.450)
  # Mismo orden en JK y SQ (propiedad del producto escalar); en HJ cambia
  for (p in c("JK", "SQ")) {
    otro <- gge_tabla_herramienta(calcular_gge(ontario(), particion = p), "examinar_ambiente",
                                  ambiente = "OA93")$tabla
    expect_equal(otro$Genotipo, t$Genotipo)
  }
  hj <- gge_tabla_herramienta(calcular_gge(ontario(), particion = "HJ"), "examinar_ambiente",
                              ambiente = "OA93")$tabla
  expect_equal(hj$Genotipo[1:4], c("zav", "ham", "del", "aug"))
})

test_that("GGE: examinar un genotipo y comparar dos genotipos usan el producto escalar", {
  res <- calcular_gge(ontario(), particion = "GH")
  aprox <- res$filas[, 1:2] %*% t(res$columnas[, 1:2])          # tabla de rango 2
  eg <- gge_tabla_herramienta(res, "examinar_genotipo", genotipo = "kat")$tabla
  expect_equal(eg$`Producto escalar`, unname(sort(aprox["kat", ], decreasing = TRUE)))
  cg <- gge_tabla_herramienta(res, "comparar_genotipos", genotipo1 = "fun", genotipo2 = "zav")$tabla
  expect_equal(cg$`Diferencia (producto escalar)`, unname(aprox["fun", ] - aprox["zav", ]))
  expect_equal(cg$`Mejor según el biplot`[cg$Ambiente %in% c("KE93", "OA93")], c("zav", "zav"))
})

test_that("GGE: quién ganó dónde separa {KE93, OA93} (zav) del resto (fun)", {
  res <- calcular_gge(ontario(), particion = "GH")
  v <- gge_tabla_herramienta(res, "quien_gano_donde")
  sectores <- stats::setNames(v$tabla2[[2]], v$tabla2[[1]])
  expect_equal(unname(sectores["zav"]), "KE93, OA93")
  expect_equal(v$tabla2$`Número`[v$tabla2[[1]] == "fun"], 7)
  # El ganador del biplot es un vértice del polígono que dibuja gge_quien_gano_donde()
  poligono <- capa(gge_quien_gano_donde(res), "GeomPolygon")[[1]]
  co <- gge_coordenadas_grafico(res)$genotipos
  vertices <- co$nombre[round(co$x, 8) %in% round(poligono$x, 8)]
  expect_true(all(v$tabla$`Ganador según el biplot` %in% vertices))
  # No cambia con la partición (f entre 0 y 1)
  for (p in c("JK", "SQ")) {
    otro <- gge_tabla_herramienta(calcular_gge(ontario(), particion = p), "quien_gano_donde")
    expect_equal(otro$tabla$`Ganador según el biplot`, v$tabla$`Ganador según el biplot`)
  }
})

test_that("GGE: media vs estabilidad estima la media de cada genotipo", {
  res <- calcular_gge(ontario(), particion = "JK")
  aprox <- res$filas[, 1:2] %*% t(res$columnas[, 1:2])
  t <- gge_tabla_herramienta(res, "media_estabilidad")$tabla
  expect_equal(t$`Media estimada (producto escalar)`, unname(rowMeans(aprox)[t$Genotipo]))
  expect_true(all(diff(t$`Proyección sobre el AEA (media)`) <= 0))
})

test_that("GGE: relación entre ambientes, discriminación y ambiente ideal", {
  res <- calcular_gge(ontario(), particion = "GH")
  rel <- gge_tabla_herramienta(res, "relacion_ambientes")
  expect_equal(unname(diag(as.matrix(rel$tabla[, -1]))), rep(1, 9))
  disc <- gge_tabla_herramienta(res, "discriminacion")$tabla
  expect_equal(disc$Ambiente[1:2], c("KE93", "OA93"))
  ra <- gge_tabla_herramienta(res, "ranking_ambientes")$tabla
  expect_equal(ra$Ambiente[c(1, 5, 9)], c("BH93", "KE93", "RN93"))
  # HJ tiene los mismos ambientes que GH: mismas relaciones
  hj <- gge_tabla_herramienta(calcular_gge(ontario(), particion = "HJ"), "relacion_ambientes")
  expect_equal(hj$tabla, rel$tabla)
})

test_that("GGE: genotipo ideal con el criterio de la GUI", {
  res <- calcular_gge(ontario(), particion = "GH")
  t <- gge_tabla_herramienta(res, "ranking_genotipos")$tabla
  expect_equal(nrow(t), 18)
  expect_equal(t$Genotipo[1:2], c("ron", "cas"))
})

# --- Sintaxis R --------------------------------------------------------------------

test_that("GGE: la sintaxis R funciona con las funciones exportadas de BiplotLab", {
  exportadas <- getNamespaceExports("biplotlab")
  expect_true(all(c("gge_centrado", "gge_descomposicion", "gge_quien_gano_donde",
                    "gge_ranking_genotipos") %in% exportadas))
  codigo <- c("datos <- ejemplo_biplot(\"ontario\")",
              "tabla <- as.matrix(datos[, vapply(datos, is.numeric, logical(1))])",
              "desc <- gge_descomposicion(gge_escalado(gge_centrado(tabla, 'tester'), 'no'), 'GH')",
              "resultados <- gge_resultados(desc, eje = c(1, 2))",
              "graficos <- list(gge_examinar_ambiente(desc, 'OA93'), gge_quien_gano_donde(desc),",
              "                 gge_dis_rep(desc), gge_ranking_genotipos(desc))")
  entorno <- new.env(parent = asNamespace("biplotlab"))
  suppressMessages(eval(parse(text = codigo), envir = entorno))
  expect_equal(round(entorno$desc$valor_singular[1], 3), 5.011)
  for (g in entorno$graficos) expect_s3_class(g, "ggplot")
})


test_that("GGE: el biplot oculta lo que no llega a la calidad mínima en el plano", {
  res <- calcular_gge(ontario(), particion = "GH")
  q <- round(gge_resultados(res)$calidad_filas[, "Plano"])
  expect_equal(unname(q[c("kat", "fun", "ann", "rub")]), c(545, 402, 181, 96))
  g <- gge_grafico_herramienta(res, "biplot", cal_filas = 400, cal_columnas = 1000)
  textos <- unlist(lapply(capa(g, "GeomTextRepel"), `[[`, "label"))
  expect_true(all(c("kat", "fun") %in% textos))
  expect_false(any(c("ann", "rub") %in% textos))
  expect_equal(sum(textos %in% names(q)), sum(q >= 400))
  expect_false(any(colnames(res$tabla) %in% textos))
  # Sin mínimo se dibujan todos
  g0 <- gge_grafico_herramienta(res, "biplot")
  textos0 <- unlist(lapply(capa(g0, "GeomTextRepel"), `[[`, "label"))
  expect_true(all(c(names(q), colnames(res$tabla)) %in% textos0))
})

test_that("GGE: quién ganó dónde se encuadra con los puntos, no con las rectas", {
  for (p in c("GH", "JK", "SQ", "HJ")) {
    res <- calcular_gge(ontario(), particion = p)
    co <- gge_coordenadas_grafico(res)
    puntos <- rbind(co$genotipos[, c("x", "y")], co$ambientes[, c("x", "y")])
    g <- gge_grafico_herramienta(res, "quien_gano_donde")
    r <- ggplot2::ggplot_build(g)$layout$panel_params[[1]]
    expect_equal(r$x.range, range(puntos$x) + c(-1, 1) * 0.05 * diff(range(puntos$x)))
    expect_equal(r$y.range, range(puntos$y) + c(-1, 1) * 0.05 * diff(range(puntos$y)))
  }
})

test_that("GGE dentro del aplicativo: plano 4:3 y volteo sin cambiar productos escalares", {
  res <- calcular_gge(ontario(), particion = "GH")
  for (h in GGE_HERRAMIENTAS) {
    g <- gge_grafico_herramienta(res, h, ambiente = "OA93", genotipo = "kat",
                                 genotipo1 = "kat", genotipo2 = "fun")
    e <- encuadre_biplot(con_proporcion_minima(g), W = 900, H = 720)
    expect_gte(e$dx / e$dy, 4 / 3 - 1e-8)
  }
  v <- res; v$filas[, 2] <- -v$filas[, 2]; v$columnas[, 2] <- -v$columnas[, 2]
  expect_equal(v$filas[, 1:2] %*% t(v$columnas[, 1:2]), res$filas[, 1:2] %*% t(res$columnas[, 1:2]))
})
