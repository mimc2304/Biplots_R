# BiplotLab: guía para el grupo

BiplotLab es un aplicativo para hacer análisis Biplot sin escribir código: se
carga una base de datos, se elige el método en la barra de arriba y se ven el
gráfico, las tablas y una lectura guiada. Está hecho en **R** con **Shiny**
(el paquete de R para crear aplicaciones web), al estilo de jamovi.

Cada método tiene además una pestaña **Sintaxis R** con el código que
reproduce el resultado, para quien quiera usarlo directamente en R.

> **Importante:** la Sintaxis R usa las funciones de BiplotLab (`hj_biplot()`,
> `leer_datos()`, …), así que empieza con `library(biplotlab)`. BiplotLab es el
> paquete del grupo y **no está en CRAN**: `install.packages("biplotlab")` no lo
> encuentra y R responde *there is no package called 'biplotlab'*. Se instala
> una sola vez desde el archivo `biplotlab_0.1.0.tar.gz` (o desde esta carpeta):
>
> ```r
> install.packages(c("shiny", "bslib", "DT", "ggplot2", "ggrepel", "htmltools",
>                    "colourpicker", "cluster", "readxl", "writexl", "haven", "nanoparquet"))
> install.packages("C:/ruta/biplotlab_0.1.0.tar.gz", repos = NULL, type = "source")
> library(biplotlab)
> ```
>
> No necesita Rtools (no tiene código compilado). La sintaxis del GGE-Biplot es
> autónoma: solo usa ggplot2 y ggrepel.

Lee esta guía completa antes de empezar: explica cómo abrir la app, qué es
cada archivo y cómo agregar tu método.

---

## 1. Qué necesitas instalar

- **R** 4.3 o posterior: <https://cran.r-project.org/>
- **RStudio**: <https://posit.co/download/rstudio-desktop/>

Los paquetes de R que usa la app se instalan solos la primera vez que la abres.

---

## 2. Cómo abrir la app

1. Descomprime el archivo `.zip` en una carpeta de tu computador.
2. Entra a la carpeta `biplotlab` y abre **`biplotlab.Rproj`** con doble clic.
   Se abre RStudio, ya ubicado en esa carpeta. Este paso es importante: si
   abres los archivos sueltos, R no encuentra el resto del código.
3. En RStudio abre **`iniciar.R`** (panel de archivos, abajo a la derecha) y
   pulsa el botón **Source** (arriba a la derecha del editor) o
   `Ctrl + Shift + S`.
4. La primera vez tarda unos minutos porque instala paquetes. Después se abre
   la app en el navegador.

**Es lo único que hay que ejecutar.** No hace falta abrir ni ejecutar los
demás archivos uno por uno.

Para cerrar la app, en RStudio pulsa el botón rojo **Stop** de la consola o
la tecla `Esc`.

---

## 3. Cómo se usa la app

1. **Datos** (botón de la izquierda en la barra de arriba): elige tu archivo.
   Formatos: CSV/TXT, Excel (XLS, XLSX), SPSS (SAV, ZSAV, POR), Stata (DTA) y
   Parquet. La primera columna debe tener los nombres de los individuos
   (países, muestras, cantones…). En SPSS y Stata, las variables con todos sus
   valores etiquetados (p. ej. 1 = Hombre, 2 = Mujer) se leen con las
   etiquetas y quedan como nominales; la casilla «Usar las etiquetas de valor»
   permite leer los códigos numéricos.
2. En **Tipo de columnas** revisa qué columnas quedaron como *nominales*. Son
   las columnas de agrupación (región, semana, grupo…). Sirven para colorear y
   agrupar, pero **nunca** entran como variables en los ejes. La app las
   propone sola; corrige si hace falta.
   En **Hoja de datos** se pueden corregir valores (doble clic en la celda y
   Enter), agregar o eliminar filas y columnas, restaurar el archivo original y
   descargar la versión editada en CSV o Excel. Los análisis usan los datos
   editados; el archivo original no se modifica.
3. Elige un método en la barra de arriba (por ejemplo **HJ-Biplot**). A la
   izquierda aparecen sus opciones y a la derecha los resultados, que se
   actualizan solos al cambiar algo.
4. Cada método tiene botones para descargar el gráfico (PNG, PDF, SVG) y las
   tablas (Excel).

---

### Tamaño de la matriz

No hay un número fijo de filas o columnas: depende de la memoria del equipo o
del servidor (la app gratuita de Posit Connect Cloud tiene 4 GB). Medido con el
aplicativo:

| Parte | Hasta dónde va bien | Por qué |
|---|---|---|
| Subir el archivo | 200 MB (`LIMITE_ARCHIVO_MB`) | Shiny solo acepta 5 MB si no se indica. |
| HJ-Biplot | 100 000 × 50 en unos 2 s; 500 000 × 20 en unos 7 s | La SVD es rápida y la memoria crece en línea con el tamaño. |
| Gráfico | Legible hasta unos pocos miles de puntos | Las etiquetas se ocultan solas cuando se superponen. |
| Bootstrap | 5000 × 20 con 500 réplicas en unos 10 s | Repite la SVD B veces. |
| Clúster | Hasta 5000 individuos (`LIMITE_CLUSTER`) | La matriz de distancias crece con n²: con 10 000 ocupa unos 4 GB. |

## 4. Qué es cada archivo

```
biplotlab/
├── iniciar.R              ← lo que se ejecuta para abrir la app
├── biplotlab.Rproj        ← proyecto de RStudio (se abre con doble clic)
├── README.md              ← esta guía
├── DESCRIPTION            ← ficha del paquete (no borrar)
├── NAMESPACE              ← índice de funciones (no borrar ni editar)
├── app.R                  ← solo para publicar la app en internet
├── R/                     ← TODO el código de la app
├── inst/                  ← archivos de apoyo (estilos, plantillas, datos)
└── tests/                 ← pruebas automáticas (ver sección 9)
```

### Archivos de la raíz

| Archivo | Para qué sirve | ¿Lo toco? |
|---|---|---|
| `iniciar.R` | Instala los paquetes que falten y abre la app. | Solo lo ejecutas. |
| `biplotlab.Rproj` | Abre RStudio en la carpeta correcta. | No. |
| `DESCRIPTION` | Ficha del paquete: nombre, versión y **lista de paquetes de R que usa** (sección `Imports`). R la necesita para cargar la app y el servidor la usa para saber qué instalar al publicarla. | Solo si tu método usa un paquete nuevo: agrégalo en `Imports`. |
| `NAMESPACE` | Índice de las funciones del paquete. Se genera automáticamente. | No. Ni borrar ni editar. |
| `app.R` | Punto de entrada para publicar la app en internet (Posit Connect Cloud). | No. |

### Carpeta `R/`: el código

Cada método está separado en **dos archivos**:

- `calculo_<método>.R`: la **estadística**. Funciones normales de R, sin nada
  de la interfaz. Es lo que también se puede usar desde la consola.
- `mod_<método>.R`: la **pantalla**. Las opciones de la izquierda, los
  resultados de la derecha y cómo se conectan con el cálculo.

| Método | Cálculo | Pantalla |
|---|---|---|
| **HJ-Biplot** (y GH / JK) | `calculo_hj_biplot.R` | `mod_hj_biplot.R` |
| Bootstrap del HJ-Biplot | `calculo_bootstrap.R` | `mod_bootstrap.R` |
| Clúster sobre coordenadas | `calculo_cluster.R` | `mod_cluster.R` |
| Biplot canónico / MANOVA (uno y dos factores, bootstrap) | `calculo_canonico.R`, `grafico_canonico.R` y `tablas_canonico.R` | `mod_canonico.R` |
| Biplot logístico, DPCA, Disjoint Biplot y CenetBiplot (app del compañero) | `equipo_logistico_externo.R`, `equipo_logistico_interno.R`, `equipo_dpca.R`, `equipo_cdpca.R`, `equipo_cenet.R`, `equipo_nucleo.R` y `equipo_graficos.R` (sin cambios) | `mod_equipo.R` |
| HJ-Biplot neutrosófico | `neutrosofico_calculo.R` (sin cambios) | `mod_neutrosofico.R` |
| GGE-Biplot (10 herramientas: quién ganó dónde, media vs estabilidad, rankings…) | `calculo_gge.R` y `tablas_gge.R` | `mod_gge.R` |

El resto de archivos de `R/` son comunes a toda la app. **No hace falta
tocarlos**, salvo `registro_analisis.R` en el paso 5 de la sección siguiente.

| Archivo | Para qué sirve |
|---|---|
| `registro_analisis.R` | La lista de métodos que aparecen en la barra de arriba. |
| `app.R` | La estructura general: barra de arriba, panel de datos y paneles. |
| `datos.R` | Lectura de archivos (CSV, Excel, SPSS, Stata, Parquet) y detección de columnas nominales. |
| `editar_datos.R` | Edición de la hoja en la app: celdas, filas, columnas y descarga de la versión editada. |
| `grafico_biplot.R` | El gráfico del biplot (lo usan HJ, Bootstrap y Clúster) y la lectura guiada. |
| `ui_componentes.R` | Piezas de pantalla comunes (encabezados, secciones, tablas, mensajes de error). |
| `sintaxis.R` | Ayudas para armar la pestaña "Sintaxis R". |
| `teoria.R` | El contenido de la pestaña **Teoría** de cada biplot: para qué sirve, qué datos espera y cuáles no usar, qué calcula (fórmulas), cómo leer el gráfico, dónde se calcula y referencias. Empieza con una tabla resumen. Las referencias van en dos grupos: `modulo` (material del Módulo 3) y `original` (obra original del método), todas comprobadas. Para tu método, agrega una entrada en la lista `TEORIA` con las mismas secciones y pon `teoria_ui("tu_id")` como pestaña. |
| `biplotlab-package.R` y `_disable_autoload.R` | Archivos técnicos del paquete. |

### Carpeta `inst/`

| Carpeta | Contenido |
|---|---|
| `inst/plantilla/` | **`mod_plantilla.R`**: la plantilla para tu método. **`test_plantilla.R`**: la plantilla para su prueba. |
| `inst/app/www/` | Estilos (colores, tipografía) de la app y KaTeX (licencia MIT), que dibuja las fórmulas de la pestaña Teoría sin conexión a internet. |
| `inst/extdata/` | Bases pequeñas que usa el código para sus pruebas internas. |

---

## 5. Cómo agregar tu método

La plantilla `inst/plantilla/mod_plantilla.R` ya **funciona tal como está**
(hace un ACP de ejemplo), así que puedes ver todo el recorrido antes de poner
tu código. Usamos el GGE como ejemplo; cambia `gge` por el nombre de tu
método.

**Paso 1. Copia la plantilla.**
Copia `inst/plantilla/mod_plantilla.R` a la carpeta `R/` y renómbrala
`mod_gge.R`.

**Paso 2. Ponle el nombre de tu método.**
Abre `R/mod_gge.R` en RStudio, pulsa `Ctrl + F`, busca `METODO` y reemplaza
**todas** las apariciones por `gge` (botón *All*). Usa un nombre corto, en
minúsculas, sin espacios ni tildes: `gge`, `canonico`, `logistico`, `dpca`,
`sparse`.

**Paso 3. Pon tu código de R.**
La plantilla tiene cinco partes, marcadas con letras:

| Parte | Qué es | Qué haces |
|---|---|---|
| **A) Cálculo** | La función `calcular_gge()`. | Pega aquí tu código de R, el que ya te funciona en tu práctica. Recibe los datos y las opciones y **devuelve una lista** con los resultados (tablas, coordenadas…). |
| **B) Registro** | Título, icono y descripción del botón. | Cambia los textos marcados con `TODO`. |
| **C) Opciones** | Los controles del panel izquierdo. | Agrega lo que el usuario debe elegir (variables, número de ejes, factor…). |
| **D) Resultados** | Las pestañas del panel derecho. | Agrega tus gráficos y tablas. |
| **E) Lógica** | Conecta las opciones con el cálculo. | Llama a tu función `calcular_gge()` con las opciones elegidas. |

Lo más importante es la parte **A**. Si se te complica la interfaz (C, D, E),
basta con que tu cálculo funcione dentro de `calcular_gge()`: lo demás se
puede completar entre todos.

**Paso 4. Si usas paquetes nuevos.**
- Agrégalos en `DESCRIPTION`, en la lista `Imports`.
- Agrégalos también en la línea `paquetes <- c(...)` de `iniciar.R`.
- En tu código llámalos con `paquete::funcion()`, por ejemplo
  `metan::gge(...)`. No uses `library()` dentro de la carpeta `R/`.

**Paso 5. Activa tu botón.**
En `R/registro_analisis.R` busca tu línea:

```r
proximamente("gge", "GGE-Biplot", "seedling",
             "Interacción genotipo × ambiente"),
```

y cámbiala por:

```r
analisis_gge(),
```

**Paso 6. Prueba en la app.**
Ejecuta otra vez `iniciar.R`. Tu método debe aparecer activo en la barra de
arriba. Carga tu base de prueba y comprueba que los números coincidan con los
de tu práctica.

**Paso 7. Escribe tu prueba automática.**
Sigue la sección 9. Así queda comprobado que tu método da los números
correctos y que nadie lo rompe después sin darse cuenta.

---

## 6. Reglas para que todo encaje

1. **Sin rutas fijas.** Nada de `C:/Users/...` ni `setwd()`. Los datos llegan
   desde el panel Datos de la app.
2. **Sin ventanas propias.** `biplotbootGUI::biplotboot()` y
   `GGEBiplotGUI::GGEBiplot()` abren ventanas aparte que no funcionan dentro de
   la app. Usa sus funciones de cálculo o el cálculo directo (por ejemplo
   `metan::gge()`).
3. **Respeta las columnas nominales.** No uses las columnas de agrupación como
   variables cuantitativas: llegan aparte, en `datos()$nominales`.
4. **Mensajes en español y claros.** Si algo falla, usa
   `stop("explicación del problema", call. = FALSE)`: la app lo muestra en un
   recuadro.
5. **Gráficos con ggplot2 y plano rectangular.** Si dibujas un biplot, usa
   `coord_equal()` para que los ángulos no se deformen, y muéstralo con el
   encuadre rectangular que pidió la Dra. (vale para todos los métodos). En tu
   `mod_<metodo>.R` basta con cambiar el `renderPlot()` del gráfico por:

   ```r
   output$grafico <- render_biplot(grafico, session, salida = "grafico")
   ```

   donde `grafico` es el `reactive()` que devuelve tu ggplot y `"grafico"` es el
   id de tu `plotOutput(ns("grafico"), height = "auto")`. Así el área del
   gráfico tiene ancho/alto = Δx/Δy con la misma escala en los dos ejes. Para
   guardar el gráfico con el mismo encuadre usa
   `descarga_biplot(grafico, function() "mi_grafico.png", "png", dpi = 300)`.
6. **No toques los archivos del HJ-Biplot, Bootstrap ni Clúster.** Si
   necesitas el resultado del HJ, la plantilla explica cómo recibirlo.
7. **Bases de prueba pequeñas.** Para probar basta un CSV o Excel pequeño; no
   hace falta subir bases grandes ni archivos `.parquet`.

---

## 7. Qué entregar

Cuando tu método funcione:

1. Tu archivo `R/mod_<método>.R` y tu prueba `tests/testthat/test-<método>.R`.
2. Si agregaste paquetes, tu `DESCRIPTION` y tu `iniciar.R` modificados (o
   simplemente la lista de paquetes).
3. Tu base de prueba pequeña.
4. Los resultados de tu práctica (valores propios, % de inercia, coordenadas o
   el estadístico principal de tu método), para comprobar que la app da lo
   mismo.

---

## 8. Problemas frecuentes

| Problema | Solución |
|---|---|
| `there is no package called 'biplotlab'` (al pegar la Sintaxis R) | BiplotLab no está en CRAN: instálalo desde `biplotlab_0.1.0.tar.gz` con `install.packages("C:/ruta/biplotlab_0.1.0.tar.gz", repos = NULL, type = "source")` (ver arriba). |
| `there is no package called '...'` (otro paquete) | Falta un paquete: instálalo con `install.packages("nombre")` o vuelve a ejecutar `iniciar.R`. |
| `could not find function "calcular_gge"` | Revisa que reemplazaste **todas** las palabras `METODO` y que el archivo está en la carpeta `R/` (no en `inst/`). |
| Mi botón sigue gris | Falta el paso 5: cambiar `proximamente(...)` por `analisis_gge()` en `R/registro_analisis.R`. |
| La app no encuentra los archivos | Abriste los archivos sueltos. Cierra RStudio y entra con doble clic en `biplotlab.Rproj`. |
| Las tildes salen raras | Guarda tu archivo en UTF-8: en RStudio, *File → Save with Encoding → UTF-8*. |
| Una columna de grupo entra como variable | En el panel Datos, márcala como nominal en **Tipo de columnas**. |

---

## 9. Pruebas automáticas: cómo comprobar que todo funciona

Una **prueba** es un pequeño código que calcula algo con la app y lo compara
con un número que ya sabemos que es correcto (por ejemplo, el % de inercia que
salió en MULTBIPLOT o en tu práctica). Si coincide, la prueba pasa; si no, R
avisa qué falló.

Las pruebas están en la carpeta `tests/testthat/`. Cada archivo prueba una
parte:

| Archivo | Qué comprueba |
|---|---|
| `test-hj_biplot.R` | Que el HJ-Biplot da lo mismo que MULTBIPLOT en la práctica de PISA y lo mismo que biplotbootGUI en la de los cantones. |
| `test-fitoplancton.R` | Que, con la matriz del fitoplancton de la Dra., la inercia, la tabla CRFE y el clúster coinciden con el material de clase. |
| `test-cluster-bootstrap.R` | Que el clúster y el bootstrap funcionan y dan resultados coherentes. |
| `test-datos.R` | Que se leen bien los archivos CSV, Excel, SPSS, Stata y Parquet (tildes, punto y coma, coma decimal, etiquetas de valor y perdidos de SPSS). |
| `test-editar-datos.R` | Que la edición de la hoja (coma decimal, nombres repetidos, filas y columnas nuevas) funciona y que el archivo descargado se vuelve a leer igual. |
| `test-encuadre.R` | Que el plano es rectangular con las ecuaciones de encuadre de la Dra. (ancho/alto = Δx/Δy, misma escala en los dos ejes). |
| `test-equipo.R` | Propiedades de DPCA (cargas disjuntas y ortonormales), CDBiplot, Cenet y el logístico externo (coeficientes iguales a `glm`), el volteo y que todas las pestañas Teoría estén completas (tabla resumen y los dos grupos de referencias). |
| `test-neutrosofico.R` | Las pruebas del módulo neutrosófico (matriz de resultado conocido, entradas inválidas) y la obtención de ternas desde respuestas o columnas _T, _I, _F. |
| `test-gge.R` | Que el GGE-Biplot da lo mismo que la práctica de Ontario y que `metan::gge()` (si está instalado), las 10 herramientas, el plano 4:3 y el volteo. |
| `test-canonico.R` | Que el Biplot canónico da lo mismo que `stats::manova()` y `MASS::lda()` con Iris, que las elipses están en la escala de W y que el diseño de dos factores (césped de Seber) coincide con `summary.manova`. |

### Cómo ejecutarlas

Con el proyecto `biplotlab.Rproj` abierto, escribe en la consola de RStudio:

```r
testthat::test_local(".")
```

El resultado muestra una línea por archivo:

```
cluster-bootstrap: ..................
datos: .......
fitoplancton: ......
gge: ..
hj_biplot: ..............................
```

- Cada **punto** (`.`) es una comprobación que pasó.
- Una **S** significa que se saltó. Pasa con `test-fitoplancton.R` si el
  archivo `familias FITOPLACTON HJ WD.xls` no está en la carpeta que contiene a
  `biplotlab`. No es un error.
- Un **número** o la palabra **Failed** indica una prueba que falló; debajo
  aparece cuál y qué valor esperaba.

Ejecútalas cada vez que cambies algo. Si antes pasaban y ahora fallan, el
último cambio rompió algo.

### Cómo escribir la prueba de tu método

1. Copia `inst/plantilla/test_plantilla.R` a `tests/testthat/` con el nombre
   `test-<método>.R` (por ejemplo `test-gge.R`).
2. Reemplaza `METODO` por el nombre de tu método, igual que en tu
   `mod_<método>.R`.
3. Cambia los **números esperados** por los de tu práctica. Por ejemplo, si en
   tu práctica el eje 1 recogió 45,93 % y el eje 2 25,95 %:

   ```r
   inercia_esperada <- c(45.93, 25.95)
   ```

   En R los decimales se escriben con **punto**.
4. Ejecuta `testthat::test_local(".")` y revisa que tu línea salga solo con
   puntos.

Si tu base de prueba no está en el paquete, guárdala en `inst/extdata/` y
léela en tu prueba con `leer_datos("../../inst/extdata/tu_base.csv")`.
