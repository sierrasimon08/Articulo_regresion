
# ============================================================
# ANALISIS DESCRIPTIVO
# Archivo: anal_descrip.R
# Repositorio: Articulo_regresion
# Base de datos: datos.xlsx
# ============================================================


# 1. INSTALAR Y CARGAR readxl
# ------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}


# 2. DESCARGAR LA BASE DE DATOS DESDE GITHUB
# ------------------------------------------------------------

url_datos <- paste0(
  "https://raw.githubusercontent.com/",
  "sierrasimon08/Articulo_regresion/main/datos.xlsx"
)

archivo_temporal <- tempfile(fileext = ".xlsx")

descarga <- tryCatch(
  {
    download.file(
      url = url_datos,
      destfile = archivo_temporal,
      mode = "wb",
      quiet = TRUE
    )
    TRUE
  },
  error = function(e) {
    message("Error al descargar la base de datos: ",
            conditionMessage(e))
    FALSE
  }
)

if (!descarga ||
    !file.exists(archivo_temporal) ||
    file.info(archivo_temporal)$size == 0) {

  stop(
    "No fue posible descargar datos.xlsx. ",
    "Verifique la conexion a internet y la URL de GitHub."
  )

}


# 3. LEER LA BASE DE DATOS
# ------------------------------------------------------------

datos <- tryCatch(
  as.data.frame(readxl::read_excel(archivo_temporal)),
  error = function(e) {
    stop(
      "No fue posible leer el archivo Excel: ",
      conditionMessage(e)
    )
  }
)

cat("\n========== BASE DE DATOS CARGADA ==========\n")
cat("Filas:", nrow(datos), "\n")
cat("Columnas:", ncol(datos), "\n")


# 4. INFORMACION GENERAL
# ------------------------------------------------------------

cat("\n========== ESTRUCTURA ==========\n")
str(datos)

cat("\n========== NOMBRES DE VARIABLES ==========\n")
print(names(datos))

cat("\n========== PRIMERAS OBSERVACIONES ==========\n")
print(head(datos))

cat("\n========== ULTIMAS OBSERVACIONES ==========\n")
print(tail(datos))


# 5. VALORES FALTANTES
# ------------------------------------------------------------

cat("\n========== VALORES FALTANTES ==========\n")

faltantes <- data.frame(
  Variable = names(datos),
  Cantidad = colSums(is.na(datos)),
  Porcentaje = round(
    colMeans(is.na(datos)) * 100, 2
  )
)

print(faltantes, row.names = FALSE)


# 6. RESUMEN DESCRIPTIVO GENERAL
# ------------------------------------------------------------

cat("\n========== RESUMEN GENERAL ==========\n")
print(summary(datos))


# 7. ESTADISTICOS DE VARIABLES NUMERICAS
# ------------------------------------------------------------

numericas <- datos[
  ,
  sapply(datos, is.numeric),
  drop = FALSE
]

if (ncol(numericas) > 0) {

  calcular_estadistico <- function(x, funcion) {
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(NA_real_)
    }

    funcion(x)
  }

  estadisticos <- data.frame(
    Variable = names(numericas),

    N = sapply(
      numericas,
      function(x) sum(is.finite(x))
    ),

    Media = sapply(
      numericas,
      calcular_estadistico,
      funcion = mean
    ),

    Mediana = sapply(
      numericas,
      calcular_estadistico,
      funcion = median
    ),

    Desviacion_estandar = sapply(
      numericas,
      calcular_estadistico,
      funcion = sd
    ),

    Varianza = sapply(
      numericas,
      calcular_estadistico,
      funcion = var
    ),

    Minimo = sapply(
      numericas,
      calcular_estadistico,
      funcion = min
    ),

    Q1 = sapply(
      numericas,
      calcular_estadistico,
      funcion = function(x) {
        quantile(x, 0.25, names = FALSE)
      }
    ),

    Q3 = sapply(
      numericas,
      calcular_estadistico,
      funcion = function(x) {
        quantile(x, 0.75, names = FALSE)
      }
    ),

    Maximo = sapply(
      numericas,
      calcular_estadistico,
      funcion = max
    ),

    row.names = NULL
  )

  estadisticos$Rango <- (
    estadisticos$Maximo - estadisticos$Minimo
  )

  estadisticos$Rango_intercuartilico <- (
    estadisticos$Q3 - estadisticos$Q1
  )

  cat("\n========== ESTADISTICOS NUMERICOS ==========\n")
  print(estadisticos, row.names = FALSE)

} else {

  cat("\nNo se encontraron variables numericas.\n")

}


# 8. HISTOGRAMAS
# ------------------------------------------------------------

if (ncol(numericas) > 0) {

  for (variable in names(numericas)) {

    valores <- numericas[[variable]]
    valores <- valores[is.finite(valores)]

    if (length(valores) > 0) {

      hist(
        valores,
        main = paste("Histograma de", variable),
        xlab = variable,
        ylab = "Frecuencia",
        col = "lightblue",
        border = "white"
      )

    }

  }

}


# 9. DIAGRAMAS DE CAJA
# ------------------------------------------------------------

if (ncol(numericas) > 0) {

  for (variable in names(numericas)) {

    valores <- numericas[[variable]]
    valores <- valores[is.finite(valores)]

    if (length(valores) > 0) {

      boxplot(
        valores,
        main = paste("Diagrama de caja de", variable),
        ylab = variable,
        col = "lightgreen"
      )

    }

  }

}


# 10. FRECUENCIAS DE VARIABLES CATEGORICAS
# ------------------------------------------------------------

categoricas <- datos[
  ,
  sapply(
    datos,
    function(x) is.factor(x) || is.character(x)
  ),
  drop = FALSE
]

if (ncol(categoricas) > 0) {

  cat("\n========== FRECUENCIAS CATEGORICAS ==========\n")

  for (variable in names(categoricas)) {

    cat("\nVariable:", variable, "\n")

    print(
      table(
        categoricas[[variable]],
        useNA = "ifany"
      )
    )

  }

} else {

  cat("\nNo se encontraron variables categoricas.\n")

}


# 11. MATRIZ DE CORRELACIONES
# ------------------------------------------------------------

if (ncol(numericas) >= 2) {

  # Reemplazar valores infinitos por NA
  numericas_cor <- numericas

  numericas_cor[] <- lapply(
    numericas_cor,
    function(x) {
      x[!is.finite(x)] <- NA_real_
      x
    }
  )

  # Conservar variables con al menos dos valores
  # observados diferentes
  variables_validas <- sapply(
    numericas_cor,
    function(x) {
      x <- x[!is.na(x)]
      length(unique(x)) >= 2
    }
  )

  numericas_cor <- numericas_cor[
    ,
    variables_validas,
    drop = FALSE
  ]

  if (ncol(numericas_cor) >= 2) {

    correlaciones <- cor(
      numericas_cor,
      use = "pairwise.complete.obs"
    )

    cat("\n========== MATRIZ DE CORRELACIONES ==========\n")
    print(round(correlaciones, 3))

  } else {

    cat(
      "\nNo hay suficientes variables numericas ",
      "para calcular correlaciones.\n",
      sep = ""
    )

  }

}


# 12. FINALIZACION
# ------------------------------------------------------------

cat("\n============================================\n")
cat("ANALISIS DESCRIPTIVO FINALIZADO\n")
cat("============================================\n")
