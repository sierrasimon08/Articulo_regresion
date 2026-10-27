
# ============================================================
# ANALISIS DESCRIPTIVO DE LA BASE DE DATOS
# Archivo: anal_descrip.R
# Repositorio: Articulo_regresion
# ============================================================


# 1. INSTALAR Y CARGAR LA LIBRERIA NECESARIA
# ------------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}


# 2. LEER LA BASE DE DATOS DESDE GITHUB
# ------------------------------------------------------------

url_datos <- "https://raw.githubusercontent.com/sierrasimon08/Articulo_regresion/main/datos.xlsx"

datos <- readxl::read_excel(url_datos)

# Convertir a data.frame para facilitar el analisis
datos <- as.data.frame(datos)


# 3. INFORMACION GENERAL DE LA BASE
# ------------------------------------------------------------

cat("\n========== DIMENSIONES DE LA BASE ==========\n")

cat("Numero de filas:", nrow(datos), "\n")
cat("Numero de columnas:", ncol(datos), "\n")


cat("\n========== ESTRUCTURA DE LA BASE ==========\n")

str(datos)


cat("\n========== NOMBRES DE LAS VARIABLES ==========\n")

print(names(datos))


cat("\n========== PRIMERAS OBSERVACIONES ==========\n")

print(head(datos))


cat("\n========== ULTIMAS OBSERVACIONES ==========\n")

print(tail(datos))


# 4. IDENTIFICAR VALORES FALTANTES
# ------------------------------------------------------------

cat("\n========== VALORES FALTANTES ==========\n")

faltantes <- data.frame(
  Variable = names(datos),
  Cantidad = colSums(is.na(datos)),
  Porcentaje = round(
    colMeans(is.na(datos)) * 100,
    2
  )
)

print(faltantes, row.names = FALSE)


# 5. RESUMEN DESCRIPTIVO GENERAL
# ------------------------------------------------------------

cat("\n========== RESUMEN GENERAL ==========\n")

print(summary(datos))


# 6. ESTADISTICOS DESCRIPTIVOS DE VARIABLES NUMERICAS
# ------------------------------------------------------------

numericas <- datos[
  ,
  sapply(datos, is.numeric),
  drop = FALSE
]

if (ncol(numericas) > 0) {

  estadisticos <- data.frame(
    Variable = names(numericas),

    Media = sapply(
      numericas, mean, na.rm = TRUE
    ),

    Mediana = sapply(
      numericas, median, na.rm = TRUE
    ),

    Desviacion_estandar = sapply(
      numericas, sd, na.rm = TRUE
    ),

    Varianza = sapply(
      numericas, var, na.rm = TRUE
    ),

    Minimo = sapply(
      numericas, min, na.rm = TRUE
    ),

    Primer_cuartil = sapply(
      numericas,
      quantile,
      probs = 0.25,
      na.rm = TRUE
    ),

    Tercer_cuartil = sapply(
      numericas,
      quantile,
      probs = 0.75,
      na.rm = TRUE
    ),

    Maximo = sapply(
      numericas, max, na.rm = TRUE
    ),

    row.names = NULL
  )

  # Calcular el rango intercuartilico y el rango total
  estadisticos$Rango_intercuartilico <-
    estadisticos$Tercer_cuartil -
    estadisticos$Primer_cuartil

  estadisticos$Rango <-
    estadisticos$Maximo -
    estadisticos$Minimo

  cat("\n========== ESTADISTICOS NUMERICOS ==========\n")

  print(estadisticos, row.names = FALSE)

} else {

  cat("\nNo se encontraron variables numericas.\n")

}


# 7. HISTOGRAMAS DE LAS VARIABLES NUMERICAS
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


# 8. DIAGRAMAS DE CAJA DE LAS VARIABLES NUMERICAS
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


# 9. TABLAS DE FRECUENCIAS PARA VARIABLES CATEGORICAS
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


# 10. MATRIZ DE CORRELACIONES ENTRE VARIABLES NUMERICAS
# ------------------------------------------------------------

if (ncol(numericas) >= 2) {

  # Identificar variables con variabilidad
  variables_validas <- sapply(
    numericas,
    function(x) {
      sum(is.finite(x)) >= 2 &&
        length(unique(x[is.finite(x)])) > 1
    }
  )

  numericas_cor <- numericas[
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

    cat("\nNo hay suficientes variables numericas\n")
    cat("con variabilidad para calcular correlaciones.\n")

  }

}


# 11. FINALIZACION
# ------------------------------------------------------------

cat("\n============================================\n")
cat("ANALISIS DESCRIPTIVO FINALIZADO\n")
cat("============================================\n")
