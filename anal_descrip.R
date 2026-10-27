
# ============================================================
# ANÁLISIS DESCRIPTIVO DE LA BASE DE DATOS
# Archivo: anal_descrip.R
# ============================================================

# 1. Leer la base de datos desde GitHub
url_datos <- "https://raw.githubusercontent.com/sierrasimon08/Articulo_regresion/main/datos.csv"

datos <- read.csv(
  url_datos,
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 2. Información general de la base
cat("\n========== DIMENSIONES DE LA BASE ==========\n")
cat("Número de filas:", nrow(datos), "\n")
cat("Número de columnas:", ncol(datos), "\n")

cat("\n========== ESTRUCTURA DE LA BASE ==========\n")
str(datos)

cat("\n========== PRIMERAS OBSERVACIONES ==========\n")
print(head(datos))

cat("\n========== NOMBRES DE LAS VARIABLES ==========\n")
print(names(datos))

# 3. Valores faltantes por variable
cat("\n========== VALORES FALTANTES ==========\n")
faltantes <- data.frame(
  Variable = names(datos),
  Cantidad = colSums(is.na(datos)),
  Porcentaje = round(colMeans(is.na(datos)) * 100, 2)
)
print(faltantes)

# 4. Resumen descriptivo general
cat("\n========== RESUMEN GENERAL ==========\n")
print(summary(datos))

# 5. Estadísticos descriptivos de variables numéricas
numericas <- datos[
  ,
  sapply(datos, is.numeric),
  drop = FALSE
]

if (ncol(numericas) > 0) {

  estadisticos <- data.frame(
    Variable = names(numericas),
    Media = sapply(numericas, mean, na.rm = TRUE),
    Mediana = sapply(numericas, median, na.rm = TRUE),
    Desviacion_estandar = sapply(
      numericas, sd, na.rm = TRUE
    ),
    Minimo = sapply(numericas, min, na.rm = TRUE),
    Q1 = sapply(
      numericas, quantile, probs = 0.25, na.rm = TRUE
    ),
    Q3 = sapply(
      numericas, quantile, probs = 0.75, na.rm = TRUE
    ),
    Maximo = sapply(numericas, max, na.rm = TRUE),
    row.names = NULL
  )

  estadisticos$Rango <- estadisticos$Maximo -
    estadisticos$Minimo

  cat("\n========== ESTADÍSTICOS NUMÉRICOS ==========\n")
  print(estadisticos)

  # 6. Histogramas de las variables numéricas
  for (variable in names(numericas)) {
    hist(
      numericas[[variable]],
      main = paste("Histograma de", variable),
      xlab = variable,
      col = "lightblue",
      border = "white"
    )
  }

  # 7. Diagramas de caja
  for (variable in names(numericas)) {
    boxplot(
      numericas[[variable]],
      main = paste("Diagrama de caja de", variable),
      ylab = variable,
      col = "lightgreen"
    )
  }

} else {
  cat("\nNo se encontraron variables numéricas.\n")
}

# 8. Frecuencias de variables categóricas
categoricas <- datos[
  ,
  sapply(datos, function(x) is.factor(x) || is.character(x)),
  drop = FALSE
]

if (ncol(categoricas) > 0) {
  cat("\n========== FRECUENCIAS CATEGÓRICAS ==========\n")

  for (variable in names(categoricas)) {
    cat("\nVariable:", variable, "\n")
    print(table(categoricas[[variable]], useNA = "ifany"))
  }
}

cat("\n========== ANÁLISIS FINALIZADO ==========\n")
