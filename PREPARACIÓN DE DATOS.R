# =============================================
# SCRIPT 1: PREPARACIÓN DE DATOS (CSV)
# Datos de matrícula en educación primaria mundial
# =============================================

# 1. Cargar librerías ----
library(tidyverse)

# 2. Crear dataset ----
datos_educacion <- data.frame(
  Año = 1970:2023,
  Alumnos_millones = c(
    401.7, 412.4, 425.9, 448.8, 463.3, 478.8, 490.7, 500.0, 502.4, 511.6,
    524.5, 533.5, 540.3, 544.4, 548.8, 551.8, 555.4, 557.6, 564.9, 571.9,
    576.8, 582.8, 588.9, 599.3, 610.4, 619.6, 628.4, 640.6, 648.8, 653.6,
    654.9, 654.0, 651.6, 655.0, 676.7, 679.3, 682.0, 689.8, 693.8, 695.0,
    697.5, 702.4, 710.5, 716.3, 715.1, 719.6, 737.0, 743.2, 732.3, 741.3,
    750.4, 754.6, 767.7, 771.4
  )
)

# 3. Agregar metadatos como comentario al dataframe ----
metadata <- paste(
  "Título: Alumnos de educación primaria en el mundo (1970-2023)",
  "Fuente: UNESCO Institute for Statistics",
  "Licencia: CC BY-4.0",
  "Unidades: Millones de alumnos",
  sep = "\n"
)

comment(datos_educacion) <- metadata

# 4. Crear carpeta 'datos' si no existe ----
if (!dir.exists("datos")) dir.create("datos")

# 5. Exportar CSV ----
write.csv(datos_educacion, 
          "datos_educacion_primaria.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")

# 6. Verificación ----
cat("✓ CSV creado en: 'datos_educacion_primaria.csv'\n",
    "Periodo:", range(datos_educacion$Año)[1], "-", 
    range(datos_educacion$Año)[2], "\n",
    "Registros:", nrow(datos_educacion), "\n")
