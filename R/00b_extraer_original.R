# 01 - Extraer del dataset original solo las columnas necesarias
library(tidyverse)
library(here)

# 02 - Ruta del archivo original (cambia por la tuya)
ruta_original <- "C:/Users/lenovo/Desktop/persona.csv"

# 03 - Leer solo las columnas que se necesitan; las demás (salud, etc.) se ignoran
columnas <- c("folio", "nro", "s01a_03", "aestudio", "phrs", "ylab", "condact", "s04b_12")
raw_min <- read_csv(ruta_original, col_select = any_of(columnas), show_col_types = FALSE)

# 04 - Verificar qué se encontró
cat("Filas:", nrow(raw_min), "\n")
cat("Columnas encontradas:", paste(names(raw_min), collapse = ", "), "\n")
cat("Columnas que faltan:", paste(setdiff(columnas, names(raw_min)), collapse = ", "), "\n")
print(colSums(is.na(raw_min)))

# 05 - Guardar el extracto dentro del proyecto
write_csv(raw_min, here("data", "persona_original_min.csv"))