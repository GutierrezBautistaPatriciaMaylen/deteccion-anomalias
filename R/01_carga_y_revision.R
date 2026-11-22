# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
source(here("R", "00_bitacora.R"))

# 02 - Leer el dataset limpio
ejecutar_paso(
  id_paso  = 1, script = "01_carga_y_revision.R",
  tipo     = "Carga",
  accion   = "Leer persona_limpia.csv (versión del script 7 del preprocesamiento)",
  criterio = "Se usa el dataset ya limpio para evaluar si el preprocesamiento dejó anomalías",
  expr = {
    df <- read_csv(here("data", "persona_limpia.csv"), show_col_types = FALSE)
    stopifnot(nrow(df) == 15326, ncol(df) == 26)
    cat("Dimensiones:", nrow(df), "filas x", ncol(df), "columnas\n")
  }
)

# 03 - Resumen y casos extremos conocidos
vars_num <- c("edad", "aestudio", "phrs", "log_ylab")
ejecutar_paso(
  id_paso  = 2, script = "01_carga_y_revision.R",
  tipo     = "Revisión", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Resumen estadístico y conteo de casos extremos",
  criterio = "Conocer el rango de las variables numéricas que usarán GMM y LOF",
  expr = {
    print(summary(df[vars_num]))
    cat("Ingreso igual a 0:", sum(df$ylab == 0), "\n")
    cat("Horas semanales >= 100:", sum(df$phrs >= 100), "\n")
  }
)

# 04 - Histogramas, guardados como PNG
ejecutar_paso(
  id_paso  = 3, script = "01_carga_y_revision.R",
  tipo     = "Gráfico", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Histogramas de las variables numéricas (output/graficos/01_histogramas.png)",
  criterio = "Ver la forma de cada distribución antes de aplicar los modelos",
  expr = {
    g <- df |>
      select(all_of(vars_num)) |>
      pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
      ggplot(aes(valor)) +
      geom_histogram(bins = 40, fill = "steelblue", color = "white") +
      facet_wrap(~ variable, scales = "free") +
      labs(title = "Distribución de las variables numéricas",
           x = NULL, y = "Frecuencia") +
      theme_minimal()
    png(here("output", "graficos", "01_histogramas.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/01_histogramas.png\n")
  }
)
