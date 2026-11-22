# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
source(here("R", "00_bitacora.R"))

# 02 - Cargar el dataset si aún no está en memoria
if (!exists("df", inherits = FALSE)) {
  df <- read_csv(here("data", "persona_limpia.csv"), show_col_types = FALSE)
}
vars_num <- c("edad", "aestudio", "phrs", "log_ylab")
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)

# 03 - Línea base: criterio de Tukey (IQR) y Z-score
ejecutar_paso(
  id_paso  = 4, script = "02_linea_base.R",
  tipo     = "Línea base", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Marcar atípicos por variable con IQR (1.5 y 3) y Z-score (|z| > 3)",
  criterio = "Son los métodos clásicos univariados; sirven de referencia para comparar GMM y LOF, que son multivariados",
  expr = {
    marca_iqr <- function(x, k = 1.5) {
      q <- quantile(x, c(0.25, 0.75))
      i <- q[2] - q[1]
      x < q[1] - k * i | x > q[2] + k * i
    }
    iqr_mat <- sapply(df[vars_num], marca_iqr)
    ext_mat <- sapply(df[vars_num], marca_iqr, k = 3)
    z_mat   <- abs(scale(df[vars_num])) > 3
    
    linea_base <- tibble(
      id           = seq_len(nrow(df)),
      base_iqr     = rowSums(iqr_mat) > 0,
      base_extremo = rowSums(ext_mat) > 0,
      base_z       = rowSums(z_mat) > 0
    )
    resumen_base <- tibble(
      variable  = vars_num,
      n_iqr     = colSums(iqr_mat),
      n_extremo = colSums(ext_mat),
      n_z       = colSums(z_mat)
    )
    write_csv(linea_base,   here("output", "tablas", "linea_base.csv"))
    write_csv(resumen_base, here("output", "tablas", "linea_base_resumen.csv"))
    
    print(as.data.frame(resumen_base))
    cat("\nFilas con al menos una variable atípica:\n")
    cat("  IQR x1.5:", sum(linea_base$base_iqr),
        "| IQR x3:", sum(linea_base$base_extremo),
        "| Z > 3:", sum(linea_base$base_z), "\n")
  }
)

# 04 - Boxplots de las variables numéricas
ejecutar_paso(
  id_paso  = 5, script = "02_linea_base.R",
  tipo     = "Gráfico", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Boxplots con los atípicos de Tukey en rojo (output/graficos/02_boxplots.png)",
  criterio = "Mostrar visualmente qué detecta la regla del IQR en cada variable",
  expr = {
    g <- df |>
      select(all_of(vars_num)) |>
      pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
      ggplot(aes(x = "", y = valor)) +
      geom_boxplot(fill = "steelblue", alpha = 0.6,
                   outlier.color = "red", outlier.alpha = 0.4) +
      facet_wrap(~ variable, scales = "free_y") +
      labs(title = "Boxplots: atípicos según el criterio de Tukey",
           x = NULL, y = NULL) +
      theme_minimal()
    png(here("output", "graficos", "02_boxplots.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/02_boxplots.png\n")
  }
)

# 05 - Dispersión años de estudio vs ingreso, con la línea base marcada
ejecutar_paso(
  id_paso  = 6, script = "02_linea_base.R",
  tipo     = "Gráfico", variable = "aestudio, log_ylab",
  accion   = "Dispersión de años de estudio vs log del ingreso, coloreada por IQR y Z-score",
  criterio = "Ver dónde caen los atípicos de la línea base en el plano que es el objetivo del proyecto",
  expr = {
    d <- df[c("aestudio", "log_ylab")]
    d$marca <- case_when(
      linea_base$base_iqr & linea_base$base_z ~ "IQR y Z-score",
      linea_base$base_iqr                      ~ "Solo IQR",
      linea_base$base_z                        ~ "Solo Z-score",
      TRUE                                     ~ "Normal"
    )
    d$marca <- factor(d$marca,
                      levels = c("Normal", "Solo IQR", "IQR y Z-score", "Solo Z-score"))
    g <- ggplot(d, aes(aestudio, log_ylab, color = marca)) +
      geom_jitter(width = 0.25, alpha = 0.45, size = 1) +
      scale_color_manual(values = c("Normal" = "grey70", "Solo IQR" = "orange",
                                    "IQR y Z-score" = "red", "Solo Z-score" = "purple")) +
      labs(title = "Línea base: atípicos univariados",
           x = "Años de estudio", y = "log(ingreso laboral)", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "03_dispersion_linea_base.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/03_dispersion_linea_base.png\n")
    print(table(d$marca))
  }
)

# 06 - Matriz estandarizada para GMM y LOF
ejecutar_paso(
  id_paso  = 7, script = "02_linea_base.R",
  tipo     = "Preparación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Estandarizar las 4 variables numéricas (media 0, desviación 1) y revisar correlaciones",
  criterio = "LOF usa distancias y GMM usa varianzas: sin estandarizar, el ingreso y las horas dominarían a edad y años de estudio",
  expr = {
    X <- scale(df[vars_num])
    stopifnot(!anyNA(X))
    cat("Medias (deben ser ~0):\n");  print(round(colMeans(X), 3))
    cat("Desviaciones (deben ser 1):\n"); print(round(apply(X, 2, sd), 3))
    cat("\nCorrelaciones:\n"); print(round(cor(X), 2))
  }
)