# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
library(dbscan)
source(here("R", "00_bitacora.R"))

# 02 - Recuperar objetos si el script se ejecuta solo (no desde main.R)
if (!exists("df", inherits = FALSE)) {
  df <- read_csv(here("data", "persona_limpia.csv"), show_col_types = FALSE)
}
vars_num <- c("edad", "aestudio", "phrs", "log_ylab")
if (!exists("X", inherits = FALSE)) X <- scale(df[vars_num])
if (!exists("linea_base", inherits = FALSE)) {
  linea_base <- read_csv(here("output", "tablas", "linea_base.csv"), show_col_types = FALSE)
}
if (!exists("res_gmm", inherits = FALSE)) {
  res_gmm <- read_csv(here("output", "tablas", "resultados_gmm.csv"), show_col_types = FALSE)
}
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)

# 03 - Funciones auxiliares de LOF
# Con puntos repetidos y k pequeño, LOF puede dar Inf: se reemplaza por el máximo finito
lof_seguro <- function(X, k) {
  s <- lof(X, minPts = k)
  s[!is.finite(s)] <- max(s[is.finite(s)])
  s
}
marca_5 <- function(s) s >= quantile(s, 0.95)
jaccard <- function(a, b) sum(a & b) / sum(a | b)

# 04 - Sensibilidad de LOF al parámetro k
ejecutar_paso(
  id_paso  = 13, script = "04_lof.R",
  tipo     = "Modelo", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Calcular LOF con k = 5, 10, 20, 30, 50 y 100 vecinos y marcar el 5% más alto en cada caso",
  criterio = "LOF es sensible a k; antes de elegir un valor hay que ver cómo cambian los resultados y si detecta las anomalías conocidas",
  expr = {
    ks <- c(5, 10, 20, 30, 50, 100)
    marcas_k <- lapply(ks, function(k) marca_5(lof_seguro(X, k)))
    sens_lof <- tibble(
      k            = ks,
      marcadas     = sapply(marcas_k, sum),
      ingreso0     = sapply(marcas_k, function(a) sum(a & df$ylab == 0)),
      horas100     = sapply(marcas_k, function(a) sum(a & df$phrs >= 100)),
      coincide_k50 = round(sapply(marcas_k, jaccard, b = marcas_k[[which(ks == 50)]]), 3)
    )
    write_csv(sens_lof, here("output", "tablas", "lof_sensibilidad.csv"))
    print(as.data.frame(sens_lof))
    cat("\n(ingreso0: de 34 | horas100: de 99 | coincide_k50: coincidencia 0 a 1 con k = 50)\n")
  }
)

# 05 - LOF principal: máximo sobre un rango de k
ejecutar_paso(
  id_paso  = 14, script = "04_lof.R",
  tipo     = "Modelo", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Puntaje LOF final = máximo de LOF para k = 10, 20, 30, 40 y 50; marcar el 5% más alto",
  criterio = "Breunig et al. (2000) recomiendan k mínimo de 10 y tomar el máximo LOF sobre un rango de k, así el resultado no depende de un solo valor",
  expr = {
    ks_rango <- c(10, 20, 30, 40, 50)
    score_lof <- do.call(pmax, lapply(ks_rango, function(k) lof_seguro(X, k)))
    umbral <- quantile(score_lof, 0.95)
    res_lof <- tibble(
      id        = seq_len(nrow(df)),
      score_lof = score_lof,
      anom_lof  = score_lof >= umbral
    )
    write_csv(res_lof, here("output", "tablas", "resultados_lof.csv"))
    cat("Umbral (percentil 95):", round(umbral, 3), "\n")
    cat("Personas marcadas:", sum(res_lof$anom_lof), "de", nrow(res_lof), "\n")
  }
)

# 06 - Gráficos del LOF: distribución del puntaje y dispersión
ejecutar_paso(
  id_paso  = 15, script = "04_lof.R",
  tipo     = "Gráfico", variable = "aestudio, log_ylab",
  accion   = "Histograma del puntaje LOF con el umbral, y dispersión años de estudio vs ingreso con las anomalías en rojo",
  criterio = "Mostrar dónde cae el corte y qué zonas del plano marca LOF como anómalas",
  expr = {
    g1 <- ggplot(res_lof, aes(score_lof)) +
      geom_histogram(bins = 60, fill = "steelblue", color = "white") +
      geom_vline(xintercept = quantile(res_lof$score_lof, 0.95),
                 color = "red", linetype = "dashed") +
      labs(title = "LOF: puntaje de anomalía (máximo para k = 10 a 50)",
           subtitle = "Línea roja: umbral del 5% más anómalo. LOF cercano a 1 = densidad similar a sus vecinos",
           x = "Puntaje LOF", y = "Frecuencia") +
      theme_minimal()
    png(here("output", "graficos", "07_lof_puntaje.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g1)
    dev.off()
    
    d <- df[c("aestudio", "log_ylab")]
    d$anomalia <- factor(ifelse(res_lof$anom_lof, "Anomalía LOF", "Normal"),
                         levels = c("Normal", "Anomalía LOF"))
    g2 <- ggplot(d, aes(aestudio, log_ylab, color = anomalia)) +
      geom_jitter(width = 0.25, alpha = 0.45, size = 1) +
      scale_color_manual(values = c("Normal" = "grey70", "Anomalía LOF" = "red")) +
      labs(title = "LOF: anomalías en el plano estudios vs ingreso",
           x = "Años de estudio", y = "log(ingreso laboral)", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "08_lof_dispersion.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g2)
    dev.off()
    cat("Gráficos guardados: 07_lof_puntaje.png y 08_lof_dispersion.png\n")
  }
)

# 07 - Comparar LOF con la línea base, los casos conocidos y el GMM
ejecutar_paso(
  id_paso  = 16, script = "04_lof.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Cruzar LOF con IQR, ingresos en 0, horas >= 100 y con las anomalías del GMM",
  criterio = "Ver qué detecta cada método, qué detectan los dos y qué solo uno; es la base de la comparación final",
  expr = {
    resumen <- tibble(
      metodo       = c("IQR x1.5", "Z-score", "GMM", "LOF"),
      marcadas     = c(sum(linea_base$base_iqr), sum(linea_base$base_z),
                       sum(res_gmm$anom_gmm), sum(res_lof$anom_lof)),
      ingreso0     = c(sum(linea_base$base_iqr & df$ylab == 0), sum(linea_base$base_z & df$ylab == 0),
                       sum(res_gmm$anom_gmm & df$ylab == 0), sum(res_lof$anom_lof & df$ylab == 0)),
      horas100     = c(sum(linea_base$base_iqr & df$phrs >= 100), sum(linea_base$base_z & df$phrs >= 100),
                       sum(res_gmm$anom_gmm & df$phrs >= 100), sum(res_lof$anom_lof & df$phrs >= 100)),
      no_ve_el_iqr = c(0, sum(linea_base$base_z & !linea_base$base_iqr),
                       sum(res_gmm$anom_gmm & !linea_base$base_iqr),
                       sum(res_lof$anom_lof & !linea_base$base_iqr))
    )
    write_csv(resumen, here("output", "tablas", "comparacion_metodos.csv"))
    print(as.data.frame(resumen))
    
    cat("\nGMM vs LOF:\n")
    print(table(GMM = res_gmm$anom_gmm, LOF = res_lof$anom_lof))
    cat("Coincidencia GMM-LOF (0 a 1):",
        round(jaccard(res_gmm$anom_gmm, res_lof$anom_lof), 3), "\n")
    
    d <- df[c("aestudio", "log_ylab")]
    d$grupo <- case_when(
      res_gmm$anom_gmm & res_lof$anom_lof ~ "Ambos",
      res_gmm$anom_gmm                    ~ "Solo GMM",
      res_lof$anom_lof                    ~ "Solo LOF",
      TRUE                                ~ "Normal"
    )
    d$grupo <- factor(d$grupo, levels = c("Normal", "Solo GMM", "Solo LOF", "Ambos"))
    g <- ggplot(d, aes(aestudio, log_ylab, color = grupo)) +
      geom_jitter(width = 0.25, alpha = 0.5, size = 1) +
      scale_color_manual(values = c("Normal" = "grey75", "Solo GMM" = "blue",
                                    "Solo LOF" = "orange", "Ambos" = "red")) +
      labs(title = "GMM vs LOF: qué marca cada método",
           x = "Años de estudio", y = "log(ingreso laboral)", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "09_gmm_vs_lof.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/09_gmm_vs_lof.png\n")
  }
)