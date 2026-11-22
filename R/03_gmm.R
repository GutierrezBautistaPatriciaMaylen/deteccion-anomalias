# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
library(mclust)
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
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)

# 03 - Ajustar el GMM y elegir el modelo por BIC
ejecutar_paso(
  id_paso  = 8, script = "03_gmm.R",
  tipo     = "Modelo", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Ajustar mezclas gaussianas (mclust) con 1 a 8 componentes y 14 estructuras de covarianza; elegir por BIC",
  criterio = "El BIC penaliza la complejidad y es el criterio estándar de mclust para elegir el modelo y el número de componentes",
  expr = {
    set.seed(123)   # mclust sortea un subconjunto inicial cuando n > 2000
    gmm <- Mclust(X, G = 1:8)
    print(summary(gmm))
    png(here("output", "graficos", "04_gmm_bic.png"),
        width = 9, height = 6, units = "in", res = 150)
    plot(gmm, what = "BIC")
    dev.off()
    cat("Gráfico guardado en output/graficos/04_gmm_bic.png\n")
  }
)

# 04 - Puntaje de anomalía por densidad y umbral del 5%
ejecutar_paso(
  id_paso  = 9, script = "03_gmm.R",
  tipo     = "Modelo", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Calcular -log(densidad) de cada persona bajo el GMM y marcar el 5% con menor densidad",
  criterio = "Una persona con densidad baja es poco probable bajo el modelo; el 5% permite comparar con LOF usando la misma proporción de marcados",
  expr = {
    log_dens  <- dens(data = X, modelName = gmm$modelName,
                      parameters = gmm$parameters, logarithm = TRUE)
    score_gmm <- -log_dens
    umbral    <- quantile(score_gmm, 0.95)
    res_gmm <- tibble(
      id        = seq_len(nrow(df)),
      score_gmm = score_gmm,
      anom_gmm  = score_gmm >= umbral
    )
    write_csv(res_gmm, here("output", "tablas", "resultados_gmm.csv"))
    cat("Umbral (percentil 95):", round(umbral, 2), "\n")
    cat("Personas marcadas:", sum(res_gmm$anom_gmm), "de", nrow(res_gmm), "\n")
  }
)

# 05 - Gráficos del GMM: distribución del puntaje y dispersión
ejecutar_paso(
  id_paso  = 10, script = "03_gmm.R",
  tipo     = "Gráfico", variable = "aestudio, log_ylab",
  accion   = "Histograma del puntaje GMM con el umbral, y dispersión años de estudio vs ingreso con las anomalías en rojo",
  criterio = "Mostrar dónde cae el corte y qué zonas del plano marca el modelo como anómalas",
  expr = {
    g1 <- ggplot(res_gmm, aes(score_gmm)) +
      geom_histogram(bins = 60, fill = "steelblue", color = "white") +
      geom_vline(xintercept = quantile(res_gmm$score_gmm, 0.95),
                 color = "red", linetype = "dashed") +
      labs(title = "GMM: puntaje de anomalía (-log densidad)",
           subtitle = "Línea roja: umbral del 5% más anómalo",
           x = "Puntaje de anomalía", y = "Frecuencia") +
      theme_minimal()
    png(here("output", "graficos", "05_gmm_puntaje.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g1)
    dev.off()
    
    d <- df[c("aestudio", "log_ylab")]
    d$anomalia <- factor(ifelse(res_gmm$anom_gmm, "Anomalía GMM", "Normal"),
                         levels = c("Normal", "Anomalía GMM"))
    g2 <- ggplot(d, aes(aestudio, log_ylab, color = anomalia)) +
      geom_jitter(width = 0.25, alpha = 0.45, size = 1) +
      scale_color_manual(values = c("Normal" = "grey70", "Anomalía GMM" = "red")) +
      labs(title = "GMM: anomalías en el plano estudios vs ingreso",
           x = "Años de estudio", y = "log(ingreso laboral)", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "06_gmm_dispersion.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g2)
    dev.off()
    cat("Gráficos guardados: 05_gmm_puntaje.png y 06_gmm_dispersion.png\n")
  }
)

# 06 - Comparar el GMM con la línea base y con los casos conocidos
ejecutar_paso(
  id_paso  = 11, script = "03_gmm.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Cruzar las anomalías del GMM con IQR, Z-score, ingresos en 0 y horas >= 100",
  criterio = "Los 34 ingresos en 0 y las 99 personas con 100 horas o más son anomalías conocidas; sirven para ver si el modelo las encuentra",
  expr = {
    cat("GMM vs IQR x1.5:\n");  print(table(GMM = res_gmm$anom_gmm, IQR = linea_base$base_iqr))
    cat("\nGMM vs Z-score:\n"); print(table(GMM = res_gmm$anom_gmm, Z = linea_base$base_z))
    cat("\nIngresos en 0 marcados por GMM:", sum(res_gmm$anom_gmm & df$ylab == 0),
        "de", sum(df$ylab == 0), "\n")
    cat("Horas >= 100 marcadas por GMM:", sum(res_gmm$anom_gmm & df$phrs >= 100),
        "de", sum(df$phrs >= 100), "\n")
    solo_gmm <- res_gmm$anom_gmm & !linea_base$base_iqr
    cat("\nMarcadas por GMM que el IQR no detecta:", sum(solo_gmm), "\n")
    print(summary(df[solo_gmm, vars_num]))
  }
)

# 07 - Sensibilidad: ¿cambia el resultado si cambia el número de componentes?
ejecutar_paso(
  id_paso  = 12, script = "03_gmm.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Repetir el GMM con 2, 3 y 4 componentes y comparar con el modelo elegido por BIC",
  criterio = "El BIC siguió mejorando hasta el máximo de componentes probado; hay que ver si las anomalías dependen de esa elección",
  expr = {
    marcar <- function(fit) {
      s <- -dens(data = X, modelName = fit$modelName,
                 parameters = fit$parameters, logarithm = TRUE)
      s >= quantile(s, 0.95)
    }
    fila <- function(nombre, a) {
      tibble(modelo = nombre,
             marcadas = sum(a),
             coincide_con_bic = round(sum(a & res_gmm$anom_gmm) / sum(a | res_gmm$anom_gmm), 3),
             ingreso0 = sum(a & df$ylab == 0),
             horas100 = sum(a & df$phrs >= 100))
    }
    set.seed(123)
    ajustes <- lapply(2:4, function(g) {
      f <- Mclust(X, G = g, modelNames = c("VVV", "VEV", "EEE"))
      if (is.null(f$modelName)) return(NULL)
      fila(paste0(f$modelName, ", G=", g), marcar(f))
    })
    sens <- bind_rows(
      fila(paste0("BIC (", gmm$modelName, ", G=", gmm$G, ")"), res_gmm$anom_gmm),
      ajustes
    )
    write_csv(sens, here("output", "tablas", "gmm_sensibilidad.csv"))
    print(as.data.frame(sens))
    cat("\n(coincide_con_bic = coincidencia entre conjuntos de anomalías, de 0 a 1)\n")
  }
)