# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
library(isotree)
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
if (!exists("res_lof", inherits = FALSE)) {
  res_lof <- read_csv(here("output", "tablas", "resultados_lof.csv"), show_col_types = FALSE)
}
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)

# 03 - Funciones auxiliares
marca_5 <- function(s) s >= quantile(s, 0.95)
jaccard <- function(a, b) sum(a & b) / sum(a | b)
ajustar_if <- function(X, sample_size = 256, seed = 123) {
  isolation.forest(X, ndim = 1, ntrees = 500, sample_size = sample_size,
                   seed = seed, nthreads = 1)
}

# 04 - Isolation Forest: ajuste y puntaje de anomalía
ejecutar_paso(
  id_paso  = 17, script = "05_isolation_forest.R",
  tipo     = "Modelo", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Ajustar Isolation Forest clásico (500 árboles, muestra de 256, semilla 123) y marcar el 5% con mayor puntaje",
  criterio = "Es un método basado en aislamiento, no en densidad; permite ver si un tercer enfoque coincide con GMM o con LOF. 256 y 500 son los valores del artículo original (Liu et al., 2008)",
  expr = {
    modelo_if <- ajustar_if(X)
    score_if  <- predict(modelo_if, X)
    umbral    <- quantile(score_if, 0.95)
    res_if <- tibble(
      id       = seq_len(nrow(df)),
      score_if = score_if,
      anom_if  = score_if >= umbral
    )
    write_csv(res_if, here("output", "tablas", "resultados_if.csv"))
    cat("Rango del puntaje:", round(range(score_if), 3), "\n")
    cat("Umbral (percentil 95):", round(umbral, 3), "\n")
    cat("Personas marcadas:", sum(res_if$anom_if), "de", nrow(res_if), "\n")
  }
)

# 05 - Gráficos del Isolation Forest
ejecutar_paso(
  id_paso  = 18, script = "05_isolation_forest.R",
  tipo     = "Gráfico", variable = "aestudio, log_ylab",
  accion   = "Histograma del puntaje con el umbral y dispersión años de estudio vs ingreso con las anomalías en rojo",
  criterio = "Mostrar dónde cae el corte y qué zonas del plano marca el Isolation Forest como anómalas",
  expr = {
    g1 <- ggplot(res_if, aes(score_if)) +
      geom_histogram(bins = 60, fill = "steelblue", color = "white") +
      geom_vline(xintercept = quantile(res_if$score_if, 0.95),
                 color = "red", linetype = "dashed") +
      labs(title = "Isolation Forest: puntaje de anomalía",
           subtitle = "Línea roja: umbral del 5% más anómalo. Más cerca de 1 = se aísla más fácil",
           x = "Puntaje", y = "Frecuencia") +
      theme_minimal()
    png(here("output", "graficos", "10_if_puntaje.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g1)
    dev.off()
    
    d <- df[c("aestudio", "log_ylab")]
    d$anomalia <- factor(ifelse(res_if$anom_if, "Anomalía IF", "Normal"),
                         levels = c("Normal", "Anomalía IF"))
    g2 <- ggplot(d, aes(aestudio, log_ylab, color = anomalia)) +
      geom_jitter(width = 0.25, alpha = 0.45, size = 1) +
      scale_color_manual(values = c("Normal" = "grey70", "Anomalía IF" = "red")) +
      labs(title = "Isolation Forest: anomalías en el plano estudios vs ingreso",
           x = "Años de estudio", y = "log(ingreso laboral)", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "11_if_dispersion.png"),
        width = 9, height = 6, units = "in", res = 150)
    print(g2)
    dev.off()
    cat("Gráficos guardados: 10_if_puntaje.png y 11_if_dispersion.png\n")
  }
)

# 06 - Comparación de los cuatro métodos
ejecutar_paso(
  id_paso  = 19, script = "05_isolation_forest.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Tabla comparativa de IQR, Z-score, GMM, LOF e Isolation Forest, y matriz de coincidencia entre métodos",
  criterio = "Medir qué tanto se parecen los métodos entre sí y cuáles de las anomalías conocidas encuentra cada uno",
  expr = {
    marcas <- list(
      "IQR x1.5" = linea_base$base_iqr,
      "Z-score"  = linea_base$base_z,
      "GMM"      = res_gmm$anom_gmm,
      "LOF"      = res_lof$anom_lof,
      "IF"       = res_if$anom_if
    )
    resumen <- tibble(
      metodo       = names(marcas),
      marcadas     = sapply(marcas, sum),
      ingreso0     = sapply(marcas, function(a) sum(a & df$ylab == 0)),
      horas100     = sapply(marcas, function(a) sum(a & df$phrs >= 100)),
      no_ve_el_iqr = sapply(marcas, function(a) sum(a & !linea_base$base_iqr))
    )
    write_csv(resumen, here("output", "tablas", "comparacion_metodos.csv"))
    print(as.data.frame(resumen))
    
    nombres <- c("IQR x1.5", "GMM", "LOF", "IF")
    mat <- outer(nombres, nombres, Vectorize(function(a, b) jaccard(marcas[[a]], marcas[[b]])))
    dimnames(mat) <- list(nombres, nombres)
    cat("\nCoincidencia entre métodos (0 a 1):\n")
    print(round(mat, 2))
    write_csv(as.data.frame(mat) |> rownames_to_column("metodo"),
              here("output", "tablas", "coincidencia_metodos.csv"))
    
    dm <- as.data.frame(as.table(mat))
    names(dm) <- c("a", "b", "coincidencia")
    g <- ggplot(dm, aes(a, b, fill = coincidencia)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.2f", coincidencia))) +
      scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
      labs(title = "Coincidencia entre métodos (índice de Jaccard)",
           x = NULL, y = NULL, fill = NULL) +
      theme_minimal()
    png(here("output", "graficos", "12_coincidencia_metodos.png"),
        width = 8, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/12_coincidencia_metodos.png\n")
  }
)

# 07 - Sensibilidad del Isolation Forest a la semilla y al tamaño de muestra
ejecutar_paso(
  id_paso  = 20, script = "05_isolation_forest.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Repetir el Isolation Forest con otras semillas y con muestras de 128, 512 y 2048",
  criterio = "El método usa azar y un tamaño de muestra; hay que ver si las anomalías cambian al variar esos parámetros",
  expr = {
    configs <- tibble(
      configuracion = c("semilla 1", "semilla 2", "semilla 3",
                        "muestra 128", "muestra 512", "muestra 2048"),
      semilla       = c(1, 2, 3, 123, 123, 123),
      muestra       = c(256, 256, 256, 128, 512, 2048)
    )
    sens_if <- bind_rows(lapply(seq_len(nrow(configs)), function(i) {
      a <- marca_5(predict(ajustar_if(X, configs$muestra[i], configs$semilla[i]), X))
      tibble(configuracion = configs$configuracion[i],
             ingreso0      = sum(a & df$ylab == 0),
             horas100      = sum(a & df$phrs >= 100),
             coincide_base = round(jaccard(a, res_if$anom_if), 3))
    }))
    write_csv(sens_if, here("output", "tablas", "if_sensibilidad.csv"))
    print(as.data.frame(sens_if))
    cat("\n(coincide_base = coincidencia con la configuración principal: muestra 256, semilla 123)\n")
  }
)