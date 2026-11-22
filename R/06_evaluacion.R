# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
library(mclust)
library(dbscan)
library(isotree)
source(here("R", "00_bitacora.R"))

# 02 - Configuración y objetos de partida
if (!exists("df", inherits = FALSE)) {
  df <- read_csv(here("data", "persona_limpia.csv"), show_col_types = FALSE)
}
vars_num <- c("edad", "aestudio", "phrs", "log_ylab")
N_REP <- 5   # número de réplicas del experimento (bajar a 2 para una prueba rápida)
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)

# 03 - Funciones: LOF seguro, Isolation Forest y métricas de evaluación
lof_seguro <- function(X, k) {
  s <- lof(X, minPts = k)
  s[!is.finite(s)] <- max(s[is.finite(s)])
  s
}
ajustar_if <- function(X, sample_size = 256, seed = 123) {
  isolation.forest(X, ndim = 1, ntrees = 500, sample_size = sample_size,
                   seed = seed, nthreads = 1)
}
# AUC-ROC por rangos (equivale a la probabilidad de que una anomalía puntúe más que un normal)
auc_roc <- function(s, y) {
  r <- rank(s); n1 <- sum(y); n0 <- sum(!y)
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# Precisión promedio (área bajo la curva precisión-recall)
auc_pr <- function(s, y) {
  yy <- y[order(s, decreasing = TRUE)]
  sum((cumsum(yy) / seq_along(yy))[yy]) / sum(yy)
}
curva_roc <- function(s, y) {
  yy <- y[order(s, decreasing = TRUE)]
  tibble(fpr = cumsum(!yy) / sum(!yy), tpr = cumsum(yy) / sum(yy))
}

# 04 - Función que inyecta anomalías de los tres tipos del marco teórico
# Global: valores muy lejanos en tres de las cuatro variables (puntos aislados).
# Contextual A: edad 18-20 con 20-23 años de estudio (cada valor es plausible solo, la combinación no).
# Contextual B: 0-1 años de estudio con ingreso entre los más altos (sin salirse de rango).
# Colectiva: grupo compacto de 40 personas en una zona casi vacía del espacio.
inyectar <- function(base, seed, n_glob = 80, n_ctx = 80, n_col = 40) {
  set.seed(seed)
  muestra <- function(n) base[sample(nrow(base), n), ]
  mu <- sapply(base, mean); sdv <- sapply(base, sd)
  
  g <- muestra(n_glob)
  for (i in seq_len(n_glob)) {
    for (v in sample(vars_num, 3)) {
      val <- mu[v] + sample(c(-1, 1), 1) * runif(1, 3.5, 5) * sdv[v]
      if (val < 0) val <- mu[v] + abs(val - mu[v])
      g[i, v] <- val
    }
  }
  ca <- muestra(n_ctx)
  ca$edad     <- sample(18:20, n_ctx, replace = TRUE)
  ca$aestudio <- sample(20:23, n_ctx, replace = TRUE)
  cb <- muestra(n_ctx)
  cb$aestudio <- sample(0:1, n_ctx, replace = TRUE)
  cb$log_ylab <- runif(n_ctx, 9.3, 9.7)
  co <- tibble(edad     = sample(55:57, n_col, replace = TRUE),
               aestudio = sample(1:2, n_col, replace = TRUE),
               phrs     = round(rnorm(n_col, 15, 2)),
               log_ylab = rnorm(n_col, 9.0, 0.1))
  
  bind_rows(
    mutate(base, tipo = "Normal"),
    mutate(g,  tipo = "Global"),
    mutate(ca, tipo = "Contextual A"),
    mutate(cb, tipo = "Contextual B"),
    mutate(co, tipo = "Colectiva")
  )
}
tipos_anom <- c("Global", "Contextual A", "Contextual B", "Colectiva")

# 05 - Construir el conjunto de prueba (réplica 1) y revisar la inyección
ejecutar_paso(
  id_paso  = 21, script = "06_evaluacion.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Crear un conjunto de prueba: datos reales sin los casos extremos conocidos + 280 anomalías inyectadas de 4 tipos (global, contextual A, contextual B, colectiva)",
  criterio = "Sin etiquetas reales no se puede medir cuál método es mejor; al inyectar anomalías de tipo conocido sí se pueden calcular precisión, recall y AUC. Se excluyen de la base los 34 ingresos en 0 y las 99 personas con 100 horas o más para que no cuenten como normales",
  expr = {
    base_eval <- df[!(df$ylab == 0 | df$phrs >= 100), vars_num]
    aug1 <- inyectar(base_eval, seed = 1)
    cat("Base real:", nrow(base_eval), "filas (se excluyeron", nrow(df) - nrow(base_eval), "casos extremos conocidos)\n")
    print(table(aug1$tipo))
    cat("Proporción de anomalías inyectadas:",
        round(100 * mean(aug1$tipo != "Normal"), 1), "%\n")
    
    # ¿Las anomalías inyectadas se ven solas en alguna variable? (|z| > 3)
    z1 <- abs(scale(aug1[vars_num]))
    cat("\nPorcentaje con alguna variable |z| > 3, por tipo:\n")
    print(round(100 * tapply(apply(z1, 1, max) > 3, aug1$tipo, mean), 1))
    
    d <- bind_rows(
      transmute(aug1, x = aestudio, y = log_ylab, tipo, panel = "Años de estudio vs log(ingreso)"),
      transmute(aug1, x = edad, y = aestudio, tipo, panel = "Edad vs años de estudio")
    )
    d$tipo <- factor(d$tipo, levels = c("Normal", tipos_anom))
    d <- d[order(d$tipo), ]
    g <- ggplot(d, aes(x, y, color = tipo)) +
      geom_jitter(width = 0.25, height = 0.1, alpha = 0.5, size = 1) +
      scale_color_manual(values = c("Normal" = "grey80", "Global" = "red",
                                    "Contextual A" = "blue", "Contextual B" = "darkgreen",
                                    "Colectiva" = "orange")) +
      facet_wrap(~ panel, scales = "free") +
      labs(title = "Anomalías inyectadas en el conjunto de prueba (réplica 1)",
           x = NULL, y = NULL, color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "13_anomalias_inyectadas.png"),
        width = 11, height = 5.5, units = "in", res = 150)
    print(g)
    dev.off()
    cat("\nGráfico guardado en output/graficos/13_anomalias_inyectadas.png\n")
  }
)

# 06 - Ejecutar el experimento: todos los métodos sobre cada réplica
ejecutar_paso(
  id_paso  = 22, script = "06_evaluacion.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Repetir 5 veces (semillas 1 a 5): inyectar anomalías, estandarizar, aplicar |z| máximo, GMM, LOF (varios k) e Isolation Forest, y calcular métricas",
  criterio = "Una sola réplica depende del azar de la inyección; con 5 réplicas se puede reportar promedio y variación. Cada método se ajusta sobre los datos con las anomalías ya mezcladas, como ocurriría en la práctica",
  expr = {
    evaluar_replica <- function(r) {
      message("Réplica ", r, " de ", N_REP, " (el GMM tarda cerca de un minuto)...")
      aug <- inyectar(base_eval, seed = r)
      y   <- aug$tipo != "Normal"
      m   <- sum(y)
      Xa  <- scale(aug[vars_num])
      
      set.seed(r)
      gmm_r <- Mclust(Xa, G = 1:8)
      scores <- list(
        "Univariado (|z| máx.)" = apply(abs(Xa), 1, max),
        "GMM" = -dens(data = Xa, modelName = gmm_r$modelName,
                      parameters = gmm_r$parameters, logarithm = TRUE),
        "LOF (máx. k 10-50)" = do.call(pmax, lapply(c(10, 20, 30, 40, 50),
                                                    function(k) lof_seguro(Xa, k))),
        "Isolation Forest" = predict(ajustar_if(Xa, seed = r), Xa),
        "LOF k=10"  = lof_seguro(Xa, 10),
        "LOF k=20"  = lof_seguro(Xa, 20),
        "LOF k=50"  = lof_seguro(Xa, 50),
        "LOF k=100" = lof_seguro(Xa, 100)
      )
      
      general <- bind_rows(lapply(names(scores), function(nm) {
        s <- scores[[nm]]
        flag <- s >= quantile(s, 0.95)
        tp <- sum(flag & y); fp <- sum(flag & !y)
        fn <- sum(!flag & y); tn <- sum(!flag & !y)
        tibble(replica = r, metodo = nm,
               auc_roc = auc_roc(s, y), auc_pr = auc_pr(s, y),
               precision_en_m = mean(y[order(s, decreasing = TRUE)[1:m]]),
               tp = tp, fp = fp, fn = fn, tn = tn,
               precision = tp / (tp + fp), recall = tp / (tp + fn),
               f1 = 2 * tp / (2 * tp + fp + fn))
      }))
      por_tipo <- bind_rows(lapply(names(scores), function(nm) {
        s <- scores[[nm]]
        flag <- s >= quantile(s, 0.95)
        bind_rows(lapply(tipos_anom, function(tp_) {
          idx <- aug$tipo %in% c("Normal", tp_)
          tibble(replica = r, metodo = nm, tipo = tp_,
                 auc_roc = auc_roc(s[idx], y[idx]),
                 recall  = mean(flag[aug$tipo == tp_]))
        }))
      }))
      list(general = general, por_tipo = por_tipo,
           scores = if (r == 1) scores else NULL, y = if (r == 1) y else NULL)
    }
    
    salidas <- lapply(seq_len(N_REP), evaluar_replica)
    eval_general  <- bind_rows(lapply(salidas, `[[`, "general"))
    eval_por_tipo <- bind_rows(lapply(salidas, `[[`, "por_tipo"))
    scores_rep1 <- salidas[[1]]$scores
    y_rep1      <- salidas[[1]]$y
    write_csv(eval_general,  here("output", "tablas", "evaluacion_general_replicas.csv"))
    write_csv(eval_por_tipo, here("output", "tablas", "evaluacion_por_tipo_replicas.csv"))
    cat("Réplicas completadas:", N_REP, "\n")
    cat("Filas de resultados:", nrow(eval_general), "(general) y", nrow(eval_por_tipo), "(por tipo)\n")
  }
)

# 07 - Tablas resumen: promedio y desviación entre réplicas
ejecutar_paso(
  id_paso  = 23, script = "06_evaluacion.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Resumir métricas generales, matriz de confusión (corte 5%) y rendimiento por tipo de anomalía",
  criterio = "El AUC no depende del umbral y compara la calidad del ranking; precisión, recall y F1 con el corte del 5% muestran el desempeño en el uso operativo",
  expr = {
    principales <- c("Univariado (|z| máx.)", "GMM", "LOF (máx. k 10-50)", "Isolation Forest")
    
    resumen_general <- eval_general |>
      filter(metodo %in% principales) |>
      group_by(metodo) |>
      summarise(across(c(auc_roc, auc_pr, precision_en_m, precision, recall, f1),
                       list(media = mean, sd = sd)), .groups = "drop") |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    write_csv(resumen_general, here("output", "tablas", "evaluacion_resumen.csv"))
    cat("MÉTRICAS GENERALES (media de", N_REP, "réplicas)\n")
    print(as.data.frame(resumen_general[, c("metodo", "auc_roc_media", "auc_roc_sd",
                                            "auc_pr_media", "precision_en_m_media",
                                            "recall_media", "f1_media")]))
    
    matriz <- eval_general |>
      filter(metodo %in% principales) |>
      group_by(metodo) |>
      summarise(VP = round(mean(tp)), FP = round(mean(fp)),
                FN = round(mean(fn)), VN = round(mean(tn)), .groups = "drop")
    write_csv(matriz, here("output", "tablas", "evaluacion_matriz_confusion.csv"))
    cat("\nMATRIZ DE CONFUSIÓN promedio (corte: 5% más anómalo)\n")
    print(as.data.frame(matriz))
    
    por_tipo <- eval_por_tipo |>
      filter(metodo %in% principales) |>
      group_by(metodo, tipo) |>
      summarise(auc_roc = round(mean(auc_roc), 3), recall = round(mean(recall), 3),
                .groups = "drop")
    write_csv(por_tipo, here("output", "tablas", "evaluacion_por_tipo.csv"))
    cat("\nAUC-ROC POR TIPO DE ANOMALÍA\n")
    print(as.data.frame(por_tipo |> select(-recall) |> pivot_wider(names_from = tipo, values_from = auc_roc)))
    cat("\nRECALL POR TIPO (corte 5%)\n")
    print(as.data.frame(por_tipo |> select(-auc_roc) |> pivot_wider(names_from = tipo, values_from = recall)))
  }
)

# 08 - Efecto del parámetro k en LOF según el tipo de anomalía
ejecutar_paso(
  id_paso  = 24, script = "06_evaluacion.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Comparar LOF con k = 10, 20, 50, 100 y con el máximo en k 10-50, por tipo de anomalía",
  criterio = "El PDF señala que LOF es sensible a k; la anomalía colectiva de 40 personas debería detectarse solo con k mayor a 40",
  expr = {
    lof_k <- eval_por_tipo |>
      filter(grepl("^LOF", metodo)) |>
      group_by(metodo, tipo) |>
      summarise(auc_roc = round(mean(auc_roc), 3), .groups = "drop") |>
      pivot_wider(names_from = tipo, values_from = auc_roc)
    write_csv(lof_k, here("output", "tablas", "evaluacion_lof_k.csv"))
    cat("AUC-ROC de LOF según k y tipo de anomalía\n")
    print(as.data.frame(lof_k))
  }
)

# 09 - Gráficos de la evaluación
ejecutar_paso(
  id_paso  = 25, script = "06_evaluacion.R",
  tipo     = "Gráfico", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Curvas ROC (réplica 1) y AUC por tipo de anomalía con variación entre réplicas",
  criterio = "Mostrar visualmente qué método rinde mejor y en qué tipo de anomalía falla cada uno",
  expr = {
    principales <- c("Univariado (|z| máx.)", "GMM", "LOF (máx. k 10-50)", "Isolation Forest")
    colores <- c("Univariado (|z| máx.)" = "grey40", "GMM" = "blue",
                 "LOF (máx. k 10-50)" = "orange", "Isolation Forest" = "red")
    
    curvas <- bind_rows(lapply(principales, function(nm) {
      mutate(curva_roc(scores_rep1[[nm]], y_rep1), metodo = nm)
    }))
    g1 <- ggplot(curvas, aes(fpr, tpr, color = metodo)) +
      geom_abline(linetype = "dashed", color = "grey60") +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = colores) +
      labs(title = "Curvas ROC (réplica 1)",
           x = "Tasa de falsos positivos", y = "Tasa de verdaderos positivos", color = NULL) +
      theme_minimal()
    png(here("output", "graficos", "14_curvas_roc.png"),
        width = 8, height = 6, units = "in", res = 150)
    print(g1)
    dev.off()
    
    resumen_tipo <- eval_por_tipo |>
      filter(metodo %in% principales) |>
      group_by(metodo, tipo) |>
      summarise(media = mean(auc_roc), sd = sd(auc_roc), .groups = "drop")
    resumen_tipo$tipo <- factor(resumen_tipo$tipo, levels = tipos_anom)
    g2 <- ggplot(resumen_tipo, aes(tipo, media, fill = metodo)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                    position = position_dodge(width = 0.8), width = 0.25) +
      scale_fill_manual(values = colores) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
      coord_cartesian(ylim = c(0.2, 1)) +
      labs(title = "AUC-ROC por tipo de anomalía",
           subtitle = "Barras: promedio de las réplicas; líneas: ± 1 desviación estándar; punteada: azar (0.5)",
           x = NULL, y = "AUC-ROC", fill = NULL) +
      theme_minimal()
    png(here("output", "graficos", "15_auc_por_tipo.png"),
        width = 10, height = 6, units = "in", res = 150)
    print(g2)
    dev.off()
    cat("Gráficos guardados: 14_curvas_roc.png y 15_auc_por_tipo.png\n")
  }
)