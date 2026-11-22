# 01 - Paquetes y bitácora
# Este archivo define ejecutar_corrida(): aplica la detección de anomalías
# (IQR, Z-score, GMM, LOF, Isolation Forest y consenso) a CUALQUIER dataset
# y deja todo registrado en runs/<id_corrida>/ y en runs/registro_corridas.csv.
library(tidyverse)
library(here)
library(mclust)
library(dbscan)
library(isotree)
source(here("R", "00_bitacora.R"))

# 02 - Lectura flexible de datos (coma o punto y coma como separador)
leer_datos <- function(ruta, encoding = "UTF-8", n_max = Inf) {
  loc <- locale(encoding = encoding)
  d <- read_csv(ruta, show_col_types = FALSE, locale = loc, progress = FALSE, n_max = n_max)
  if (ncol(d) == 1) {
    d <- read_delim(ruta, delim = ";", show_col_types = FALSE, locale = loc,
                    progress = FALSE, n_max = n_max)
  }
  d
}

# 03 - Funciones auxiliares del motor
mt_lof_seguro <- function(X, k) {
  s <- lof(X, minPts = k)
  s[!is.finite(s)] <- max(s[is.finite(s)])
  s
}
mt_marca <- function(s, tasa) s >= quantile(s, 1 - tasa)
mt_jaccard <- function(a, b) sum(a & b) / sum(a | b)
mt_marca_iqr <- function(x, k = 1.5) {
  q <- quantile(x, c(0.25, 0.75)); i <- q[2] - q[1]
  x < q[1] - k * i | x > q[2] + k * i
}

# 04 - Función principal: ejecutar una corrida completa
# ruta_datos     archivo CSV a analizar
# nombre         nombre corto de la corrida (aparece en el dashboard)
# variables      columnas numéricas que usan los modelos (mínimo 2)
# log1p_vars     columnas (de variables) a transformar con log(1 + x)
# filtro         condición opcional en texto, por ejemplo "condact == 1 & edad >= 18"
# ejes           dos columnas para los gráficos de dispersión
# tasa           proporción marcada como anomalía por cada método (0.05 = 5%)
# publicar_filas TRUE guarda resultados fila por fila (solo para datos que se pueden publicar)
# id_fijo        identificador fijo (se sobrescribe); NULL crea uno nuevo con fecha y hora
ejecutar_corrida <- function(ruta_datos, nombre, variables,
                             log1p_vars = character(0), filtro = NULL,
                             ejes = variables[1:2], tasa = 0.05, semilla = 123,
                             k_lof = c(10, 20, 30, 40, 50),
                             publicar_filas = FALSE, id_fijo = NULL,
                             encoding = "UTF-8") {
  t_ini <- Sys.time()
  stopifnot(file.exists(ruta_datos), length(variables) >= 2,
            all(log1p_vars %in% variables), all(ejes %in% variables), length(ejes) == 2)

  slug   <- gsub("[^A-Za-z0-9]+", "_", nombre)
  run_id <- if (!is.null(id_fijo)) id_fijo else paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", slug)
  carpeta <- here("runs", run_id)
  if (dir.exists(carpeta)) unlink(carpeta, recursive = TRUE)
  dir.create(file.path(carpeta, "tablas"), recursive = TRUE)
  dir.create(file.path(carpeta, "graficos"), recursive = TRUE)
  archivo <- basename(ruta_datos)
  etiqueta <- ifelse(variables %in% log1p_vars, paste0("log1p(", variables, ")"), variables)
  names(etiqueta) <- variables
  message("== Corrida '", run_id, "' sobre ", archivo, " ==")

  # La bitácora de esta corrida se guarda en su propia carpeta
  ruta_previa <- get("RUTA_BITACORA", envir = globalenv())
  assign("RUTA_BITACORA", file.path(carpeta, "bitacora.csv"), envir = globalenv())
  on.exit(assign("RUTA_BITACORA", ruta_previa, envir = globalenv()), add = TRUE)

  # 05 - Carga
  ejecutar_paso(
    id_paso = 1, script = "motor.R", tipo = "Carga", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = paste("Leer", archivo),
    criterio = "Dataset elegido para esta corrida",
    expr = {
      df <- leer_datos(ruta_datos, encoding)
      n_leidas <- nrow(df)
      cat("Filas:", n_leidas, "| Columnas:", ncol(df), "\n")
      faltan <- setdiff(c(variables), names(df))
      if (length(faltan) > 0) stop("No existen estas columnas en el archivo: ", paste(faltan, collapse = ", "))
    }
  )

  # 06 - Preparación: filtro, filas completas y transformación
  ejecutar_paso(
    id_paso = 2, script = "motor.R", tipo = "Preparación", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = paste0("Aplicar filtro (", ifelse(is.null(filtro), "ninguno", filtro),
                      "), descartar filas con valores faltantes y transformar con log1p: ",
                      ifelse(length(log1p_vars) == 0, "ninguna", paste(log1p_vars, collapse = ", "))),
    criterio = "Los modelos necesitan valores numéricos completos; log1p reduce la asimetría de variables como el ingreso",
    expr = {
      if (!is.null(filtro)) df <- dplyr::filter(df, !!rlang::parse_expr(filtro))
      n_filtrado <- nrow(df)
      df <- df[, variables]
      df[] <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
      completos <- complete.cases(df)
      n_descartadas <- sum(!completos)
      df <- df[completos, ]
      for (v in log1p_vars) df[[v]] <- log1p(df[[v]])
      if (any(!is.finite(as.matrix(df)))) stop("Hay valores no finitos tras la transformación (¿negativos en log1p?)")
      constantes <- names(which(sapply(df, function(x) sd(x) == 0)))
      if (length(constantes) > 0) stop("Variables constantes: ", paste(constantes, collapse = ", "))
      if (nrow(df) < 200) stop("Quedan menos de 200 filas; revisa el filtro y las variables")
      X <- scale(df)
      cat("Filas tras el filtro:", n_filtrado, "\n")
      cat("Filas descartadas por valores faltantes:", n_descartadas, "\n")
      cat("Filas usadas en los modelos:", nrow(df), "\n")
    }
  )

  # 07 - Línea base univariada
  ejecutar_paso(
    id_paso = 3, script = "motor.R", tipo = "Línea base", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = "Marcar atípicos por variable con IQR (1.5 y 3) y Z-score (|z| > 3)",
    criterio = "Métodos clásicos univariados, sirven de referencia para los multivariados",
    expr = {
      iqr_mat <- sapply(df, mt_marca_iqr)
      ext_mat <- sapply(df, mt_marca_iqr, k = 3)
      z_mat   <- abs(X) > 3
      base_iqr <- rowSums(iqr_mat) > 0
      base_ext <- rowSums(ext_mat) > 0
      base_z   <- rowSums(z_mat) > 0
      cat("Filas con algún atípico: IQR x1.5 =", sum(base_iqr),
          "| IQR x3 =", sum(base_ext), "| Z > 3 =", sum(base_z), "\n")
    }
  )

  # 08 - GMM
  ejecutar_paso(
    id_paso = 4, script = "motor.R", tipo = "Modelo", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = "Ajustar GMM (mclust, 1 a 8 componentes, elegido por BIC) y marcar la proporción con menor densidad",
    criterio = "El BIC elige modelo y número de componentes; -log(densidad) es el puntaje de anomalía",
    expr = {
      message("   Ajustando GMM (puede tardar de uno a varios minutos)...")
      set.seed(semilla)
      gmm <- Mclust(X, G = 1:8)
      if (is.null(gmm$modelName)) stop("mclust no pudo ajustar ningún modelo con estos datos")
      score_gmm <- -dens(data = X, modelName = gmm$modelName,
                         parameters = gmm$parameters, logarithm = TRUE)
      anom_gmm <- mt_marca(score_gmm, tasa)
      cat("Modelo elegido:", gmm$modelName, "con", gmm$G, "componentes\n")
      cat("Marcadas:", sum(anom_gmm), "\n")
    }
  )

  # 09 - LOF
  ejecutar_paso(
    id_paso = 5, script = "motor.R", tipo = "Modelo", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = paste0("LOF: máximo sobre k = ", paste(k_lof, collapse = ", "), "; marcar la proporción más alta"),
    criterio = "Breunig et al. (2000): tomar el máximo LOF sobre un rango de k para no depender de un solo valor",
    expr = {
      k_ok <- k_lof[k_lof < nrow(X)]
      score_lof <- do.call(pmax, lapply(k_ok, function(k) mt_lof_seguro(X, k)))
      anom_lof <- mt_marca(score_lof, tasa)
      cat("Marcadas:", sum(anom_lof), "\n")
    }
  )

  # 10 - Isolation Forest
  ejecutar_paso(
    id_paso = 6, script = "motor.R", tipo = "Modelo", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = "Isolation Forest (500 árboles, muestra de 256) y marcar la proporción con mayor puntaje",
    criterio = "Valores del artículo original (Liu et al., 2008); enfoque basado en aislamiento",
    expr = {
      modelo_if <- isolation.forest(X, ndim = 1, ntrees = 500,
                                    sample_size = min(256, nrow(X)),
                                    seed = semilla, nthreads = 1)
      score_if <- predict(modelo_if, X)
      anom_if <- mt_marca(score_if, tasa)
      cat("Marcadas:", sum(anom_if), "\n")
    }
  )

  # 11 - Consenso y tablas de resultados
  ejecutar_paso(
    id_paso = 7, script = "motor.R", tipo = "Evaluación", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = "Calcular el consenso (2 o más de GMM, LOF e IF), la coincidencia entre métodos y el perfil de las anomalías",
    criterio = "Una persona marcada por métodos de enfoques distintos es una anomalía más fiable",
    expr = {
      votos <- as.integer(anom_gmm) + as.integer(anom_lof) + as.integer(anom_if)
      consenso <- votos >= 2
      marcas <- list("IQR x1.5" = base_iqr, "Z-score" = base_z, "GMM" = anom_gmm,
                     "LOF" = anom_lof, "IF" = anom_if)
      resumen <- tibble(metodo = c(names(marcas), "Consenso (2+)"),
                        marcadas = c(sapply(marcas, sum), sum(consenso))) |>
        mutate(porcentaje = round(100 * marcadas / nrow(X), 2))
      nombres <- c("IQR x1.5", "GMM", "LOF", "IF")
      mat <- outer(nombres, nombres, Vectorize(function(a, b) mt_jaccard(marcas[[a]], marcas[[b]])))
      dimnames(mat) <- list(nombres, nombres)
      origen <- tibble(
        origen = ifelse(base_ext[consenso], "Extremo en una variable (IQR x3)",
                        "Combinación atípica (solo multivariada)")
      ) |> dplyr::count(origen, name = "n") |> mutate(porcentaje = round(100 * n / sum(n), 1))
      perfil <- tibble(
        variable = variables,
        media_normales  = round(sapply(df, function(x) mean(x[!consenso])), 3),
        media_anomalias = round(sapply(df, function(x) mean(x[consenso])), 3)
      )
      descriptivos <- tibble(
        variable = variables,
        n = nrow(df), media = round(sapply(df, mean), 3), sd = round(sapply(df, sd), 3),
        minimo = sapply(df, min), p25 = sapply(df, quantile, 0.25), mediana = sapply(df, median),
        p75 = sapply(df, quantile, 0.75), maximo = sapply(df, max)
      )
      distribucion_votos <- tibble(metodos_que_la_marcan = 0:3,
                                   personas = as.integer(table(factor(votos, levels = 0:3))))

      dir_t <- file.path(carpeta, "tablas")
      write_csv(resumen, file.path(dir_t, "resumen_metodos.csv"))
      write_csv(as.data.frame(mat) |> rownames_to_column("metodo"), file.path(dir_t, "coincidencia.csv"))
      write_csv(origen, file.path(dir_t, "origen.csv"))
      write_csv(perfil, file.path(dir_t, "perfil_anomalias.csv"))
      write_csv(descriptivos, file.path(dir_t, "descriptivos.csv"))
      write_csv(distribucion_votos, file.path(dir_t, "votos.csv"))
      if (publicar_filas) {
        write_csv(bind_cols(tibble(id = seq_len(nrow(df))), df,
                            tibble(score_gmm, score_lof, score_if, base_iqr, base_z,
                                   anom_gmm, anom_lof, anom_if, votos)),
                  file.path(dir_t, "resultados.csv"))
      }
      print(as.data.frame(resumen))
      cat("\nCoincidencia entre métodos (Jaccard):\n"); print(round(mat, 2))
    }
  )

  # 12 - Gráficos
  ejecutar_paso(
    id_paso = 8, script = "motor.R", tipo = "Gráfico", dataset = archivo,
    variable = paste(variables, collapse = ", "),
    accion   = "Histogramas, dispersión con el consenso y matriz de coincidencia entre métodos",
    criterio = "Mostrar la forma de los datos y dónde caen las anomalías",
    expr = {
      g_dir <- file.path(carpeta, "graficos")
      g1 <- df |>
        pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
        mutate(variable = etiqueta[variable]) |>
        ggplot(aes(valor)) +
        geom_histogram(bins = 40, fill = "steelblue", color = "white") +
        facet_wrap(~ variable, scales = "free") +
        labs(title = "Distribución de las variables usadas", x = NULL, y = "Frecuencia") +
        theme_minimal()
      png(file.path(g_dir, "histogramas.png"), width = 9, height = 6, units = "in", res = 150)
      print(g1); dev.off()

      ancho <- function(v) if (length(unique(df[[v]])) <= 40) 0.25 else 0
      d <- tibble(x = df[[ejes[1]]], y = df[[ejes[2]]],
                  votos = factor(votos, levels = 0:3,
                                 labels = c("Ninguno", "1 método", "2 métodos", "3 métodos")))
      d <- d[order(d$votos), ]
      g2 <- ggplot(d, aes(x, y, color = votos)) +
        geom_point(alpha = 0.55, size = 1,
                   position = position_jitter(width = ancho(ejes[1]), height = 0)) +
        scale_color_manual(values = c("Ninguno" = "grey80", "1 método" = "gold",
                                      "2 métodos" = "orange", "3 métodos" = "red")) +
        labs(title = "Consenso entre GMM, LOF e Isolation Forest",
             x = etiqueta[ejes[1]], y = etiqueta[ejes[2]], color = "Marcada por") +
        theme_minimal()
      png(file.path(g_dir, "dispersion_consenso.png"), width = 9, height = 6, units = "in", res = 150)
      print(g2); dev.off()

      dm <- as.data.frame(as.table(mat)); names(dm) <- c("a", "b", "coincidencia")
      g3 <- ggplot(dm, aes(a, b, fill = coincidencia)) +
        geom_tile(color = "white") +
        geom_text(aes(label = sprintf("%.2f", coincidencia))) +
        scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
        labs(title = "Coincidencia entre métodos (Jaccard)", x = NULL, y = NULL, fill = NULL) +
        theme_minimal()
      png(file.path(g_dir, "coincidencia.png"), width = 8, height = 6, units = "in", res = 150)
      print(g3); dev.off()
      cat("Gráficos guardados en", g_dir, "\n")
    }
  )

  # 13 - Registro de la corrida
  ejecutar_paso(
    id_paso = 9, script = "motor.R", tipo = "Registro", dataset = archivo,
    variable = "-",
    accion   = "Guardar parámetros de la corrida y agregar una fila a runs/registro_corridas.csv",
    criterio = "Cada corrida debe poder reproducirse: se guardan parámetros, huella MD5 del archivo y versiones de R y de los paquetes",
    expr = {
      fila <- tibble(
        run_id = run_id, fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        responsable = get("RESPONSABLE", envir = globalenv()), nombre = nombre,
        archivo = archivo, md5_archivo = unname(tools::md5sum(ruta_datos)),
        filas_leidas = n_leidas, filas_tras_filtro = n_filtrado,
        filas_descartadas_na = n_descartadas, filas_usadas = nrow(df),
        variables = paste(variables, collapse = ", "),
        log1p = ifelse(length(log1p_vars) == 0, "", paste(log1p_vars, collapse = ", ")),
        filtro = ifelse(is.null(filtro), "", filtro), tasa = tasa, semilla = semilla,
        modelo_gmm = paste0(gmm$modelName, " (G=", gmm$G, ")"),
        anom_iqr = sum(base_iqr), anom_z = sum(base_z), anom_gmm = sum(anom_gmm),
        anom_lof = sum(anom_lof), anom_if = sum(anom_if),
        consenso_2 = sum(consenso), consenso_3 = sum(votos == 3),
        duracion_seg = round(as.numeric(difftime(Sys.time(), t_ini, units = "secs"))),
        version_R = R.version.string,
        version_mclust = as.character(packageVersion("mclust")),
        version_dbscan = as.character(packageVersion("dbscan")),
        version_isotree = as.character(packageVersion("isotree")),
        filas_publicadas = publicar_filas
      )
      jsonlite::write_json(as.list(fila), file.path(carpeta, "parametros.json"),
                           auto_unbox = TRUE, pretty = TRUE)
      ruta_reg <- here("runs", "registro_corridas.csv")
      if (file.exists(ruta_reg)) {
        previo <- read_csv(ruta_reg, col_types = cols(.default = "c"), show_col_types = FALSE)
        previo <- previo[previo$run_id != run_id, ]
        fila_c <- fila |> mutate(across(everything(), as.character))
        fila_c <- bind_rows(previo, fila_c)
      } else {
        fila_c <- fila |> mutate(across(everything(), as.character))
      }
      write_excel_csv(fila_c, ruta_reg)
      cat("Corrida registrada:", run_id, "\n")
    }
  )

  message("== Corrida terminada en ", round(as.numeric(difftime(Sys.time(), t_ini, units = "secs"))), " s ==")
  invisible(run_id)
}
