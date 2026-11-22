# 01 - Cargar paquetes y la bitácora
library(tidyverse)
library(here)
source(here("R", "00_bitacora.R"))

# 02 - Recuperar objetos si el script se ejecuta solo (no desde main.R)
if (!exists("df", inherits = FALSE)) {
  df <- read_csv(here("data", "persona_limpia.csv"), show_col_types = FALSE)
}
leer_tabla <- function(nombre) read_csv(here("output", "tablas", nombre), show_col_types = FALSE)
if (!exists("linea_base", inherits = FALSE)) linea_base <- leer_tabla("linea_base.csv")
if (!exists("res_gmm", inherits = FALSE))    res_gmm    <- leer_tabla("resultados_gmm.csv")
if (!exists("res_lof", inherits = FALSE))    res_lof    <- leer_tabla("resultados_lof.csv")
if (!exists("res_if", inherits = FALSE))     res_if     <- leer_tabla("resultados_if.csv")
dir.create(here("output", "tablas"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "graficos"), recursive = TRUE, showWarnings = FALSE)

# 03 - Consenso: cuántos métodos marcan a cada persona
ejecutar_paso(
  id_paso  = 26, script = "07_casos.R",
  tipo     = "Evaluación", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Contar cuántos de los tres métodos (GMM, LOF, Isolation Forest) marcan a cada persona; anomalía de consenso = 2 o más métodos",
  criterio = "Una persona marcada por varios métodos de enfoques distintos (probabilístico, densidad local, aislamiento) es una anomalía más fiable que una marcada por uno solo",
  expr = {
    consenso <- tibble(
      id       = seq_len(nrow(df)),
      folio    = df$folio,
      nro      = df$nro,
      votos    = as.integer(res_gmm$anom_gmm) + as.integer(res_lof$anom_lof) + as.integer(res_if$anom_if),
      score_if = res_if$score_if
    )
    write_csv(consenso, here("output", "tablas", "consenso.csv"))
    print(table(metodos_que_la_marcan = consenso$votos))
    cat("\nAnomalías de consenso (2 o más métodos):", sum(consenso$votos >= 2),
        "(", round(100 * mean(consenso$votos >= 2), 1), "% )\n")
    cat("Marcadas por los 3 métodos:", sum(consenso$votos == 3), "\n")
  }
)

# 04 - Origen de las anomalías de consenso
ejecutar_paso(
  id_paso  = 27, script = "07_casos.R",
  tipo     = "Evaluación", variable = "ylab, phrs, edad, aestudio, log_ylab",
  accion   = "Clasificar las anomalías de consenso según su origen: cero estructural, horas extremas, extremo en una variable o combinación atípica",
  criterio = "Un valor atípico puede ser un error o información valiosa; hay que investigar su origen antes de decidir si se corrige, elimina o conserva",
  expr = {
    anom <- consenso$votos >= 2
    origen <- dplyr::case_when(
      df$ylab == 0            ~ "Cero estructural (ingreso 0)",
      df$phrs >= 100          ~ "Horas extremas (>= 100)",
      linea_base$base_extremo ~ "Extremo en una variable (IQR x3)",
      TRUE                    ~ "Combinación atípica (solo multivariada)"
    )
    tabla_origen <- tibble::tibble(origen = origen[anom]) |>
      dplyr::count(origen, name = "n", sort = TRUE) |>
      dplyr::mutate(porcentaje = round(100 * n / sum(n), 1))
    readr::write_csv(tabla_origen, here::here("output", "tablas", "anomalias_origen.csv"))
    print(as.data.frame(tabla_origen))
    
    g <- ggplot2::ggplot(tabla_origen, ggplot2::aes(reorder(origen, n), n)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2) +
      ggplot2::coord_flip() +
      ggplot2::expand_limits(y = max(tabla_origen$n) * 1.1) +
      ggplot2::labs(title = "Origen de las anomalías de consenso",
                    x = NULL, y = "Personas") +
      ggplot2::theme_minimal()
    png(here::here("output", "graficos", "16_origen_anomalias.png"),
        width = 9, height = 5, units = "in", res = 150)
    print(g)
    dev.off()
    cat("Gráfico guardado en output/graficos/16_origen_anomalias.png\n")
  }
)

# 05 - Tasa de anomalías por grupo (categoría ocupacional, nivel educativo, área)
ejecutar_paso(
  id_paso  = 28, script = "07_casos.R",
  tipo     = "Evaluación", variable = "categoria_ocup_desc, niv_ed_g, area_desc",
  accion   = "Calcular qué porcentaje de cada grupo es anomalía de consenso y compararlo con el porcentaje general",
  criterio = "Si un grupo concentra muchas más anomalías que el promedio, el problema puede ser de definición o preprocesamiento de ese grupo y no de las personas individuales",
  expr = {
    anom <- consenso$votos >= 2
    tasa <- function(var) {
      df |>
        mutate(anom = anom) |>
        group_by(grupo = as.character(.data[[var]])) |>
        summarise(n = n(), anomalias = sum(anom),
                  porcentaje = round(100 * mean(anom), 1), .groups = "drop") |>
        mutate(variable = var)
    }
    por_grupo <- bind_rows(tasa("categoria_ocup_desc"), tasa("niv_ed_g"), tasa("area_desc"))
    write_csv(por_grupo, here("output", "tablas", "anomalias_por_grupo.csv"))
    tasa_global <- round(100 * mean(anom), 1)
    cat("Porcentaje general de anomalías de consenso:", tasa_global, "%\n\n")
    print(as.data.frame(por_grupo |> arrange(variable, desc(porcentaje))))
    
    g <- ggplot(por_grupo, aes(reorder(grupo, porcentaje), porcentaje)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = tasa_global, color = "red", linetype = "dashed") +
      geom_text(aes(label = paste0(porcentaje, "% (n=", n, ")")), hjust = -0.05, size = 3) +
      coord_flip() +
      facet_wrap(~ variable, scales = "free") +
      expand_limits(y = 160) +
      labs(title = "Porcentaje de anomalías de consenso por grupo",
           subtitle = "Línea roja: porcentaje general",
           x = NULL, y = "% del grupo marcado por 2 o más métodos") +
      theme_minimal()
    png(here("output", "graficos", "17_anomalias_por_grupo.png"),
        width = 12, height = 6, units = "in", res = 150)
    print(g)
    dev.off()
    cat("\nGráfico guardado en output/graficos/17_anomalias_por_grupo.png\n")
  }
)

# 06 - Evaluación del preprocesamiento: qué decisiones dejaron casos detectables
ejecutar_paso(
  id_paso  = 29, script = "07_casos.R",
  tipo     = "Evaluación", variable = "ylab, phrs, niv_ed_g, aestudio, edad",
  accion   = "Revisar si los casos tratados o dejados sin tocar en el preprocesamiento aparecen como anomalías de consenso",
  criterio = "La detección de anomalías sirve para evaluar el preprocesamiento: indica qué decisiones dejaron residuos y cuáles no generaron problemas",
  expr = {
    anom <- consenso$votos >= 2
    reglas <- list(
      "Ingreso 0 en Familiar sin remuneración (preproc. paso 16)" = df$ylab == 0,
      "Horas semanales >= 100"                                    = df$phrs >= 100,
      "Horas = 168, capadas (preproc. paso 19)"                   = df$phrs == 168,
      "niv_ed_g = 4, sin documentar (preproc. paso 22)"           = df$niv_ed_g == 4,
      "Años de estudio > edad - 6, casos límite (preproc. paso 23)" = df$aestudio > df$edad - 6
    )
    residuos <- tibble(
      caso              = names(reglas),
      n                 = sapply(reglas, sum),
      marcados_consenso = sapply(reglas, function(r) sum(r & anom))
    ) |>
      mutate(porcentaje = round(100 * marcados_consenso / n, 1))
    write_csv(residuos, here("output", "tablas", "preprocesamiento_residuos.csv"))
    print(as.data.frame(residuos))
  }
)

# 07 - Los casos más anómalos y gráfico del consenso
ejecutar_paso(
  id_paso  = 30, script = "07_casos.R",
  tipo     = "Gráfico", variable = "edad, aestudio, phrs, log_ylab",
  accion   = "Listar las 20 personas más anómalas (más métodos y mayor puntaje de Isolation Forest) y graficar el consenso",
  criterio = "Revisar casos concretos permite juzgar si son errores de datos o situaciones reales poco frecuentes",
  expr = {
    orden <- order(-consenso$votos, -consenso$score_if)
    top <- df[orden[1:20], c("folio", "nro", "edad", "aestudio", "phrs", "ylab",
                             "categoria_ocup_desc", "area_desc")]
    top$votos <- consenso$votos[orden[1:20]]
    write_csv(top, here("output", "tablas", "top_casos.csv"))
    print(as.data.frame(top))
    
    d <- bind_rows(
      tibble(x = df$aestudio, y = df$log_ylab, votos = consenso$votos,
             panel = "Años de estudio vs log(ingreso)"),
      tibble(x = df$edad, y = df$phrs, votos = consenso$votos,
             panel = "Edad vs horas semanales")
    )
    d$votos <- factor(d$votos, levels = 0:3,
                      labels = c("Ninguno", "1 método", "2 métodos", "3 métodos"))
    d <- d[order(d$votos), ]
    g <- ggplot(d, aes(x, y, color = votos)) +
      geom_jitter(width = 0.25, height = 0.05, alpha = 0.6, size = 1) +
      scale_color_manual(values = c("Ninguno" = "grey80", "1 método" = "gold",
                                    "2 métodos" = "orange", "3 métodos" = "red")) +
      facet_wrap(~ panel, scales = "free") +
      labs(title = "Consenso entre GMM, LOF e Isolation Forest",
           x = NULL, y = NULL, color = "Marcada por") +
      theme_minimal()
    png(here("output", "graficos", "18_consenso.png"),
        width = 11, height = 5.5, units = "in", res = 150)
    print(g)
    dev.off()
    cat("\nGráfico guardado en output/graficos/18_consenso.png\n")
  }
)