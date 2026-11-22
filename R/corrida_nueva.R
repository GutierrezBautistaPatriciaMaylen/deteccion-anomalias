# 01 - Cargar el motor de corridas
library(here)
source(here("R", "motor.R"))

# 02 - Elegir el dataset desde la computadora (se abre una ventana)
ruta <- file.choose()
cat("Archivo elegido:", ruta, "\n")

# 03 - Ver las columnas numéricas disponibles (solo se leen las primeras 2000 filas)
muestra   <- leer_datos(ruta, n_max = 2000)
numericas <- names(muestra)[sapply(muestra, is.numeric)]
cat("Columnas numéricas encontradas:", paste(numericas, collapse = ", "), "\n")

# 04 - Elegir nombre, variables y opciones de la corrida
nombre     <- readline("Nombre corto de esta corrida (ej. original sin log): ")
variables  <- select.list(numericas, multiple = TRUE,
                          title = "Variables numéricas para los modelos (mínimo 2)")
log_vars   <- select.list(variables, multiple = TRUE,
                          title = "Variables a transformar con log1p (cancelar = ninguna)")
ejes       <- select.list(variables, multiple = TRUE,
                          title = "Dos variables para el gráfico de dispersión")
if (length(ejes) != 2) ejes <- variables[1:2]
filtro_txt <- readline("Filtro opcional, ej. condact == 1 & s01a_03 >= 18 (Enter = sin filtro): ")

# 05 - Ejecutar la corrida (se registra sola: carpeta en runs/, bitácora y fila en el registro)
ejecutar_corrida(
  ruta_datos     = ruta,
  nombre         = nombre,
  variables      = variables,
  log1p_vars     = log_vars,
  ejes           = ejes,
  filtro         = if (nzchar(filtro_txt)) filtro_txt else NULL,
  publicar_filas = FALSE   # de datasets externos solo se guardan resultados agregados
)

# 06 - Regenerar el dashboard para ver la nueva corrida
source(here("R", "render_dashboard.R"))

# EJEMPLO SIN VENTANAS (dataset original del INE, dos corridas para comparar el efecto del log):
# ejecutar_corrida(ruta_datos = "C:/ruta/Persona.csv", nombre = "Original sin log",
#                  variables = c("s01a_03", "aestudio", "phrs", "ylab"),
#                  filtro = "condact == 1 & s01a_03 >= 18 & s01a_03 <= 64",
#                  ejes = c("aestudio", "ylab"))
# ejecutar_corrida(ruta_datos = "C:/ruta/Persona.csv", nombre = "Original con log",
#                  variables = c("s01a_03", "aestudio", "phrs", "ylab"), log1p_vars = "ylab",
#                  filtro = "condact == 1 & s01a_03 >= 18 & s01a_03 <= 64",
#                  ejes = c("aestudio", "ylab"))
