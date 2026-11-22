# 01 - Cargar el motor de corridas
library(here)
source(here("R", "motor.R"))

# 02 - Corrida principal: dataset limpio (el mismo que usan los scripts 01 a 07)
# id_fijo hace que, al volver a ejecutar main.R, esta corrida se actualice en lugar de duplicarse.
ejecutar_corrida(
  ruta_datos     = here("data", "persona_limpia.csv"),
  nombre         = "Dataset limpio (preprocesado)",
  id_fijo        = "01_limpio",
  variables      = c("edad", "aestudio", "phrs", "log_ylab"),
  ejes           = c("aestudio", "log_ylab"),
  publicar_filas = TRUE
)
