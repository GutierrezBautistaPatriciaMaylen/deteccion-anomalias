# 01 - Configuración general del proyecto
# Ejecutar este archivo reproduce todo el análisis de principio a fin.
options(keep.source = TRUE)
library(here)
set.seed(123)

# 02 - Scripts en orden de ejecución
scripts <- c(
  "R/01_carga_y_revision.R",
  "R/02_linea_base.R",
  "R/03_gmm.R",
  "R/04_lof.R",
  "R/05_isolation_forest.R",
  "R/06_evaluacion.R",
  "R/07_casos.R",
  "R/08_corrida_principal.R"
  # (los nuevos scripts de análisis se agregan aquí, antes de esta línea)
)

# 03 - Ejecutar cada script (la bitácora se actualiza sola en cada paso)
for (s in scripts) {
  cat("\n=====", s, "=====\n")
  source(here(s), keep.source = TRUE)
}
# 04 - Regenerar el dashboard (necesita Quarto)
source(here("R", "render_dashboard.R"))
cat("\nListo. Bitácora en output/bitacora/bitacora.csv\n")
