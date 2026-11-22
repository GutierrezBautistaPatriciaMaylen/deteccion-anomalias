# 01 - Instalar paquetes necesarios (solo se ejecuta una vez)
install.packages(c("tidyverse", "here", "mclust", "dbscan",
                   "isotree", "plotly", "pROC", "knitr", "rmarkdown"))

# 02 - Crear la estructura de carpetas del proyecto
dir.create("data", showWarnings = FALSE)
dir.create("R", showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("output/bitacora", recursive = TRUE, showWarnings = FALSE)
dir.create("dashboard", showWarnings = FALSE)