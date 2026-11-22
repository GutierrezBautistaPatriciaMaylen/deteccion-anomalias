# 01 - Regenerar el dashboard con Quarto y preparar la carpeta docs/ para GitHub Pages
library(here)
renderizar_dashboard <- function() {
  owd <- setwd(here()); on.exit(setwd(owd))
  ok <- FALSE
  if (requireNamespace("quarto", quietly = TRUE)) {
    ok <- tryCatch({ quarto::quarto_render(here()); TRUE },
                   error = function(e) { message(conditionMessage(e)); FALSE })
  } else if (nzchar(Sys.which("quarto"))) {
    ok <- system2("quarto", "render") == 0
  }
  if (!ok) {
    message("No se pudo renderizar el dashboard automáticamente. ",
            "Abre dashboard/index.qmd en RStudio y pulsa Render.")
    return(invisible(FALSE))
  }
  # 02 - Archivos que necesita GitHub Pages
  writeLines("", here("docs", ".nojekyll"))
  writeLines('<meta http-equiv="refresh" content="0; url=dashboard/index.html">',
             here("docs", "index.html"))
  message("Dashboard listo: docs/dashboard/index.html")
  invisible(TRUE)
}
renderizar_dashboard()
