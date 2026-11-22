# 01 - Paquetes y configuración de la bitácora
library(here)
library(readr)

RESPONSABLE   <- "Patricia"
RUTA_BITACORA <- here("output", "bitacora", "bitacora.csv")

# 02 - Función que ejecuta un paso y lo registra automáticamente
# Guarda: fecha/hora, script, tipo, acción, justificación, filas/columnas
# antes y después, el código ejecutado y lo que imprimió en consola.
# Si se vuelve a ejecutar el mismo id_paso, se actualiza (no se duplica).
ejecutar_paso <- function(id_paso, script, tipo, accion, criterio,
                          variable = "-", datos = "df",
                          dataset = "persona_limpia.csv", expr) {
  entorno  <- parent.frame()
  expr_cod <- substitute(expr)
  
  # Código del bloque tal como lo escribiste (con tus comentarios).
  # Si R no guardó el texto original, se usa una versión reconstruida.
  sr <- attr(expr_cod, "srcref")
  sf <- attr(expr_cod, "srcfile")
  ws <- attr(expr_cod, "wholeSrcref")
  codigo <- if (!is.null(sr) && !is.null(sf) && !is.null(ws)) {
    lineas <- getSrcLines(sf, sr[[1]][1] + 1, ws[3] - 1)
    paste(sub("^ {4}", "", lineas), collapse = "\n")
  } else {
    paste(deparse(expr_cod), collapse = "\n")
  }
  
  # inherits = FALSE: evita confundir tu dataset con funciones de R (ej. df)
  obtener <- function() {
    if (exists(datos, envir = entorno, inherits = FALSE)) {
      obj <- get(datos, envir = entorno, inherits = FALSE)
      if (is.data.frame(obj)) return(obj)
    }
    NULL
  }
  obj_antes   <- obtener()
  salida      <- capture.output(eval(expr_cod, entorno))
  obj_despues <- obtener()
  antes   <- if (is.null(obj_antes))   c(NA_integer_, NA_integer_) else dim(obj_antes)
  despues <- if (is.null(obj_despues)) c(NA_integer_, NA_integer_) else dim(obj_despues)
  modifica <- if (is.null(obj_antes) && !is.null(obj_despues)) "Carga inicial"
  else if (identical(obj_antes, obj_despues)) "No"
  else "Sí"
  if (length(salida) > 0) cat(salida, sep = "\n")
  
  fila <- data.frame(
    id_paso          = id_paso,
    fecha_hora       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    responsable      = RESPONSABLE,
    dataset          = dataset,
    script           = script,
    variable         = variable,
    tipo             = tipo,
    accion           = accion,
    criterio         = criterio,
    filas_antes      = antes[1],
    filas_despues    = despues[1],
    columnas_antes   = antes[2],
    columnas_despues = despues[2],
    modifica_datos   = modifica,
    codigo           = codigo,
    resultado        = substr(paste(salida, collapse = "\n"), 1, 3000),
    stringsAsFactors = FALSE
  )
  fila[] <- lapply(fila, as.character)
  
  dir.create(dirname(RUTA_BITACORA), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(RUTA_BITACORA)) {
    previa <- read_csv(RUTA_BITACORA, col_types = cols(.default = "c"),
                       show_col_types = FALSE)
    previa <- previa[previa$id_paso != as.character(id_paso), ]
    fila   <- rbind(previa, fila)
  }
  fila <- fila[order(as.numeric(fila$id_paso)), ]
  write_excel_csv(fila, RUTA_BITACORA)   # UTF-8 con BOM: Excel abre bien las tildes
  invisible(fila)
}
