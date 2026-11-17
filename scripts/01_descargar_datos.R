# =============================================================
# 01_descargar_datos.R
# Descarga los datos oficiales del Servel sobre elecciones de
# alcaldes y los guarda, sin modificar, en data/raw/
# =============================================================

# Dar más tiempo a las descargas (el archivo de 2021 pesa ~17 MB)
options(timeout = 600)

# Carpeta donde se guardan los datos originales
carpeta_raw <- "data/raw"
dir.create(carpeta_raw, recursive = TRUE, showWarnings = FALSE)

# Dirección base de los archivos en el sitio del Servel
base_url <- "https://www.servel.cl/wp-content/uploads/2022/12/"

# Archivos a descargar:
# 1. Resultados de alcaldes 2004, 2008, 2012 y 2016 (incluye sexo y electo)
# 2. Resultados definitivos de alcaldes 2021, por mesa (16 regiones, en .zip)
# 3. Candidaturas 2021 (incluye sexo, edad y pacto de cada candidatura)
archivos <- c(
  "resultados_elecciones_alcaldes_ce_2004_2016.xlsx",
  "Definitivos_Alcalde_2021.zip",
  "candidaturas_convencionales_gobernadores_alcaldes_y_concejales_2021.xlsx"
)

for (archivo in archivos) {
  destino <- file.path(carpeta_raw, archivo)
  if (!file.exists(destino)) {
    message("Descargando: ", archivo)
    download.file(paste0(base_url, archivo), destfile = destino, mode = "wb")
  } else {
    message("Ya existe, se omite: ", archivo)
  }
}

# Descomprimir los resultados de 2021
unzip(file.path(carpeta_raw, "Definitivos_Alcalde_2021.zip"), exdir = carpeta_raw)

# -------------------------------------------------------------
# Elección 2024
# El Servel publica los resultados de 2024 solo en un tablero de
# Power BI (no como archivo descargable). Este bloque consulta los
# datos de ese tablero directamente y los guarda como CSV.
# Tablero: sitio del Servel > Resultados electorales históricos >
# 2024 > Alcaldes. Solo se descarga si el archivo aún no existe.
# -------------------------------------------------------------
archivo_2024 <- file.path(carpeta_raw, "resultados_alcaldes_2024.csv")

if (!file.exists(archivo_2024)) {
  message("Descargando: resultados de alcaldes 2024 (tablero Power BI)")
  for (p in c("httr", "jsonlite")) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }

  clave    <- "02cb3094-0711-4027-891f-19efb78b9d0f"  # clave pública del tablero
  servidor <- "https://wabi-south-central-us-api.analysis.windows.net/public/reports/"
  cabecera <- httr::add_headers(`X-PowerBI-ResourceKey` = clave)

  # 1. Datos técnicos del tablero (modelo y reporte)
  info <- jsonlite::fromJSON(httr::content(
    httr::GET(paste0(servidor, clave, "/modelsAndExploration?preferReadOnlySession=true"),
              cabecera),
    as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

  # 2. Consulta: votos por comuna y candidatura, con sus características
  columna <- function(fuente, variable) {
    list(Column = list(Expression = list(SourceRef = list(Source = fuente)),
                       Property = variable),
         Name = paste0(fuente, ".", variable))
  }
  seleccion <- list(
    columna("r", "Comuna"), columna("c", "can_id"),
    columna("c", "can_nombre_completo"), columna("c", "can_primer_nombre"),
    columna("c", "can_apellido_paterno"), columna("c", "can_apellido_materno"),
    columna("c", "can_sexo"), columna("c", "can_electo"),
    columna("p", "partido_nombre"), columna("k", "pacto_nombre"),
    list(Aggregation = list(Expression = list(Column = list(
      Expression = list(SourceRef = list(Source = "f")), Property = "votos_total")),
      Function = 0), Name = "votos")
  )
  consulta <- list(
    version = "1.0.0",
    queries = list(list(
      Query = list(Commands = list(list(SemanticQueryDataShapeCommand = list(
        Query = list(
          Version = 2,
          From = list(list(Name = "r", Entity = "DimRegion", Type = 0),
                      list(Name = "c", Entity = "DimCandidato", Type = 0),
                      list(Name = "p", Entity = "DimPartido", Type = 0),
                      list(Name = "k", Entity = "DimPacto", Type = 0),
                      list(Name = "f", Entity = "FT_votos_todos", Type = 0)),
          Select = seleccion),
        Binding = list(
          Primary = list(Groupings = list(list(Projections = as.list(0:10)))),
          DataReduction = list(DataVolume = 4,
                               Primary = list(Window = list(Count = 30000))),
          Version = 1))))),
      QueryId = "",
      ApplicationContext = list(
        DatasetId = info$models[[1]]$dbName,
        Sources = list(list(ReportId = info$exploration$report$objectId)))
    )),
    cancelQueries = list(),
    modelId = info$models[[1]]$id
  )
  respuesta <- httr::POST(paste0(servidor, "querydata?synchronous=true"), cabecera,
                          body = jsonlite::toJSON(consulta, auto_unbox = TRUE),
                          httr::content_type_json())
  datos <- jsonlite::fromJSON(httr::content(respuesta, as = "text", encoding = "UTF-8"),
                              simplifyVector = FALSE)
  datos <- datos$results[[1]]$result$data$dsr$DS[[1]]

  # 3. Power BI entrega los datos "comprimidos": cuando un valor se
  # repite respecto de la fila anterior no lo envía (marca R), los
  # valores vacíos se marcan con Ø, y los textos se reemplazan por
  # números que remiten a un diccionario. Aquí se reconstruye la tabla.
  filas   <- datos$PH[[1]]$DM0
  esquema <- filas[[1]]$S
  dicc    <- datos$ValueDicts
  n_col   <- length(esquema)
  anterior <- vector("list", n_col)
  tabla <- vector("list", length(filas))
  for (j in seq_along(filas)) {
    fila <- filas[[j]]
    repetidos <- if (is.null(fila$R)) 0 else fila$R
    vacios    <- if (is.null(fila[["\u00d8"]])) 0 else fila[["\u00d8"]]
    valores <- fila$C
    k <- 1
    actual <- vector("list", n_col)
    for (i in seq_len(n_col)) {
      bit <- 2^(i - 1)
      if ((repetidos %/% bit) %% 2 == 1) {
        actual[i] <- list(anterior[[i]])
      } else if ((vacios %/% bit) %% 2 == 1) {
        actual[i] <- list(NA)
      } else {
        v <- valores[[k]]; k <- k + 1
        if (!is.null(esquema[[i]]$DN) && is.numeric(v)) {
          v <- dicc[[esquema[[i]]$DN]][[v + 1]]
        }
        actual[i] <- list(v)
      }
    }
    anterior <- actual
    tabla[[j]] <- actual
  }
  res_2024 <- as.data.frame(do.call(rbind, lapply(tabla, function(x)
    sapply(x, function(v) if (is.null(v)) NA else as.character(v)))))
  names(res_2024) <- c("comuna", "id_candidato", "candidato", "nombres", "paterno",
                       "materno", "sexo", "electo", "partido", "pacto", "votos")
  write.csv(res_2024, archivo_2024, row.names = FALSE, fileEncoding = "UTF-8")
} else {
  message("Ya existe, se omite: resultados_alcaldes_2024.csv")
}

message("Listo. Archivos en data/raw:")
print(list.files(carpeta_raw, recursive = TRUE))
