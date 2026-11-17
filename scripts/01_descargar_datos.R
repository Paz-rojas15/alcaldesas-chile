# Descarga los datos oficiales del Servel y los guarda en data/raw/
options(timeout = 600)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

base_url <- "https://www.servel.cl/wp-content/uploads/2022/12/"
archivos <- c(
  "resultados_elecciones_alcaldes_ce_2004_2016.xlsx",
  "Definitivos_Alcalde_2021.zip",
  "candidaturas_convencionales_gobernadores_alcaldes_y_concejales_2021.xlsx"
)

for (archivo in archivos) {
  destino <- file.path("data/raw", archivo)
  if (!file.exists(destino)) {
    download.file(paste0(base_url, archivo), destfile = destino, mode = "wb")
  }
}

unzip(file.path("data/raw", "Definitivos_Alcalde_2021.zip"), exdir = "data/raw")
list.files("data/raw")
