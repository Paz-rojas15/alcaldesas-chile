# =============================================================
# 02b_datos_comunales.R
# Descarga y prepara características sociodemográficas de las
# comunas, para unirlas a la base de candidaturas:
#   - Población (Censo 2024, INE)
#   - Años de escolaridad promedio, población de 18 años o más (Censo 2024, INE)
#   - Tasa de pobreza por ingresos 2017 (estimaciones comunales SAE,
#     Ministerio de Desarrollo Social), año intermedio del período
# Además, a partir de la propia base, se construye si la comuna ya
# había tenido una alcaldesa en alguna elección anterior.
# Resultado: data/clean/datos_comunales.csv
# =============================================================

paquetes <- c("readxl", "dplyr", "stringr", "readr")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readxl)
library(dplyr)
library(stringr)
library(readr)
options(timeout = 600)

# -------------------------------------------------------------
# 1. Descargar los archivos (solo si no existen)
# -------------------------------------------------------------
fuentes <- c(
  "censo2024_poblacion.xlsx" =
    "https://censo2024.ine.gob.cl/wp-content/uploads/2025/03/D1_Poblacion-censada-por-sexo-y-edad-en-grupos-quinquenales.xlsx",
  "censo2024_educacion.xlsx" =
    "https://censo2024.ine.gob.cl/wp-content/uploads/2025/06/P7_Educacion.xlsx",
  "pobreza_comunal_2017.xlsx" =
    "https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/pobreza-comunal/2017/PLANILLA_Estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx"
)
for (archivo in names(fuentes)) {
  destino <- file.path("data/raw", archivo)
  if (!file.exists(destino)) {
    message("Descargando: ", archivo)
    download.file(fuentes[[archivo]], destfile = destino, mode = "wb")
  }
}

# -------------------------------------------------------------
# 2. Funciones para que los nombres de comuna coincidan con la base
# (las mismas reglas que en 02_construir_base.R)
# -------------------------------------------------------------
limpiar <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z ]", " ")
  str_squish(x)
}
unificar_comuna <- function(x) {
  x <- limpiar(x)
  x[str_detect(x, "HORNOS|ANTARTICA")] <- "CABO DE HORNOS"
  x[x == "AYSEN"]     <- "AISEN"
  x[x == "LLAILLAY"]  <- "LLAY LLAY"
  x[x == "TREHUACO"]  <- "TREGUACO"
  x[x == "MARCHIGUE"] <- "MARCHIHUE"
  x[x == "PAIGUANO"]  <- "PAIHUANO"   # así aparece en la planilla de pobreza
  x
}

# -------------------------------------------------------------
# 3. Leer cada fuente (las planillas tienen títulos arriba, por eso
# se saltan las primeras filas y se eligen columnas por posición)
# -------------------------------------------------------------
# Población: hoja 2 (por comuna). Col 5 = código, 6 = comuna, 7 = población
pob <- suppressMessages(read_excel("data/raw/censo2024_poblacion.xlsx", sheet = "2", skip = 4,
                  col_names = FALSE, col_types = "text"))
pob <- tibble(comuna = pob[[6]], poblacion = as.numeric(pob[[7]]),
              codigo = as.numeric(pob[[5]])) %>%
  filter(codigo > 0, !is.na(poblacion))

# Escolaridad: hoja 4 (por sexo y comuna). Se usa el total comunal,
# columna 9 = años de escolaridad promedio de 18 años o más
edu <- suppressMessages(read_excel("data/raw/censo2024_educacion.xlsx", sheet = "4", skip = 4,
                  col_names = FALSE, col_types = "text"))
edu <- tibble(comuna = edu[[6]], sexo = edu[[7]], codigo = as.numeric(edu[[5]]),
              escolaridad = as.numeric(edu[[9]])) %>%
  filter(codigo > 0, sexo == "Total Comuna") %>%
  select(comuna, escolaridad)

# Pobreza por ingresos 2017. Col 3 = comuna, col 5 = tasa (proporción)
pobreza <- suppressMessages(read_excel("data/raw/pobreza_comunal_2017.xlsx", sheet = "Ingresos 2017",
                      skip = 3, col_names = FALSE, col_types = "text"))
pobreza <- tibble(comuna = pobreza[[3]],
                  pobreza = 100 * as.numeric(pobreza[[5]])) %>%
  filter(!is.na(pobreza))

# -------------------------------------------------------------
# 4. Unir las tres fuentes por nombre de comuna
# -------------------------------------------------------------
comunas <- pob %>%
  select(comuna, poblacion) %>%
  mutate(comuna = unificar_comuna(comuna)) %>%
  group_by(comuna) %>% summarise(poblacion = sum(poblacion), .groups = "drop") %>%
  left_join(edu %>% mutate(comuna = unificar_comuna(comuna)) %>%
              group_by(comuna) %>% summarise(escolaridad = mean(escolaridad)),
            by = "comuna") %>%
  left_join(pobreza %>% mutate(comuna = unificar_comuna(comuna)) %>%
              group_by(comuna) %>% summarise(pobreza = mean(pobreza)),
            by = "comuna") %>%
  mutate(log_poblacion = log(poblacion))

# Control: comunas de la base electoral que no encontraron datos
base <- read_csv("data/clean/base_alcaldes.csv", show_col_types = FALSE)
sin_datos <- setdiff(unique(base$comuna),
                     comunas$comuna[!is.na(comunas$escolaridad) & !is.na(comunas$pobreza)])
message("Comunas sin datos sociodemográficos: ", length(sin_datos))
if (length(sin_datos) > 0) print(sin_datos)

# -------------------------------------------------------------
# 5. ¿La comuna ya tuvo una alcaldesa antes? (por elección)
# Se calcula con la propia base: 1 si en alguna elección anterior
# (desde 2004) ganó una mujer. En 2004 queda NA.
# -------------------------------------------------------------
alcaldesa_previa <- base %>%
  group_by(comuna, anio) %>%
  summarise(gano_mujer = as.integer(any(gano == 1 & mujer == 1)), .groups = "drop") %>%
  arrange(comuna, anio) %>%
  group_by(comuna) %>%
  mutate(alcaldesa_previa = as.integer(lag(cummax(gano_mujer)))) %>%
  ungroup() %>%
  select(comuna, anio, alcaldesa_previa)

datos_comunales <- alcaldesa_previa %>%
  left_join(comunas, by = "comuna")

write_csv(datos_comunales, "data/clean/datos_comunales.csv")
message("Datos comunales guardados en data/clean/datos_comunales.csv")
print(summary(comunas %>% select(poblacion, escolaridad, pobreza)))
