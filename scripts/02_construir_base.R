# =============================================================
# 02_construir_base.R
# Construye la base de datos de candidaturas a alcalde/sa
# (2004, 2008, 2012, 2016, 2021 y 2024) a partir de los archivos
# oficiales del Servel guardados en data/raw/.
# Resultado: data/clean/base_alcaldes.csv
# Unidad de análisis: una candidatura (persona x comuna x elección)
# =============================================================

# Paquetes (si alguno no está instalado, se instala solo)
paquetes <- c("readxl", "openxlsx", "dplyr", "stringr", "readr")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readxl)
library(dplyr)
library(stringr)
library(readr)

# -------------------------------------------------------------
# Función auxiliar: limpia textos (sin tildes, en mayúscula,
# sin signos) para poder comparar nombres y comunas entre años
# -------------------------------------------------------------
limpiar <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z ]", " ")
  str_squish(x)
}

# Cabo de Hornos y Antártica aparecen con distintos nombres según
# el año; Antártica vota junto a Cabo de Hornos, así que se unifican
unificar_comuna <- function(x) {
  x <- limpiar(x)
  x[str_detect(x, "HORNOS|ANTARTICA")] <- "CABO DE HORNOS"
  # Comunas escritas de forma distinta en 2021: se usa la forma
  # de los archivos 2004-2016
  x[x == "AYSEN"]     <- "AISEN"
  x[x == "LLAILLAY"]  <- "LLAY LLAY"
  x[x == "TREHUACO"]  <- "TREGUACO"
  x[x == "MARCHIGUE"] <- "MARCHIHUE"
  x
}

# =============================================================
# PARTE 1: Elecciones 2004, 2008, 2012 y 2016
# =============================================================
message("Paso 1 de 4: leyendo elecciones 2004-2016...")
r1 <- read_excel("data/raw/resultados_elecciones_alcaldes_ce_2004_2016.xlsx")

# Se renombran las columnas por posición para evitar problemas
# con tildes en los nombres originales
names(r1) <- c("tipo", "cargo", "fecha", "anio", "inicio", "fin", "periodo",
               "id_region", "region", "circ_senatorial", "distrito", "comuna",
               "id_circ", "circ_electoral", "sexo_mesa", "candidato",
               "nombres", "paterno", "materno", "sexo", "electo", "partido",
               "sigla", "lista", "votos")

# El archivo viene desagregado por circunscripción y tipo de mesa;
# se suman los votos para tener una fila por candidatura
base_04_16 <- r1 %>%
  filter(!candidato %in% c("VOTOS EN BLANCO", "VOTOS NULOS")) %>%
  mutate(comuna = unificar_comuna(comuna)) %>%
  group_by(anio, comuna, candidato) %>%
  summarise(
    nombres = first(nombres),
    paterno = first(paterno),
    materno = first(materno),
    sexo    = first(sexo),
    partido = str_trim(first(sigla)),
    lista   = str_trim(first(lista)),
    gano    = any(electo == "SI", na.rm = TRUE),
    votos   = sum(votos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Candidaturas fuera de pacto: el Servel las identifica con una
    # lista especial que cambia según el año
    fuera_pacto = (anio %in% c(2004, 2008) & lista == "CI") |
                  (anio == 2012 & (is.na(lista) | lista == "")) |
                  (anio == 2016 & lista == "T"),
    lista = paste0(anio, "_", lista)
  ) %>%
  # En Río Claro 2016 el archivo no marca a nadie como electo;
  # en esos casos se considera ganador a quien obtuvo más votos
  group_by(anio, comuna) %>%
  mutate(gano = if (any(gano)) gano else votos == max(votos)) %>%
  ungroup()

# =============================================================
# PARTE 2: Elección 2021
# =============================================================

# 2.1 Resultados por mesa (un archivo por región)
archivos_2021 <- list.files("data/raw/Definitivos Alcalde 2021",
                            pattern = "xlsx$", full.names = TRUE)

# Cada archivo regional tiene varias hojas (una por distrito),
# por lo que se leen todas las hojas de cada archivo.
# Se usa el paquete openxlsx porque gasta mucha menos memoria que
# readxl con archivos grandes (el de la Región Metropolitana tiene
# más de 125 mil filas y hacía colapsar R en Posit Cloud).
# Solo se leen las 5 columnas necesarias, seleccionadas por posición
# porque sus nombres cambian entre regiones:
# 6 = comuna, 13 = pacto, 14 = partido, 15 = candidato, 16 = votos
leer_hoja <- function(archivo, hoja) {
  x <- openxlsx::read.xlsx(archivo, sheet = hoja, cols = c(6, 13, 14, 15, 16),
                           skipEmptyCols = FALSE)
  # Se suman los votos de todas las mesas de inmediato, para que la
  # tabla quede pequeña y no ocupe memoria
  tibble(
    comuna    = x[[6]],
    pacto     = x[[13]],
    partido   = x[[14]],
    candidato = x[[15]],
    votos     = suppressWarnings(as.numeric(x[[16]]))
  ) %>%
    group_by(comuna, candidato) %>%
    summarise(pacto = first(pacto), partido = first(partido),
              votos = sum(votos, na.rm = TRUE), .groups = "drop")
}
leer_region <- function(archivo) {
  hojas <- openxlsx::getSheetNames(archivo)
  resultado <- bind_rows(lapply(hojas, function(h) leer_hoja(archivo, h)))
  gc()  # libera memoria antes de pasar al siguiente archivo
  resultado
}

message("Paso 2 de 4: leyendo resultados 2021 (16 regiones, es la parte más lenta)...")
res_2021 <- bind_rows(lapply(archivos_2021, function(a) {
  message("   leyendo ", basename(a))
  leer_region(a)
})) %>%
  filter(!is.na(candidato),
         !candidato %in% c("VOTOS EN BLANCO", "VOTOS NULOS")) %>%
  mutate(comuna = unificar_comuna(comuna)) %>%
  group_by(comuna, candidato) %>%
  summarise(pacto   = first(pacto),
            partido = first(partido),
            votos   = sum(votos, na.rm = TRUE),
            .groups = "drop")

# 2.2 Datos de las candidaturas (sexo y nombres separados)
cand_2021 <- read_excel(
  "data/raw/candidaturas_convencionales_gobernadores_alcaldes_y_concejales_2021.xlsx",
  col_types = "text"
) %>%
  filter(eleccion == "ALCALDES") %>%
  transmute(comuna    = unificar_comuna(territorio),
            nombre_c  = limpiar(`candidato(a)`),
            nombres   = nombre,
            paterno, materno,
            sexo = ifelse(sexo == "FEMENINO", "MUJER", "HOMBRE")) %>%
  distinct(comuna, nombre_c, .keep_all = TRUE)

# 2.3 Unir resultados con candidaturas. Como algunos nombres
# están escritos distinto (tildes, segundo nombre o errores de
# tipeo), dentro de cada comuna se busca el nombre más parecido,
# y solo se acepta si los apellidos coinciden
cand_2021 <- cand_2021 %>%
  mutate(pat_l = limpiar(paterno), mat_l = limpiar(materno))

message("Paso 3 de 4: uniendo resultados y candidaturas 2021...")
res_2021$nombre_c <- limpiar(res_2021$candidato)
res_2021$id_cand <- NA_integer_
for (i in seq_len(nrow(res_2021))) {
  palabras <- str_split(res_2021$nombre_c[i], " ")[[1]]
  opciones <- which(cand_2021$comuna == res_2021$comuna[i])
  # se quedan solo las candidaturas cuyos dos apellidos aparecen
  # en el nombre (o que difieren en máximo 2 letras)
  ok <- opciones[(cand_2021$pat_l[opciones] %in% palabras &
                  cand_2021$mat_l[opciones] %in% palabras) |
                 adist(res_2021$nombre_c[i], cand_2021$nombre_c[opciones])[1, ] <= 2]
  if (length(ok) > 0) {
    distancias <- adist(res_2021$nombre_c[i], cand_2021$nombre_c[ok])
    res_2021$id_cand[i] <- ok[which.min(distancias)]
  }
}

# Candidaturas que no aparecen en el archivo de candidaturas: los
# apellidos se toman de las dos últimas palabras del nombre y el
# sexo se codifica manualmente a partir del nombre de pila
sin_match <- res_2021 %>% filter(is.na(id_cand))
message("Candidaturas 2021 sin datos en el archivo de candidaturas: ", nrow(sin_match))
print(sin_match %>% select(comuna, candidato), n = 50)

mujeres_manual <- c("GIOVANNA ANDREA FLORES PINTO", "AMPARO GARCIA SALDIAS")

base_21 <- res_2021 %>%
  mutate(
    n_pal   = str_count(nombre_c, " ") + 1,
    nombres = ifelse(is.na(id_cand), word(nombre_c, 1), cand_2021$nombres[id_cand]),
    paterno = ifelse(is.na(id_cand), word(nombre_c, n_pal - 1), cand_2021$paterno[id_cand]),
    materno = ifelse(is.na(id_cand), word(nombre_c, n_pal), cand_2021$materno[id_cand]),
    sexo    = case_when(!is.na(id_cand) ~ cand_2021$sexo[id_cand],
                        nombre_c %in% mujeres_manual ~ "MUJER",
                        TRUE ~ "HOMBRE")
  ) %>%
  select(-n_pal) %>%
  group_by(comuna) %>%
  mutate(gano = votos == max(votos)) %>%   # gana quien tiene más votos
  ungroup() %>%
  mutate(anio = 2021,
         fuera_pacto = is.na(pacto) | pacto == "CANDIDATURA INDEPENDIENTE",
         lista = paste0(anio, "_", pacto)) %>%
  select(-nombre_c, -id_cand, -pacto)

# =============================================================
# PARTE 2b: Elección 2024
# (archivo creado por 01_descargar_datos.R desde el tablero del Servel)
# =============================================================
message("Paso 3b: leyendo resultados 2024...")
base_24 <- read.csv("data/raw/resultados_alcaldes_2024.csv",
                    encoding = "UTF-8", colClasses = "character") %>%
  filter(!candidato %in% c("Votos Nulos", "Votos En Blanco")) %>%
  mutate(comuna = unificar_comuna(comuna),
         votos  = as.numeric(votos)) %>%
  # Antártica y Cabo de Hornos comparten elección: se suman sus votos
  group_by(comuna, id_candidato) %>%
  summarise(candidato = toupper(first(candidato)),
            nombres = first(nombres), paterno = first(paterno),
            materno = first(materno),
            sexo    = ifelse(first(sexo) == "M", "MUJER", "HOMBRE"),
            partido = first(partido),
            pacto   = first(pacto),
            gano    = any(electo == "1"),
            votos   = sum(votos),
            .groups = "drop") %>%
  mutate(anio = 2024,
         fuera_pacto = pacto == "Independientes",
         lista = paste0(anio, "_", pacto)) %>%
  select(-id_candidato, -pacto)

# =============================================================
# PARTE 3: Unir todos los años y construir variables
# =============================================================
message("Paso 4 de 4: uniendo todos los años y construyendo variables...")
base <- bind_rows(base_04_16, base_21, base_24) %>%
  mutate(partido = limpiar(partido))

# Correcciones manuales: candidaturas que vienen sin sexo ni
# nombres separados en el archivo del Servel. El sexo se codifica
# a partir del nombre, como se indicó en la Entrega 1
sin_sexo <- is.na(base$sexo)
message("Candidaturas sin sexo en los datos originales: ", sum(sin_sexo))
print(base[sin_sexo, c("anio", "comuna", "candidato")])
correcciones <- tibble(
  candidato = c("CARLOS  LOPEZ VEGA", "LUIS  MOYANO CRUZ", "OSVALDO  VILLACORTA TORO"),
  sexo_c    = c("HOMBRE", "HOMBRE", "HOMBRE"),
  nombres_c = c("CARLOS", "LUIS", "OSVALDO"),
  paterno_c = c("LOPEZ", "MOYANO", "VILLACORTA"),
  materno_c = c("VEGA", "CRUZ", "TORO")
)
base <- base %>%
  left_join(correcciones, by = "candidato") %>%
  mutate(sexo    = coalesce(sexo, sexo_c),
         nombres = coalesce(nombres, nombres_c),
         paterno = coalesce(paterno, paterno_c),
         materno = coalesce(materno, materno_c)) %>%
  select(-ends_with("_c"))

# 3.1 Sector político según partido
izquierda <- c("PS", "PSCH", "PPD", "PRSD", "PCCH", "PH", "PRO", "IGUALDAD",
               "MAS", "MAS REGION", "PEV", "RD", "PODER", "PL", "UP",
               "PARTIDO SOCIALISTA DE CHILE", "PARTIDO POR LA DEMOCRACIA",
               "PARTIDO RADICAL DE CHILE", "PARTIDO COMUNISTA DE CHILE",
               "PARTIDO HUMANISTA", "UNION PATRIOTICA", "CONVERGENCIA SOCIAL",
               "REVOLUCION DEMOCRATICA", "PARTIDO ECOLOGISTA VERDE", "COMUNES",
               "FEDERACION REGIONALISTA VERDE SOCIAL", "PARTIDO LIBERAL DE CHILE",
               "FRENTE AMPLIO", "PARTIDO ALIANZA VERDE POPULAR",
               "PARTIDO ACCION HUMANISTA")
centro    <- c("PDC", "FP", "CIUDADANOS", "PARTIDO DEMOCRATA CRISTIANO",
               "PARTIDO DEMOCRATAS CHILE", "MOVIMIENTO AMARILLOS POR CHILE")
derecha   <- c("UDI", "RN", "PRI", "AMPLITUD", "EVOP",
               "UNION DEMOCRATA INDEPENDIENTE", "RENOVACION NACIONAL",
               "EVOLUCION POLITICA", "PARTIDO REPUBLICANO DE CHILE",
               "PARTIDO REGIONALISTA INDEPENDIENTE DEMOCRATA", "NUEVO TIEMPO",
               "PARTIDO SOCIAL CRISTIANO")

base <- base %>%
  mutate(sector_partido = case_when(
    partido %in% izquierda ~ "Izquierda",
    partido %in% centro    ~ "Centro",
    partido %in% derecha   ~ "Derecha",
    TRUE ~ NA_character_
  ))

# Independientes dentro de un pacto: se les asigna el sector más
# frecuente entre los candidatos con partido de su misma lista
sector_lista <- base %>%
  filter(!is.na(sector_partido)) %>%
  count(lista, sector_partido) %>%
  group_by(lista) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(lista, sector_lista = sector_partido)

base <- base %>%
  left_join(sector_lista, by = "lista") %>%
  mutate(sector = case_when(
    fuera_pacto            ~ "Independiente",
    !is.na(sector_partido) ~ sector_partido,
    !is.na(sector_lista)   ~ sector_lista,
    TRUE                   ~ "Otro"
  ))

# 3.2 Clave de persona (apellidos + inicial del nombre) para seguir
# a una misma persona entre elecciones. Se usa la inicial porque
# a veces el nombre cambia (por ejemplo, "Catherine" y "Cathy")
base <- base %>%
  mutate(clave = paste(limpiar(paterno), limpiar(materno),
                       str_sub(limpiar(nombres), 1, 1)))

# 3.3 Incumbencia: era alcalde/sa en ejercicio si ganó la elección
# anterior en la misma comuna. En 2004 no se puede saber (la
# elección anterior usaba otro sistema), por lo que queda NA
anios <- c(2004, 2008, 2012, 2016, 2021, 2024)
ganadores <- base %>%
  filter(gano) %>%
  mutate(anio = anios[match(anio, anios) + 1]) %>%  # se "corre" a la elección siguiente
  distinct(anio, comuna, clave) %>%
  mutate(incumbente = TRUE)

base <- base %>%
  left_join(ganadores, by = c("anio", "comuna", "clave")) %>%
  mutate(incumbente = case_when(anio == 2004 ~ NA,
                                is.na(incumbente) ~ FALSE,
                                TRUE ~ incumbente))

# 3.4 Número de períodos consecutivos que lleva en el cargo
# (0 si no es incumbente). Sirve para el modelo de reelección
base <- base %>%
  arrange(comuna, clave, anio) %>%
  group_by(comuna, clave) %>%
  mutate(periodos_previos = {
    n <- integer(length(incumbente))
    for (k in seq_along(n)) {
      if (is.na(incumbente[k])) n[k] <- NA
      else if (!incumbente[k]) n[k] <- 0
      else n[k] <- ifelse(k > 1 && !is.na(n[k - 1]), n[k - 1] + 1, 1)
    }
    n
  }) %>%
  ungroup()

# 3.5 Porcentaje de votos válidos y número de candidaturas por comuna
base <- base %>%
  group_by(anio, comuna) %>%
  mutate(pct_votos = 100 * votos / sum(votos),
         n_candidatos = n()) %>%
  ungroup()

# 3.6 Ordenar y dejar las variables finales con valores legibles
base_final <- base %>%
  transmute(
    anio,
    comuna,
    candidato,
    sexo  = ifelse(sexo == "MUJER", "Mujer", "Hombre"),
    mujer = as.integer(sexo == "Mujer"),
    partido,
    pacto = ifelse(fuera_pacto, "Fuera de pacto", "Dentro de pacto"),
    sector,
    incumbente = as.integer(incumbente),
    periodos_previos,
    votos,
    pct_votos = round(pct_votos, 2),
    gano = as.integer(gano),
    n_candidatos
  ) %>%
  arrange(anio, comuna, desc(votos))

# =============================================================
# PARTE 4: Guardar y revisar
# =============================================================
dir.create("data/clean", showWarnings = FALSE)
write_csv(base_final, "data/clean/base_alcaldes.csv")

message("Base guardada en data/clean/base_alcaldes.csv")
message("Filas (candidaturas): ", nrow(base_final))
print(table(base_final$anio, base_final$sexo))
