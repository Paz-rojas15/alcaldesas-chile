# ¿Por qué hay tan pocas alcaldesas en Chile?

Proyecto del curso Medición y análisis dimensional de datos políticos (ICP5006), Magíster en Ciencia Política, Pontificia Universidad Católica de Chile.

**Autora:** María Paz Rojas Orrego

Este proyecto analiza las elecciones de alcaldes en Chile entre 2004 y 2024 para entender si la baja presencia de alcaldesas se explica porque se presentan pocas mujeres (oferta), porque la ciudadanía vota menos por ellas (demanda) o porque les cuesta más ser reelegidas.

## Organización del repositorio

```
├── project.Rproj               Archivo de proyecto de R
├── informe_entrega2.Rmd        Documento R Markdown que genera el informe en Word
├── plantilla_word.docx         Plantilla de formato del Word (Arial 12)
├── scripts/
│   ├── 01_descargar_datos.R    Descarga los datos oficiales del Servel (2004-2024)
│   ├── 02_construir_base.R     Limpia los datos y construye la base de candidaturas
│   ├── 02b_datos_comunales.R   Descarga y prepara datos sociodemográficos de las comunas
│   ├── 03_descriptivos.R       Tablas y gráficos descriptivos
│   └── 04_modelos.R            Estimación de los modelos y probabilidades predichas
├── data/
│   ├── raw/                    Datos originales, sin modificar
│   └── clean/                  Bases procesadas (base_alcaldes.csv y datos_comunales.csv)
├── output/                     Tablas (.csv) y gráficos (.png) generados por los scripts
└── entregas/                   Entregas anteriores
```

## Cómo reproducir el análisis

1. Abrir `project.Rproj` en RStudio.
2. Ejecutar los scripts de la carpeta `scripts/` en este orden: `01`, `02`, `02b`, `03` y `04`. Cada script instala automáticamente los paquetes que necesita.
3. Abrir `informe_entrega2.Rmd` y presionar **Knit** para generar el informe en Word.

Los datos originales ya están incluidos en `data/raw/`, por lo que el paso 1 de descarga solo es necesario si se quieren obtener de nuevo desde las fuentes.

## Datos

| Fuente | Contenido | Años |
|---|---|---|
| Servicio Electoral (Servel) | Resultados de elecciones de alcaldes por candidatura, con sexo y electo | 2004, 2008, 2012, 2016 |
| Servel | Resultados definitivos por mesa y nómina de candidaturas | 2021 |
| Servel (tablero de resultados en Power BI) | Resultados por candidatura, con sexo, partido y pacto | 2024 |
| INE, Censo de Población y Vivienda | Población y años de escolaridad por comuna | 2024 |
| Ministerio de Desarrollo Social y Familia | Tasa de pobreza por ingresos por comuna | 2017 |

**Unidad de análisis:** candidatura a alcalde o alcaldesa (persona × comuna × elección). La base final tiene 7.866 candidaturas.

## Variables principales de `data/clean/base_alcaldes.csv`

| Variable | Descripción |
|---|---|
| `anio`, `comuna`, `candidato` | Elección, comuna y nombre |
| `sexo`, `mujer` | Sexo de la candidatura (Mujer = 1) |
| `partido`, `pacto`, `sector` | Partido, dentro/fuera de pacto y sector político |
| `incumbente` | 1 = era alcalde/sa en ejercicio (NA en 2004) |
| `periodos_previos` | Períodos consecutivos previos en el cargo |
| `votos`, `pct_votos` | Votos y % de votos válidos en la comuna |
| `gano` | 1 = ganó la alcaldía |
| `n_candidatos` | Número de candidaturas en la comuna |

## Requisitos

R (versión 4.3 o superior). Paquetes: readxl, openxlsx, dplyr, tidyr, stringr, readr, ggplot2, scales, httr, jsonlite, sandwich, lmtest, MASS, marginaleffects, knitr y rmarkdown.
