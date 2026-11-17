# =============================================================
# 03_descriptivos.R
# Análisis descriptivo: tablas y gráficos sobre la presencia de
# mujeres entre las candidaturas y las alcaldías (2004-2024).
# Usa la base creada por 02_construir_base.R
# Resultados: tablas (.csv) y gráficos (.png) en la carpeta output/
# =============================================================

paquetes <- c("dplyr", "tidyr", "ggplot2", "readr", "scales")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)

base <- read_csv("data/clean/base_alcaldes.csv", show_col_types = FALSE)
dir.create("output", showWarnings = FALSE)

# Estilo común para todos los gráficos
tema <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())
colores_sexo <- c("Mujer" = "#B5338A", "Hombre" = "#3A6EA5")

# -------------------------------------------------------------
# Tabla 1. Candidaturas y alcaldías por año
# (representación descriptiva y oferta, H1)
# -------------------------------------------------------------
tabla1 <- base %>%
  group_by(Año = anio) %>%
  summarise(
    `Candidaturas` = n(),
    `Candidatas mujeres` = sum(mujer),
    `% candidatas` = round(100 * mean(mujer), 1),
    `Alcaldías` = sum(gano),
    `Alcaldesas electas` = sum(gano * mujer),
    `% alcaldesas` = round(100 * sum(gano * mujer) / sum(gano), 1)
  )
print(tabla1)
write_csv(tabla1, "output/tabla1_candidaturas_alcaldias.csv")

# -------------------------------------------------------------
# Gráfico 1. % de mujeres entre candidaturas y entre electos
# -------------------------------------------------------------
g1_datos <- tabla1 %>%
  select(Año, `% candidatas`, `% alcaldesas`) %>%
  pivot_longer(-Año, names_to = "Indicador", values_to = "Porcentaje") %>%
  mutate(Indicador = recode(Indicador,
                            "% candidatas" = "Candidaturas de mujeres",
                            "% alcaldesas" = "Alcaldías ganadas por mujeres"))

g1 <- ggplot(g1_datos, aes(Año, Porcentaje, color = Indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -1, size = 3.3,
            show.legend = FALSE) +
  scale_x_continuous(breaks = c(2004, 2008, 2012, 2016, 2021, 2024)) +
  scale_y_continuous(limits = c(0, 30), labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c("#B5338A", "#E8A33D")) +
  labs(title = "Mujeres entre candidaturas y alcaldías, 2004-2024",
       x = NULL, y = NULL, color = NULL,
       caption = "Fuente: elaboración propia con datos del Servel.") +
  tema
ggsave("output/grafico1_mujeres_candidaturas_alcaldias.png", g1,
       width = 8, height = 5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# Tabla 2 y Gráfico 2. Oferta de candidatas por pacto y sector (H1)
# -------------------------------------------------------------
tabla2 <- base %>%
  group_by(Sector = sector) %>%
  summarise(Candidaturas = n(),
            `% candidatas` = round(100 * mean(mujer), 1)) %>%
  bind_rows(
    base %>% group_by(Sector = pacto) %>%
      summarise(Candidaturas = n(), `% candidatas` = round(100 * mean(mujer), 1))
  )
print(tabla2)
write_csv(tabla2, "output/tabla2_candidatas_sector_pacto.csv")

g2_datos <- base %>%
  filter(sector != "Otro") %>%   # categoría muy pequeña
  group_by(anio, sector) %>%
  summarise(pct = 100 * mean(mujer), .groups = "drop")

g2 <- ggplot(g2_datos, aes(anio, pct, color = sector)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2004, 2008, 2012, 2016, 2021, 2024)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Porcentaje de candidaturas de mujeres según sector político",
       x = NULL, y = NULL, color = NULL,
       caption = "Fuente: elaboración propia con datos del Servel.") +
  tema
ggsave("output/grafico2_candidatas_por_sector.png", g2,
       width = 8, height = 5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# Tabla 3 y Gráfico 3. Éxito electoral según sexo e incumbencia (H2)
# Se excluye 2004 porque no se puede saber quién era incumbente
# -------------------------------------------------------------
tabla3 <- base %>%
  filter(anio > 2004) %>%
  mutate(Condición = ifelse(incumbente == 1, "Incumbente", "No incumbente")) %>%
  group_by(Condición, Sexo = sexo) %>%
  summarise(Candidaturas = n(),
            `% que ganó` = round(100 * mean(gano), 1),
            `% votos (promedio)` = round(mean(pct_votos), 1),
            .groups = "drop")
print(tabla3)
write_csv(tabla3, "output/tabla3_exito_sexo_incumbencia.csv")

g3 <- ggplot(tabla3, aes(Condición, `% que ganó`, fill = Sexo)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = paste0(`% que ganó`, "%")),
            position = position_dodge(0.8), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = colores_sexo) +
  scale_y_continuous(limits = c(0, 80), labels = function(x) paste0(x, "%")) +
  labs(title = "Porcentaje de candidaturas que ganaron, según sexo e incumbencia",
       subtitle = "Elecciones 2008-2024",
       x = NULL, y = NULL, fill = NULL,
       caption = "Fuente: elaboración propia con datos del Servel.") +
  tema
ggsave("output/grafico3_exito_sexo_incumbencia.png", g3,
       width = 8, height = 5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# Gráfico 4. Distribución del % de votos según sexo (H2)
# Solo candidaturas no incumbentes, para comparar en condiciones similares
# -------------------------------------------------------------
g4 <- base %>%
  filter(anio > 2004, incumbente == 0) %>%
  ggplot(aes(sexo, pct_votos, fill = sexo)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = colores_sexo, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Porcentaje de votos obtenido por candidaturas no incumbentes",
       subtitle = "Elecciones 2008-2024",
       x = NULL, y = "% de votos válidos en la comuna",
       caption = "Fuente: elaboración propia con datos del Servel.") +
  tema
ggsave("output/grafico4_votos_sexo_no_incumbentes.png", g4,
       width = 7, height = 5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# Tabla 4 y Gráfico 5. Reelección de incumbentes según sexo (H3)
# -------------------------------------------------------------
tabla4 <- base %>%
  filter(incumbente == 1) %>%
  group_by(Año = anio, Sexo = sexo) %>%
  summarise(`Buscaron reelección` = n(),
            `Reelectos/as` = sum(gano),
            `% reelección` = round(100 * mean(gano), 1),
            .groups = "drop")
print(tabla4)
write_csv(tabla4, "output/tabla4_reeleccion_sexo.csv")

g5 <- ggplot(tabla4, aes(Año, `% reelección`, color = Sexo)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = `Buscaron reelección`)) +
  scale_color_manual(values = colores_sexo) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  scale_x_continuous(breaks = c(2008, 2012, 2016, 2021, 2024)) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "Porcentaje de alcaldes y alcaldesas reelectos",
       subtitle = "Entre quienes buscaron la reelección. El tamaño del punto indica cuántos compitieron",
       x = NULL, y = NULL, color = NULL,
       caption = "Fuente: elaboración propia con datos del Servel.") +
  tema
ggsave("output/grafico5_reeleccion_sexo.png", g5,
       width = 8, height = 5, dpi = 300, bg = "white")

# -------------------------------------------------------------
# Tabla 5. Estadísticos descriptivos de las variables del análisis
# -------------------------------------------------------------
tabla5 <- tibble(
  Variable = c("Mujer (1 = sí)", "Ganó (1 = sí)", "% de votos",
               "Incumbente (1 = sí)", "Períodos previos en el cargo",
               "Fuera de pacto (1 = sí)", "N° de candidaturas en la comuna"),
  N = c(sum(!is.na(base$mujer)), sum(!is.na(base$gano)), sum(!is.na(base$pct_votos)),
        sum(!is.na(base$incumbente)), sum(!is.na(base$periodos_previos)),
        nrow(base), sum(!is.na(base$n_candidatos))),
  Media = c(mean(base$mujer), mean(base$gano), mean(base$pct_votos),
            mean(base$incumbente, na.rm = TRUE), mean(base$periodos_previos, na.rm = TRUE),
            mean(base$pacto == "Fuera de pacto"), mean(base$n_candidatos)),
  `Desv. estándar` = c(sd(base$mujer), sd(base$gano), sd(base$pct_votos),
                       sd(base$incumbente, na.rm = TRUE), sd(base$periodos_previos, na.rm = TRUE),
                       sd(base$pacto == "Fuera de pacto"), sd(base$n_candidatos)),
  Mínimo = c(0, 0, min(base$pct_votos), 0, 0, 0, min(base$n_candidatos)),
  Máximo = c(1, 1, max(base$pct_votos), 1, max(base$periodos_previos, na.rm = TRUE),
             1, max(base$n_candidatos))
) %>%
  mutate(across(c(Media, `Desv. estándar`, Mínimo, Máximo), ~ round(.x, 2)))
print(tabla5)
write_csv(tabla5, "output/tabla5_estadisticos_descriptivos.csv")

message("Listo: tablas y gráficos guardados en la carpeta output/")
