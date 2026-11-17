# =============================================================
# 04_modelos.R
# Estimación de los modelos para cada hipótesis:
#   H1 (oferta):    logit, ¿qué explica que una candidatura sea de una mujer?
#   H2 (demanda):   regresión lineal (% de votos) y logit (ganó o no)
#   H3 (reelección): logit de reelección entre incumbentes y
#                    logit ordenado del número de reelecciones
# Errores estándar agrupados por comuna, porque las candidaturas de
# una misma comuna comparten condiciones y no son independientes.
# Resultados: tablas (.csv) y gráficos (.png) en output/
# =============================================================

paquetes <- c("dplyr", "readr", "ggplot2", "sandwich", "lmtest", "MASS",
              "marginaleffects")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(readr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(marginaleffects)

base <- read_csv("data/clean/base_alcaldes.csv", show_col_types = FALSE) %>%
  mutate(
    anio   = factor(anio),
    # Categorías de referencia: hombre, izquierda
    sector = relevel(factor(sector), ref = "Izquierda")
  )
dir.create("output", showWarnings = FALSE)

# -------------------------------------------------------------
# Función para resumir un modelo en una tabla: coeficientes,
# errores agrupados por comuna, valor p e intervalo de confianza.
# En los logit se agregan las razones de odds (exp del coeficiente)
# -------------------------------------------------------------
resumir <- function(modelo, datos, nombre, logit = FALSE) {
  vc <- vcovCL(modelo, cluster = datos$comuna)
  ct <- coeftest(modelo, vcov. = vc)
  tabla <- data.frame(
    modelo      = nombre,
    variable    = rownames(ct),
    coeficiente = ct[, 1],
    error_est   = ct[, 2],
    valor_p     = ct[, 4],
    ic_inf      = ct[, 1] - 1.96 * ct[, 2],
    ic_sup      = ct[, 1] + 1.96 * ct[, 2],
    row.names = NULL
  )
  if (logit) {
    tabla$odds_ratio <- exp(tabla$coeficiente)
    tabla$or_ic_inf  <- exp(tabla$ic_inf)
    tabla$or_ic_sup  <- exp(tabla$ic_sup)
  }
  tabla$signif <- cut(tabla$valor_p, c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      c("***", "**", "*", ".", ""))
  tabla$n <- nobs(modelo)
  tabla
}

# =============================================================
# H1. OFERTA: ¿en qué condiciones se presentan más mujeres?
# Variable dependiente: la candidatura es de una mujer (1/0)
# Muestra: todas las candidaturas 2004-2024
# Nota: el sector "Independiente" corresponde a las candidaturas
# fuera de pacto, por lo que captura también la dimensión de pacto
# =============================================================
m_h1 <- glm(mujer ~ sector + anio, data = base, family = binomial)
t_h1 <- resumir(m_h1, base, "H1: logit candidata mujer", logit = TRUE)

# =============================================================
# H2. DEMANDA: ¿se vota menos por las mujeres?
# Muestra: 2008-2024 (en 2004 no se sabe quién era incumbente)
# Controles: incumbencia, sector, año y n° de candidaturas en la comuna
# =============================================================
base_h2 <- base %>% filter(!is.na(incumbente)) %>% droplevels()

# H2a. Regresión lineal: % de votos obtenidos
m_h2a <- lm(pct_votos ~ mujer + incumbente + sector + anio + n_candidatos,
            data = base_h2)
t_h2a <- resumir(m_h2a, base_h2, "H2a: MCO % de votos")

# H2b. Logit: ganó la elección (1/0)
m_h2b <- glm(gano ~ mujer + incumbente + sector + anio + n_candidatos,
             data = base_h2, family = binomial)
t_h2b <- resumir(m_h2b, base_h2, "H2b: logit ganó", logit = TRUE)

# H2c. Igual que H2b, pero con interacción mujer x incumbente,
# para ver si la brecha es distinta entre incumbentes y desafiantes
m_h2c <- glm(gano ~ mujer * incumbente + sector + anio + n_candidatos,
             data = base_h2, family = binomial)
t_h2c <- resumir(m_h2c, base_h2, "H2c: logit ganó (interacción)", logit = TRUE)

# =============================================================
# H3. REELECCIÓN
# H3a. Logit: entre quienes buscaron la reelección, ¿ganaron? (1/0)
# =============================================================
base_h3 <- base_h2 %>% filter(incumbente == 1) %>% droplevels()
m_h3a <- glm(gano ~ mujer + periodos_previos + sector + anio + n_candidatos,
             data = base_h3, family = binomial)
t_h3a <- resumir(m_h3a, base_h3, "H3a: logit reelección", logit = TRUE)

# H3b. Logit ordenado: entre quienes ganaron la alcaldía, cuántas
# veces consecutivas llevaban reelectos (0 = primer período,
# 1 = primera reelección, 2 = segunda, 3 = tercera)
# Se cuentan solo los períodos desde 2004 en adelante
base_h3b <- base_h2 %>%
  filter(gano == 1) %>%
  mutate(n_periodo = periodos_previos,
         n_reelecciones = factor(pmin(n_periodo, 3), levels = 0:3, ordered = TRUE)) %>%
  droplevels()
m_h3b <- MASS::polr(n_reelecciones ~ mujer + sector + anio,
                    data = base_h3b, Hess = TRUE)
t_h3b <- resumir(m_h3b, base_h3b, "H3b: logit ordenado n° reelecciones", logit = TRUE)

# -------------------------------------------------------------
# Guardar todas las tablas de coeficientes
# -------------------------------------------------------------
tablas <- bind_rows(t_h1, t_h2a, t_h2b, t_h2c, t_h3a, t_h3b) %>%
  mutate(across(where(is.numeric) & !n, ~ round(.x, 3)))
write_csv(tablas, "output/tabla6_resultados_modelos.csv")

# Mostrar en consola los coeficientes de la variable mujer
print(tablas %>% filter(grepl("mujer", variable)) %>%
        select(modelo, variable, coeficiente, valor_p, odds_ratio, signif, n))

# =============================================================
# PROBABILIDADES PREDICHAS (para interpretar sustantivamente)
# =============================================================

# H2: probabilidad de ganar de hombres y mujeres, según incumbencia,
# manteniendo el resto de las variables en sus valores observados
pred_h2 <- avg_predictions(m_h2c, by = c("mujer", "incumbente"),
                           vcov = ~comuna) %>%
  as.data.frame() %>%
  mutate(Sexo = ifelse(mujer == 1, "Mujer", "Hombre"),
         Condición = ifelse(incumbente == 1, "Incumbente", "No incumbente"))
write_csv(pred_h2 %>% select(Condición, Sexo, estimate, conf.low, conf.high),
          "output/tabla7_prob_predicha_ganar.csv")
print(pred_h2 %>% select(Condición, Sexo, estimate, conf.low, conf.high))

g6 <- ggplot(pred_h2, aes(Condición, estimate, color = Sexo)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(0.4), size = 0.6) +
  scale_color_manual(values = c("Mujer" = "#B5338A", "Hombre" = "#3A6EA5")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  labs(title = "Probabilidad predicha de ganar la alcaldía",
       subtitle = "Modelo logit H2c, con intervalos de confianza al 95%",
       x = NULL, y = NULL, color = NULL,
       caption = "Controles: sector, año y n° de candidaturas. Errores agrupados por comuna.") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("output/grafico6_prob_predicha_ganar.png", g6,
       width = 7, height = 5, dpi = 300, bg = "white")

# Efecto marginal promedio de ser mujer (en puntos porcentuales)
ame_h2b <- avg_slopes(m_h2b, variables = "mujer", vcov = ~comuna)
ame_h3a <- avg_slopes(m_h3a, variables = "mujer", vcov = ~comuna)
efectos <- bind_rows(
  as.data.frame(ame_h2b) %>% mutate(modelo = "H2b: ganar (todas)"),
  as.data.frame(ame_h3a) %>% mutate(modelo = "H3a: reelección (incumbentes)")
) %>% select(modelo, estimate, std.error, p.value, conf.low, conf.high)
write_csv(efectos, "output/tabla8_efectos_marginales_mujer.csv")
print(efectos)

# H3b: probabilidad predicha de cada número de reelecciones, por sexo
pred_h3b <- avg_predictions(m_h3b, by = "mujer") %>%
  as.data.frame() %>%
  mutate(Sexo = ifelse(mujer == 1, "Mujer", "Hombre"))
write_csv(pred_h3b %>% select(Sexo, group, estimate),
          "output/tabla9_prob_predicha_reelecciones.csv")
print(pred_h3b %>% select(Sexo, group, estimate))

message("Listo: resultados de los modelos guardados en output/")
