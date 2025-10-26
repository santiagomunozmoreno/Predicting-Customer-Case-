# ============================================================
# CASO HARVARD FINAL - ANALÍTICA DE LOS NEGOCIOS
# Predicción de Customer Churn utilizando Modelos Logit y Probit
# Autores: Ángela Lucía Vargas, Santiago Muñoz, Josymar Nocua
# Fecha: 25 de Octubre de 2025
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(12345)

# Cargar librerías necesarias
librerias <- c("tidyverse", "readxl", "broom", "modelsummary", "pROC", 
               "caret", "janitor", "knitr", "patchwork", "stargazer")

for(lib in librerias) {
  if(!requireNamespace(lib, quietly = TRUE)) install.packages(lib)
}

library(tidyverse)
library(readxl)
library(broom)
library(modelsummary)
library(pROC)
library(caret)
library(janitor)
library(knitr)
library(patchwork)
library(stargazer)

# Crear directorio para guardar resultados
directorio_salida <- "resultados_churn"
if (!dir.exists(directorio_salida)) dir.create(directorio_salida)

cat("Iniciando análisis de Customer Churn\n\n")

# ------------------------------------------------------------------------------
# 1. Cargar y preparar datos
# ------------------------------------------------------------------------------

ruta_archivo <- "C:/Users/angel/OneDrive - Pontificia Universidad Javeriana/Github- Analitica de los Negocios/Caso Harvard Final - Predicting Costumer/DATA.xlsx"

if (!file.exists(ruta_archivo)) {
  cat("Archivo no encontrado. Por favor selecciona el archivo:\n")
  ruta_archivo <- file.choose()
}

datos <- read_excel(ruta_archivo, sheet = "Case Data") %>%
  clean_names()

cat("Datos cargados:", nrow(datos), "observaciones,", ncol(datos), "variables\n\n")
# Identificar y limpiar variable churn
col_churn <- names(datos)[grepl("churn", names(datos), ignore.case = TRUE)]
if (length(col_churn) == 0) stop("No se encontró la variable 'churn'")
if (col_churn[1] != "churn") datos <- rename(datos, churn = !!sym(col_churn[1]))

datos <- datos %>%
  mutate(churn = case_when(
    is.na(churn) ~ NA_real_,
    churn == 0 | churn == "-" ~ 0,
    TRUE ~ 1
  )) %>%
  filter(!is.na(churn))

# Distribución de churn
tabla_churn <- data.frame(
  Churn = c("No (0)", "Sí (1)", "TOTAL"),
  Frecuencia = c(sum(datos$churn == 0), sum(datos$churn == 1), nrow(datos)),
  Porcentaje = c(
    paste0(round(100 * sum(datos$churn == 0) / nrow(datos), 1), "%"),
    paste0(round(100 * sum(datos$churn == 1) / nrow(datos), 1), "%"),
    "100.0%"
  )
)

print(kable(tabla_churn, format = "simple", align = c("l", "r", "r")))

# Gráfico de distribución
grafico_dist <- datos %>%
  count(churn) %>%
  mutate(
    etiqueta = ifelse(churn == 0, "No (0)", "Sí (1)"),
    porcentaje = paste0(round(100 * n / sum(n), 1), "%")
  ) %>%
  ggplot(aes(x = etiqueta, y = n, fill = etiqueta)) +
  geom_col() +
  geom_text(aes(label = porcentaje), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#06D6A0", "#EF476F")) +
  labs(title = "Distribución de Churn", x = "Churn", y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(directorio_salida, "grafico_distribucion_churn.png"), grafico_dist, width = 8, height = 6)
print(grafico_dist)

# ------------------------------------------------------------------------------
# 2. Estadísticas descriptivas
# ------------------------------------------------------------------------------

vars_numericas <- names(datos)[sapply(datos, is.numeric)]
vars_numericas <- vars_numericas[!vars_numericas %in% c("churn", "id")]

tabla_descriptiva <- datos %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  group_by(Variable) %>%
  summarise(
    N = sum(!is.na(value)),
    Media = round(mean(value, na.rm = TRUE), 2),
    Desv_Std = round(sd(value, na.rm = TRUE), 2),
    Mínimo = round(min(value, na.rm = TRUE), 2),
    Mediana = round(median(value, na.rm = TRUE), 2),
    Máximo = round(max(value, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- Estadísticas Descriptivas ---\n\n")
print(kable(tabla_descriptiva, format = "simple", align = "lrrrrrrr"))

write_csv(tabla_descriptiva, file.path(directorio_salida, "estadisticas_descriptivas.csv"))

# ------------------------------------------------------------------------------
# 3. División train/test
# ------------------------------------------------------------------------------

set.seed(12345)
indices_train <- createDataPartition(datos$churn, p = 0.70, list = FALSE)
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

cat("\n--- División de datos ---\n")
cat("Train:", nrow(datos_train), "observaciones (70%)\n")
cat("Test:", nrow(datos_test), "observaciones (30%)\n\n")

# ------------------------------------------------------------------------------
# 4. Estimación de modelos
# ------------------------------------------------------------------------------

formula_modelo <- as.formula(paste("churn ~", paste(vars_numericas, collapse = " + ")))

modelo_logit <- glm(formula_modelo, data = datos_train, family = binomial(link = "logit"))
modelo_probit <- glm(formula_modelo, data = datos_train, family = binomial(link = "probit"))

cat("Modelos estimados: Logit y Probit\n\n")

# Tabla de regresión
stargazer(modelo_logit, modelo_probit,
          type = "text",
          dep.var.labels = "Churn (1 = Sí, 0 = No)",
          column.labels = c("Logit", "Probit"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = file.path(directorio_salida, "tabla_regresion.txt"))

modelsummary(
  list("Logit" = modelo_logit, "Probit" = modelo_probit),
  output = file.path(directorio_salida, "tabla_regresion.html"),
  stars = c('*' = .05, '**' = .01, '***' = .001)
)

# Gráfico de coeficientes
grafico_coef <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(significativo = ifelse(p.value < 0.05, "Sí", "No")) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = significativo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  scale_color_manual(values = c("No" = "#2E86AB", "Sí" = "#EF476F")) +
  labs(title = "Coeficientes del Modelo Logit (IC 95%)",
       x = "Coeficiente", y = "", color = "Significativo (p < 0.05)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "top")

ggsave(file.path(directorio_salida, "grafico_coeficientes.png"), grafico_coef, width = 10, height = 8)
print(grafico_coef)

# ------------------------------------------------------------------------------
# 5. Métricas del modelo
# ------------------------------------------------------------------------------

log_lik_completo <- as.numeric(logLik(modelo_logit))
log_lik_nulo <- as.numeric(logLik(update(modelo_logit, . ~ 1)))
pseudo_r2 <- 1 - (log_lik_completo / log_lik_nulo)

metricas <- data.frame(
  Métrica = c("Log-Likelihood Completo", "Log-Likelihood Nulo", 
              "Pseudo R² (McFadden)", "AIC", "BIC", "Observaciones"),
  Valor = round(c(log_lik_completo, log_lik_nulo, pseudo_r2, 
                  AIC(modelo_logit), BIC(modelo_logit), nobs(modelo_logit)), 4)
)

cat("\n--- Métricas del Modelo ---\n\n")
print(kable(metricas, format = "simple", align = c("l", "r")))

write_csv(metricas, file.path(directorio_salida, "metricas_modelo.csv"))

# ------------------------------------------------------------------------------
# 6. Interpretación de coeficientes
# ------------------------------------------------------------------------------

coeficientes <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    cambio_pct = (odds_ratio - 1) * 100,
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(p.value)

top_coef <- head(coeficientes, 2)

interpretacion <- data.frame(
  Variable = top_coef$term,
  Coeficiente = round(top_coef$estimate, 4),
  Odds_Ratio = round(top_coef$odds_ratio, 4),
  Cambio_Porcentual = paste0(round(top_coef$cambio_pct, 1), "%"),
  P_valor = format.pval(top_coef$p.value, digits = 3),
  Sig = top_coef$sig
)

cat("\n--- Coeficientes Más Significativos ---\n\n")
print(kable(interpretacion, format = "simple", align = "lrrrrr"))

write_csv(interpretacion, file.path(directorio_salida, "interpretacion_coeficientes.csv"))
write_csv(coeficientes, file.path(directorio_salida, "todos_coeficientes.csv"))

# ------------------------------------------------------------------------------
# 7. Predicciones
# ------------------------------------------------------------------------------

datos_test$prob_churn <- predict(modelo_logit, newdata = datos_test, type = "response")
datos_test$pred_churn <- ifelse(datos_test$prob_churn >= 0.5, 1, 0)

cat("\nPredicciones generadas\n\n")

# Gráfico de regresión
grafico_regresion <- datos_test %>%
  ggplot(aes(x = prob_churn, y = churn)) +
  geom_jitter(aes(color = factor(churn)), alpha = 0.5, height = 0.05, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E86AB", linewidth = 1.5) +
  scale_color_manual(values = c("0" = "#06D6A0", "1" = "#EF476F"),
                     labels = c("No Churn", "Churn")) +
  labs(title = "Probabilidad Predicha vs Churn Observado",
       x = "Probabilidad Predicha", y = "Churn Observado",
       color = "Clase Real") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "top")

ggsave(file.path(directorio_salida, "grafico_regresion.png"), grafico_regresion, width = 10, height = 6)
print(grafico_regresion)

# ------------------------------------------------------------------------------
# 8. Top 100 clientes en riesgo
# ------------------------------------------------------------------------------

if (!"id" %in% names(datos_test)) datos_test$id <- seq_len(nrow(datos_test))

datos_ordenados <- datos_test %>%
  arrange(desc(prob_churn)) %>%
  mutate(ranking = row_number())

columnas_mostrar <- c("ranking", "id", "prob_churn", "churn")
vars_extra <- intersect(vars_numericas, names(datos_ordenados))[1:3]
vars_extra <- vars_extra[!is.na(vars_extra)]
columnas_finales <- c(columnas_mostrar, vars_extra)

top_100 <- datos_ordenados[1:min(100, nrow(datos_ordenados)), columnas_finales]

cat("\n--- Top 10 Clientes en Riesgo ---\n\n")
print(kable(head(top_100, 10), format = "simple", digits = 4, align = "r"))

write_csv(top_100, file.path(directorio_salida, "top_100_clientes_riesgo.csv"))

# ------------------------------------------------------------------------------
# 9. Evaluación del modelo
# ------------------------------------------------------------------------------

matriz_confusion <- confusionMatrix(
  factor(datos_test$pred_churn, levels = c(0, 1)),
  factor(datos_test$churn, levels = c(0, 1)),
  positive = "1"
)

metricas_eval <- data.frame(
  Métrica = c("Exactitud", "Sensibilidad", "Especificidad", 
              "Precisión", "F1-Score", "Kappa"),
  Valor = round(c(
    matriz_confusion$overall["Accuracy"],
    matriz_confusion$byClass["Sensitivity"],
    matriz_confusion$byClass["Specificity"],
    matriz_confusion$byClass["Pos Pred Value"],
    matriz_confusion$byClass["F1"],
    matriz_confusion$overall["Kappa"]
  ), 4)
)

cat("\n--- Métricas de Evaluación ---\n\n")
print(kable(metricas_eval, format = "simple", align = c("l", "r")))

write_csv(metricas_eval, file.path(directorio_salida, "metricas_evaluacion.csv"))
