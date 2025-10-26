# ============================================================
# CASO HARVARD FINAL - ANALÍTICA DE LOS NEGOCIOS
# Análisis de Daily Visits - Web Analytics (Quality Alloys)
# Autores: Ángela Lucía Vargas, Santiago Muñoz, Josymar Nocua
# Fecha: 25 de Octubre de 2025
# ============================================================

# --- CONFIGURACIÓN INICIAL ---------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(12345)

cat("\n========================================\n")
cat("INSTALANDO Y CARGANDO PAQUETES...\n")
cat("========================================\n\n")

# Paquetes necesarios
pkgs <- c("tidyverse", "readxl", "broom", "modelsummary", "pROC", 
          "caret", "janitor", "knitr", "patchwork", "stargazer")
for(p in pkgs) if(!requireNamespace(p, quietly = TRUE)) install.packages(p)

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

# Crear carpeta de salida
dir_salida <- "resultados_churn"
if (!dir.exists(dir_salida)) dir.create(dir_salida)

cat("========================================\n")
cat("ANÁLISIS DE CUSTOMER CHURN - QWE INC.\n")
cat("========================================\n\n")

# --- CARGAR DATOS ------------------------------------------------------------
cat("1. Cargando datos desde Excel...\n")

archivo_datos <- "C:/Users/angel/OneDrive - Pontificia Universidad Javeriana/Github- Analitica de los Negocios/Caso Harvard Final - Predicting Costumer/DATA.xlsx"

if (!file.exists(archivo_datos)) {
  cat("⚠ ADVERTENCIA: No se encontró el archivo\n")
  archivo_datos <- file.choose()
}

datos <- read_excel(archivo_datos, sheet = "Case Data") %>%
  clean_names()

cat("   ✓ Datos cargados:", nrow(datos), "observaciones,", ncol(datos), "variables\n\n")

# --- PREPARACIÓN DE DATOS ----------------------------------------------------
cat("2. Preparando datos...\n")

nombre_churn <- names(datos)[grepl("churn", names(datos), ignore.case = TRUE)]
if (length(nombre_churn) == 0) stop("ERROR: No se encontró la variable 'churn'")
if (nombre_churn[1] != "churn") datos <- datos %>% rename(churn = !!sym(nombre_churn[1]))

datos <- datos %>%
  mutate(churn = case_when(
    is.na(churn) ~ NA_real_,
    churn == 0 | churn == "-" ~ 0,
    TRUE ~ 1
  )) %>%
  filter(!is.na(churn))

# Tabla de distribución
tabla_churn <- data.frame(
  Churn = c("No (0)", "Sí (1)", "TOTAL"),
  Frecuencia = c(sum(datos$churn == 0), sum(datos$churn == 1), nrow(datos)),
  Porcentaje = c(
    paste0(round(100 * sum(datos$churn == 0) / nrow(datos), 1), "%"),
    paste0(round(100 * sum(datos$churn == 1) / nrow(datos), 1), "%"),
    "100.0%"
  )
)

cat("\n")
print(kable(tabla_churn, format = "simple", align = c("l", "r", "r")))
cat("\n")

# GRÁFICO 0: Distribución de Churn (ggplot2)
g0 <- datos %>%
  count(churn) %>%
  mutate(
    churn_label = ifelse(churn == 0, "No (0)", "Sí (1)"),
    pct = paste0(round(100 * n / sum(n), 1), "%")
  ) %>%
  ggplot(aes(x = churn_label, y = n, fill = churn_label)) +
  geom_col() +
  geom_text(aes(label = pct), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#06D6A0", "#EF476F")) +
  labs(title = "Gráfico 0: Distribución de la Variable Churn",
       x = "Churn", y = "Frecuencia") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(dir_salida, "grafico_0_distribucion_churn.png"), g0, width = 8, height = 6)
print(g0)
cat("   ✓ Gráfico guardado\n\n")

# --- TABLA DESCRIPTIVA -------------------------------------------------------
cat("3. Generando tabla descriptiva...\n\n")

columnas_numericas <- sapply(datos, is.numeric)
vars_numericas <- names(datos)[columnas_numericas]
vars_numericas <- vars_numericas[!vars_numericas %in% c("churn", "id")]

tabla_desc_general <- datos %>%
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

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 1: ESTADÍSTICAS DESCRIPTIVAS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(tabla_desc_general, format = "simple", align = "lrrrrrrr"))
cat("\n")

write_csv(tabla_desc_general, file.path(dir_salida, "tabla_1_descriptivos_generales.csv"))
cat("   ✓ Tabla guardada\n\n")

# --- PARTICIÓN TRAIN/TEST ----------------------------------------------------
cat("4. Dividiendo datos (70% train, 30% test)...\n")

set.seed(12345)
indices_train <- createDataPartition(datos$churn, p = 0.70, list = FALSE)
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

particion_tabla <- data.frame(
  Conjunto = c("Train", "Test", "Total"),
  Observaciones = c(nrow(datos_train), nrow(datos_test), nrow(datos)),
  Porcentaje = c("70%", "30%", "100%")
)

cat("\n")
print(kable(particion_tabla, format = "simple", align = c("l", "r", "r")))
cat("\n")

# --- ESTIMAR MODELOS ---------------------------------------------------------
cat("5. Estimando modelos de probabilidad...\n")

formula_modelo <- as.formula(paste("churn ~", paste(vars_numericas, collapse = " + ")))

modelo_logit <- glm(formula_modelo, data = datos_train, family = binomial(link = "logit"))
cat("   ✓ Modelo Logit estimado\n")

modelo_probit <- glm(formula_modelo, data = datos_train, family = binomial(link = "probit"))
cat("   ✓ Modelo Probit estimado\n\n")

# --- TABLA DE REGRESIÓN ------------------------------------------------------
cat("6. Tabla de regresión...\n\n")

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 2: MODELOS DE PREDICCIÓN DE CUSTOMER CHURN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

stargazer(modelo_logit, modelo_probit,
          type = "text",
          dep.var.labels = "Churn (1 = Sí, 0 = No)",
          column.labels = c("Logit", "Probit"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = file.path(dir_salida, "tabla_2_regresion.txt"))

modelsummary(
  list("Logit" = modelo_logit, "Probit" = modelo_probit),
  output = file.path(dir_salida, "tabla_2_regresion.html"),
  stars = c('*' = .05, '**' = .01, '***' = .001)
)

cat("\n   ✓ Tabla exportada\n\n")

# GRÁFICO 1: Coeficientes del modelo
cat("📊 Generando gráfico de coeficientes de regresión...\n")

g1 <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(significativo = ifelse(p.value < 0.05, "Sí", "No")) %>%
  ggplot(aes(x = estimate, y = reorder(term, estimate), color = significativo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  scale_color_manual(values = c("No" = "#2E86AB", "Sí" = "#EF476F")) +
  labs(title = "Gráfico 1: Coeficientes del Modelo Logit\ncon Intervalos de Confianza 95%",
       x = "Coeficiente (Log-Odds)", y = "", color = "Significativo (p < 0.05)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")

ggsave(file.path(dir_salida, "grafico_1_coeficientes_regresion.png"), g1, width = 10, height = 8)
print(g1)
cat("   ✓ Gráfico de coeficientes guardado\n\n")

# --- MÉTRICAS DE BONDAD DE AJUSTE --------------------------------------------
cat("7. Métricas de bondad de ajuste...\n\n")

ll_completo <- as.numeric(logLik(modelo_logit))
ll_nulo <- as.numeric(logLik(update(modelo_logit, . ~ 1)))
pseudo_r2 <- 1 - (ll_completo / ll_nulo)

metricas_modelo <- data.frame(
  Métrica = c("Log-Likelihood Completo", "Log-Likelihood Nulo", 
              "Pseudo R² (McFadden)", "AIC", "BIC", "Observaciones"),
  Valor = round(c(ll_completo, ll_nulo, pseudo_r2, 
                  AIC(modelo_logit), BIC(modelo_logit), nobs(modelo_logit)), 4)
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 3: MÉTRICAS DEL MODELO LOGIT\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(metricas_modelo, format = "simple", align = c("l", "r")))
cat("\n")

write_csv(metricas_modelo, file.path(dir_salida, "tabla_3_metricas_modelo.csv"))

# --- INTERPRETACIÓN DE COEFICIENTES ------------------------------------------
cat("8. Interpretación de coeficientes...\n\n")

coefs <- tidy(modelo_logit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(estimate),
    cambio_porcentual = (odds_ratio - 1) * 100,
    significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(p.value)

top_coefs <- head(coefs, 2)

interpretaciones <- data.frame(
  Variable = top_coefs$term,
  Coeficiente = round(top_coefs$estimate, 4),
  Odds_Ratio = round(top_coefs$odds_ratio, 4),
  `Cambio_%` = paste0(round(top_coefs$cambio_porcentual, 1), "%"),
  P_valor = format.pval(top_coefs$p.value, digits = 3),
  Significancia = top_coefs$significancia,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 4: INTERPRETACIÓN DE LOS 2 COEFICIENTES MÁS SIGNIFICATIVOS\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(interpretaciones, format = "simple", align = "lrrrrr"))
cat("\n")

write_csv(interpretaciones, file.path(dir_salida, "tabla_4_interpretacion_coeficientes.csv"))
write_csv(coefs, file.path(dir_salida, "tabla_5_todos_coeficientes.csv"))

# --- PREDICCIONES EN TEST ----------------------------------------------------
cat("9. Generando predicciones...\n")

datos_test$prob_churn <- predict(modelo_logit, newdata = datos_test, type = "response")
datos_test$pred_churn <- ifelse(datos_test$prob_churn >= 0.5, 1, 0)

cat("   ✓ Predicciones completadas\n\n")

# --- GRÁFICO NUEVO: REGRESIÓN LINEAL (Probabilidad predicha vs Churn) -------
cat("📊 Generando gráfico de regresión lineal...\n")

# Para visualización de regresión, usamos scatter + smooth
g_regresion <- datos_test %>%
  ggplot(aes(x = prob_churn, y = churn)) +
  geom_jitter(aes(color = factor(churn)), alpha = 0.5, height = 0.05, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E86AB", linewidth = 1.5) +
  scale_color_manual(values = c("0" = "#06D6A0", "1" = "#EF476F"),
                     labels = c("No Churn (0)", "Churn (1)")) +
  labs(title = "Gráfico Nuevo: Regresión Lineal\nProbabilidad Predicha vs Churn Observado",
       x = "Probabilidad Predicha de Churn",
       y = "Churn Observado (0 = No, 1 = Sí)",
       color = "Clase Real") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")

ggsave(file.path(dir_salida, "grafico_regresion_lineal.png"), g_regresion, width = 10, height = 6)
print(g_regresion)
cat("   ✓ Gráfico de regresión lineal guardado\n\n")

# --- TOP 100 CLIENTES EN RIESGO ---------------------------------------------
cat("10. Identificando top 100 clientes en riesgo...\n\n")

if (!"id" %in% names(datos_test)) datos_test$id <- seq_len(nrow(datos_test))

datos_test_ordenado <- datos_test %>%
  arrange(desc(prob_churn)) %>%
  mutate(ranking = row_number())

columnas_disponibles <- names(datos_test_ordenado)
columnas_deseadas <- c("ranking", "id", "prob_churn", "churn")
vars_adicionales <- intersect(vars_numericas, columnas_disponibles)[1:3]
vars_adicionales <- vars_adicionales[!is.na(vars_adicionales)]
columnas_finales <- c(columnas_deseadas, vars_adicionales)
columnas_finales <- columnas_finales[columnas_finales %in% columnas_disponibles]

top_100_riesgo <- datos_test_ordenado[1:min(100, nrow(datos_test_ordenado)), columnas_finales]

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 6: TOP 10 CLIENTES CON MAYOR PROBABILIDAD DE CHURN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

print(kable(head(top_100_riesgo, 10), format = "simple", digits = 4, align = "r"))
cat("\n(Lista completa guardada en CSV)\n\n")

write_csv(top_100_riesgo, file.path(dir_salida, "tabla_6_top_100_clientes_riesgo.csv"))

# --- MATRIZ DE CONFUSIÓN -----------------------------------------------------
cat("11. Evaluación del modelo...\n\n")

conf_matrix <- confusionMatrix(
  factor(datos_test$pred_churn, levels = c(0, 1)),
  factor(datos_test$churn, levels = c(0, 1)),
  positive = "1"
)

metricas_eval <- data.frame(
  Métrica = c("Exactitud", "Sensibilidad", "Especificidad", 
              "Precisión", "F1-Score", "Kappa"),
  Valor = round(c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["F1"],
    conf_matrix$overall["Kappa"]
  ), 4)
)

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 7: MÉTRICAS DE EVALUACIÓN\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(metricas_eval, format = "simple", align = c("l", "r")))
cat("\n")

write_csv(metricas_eval, file.path(dir_salida, "tabla_7_metricas_evaluacion.csv"))

# --- CURVA ROC ---------------------------------------------------------------
cat("12. Generando curva ROC...\n")

roc_obj <- roc(datos_test$churn, datos_test$prob_churn, quiet = TRUE)
auc_valor <- auc(roc_obj)

# Extraer coordenadas para ggplot
roc_data <- data.frame(
  specificities = roc_obj$specificities,
  sensitivities = roc_obj$sensitivities
)

g2 <- ggplot(roc_data, aes(x = 1 - specificities, y = sensitivities)) +
  geom_line(color = "#2E86AB", linewidth = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
  annotate("text", x = 0.7, y = 0.3, 
           label = paste("AUC =", round(auc_valor, 3)), 
           size = 6, fontface = "bold", color = "#2E86AB") +
  labs(title = paste0("Gráfico 2: Curva ROC\nAUC = ", round(auc_valor, 3)),
       x = "1 - Especificidad (Tasa de Falsos Positivos)",
       y = "Sensibilidad (Tasa de Verdaderos Positivos)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(dir_salida, "grafico_2_curva_roc.png"), g2, width = 8, height = 6)
print(g2)
cat("   ✓ ROC guardada (AUC =", round(auc_valor, 3), ")\n\n")

# --- GRÁFICOS DE ERRORES (COMBINADOS CON PATCHWORK) -------------------------
cat("13. Generando gráficos de errores...\n")

datos_test$error <- datos_test$churn - datos_test$prob_churn

g3a <- ggplot(datos_test, aes(x = prob_churn, y = error)) +
  geom_point(color = rgb(0.2, 0.4, 0.8, 0.4), size = 2) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1.5, linetype = "dashed") +
  labs(title = "Gráfico 3A: Errores de Predicción",
       x = "Probabilidad Predicha",
       y = "Error (Real - Predicho)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

g3b <- ggplot(datos_test, aes(x = error)) +
  geom_histogram(fill = "#06D6A0", color = "white", bins = 30) +
  geom_vline(xintercept = 0, color = "red", linewidth = 1.5, linetype = "dashed") +
  labs(title = "Gráfico 3B: Distribución de Errores",
       x = "Error",
       y = "Frecuencia") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

g3 <- g3a + g3b + plot_annotation(
  title = "Gráfico 3: Análisis de Errores del Modelo",
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
)

ggsave(file.path(dir_salida, "grafico_3_errores.png"), g3, width = 12, height = 6)
print(g3)
cat("   ✓ Gráfico de errores guardado\n\n")

# --- GRÁFICO PREDICHOS VS REALES ---------------------------------------------
cat("14. Generando gráfico predichos vs reales...\n")

g4 <- ggplot(datos_test, aes(x = prob_churn, fill = factor(churn))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
  scale_fill_manual(
    values = c("0" = "#06D6A0", "1" = "#EF476F"),
    labels = c("No Churn (0)", "Churn (1)")
  ) +
  labs(
    title = "Gráfico 4: Distribución de Probabilidades Predichas",
    subtitle = paste0("Modelo Logit - N = ", nrow(datos_test)),
    x = "Probabilidad Predicha de Churn",
    y = "Frecuencia",
    fill = "Clase Real"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top"
  )

ggsave(file.path(dir_salida, "grafico_4_predichos_vs_reales.png"), g4, width = 10, height = 6)
print(g4)
cat("   ✓ Gráfico guardado\n\n")

# --- CALIBRACIÓN -------------------------------------------------------------
cat("15. Análisis de calibración...\n\n")

calibracion <- datos_test %>%
  mutate(decil = ntile(prob_churn, 10)) %>%
  group_by(decil) %>%
  summarise(
    n = n(),
    prob_media = round(mean(prob_churn), 4),
    tasa_observada = round(mean(churn), 4),
    .groups = "drop"
  )

cat("═══════════════════════════════════════════════════════════════════\n")
cat("TABLA 8: CALIBRACIÓN POR DECILES\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")
print(kable(calibracion, format = "simple", align = "rrrr",
            col.names = c("Decil", "N", "Prob. Predicha", "Tasa Observada")))
cat("\n")

write_csv(calibracion, file.path(dir_salida, "tabla_8_calibracion.csv"))

g5 <- ggplot(calibracion, aes(x = prob_media, y = tasa_observada)) +
  geom_point(color = "#2E86AB", size = 4) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5, linetype = "dashed") +
  labs(title = "Gráfico 5: Curva de Calibración",
       x = "Probabilidad Predicha (promedio por decil)",
       y = "Tasa de Churn Observada") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(file.path(dir_salida, "grafico_5_calibracion.png"), g5, width = 8, height = 6)
print(g5)
cat("   ✓ Calibración completada\n\n")

# --- GUARDAR MODELOS ---------------------------------------------------------
cat("16. Guardando modelos...\n")

saveRDS(modelo_logit, file.path(dir_salida, "modelo_logit.rds"))
saveRDS(modelo_probit, file.path(dir_salida, "modelo_probit.rds"))

cat("   ✓ Modelos guardados\n\n")

# --- RESUMEN FINAL -----------------------------------------------------------

resumen_final <- data.frame(
  Métrica = c("Pseudo R² (McFadden)", "AUC", "Exactitud", 
              "Sensibilidad", "F1-Score", "Observaciones Test"),
  Valor = c(
    round(pseudo_r2, 4),
    round(auc_valor, 4),
    round(conf_matrix$overall["Accuracy"], 4),
    round(conf_matrix$byClass["Sensitivity"], 4),
    round(conf_matrix$byClass["F1"], 4),
    nrow(datos_test)
  )
)

print(kable(resumen_final, format = "simple", align = c("l", "r")))
