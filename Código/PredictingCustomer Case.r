# ============================================================================
# PREDICCIÓN DE CUSTOMER CHURN - QWE INC.
# Caso Harvard Business School - UV6694
# ============================================================================

# --- CONFIGURACIÓN INICIAL ---------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
set.seed(12345)

cat("\n========================================\n")
cat("INSTALANDO Y CARGANDO PAQUETES...\n")
cat("========================================\n\n")

# Instalar y cargar paquetes necesarios
paquetes <- c("tidyverse", "readxl", "broom", "modelsummary", "stargazer",
              "margins", "pROC", "caret", "janitor", "gt", "MASS", "knitr")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Instalando paquete:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  }
  cat("Cargando paquete:", pkg, "\n")
  library(pkg, character.only = TRUE)
}

cat("\n✓ Todos los paquetes cargados exitosamente\n\n")

# Crear carpeta de salida
dir_salida <- "resultados_churn"
if (!dir.exists(dir_salida)) dir.create(dir_salida)

cat("========================================\n")
cat("ANÁLISIS DE CUSTOMER CHURN - QWE INC.\n")
cat("========================================\n\n")

# --- CARGAR DATOS DESDE EXCEL ------------------------------------------------
cat("1. Cargando datos desde Excel...\n")

# Ruta del archivo
archivo_datos <- "C:/Users/angel/OneDrive - Pontificia Universidad Javeriana/Github- Analitica de los Negocios/Caso Harvard Final -_Predicting Costumer/DATA.xlsx"

# Verificar que el archivo existe
if (!file.exists(archivo_datos)) {
  cat("⚠ ADVERTENCIA: No se encontró el archivo en la ruta especificada\n")
  cat("Por favor, selecciona el archivo manualmente:\n")
  archivo_datos <- file.choose()
}

# Leer la pestaña "Case Data"
cat("   - Leyendo pestaña 'Case Data'...\n")
datos <- read_excel(archivo_datos, sheet = "Case Data") %>%
  clean_names()

cat("   ✓ Datos cargados exitosamente\n")
cat("   - Número de observaciones:", nrow(datos), "\n")
cat("   - Número de variables:", ncol(datos), "\n")
cat("   - Columnas detectadas:\n")
print(names(datos))
cat("\n")

# --- PREPARACIÓN DE DATOS ----------------------------------------------------
cat("2. Preparando datos...\n")

# Buscar variable churn
nombre_churn <- names(datos)[grepl("churn", names(datos), ignore.case = TRUE)]

if (length(nombre_churn) == 0) {
  stop("ERROR: No se encontró la variable 'churn'")
}

# Renombrar si es necesario
if (nombre_churn[1] != "churn") {
  datos <- datos %>% rename(churn = !!sym(nombre_churn[1]))
}

# Convertir churn a 0/1
datos <- datos %>%
  mutate(churn = case_when(
    is.na(churn) ~ NA_real_,
    churn == 0 | churn == "-" ~ 0,
    TRUE ~ 1
  )) %>%
  filter(!is.na(churn))

# Distribución
tabla_churn <- table(datos$churn)
cat("   - Distribución de Churn:\n")
cat("     No (0):", tabla_churn["0"], "\n")
cat("     Sí (1):", tabla_churn["1"], "\n\n")

# --- TABLA DESCRIPTIVA - VERSIÓN CORREGIDA -----------------------------------
cat("3. Generando tabla descriptiva...\n")

# Método simple y directo - SIN ERRORES
# Paso 1: Identificar columnas numéricas
columnas_numericas <- sapply(datos, is.numeric)
nombres_numericos <- names(datos)[columnas_numericas]

# Paso 2: Excluir 'churn' e 'id'
vars_numericas <- nombres_numericos[!nombres_numericos %in% c("churn", "id")]

cat("   - Variables numéricas identificadas:", length(vars_numericas), "\n")

# Crear tabla descriptiva
desc_list <- list()

for (var in vars_numericas) {
  desc_list[[var]] <- data.frame(
    Variable = var,
    N = sum(!is.na(datos[[var]])),
    Media = mean(datos[[var]], na.rm = TRUE),
    Desv_Std = sd(datos[[var]], na.rm = TRUE),
    Minimo = min(datos[[var]], na.rm = TRUE),
    Mediana = median(datos[[var]], na.rm = TRUE),
    Maximo = max(datos[[var]], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

tabla_desc_general <- do.call(rbind, desc_list)
rownames(tabla_desc_general) <- NULL

# Guardar tabla
write_csv(tabla_desc_general, 
          file.path(dir_salida, "tabla_1_descriptivos_generales.csv"))

# Tabla por grupo de churn
desc_by_churn_list <- list()

for (var in vars_numericas) {
  temp <- datos %>%
    group_by(churn) %>%
    summarise(
      Variable = var,
      N = sum(!is.na(.data[[var]])),
      Media = mean(.data[[var]], na.rm = TRUE),
      Desv_Std = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
  desc_by_churn_list[[var]] <- temp
}

tabla_desc_churn <- do.call(rbind, desc_by_churn_list)

write_csv(tabla_desc_churn, 
          file.path(dir_salida, "tabla_2_descriptivos_por_churn.csv"))

cat("   ✓ Tablas descriptivas generadas\n\n")

# --- PARTICIÓN TRAIN/TEST ----------------------------------------------------
cat("4. Dividiendo datos (70% train, 30% test)...\n")

set.seed(12345)
indices_train <- createDataPartition(datos$churn, p = 0.70, list = FALSE)
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

cat("   - Train:", nrow(datos_train), "| Test:", nrow(datos_test), "\n\n")

# --- ESPECIFICAR VARIABLES PREDICTORAS ---------------------------------------
cat("5. Seleccionando variables predictoras...\n")

vars_predictoras <- vars_numericas

cat("   - Variables predictoras:", length(vars_predictoras), "\n")

# Crear fórmula
formula_modelo <- as.formula(paste("churn ~", paste(vars_predictoras, collapse = " + ")))

# --- ESTIMAR MODELOS ---------------------------------------------------------
cat("6. Estimando modelos...\n")

# MODELO LOGIT
modelo_logit <- glm(formula_modelo, 
                    data = datos_train, 
                    family = binomial(link = "logit"))

cat("   ✓ Modelo Logit estimado\n")

# MODELO PROBIT
modelo_probit <- glm(formula_modelo, 
                     data = datos_train, 
                     family = binomial(link = "probit"))

cat("   ✓ Modelo Probit estimado\n\n")

# --- TABLA DE REGRESIÓN ------------------------------------------------------
cat("7. Exportando tabla de regresión...\n")

# Stargazer (texto)
stargazer(modelo_logit, modelo_probit,
          type = "text",
          title = "Modelos de Predicción de Customer Churn",
          dep.var.labels = "Churn (1 = Sí, 0 = No)",
          column.labels = c("Logit", "Probit"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = file.path(dir_salida, "tabla_2_regresion.txt"))

# HTML profesional
modelsummary(
  list("Logit" = modelo_logit, "Probit" = modelo_probit),
  output = file.path(dir_salida, "tabla_2_regresion.html"),
  stars = c('*' = .05, '**' = .01, '***' = .001)
)

cat("   ✓ Tabla exportada\n\n")

# --- PSEUDO R-CUADRADO -------------------------------------------------------
cat("8. Calculando métricas...\n")

ll_completo <- as.numeric(logLik(modelo_logit))
ll_nulo <- as.numeric(logLik(update(modelo_logit, . ~ 1)))
pseudo_r2 <- 1 - (ll_completo / ll_nulo)

metricas_modelo <- data.frame(
  Metrica = c("Log-Likelihood Completo", "Log-Likelihood Nulo", 
              "Pseudo R² (McFadden)", "AIC", "BIC", "Observaciones"),
  Valor = c(ll_completo, ll_nulo, pseudo_r2, 
            AIC(modelo_logit), BIC(modelo_logit), nobs(modelo_logit))
)

write_csv(metricas_modelo, 
          file.path(dir_salida, "tabla_3_metricas_modelo.csv"))

cat("   - Pseudo R²:", round(pseudo_r2, 4), "\n")
cat("   - AIC:", round(AIC(modelo_logit), 2), "\n\n")

# --- INTERPRETACIÓN DE COEFICIENTES ------------------------------------------
cat("9. Generando interpretaciones...\n")

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

# Top 2 coeficientes
top_coefs <- head(coefs, 2)

interpretaciones <- data.frame(
  Variable = top_coefs$term,
  Coeficiente = round(top_coefs$estimate, 4),
  Odds_Ratio = round(top_coefs$odds_ratio, 4),
  Cambio_Porcentual = round(top_coefs$cambio_porcentual, 2),
  P_valor = format.pval(top_coefs$p.value, digits = 3),
  Interpretacion = paste0(
    "Por cada unidad adicional en '", top_coefs$term, 
    "', las probabilidades (odds) de churn cambian en ", 
    round(top_coefs$cambio_porcentual, 1), "%."
  ),
  stringsAsFactors = FALSE
)

write_csv(interpretaciones, 
          file.path(dir_salida, "tabla_4_interpretacion_coeficientes.csv"))

write_csv(coefs, file.path(dir_salida, "tabla_5_todos_coeficientes.csv"))

cat("   ✓ Interpretaciones generadas\n\n")

# --- PREDICCIONES EN TEST ----------------------------------------------------
cat("10. Generando predicciones...\n")

datos_test$prob_churn <- predict(modelo_logit, 
                                  newdata = datos_test, 
                                  type = "response")

datos_test$pred_churn <- ifelse(datos_test$prob_churn >= 0.5, 1, 0)

cat("   ✓ Predicciones completadas\n\n")

# --- TOP 100 CLIENTES EN RIESGO ---------------------------------------------
cat("11. Top 100 clientes en riesgo...\n")

# Agregar ID si no existe
if (!"id" %in% names(datos_test)) {
  datos_test$id <- seq_len(nrow(datos_test))
}

top_100_riesgo <- datos_test %>%
  arrange(desc(prob_churn)) %>%
  head(100) %>%
  select(id, prob_churn, churn, customer_age, chi_score_month_0, 
         support_cases_month_0) %>%
  mutate(ranking = row_number())

write_csv(top_100_riesgo, 
          file.path(dir_salida, "tabla_6_top_100_clientes_riesgo.csv"))

cat("   ✓ Top 100 exportado\n\n")

# --- MATRIZ DE CONFUSIÓN -----------------------------------------------------
cat("12. Evaluando modelo...\n")

conf_matrix <- confusionMatrix(
  factor(datos_test$pred_churn, levels = c(0, 1)),
  factor(datos_test$churn, levels = c(0, 1)),
  positive = "1"
)

metricas_eval <- data.frame(
  Metrica = c("Exactitud", "Sensibilidad", "Especificidad", 
              "Precisión", "F1-Score", "Kappa"),
  Valor = c(
    conf_matrix$overall["Accuracy"],
    conf_matrix$byClass["Sensitivity"],
    conf_matrix$byClass["Specificity"],
    conf_matrix$byClass["Pos Pred Value"],
    conf_matrix$byClass["F1"],
    conf_matrix$overall["Kappa"]
  )
)

write_csv(metricas_eval, 
          file.path(dir_salida, "tabla_7_metricas_evaluacion.csv"))

cat("   - Exactitud:", round(conf_matrix$overall["Accuracy"], 3), "\n")
cat("   - F1-Score:", round(conf_matrix$byClass["F1"], 3), "\n\n")

# --- CURVA ROC ---------------------------------------------------------------
cat("13. Generando curva ROC...\n")

roc_obj <- roc(datos_test$churn, datos_test$prob_churn, quiet = TRUE)
auc_valor <- auc(roc_obj)

png(file.path(dir_salida, "grafico_1_curva_roc.png"), 
    width = 800, height = 600)
plot(roc_obj, 
     main = paste0("Curva ROC - AUC = ", round(auc_valor, 3)),
     col = "#2E86AB", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

cat("   ✓ ROC generada (AUC =", round(auc_valor, 3), ")\n\n")

# --- GRÁFICO DE ERRORES ------------------------------------------------------
cat("14. Generando gráfico de errores...\n")

datos_test$error <- datos_test$churn - datos_test$prob_churn

png(file.path(dir_salida, "grafico_2_errores.png"), 
    width = 1000, height = 600)
par(mfrow = c(1, 2))

plot(datos_test$prob_churn, datos_test$error,
     main = "Errores de Predicción",
     xlab = "Probabilidad Predicha",
     ylab = "Error (Real - Predicho)",
     pch = 16, col = rgb(0, 0, 1, 0.3))
abline(h = 0, col = "red", lwd = 2, lty = 2)
grid()

hist(datos_test$error,
     main = "Distribución de Errores",
     xlab = "Error",
     col = "lightblue",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)

dev.off()

cat("   ✓ Gráfico de errores generado\n\n")

# --- GRÁFICO PREDICHOS VS REALES ---------------------------------------------
cat("15. Generando gráfico predichos vs reales...\n")

png(file.path(dir_salida, "grafico_3_predichos_vs_reales.png"), 
    width = 900, height = 600)

ggplot(datos_test, aes(x = prob_churn, fill = factor(churn))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(
    values = c("0" = "#06D6A0", "1" = "#EF476F"),
    labels = c("No Churn", "Churn")
  ) +
  labs(
    title = "Distribución de Probabilidades Predichas",
    x = "Probabilidad Predicha de Churn",
    y = "Frecuencia",
    fill = "Churn Real"
  ) +
  theme_minimal()

ggsave(file.path(dir_salida, "grafico_3_predichos_vs_reales.png"), 
       width = 9, height = 6, dpi = 300)
dev.off()

cat("   ✓ Gráfico generado\n\n")

# --- CALIBRACIÓN -------------------------------------------------------------
cat("16. Análisis de calibración...\n")

calibracion <- datos_test %>%
  mutate(decil = ntile(prob_churn, 10)) %>%
  group_by(decil) %>%
  summarise(
    n = n(),
    prob_media = mean(prob_churn),
    tasa_observada = mean(churn),
    .groups = "drop"
  )

write_csv(calibracion, 
          file.path(dir_salida, "tabla_8_calibracion.csv"))

png(file.path(dir_salida, "grafico_4_calibracion.png"), 
    width = 800, height = 600)
plot(calibracion$prob_media, calibracion$tasa_observada,
     main = "Curva de Calibración",
     xlab = "Probabilidad Predicha",
     ylab = "Tasa Observada",
     pch = 19, col = "#2E86AB", cex = 1.5)
abline(0, 1, col = "red", lwd = 2, lty = 2)
grid()
dev.off()

cat("   ✓ Calibración completada\n\n")

# --- GUARDAR MODELOS ---------------------------------------------------------
cat("17. Guardando modelos...\n")

saveRDS(modelo_logit, file.path(dir_salida, "modelo_logit.rds"))
saveRDS(modelo_probit, file.path(dir_salida, "modelo_probit.rds"))

cat("   ✓ Modelos guardados\n\n")

# --- RESUMEN FINAL -----------------------------------------------------------
cat("================================================================================\n")
cat("✓✓✓ ANÁLISIS COMPLETADO EXITOSAMENTE ✓✓✓\n")
cat("================================================================================\n\n")
cat("MÉTRICAS PRINCIPALES:\n")
cat("  Pseudo R²:", round(pseudo_r2, 4), "\n")
cat("  AUC:      ", round(auc_valor, 3), "\n")
cat("  Exactitud:", round(conf_matrix$overall["Accuracy"], 3), "\n")
cat("  F1-Score: ", round(conf_matrix$byClass["F1"], 3), "\n\n")
cat("📁 Archivos en:", normalizePath(dir_salida), "\n")
cat("================================================================================\n\n")
cat("🎉 ¡LISTO PARA ARMAR EL PDF!\n")
cat("⏰ Entrega: HOY 25 de octubre, 11:59 PM\n\n")
