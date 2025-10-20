# Script completo (R) — Predicción de deserción (churn) - guardar este archivo en UTF-8
# Reemplazar 'data_path' por la ruta a tu CSV o usar el diálogo para seleccionar el archivo.
# Ejecutar con: source("ruta\\a\\este_script.R", encoding = "UTF-8")

# --- Configuración -------------------------------------------------------------------
options(stringsAsFactors = FALSE)
set.seed(12345)

# Paquetes (instala si falta)
pkgs <- c("tidyverse","readr","broom","modelsummary","margins","pROC","caret","janitor","gt")
for(p in pkgs) if(!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(tidyverse); library(readr); library(broom); library(modelsummary)
library(margins); library(pROC); library(caret); library(janitor); library(gt)

# Crear carpeta de salida
outdir <- "outputs"
if(!dir.exists(outdir)) dir.create(outdir)

# --- Cargar datos --------------------------------------------------------------------
# Opción A: especificar ruta
# data_path <- "c:\\Users\\santi\\Downloads\\tu_archivo.csv"
# Opción B: usar diálogo (descomentar si querés elegir archivo)
data_path <- file.choose()    # comenta esta línea y descomenta la anterior si querés ruta fija

df <- read_csv(data_path, locale = locale(encoding = "UTF-8")) %>% clean_names()
cat("Columnas detectadas:\n"); print(names(df))

# --- Definir variable dependiente y control básico -----------------------------------
# Cambiar 'churn' por el nombre correcto si tu columna tiene otro nombre
dep_var <- "churn"
if(!(dep_var %in% names(df))){
  stop(paste0("No se encontró la variable dependiente '", dep_var, "'. Cambia dep_var al nombre correcto."))
}

# Asegurar formato binario 0/1
df <- df %>%
  mutate(!!dep_var := case_when(
    .data[[dep_var]] %in% c("yes","Yes","Y","y","TRUE", TRUE) ~ 1,
    .data[[dep_var]] %in% c("no","No","N","n","FALSE", FALSE) ~ 0,
    .data[[dep_var]] %in% c(1,0) ~ as.numeric(.data[[dep_var]]),
    TRUE ~ as.numeric(.data[[dep_var]])
  ))

# --- Limpieza mínima ------------------------------------------------------------------
# Eliminar filas con NA en variable objetivo
df <- df %>% filter(!is.na(.data[[dep_var]]))
n_obs <- nrow(df)
cat("Observaciones después de filtrar NAs en objetivo:", n_obs, "\n")

# Convertir caracteres categóricas a factores (automático para columnas con pocas categorías)
df <- df %>%
  mutate(across(where(is.character), ~ if_else(n_distinct(.) <= 10, factor(.), .)))

# --- Tabla descriptiva ---------------------------------------------------------------
desc_tbl <- df %>%
  select(where(is.numeric)) %>%
  summary() %>%
  capture.output()

write_lines(desc_tbl, file.path(outdir, "tabla_descriptiva.txt"))

# Alternativa: tabla descriptiva por variable dependiente
desc_by_churn <- df %>%
  select(all_of(dep_var), where(is.numeric)) %>%
  group_by(.data[[dep_var]]) %>%
  summarise(across(where(is.numeric), list(n = ~sum(!is.na(.)), mean = mean, sd = sd), .names = "{.col}_{.fn}"), .groups = "drop")

write_csv(desc_by_churn, file.path(outdir, "tabla_descriptiva_por_churn.csv"))

# --- Partición train / test ----------------------------------------------------------
set.seed(12345)
train_index <- createDataPartition(df[[dep_var]], p = 0.7, list = FALSE)
train <- df[train_index, ]
test  <- df[-train_index, ]

cat("Train:", nrow(train), " Test:", nrow(test), "\n")

# --- Selección de variables (ejemplo) -----------------------------------------------
# Aquí se usa un conjunto de columnas predictoras por defecto: todas menos la dependiente
# Si querés, explícitamente elige variables: predictors <- c("age","tenure", "monthly_spend", "is_vip")
predictors <- setdiff(names(df), dep_var)
# Para glm, remover columnas no numéricas/factor con demasiadas categorías (simplificación)
predictors <- predictors[!predictors %in% c()]  # editar si es necesario

# Fórmula
fmla <- as.formula(paste(dep_var, "~", paste(predictors, collapse = " + ")))

# --- Estimar modelo logit -----------------------------------------------------------
mod_logit <- glm(fmla, data = train, family = binomial(link = "logit"))

# Guardar resumen del modelo
mod_tidy <- broom::tidy(mod_logit, conf.int = TRUE) %>%
  mutate(odds = exp(estimate))
write_csv(mod_tidy, file.path(outdir, "regression_tidy.csv"))

# Tabla exportable (HTML/GT)
modelsummary(list(Logit = mod_logit),
             output = file.path(outdir, "regression_table.html"),
             gof_omit = "DF|AIC|BIC|Log")

# --- Estadísticos adicionales --------------------------------------------------------
# Pseudo R2 (McFadden)
ll_full <- as.numeric(logLik(mod_logit))
ll_null <- as.numeric(logLik(update(mod_logit, . ~ 1)))
pseudo_r2_mcf <- 1 - (ll_full / ll_null)

metrics <- tibble(
  n_obs = n_obs,
  ll_full = ll_full,
  ll_null = ll_null,
  pseudo_r2_mcf = pseudo_r2_mcf,
  aic = AIC(mod_logit),
  bic = BIC(mod_logit)
)
write_csv(metrics, file.path(outdir, "model_metrics.csv"))

# --- Efectos marginales promedio ----------------------------------------------------
# Requiere paquete margins
mfx <- try(margins::margins(mod_logit), silent = TRUE)
if(inherits(mfx, "try-error")){
  cat("No se pudieron calcular margins() automáticamente. Revisa variables.\n")
} else {
  mfx_sum <- summary(mfx)
  write_csv(as_tibble(mfx_sum), file.path(outdir, "marginal_effects.csv"))
}

# --- Predicciones en test -----------------------------------------------------------
test$prob <- predict(mod_logit, newdata = test, type = "response")
test$pred <- ifelse(test$prob >= 0.5, 1, 0)

# Confusion matrix y métricas
conf <- confusionMatrix(factor(test$pred), factor(test[[dep_var]]), positive = "1")
conf_df <- tibble(
  accuracy = conf$overall["Accuracy"],
  kappa = conf$overall["Kappa"],
  sensitivity = conf$byClass["Sensitivity"],
  specificity = conf$byClass["Specificity"],
  pos_pred_value = conf$byClass["Pos Pred Value"],
  f1 = conf$byClass["F1"]
)
write_csv(conf_df, file.path(outdir, "confusion_metrics.csv"))
saveRDS(conf, file.path(outdir, "confusion_matrix.rds"))

# --- ROC / AUC ----------------------------------------------------------------------
roc_obj <- roc(test[[dep_var]], test$prob)
auc_val <- auc(roc_obj)
png(file.path(outdir, "roc_curve.png"), width = 800, height = 600)
plot(roc_obj, main = paste0("ROC curve (AUC = ", round(auc_val,3), ")"))
dev.off()

# --- Calibración por deciles --------------------------------------------------------
test_cal <- test %>%
  mutate(decile = ntile(prob, 10)) %>%
  group_by(decile) %>%
  summarise(mean_prob = mean(prob), obs_rate = mean(.data[[dep_var]]), n = n(), .groups = "drop")

write_csv(test_cal, file.path(outdir, "calibration_by_decile.csv"))

png(file.path(outdir, "calibration_plot.png"), width = 800, height = 600)
plot(test_cal$mean_prob, test_cal$obs_rate, xlab = "Probabilidad predicha (media por decil)", ylab = "Tasa observada", main = "Curva de calibración")
abline(0,1, col = "red", lty = 2)
dev.off()

# --- Distribución de probabilidades predichas ---------------------------------------
png(file.path(outdir, "predicted_prob_hist.png"), width = 800, height = 600)
ggplot(test, aes(x = prob, fill = factor(.data[[dep_var]]))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Distribución de probabilidades predichas por clase", x = "Probabilidad predicha", fill = dep_var) +
  theme_minimal()
ggsave(file.path(outdir, "predicted_prob_hist_gg.png"), width = 8, height = 5, dpi = 300)
dev.off()

# --- Tabla de comparación predicho vs real por deciles (lift table) ------------------
lift_tbl <- test %>%
  arrange(desc(prob)) %>%
  mutate(rank = row_number(), decile = ntile(-prob, 10)) %>%
  group_by(decile) %>%
  summarise(n = n(), positives = sum(.data[[dep_var]] == 1), rate = mean(.data[[dep_var]] == 1), avg_prob = mean(prob), .groups = "drop") %>%
  arrange(decile)

write_csv(lift_tbl, file.path(outdir, "lift_table.csv"))

# --- Guardar objetos útiles ---------------------------------------------------------
saveRDS(mod_logit, file.path(outdir, "model_logit.rds"))
write_csv(as_tibble(mod_tidy), file.path(outdir, "model_coefficients.csv"))

# --- Mensajes finales ----------------------------------------------------------------
cat("Script finalizado. Archivos guardados en:", normalizePath(outdir), "\n")
cat("Asegurate de que este script esté guardado en UTF-8 y ejecútalo con source(..., encoding = 'UTF-8')\n")