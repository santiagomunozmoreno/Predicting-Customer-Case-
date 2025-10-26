# 📊 Predicting Customer Case

## 🧩 Descripción general

Este proyecto tiene como objetivo predecir y prevenir la deserción de clientes (churn o attrition) en la empresa QWE INC..

Para ello, se estima un modelo de probabilidad para una variable binaria, pudiendo emplear Mínimos Cuadrados Lineales (MPL), Logit o Probit, según la especificación seleccionada por cada grupo.

El análisis incluye:

Limpieza y exploración de los datos (EDA).

Estimación del modelo probabilístico.

Interpretación de los coeficientes relevantes.

Cálculo de efectos marginales.

Evaluación del modelo (ROC/AUC, matriz de confusión, R², errores, calibración y comparación entre valores predichos y reales).

Exportación y presentación profesional de tablas y gráficos.

## 🎯 Objetivos del caso

- Estimar un modelo estadístico que permita predecir la probabilidad de deserción de los clientes de QWE Inc.
- Identificar las variables más determinantes en la decisión de abandono.
- Evaluar la capacidad predictiva del modelo y su posible aplicación práctica para la retención proactiva de clientes.
- Presentar los resultados mediante tablas y gráficos claros, precisos y profesionalmente formateados.

## 📂 Predicting-Customer-Case Contiene las siguientes carpetas

  ┣ 📁 código →  contiene el script principal en R, donde se desarrolla todo el proceso analítico. En este archivo se realiza la limpieza de los datos, la estimación del modelo logit, el cálculo de efectos marginales y la evaluación del desempeño predictivo del modelo.
  ┣ 📁 datos  → en esta carpeta se almacena la base de datos utilizada en el caso, correspondiente al archivo proporcionado por la Darden School of Business. Aquí se incluyen los datos originales necesarios para la ejecución del análisis.
  ┣ 📁 resultados  → guarda las salidas generadas por el script, como tablas descriptivas, gráficos, evaluaciones del modelo (ROC, matriz de confusión, etc.) y cualquier otro producto intermedio o final del estudio.


## 🛠️ Librerías utilizadas

El script en R requiere las siguientes librerías:

r
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



