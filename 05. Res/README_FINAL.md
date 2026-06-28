# Proyecto CA-0514 - Cierre de entregables

Este cierre agrega los entregables faltantes del proyecto:

- `04. Preprocesamiento/preprocesamiento.R`: carga datos, limpia variables, crea variables derivadas y exporta bases procesadas.
- `04. Preprocesamiento/modelos.R`: ajusta modelos comparativos de frecuencia, ocurrencia y severidad.
- `05. Res/informe_final.tex`: fuente LaTeX del informe final.
- `05. Res/referencias.bib`: bibliografia BibTeX usada por el informe.
- `05. Res/presentacion_10min.tex`: presentacion breve en Beamer.

## Orden de reproduccion

Desde `C:/Users/Anthony/Desktop/Licenciatura/Seguros/Proyecto/04. Preprocesamiento`:

```r
source("preprocesamiento.R", encoding = "UTF-8")
source("modelos.R", encoding = "UTF-8")
```

El segundo script crea:

- `05. Res/base_modelo.rds`
- `05. Res/base_integrada.rds`
- `05. Res/modelos/comparativo_modelos.xlsx`
- `05. Res/modelos/indicadores_actuariales_segmentos.xlsx`
- `05. Res/modelos/indicadores_actuariales_anio.xlsx`
- `05. Res/modelos/perfiles_riesgo_bivariados.xlsx`
- `05. Res/modelos/perfiles_riesgo_bivariados_anio.xlsx`
- `05. Res/modelos/perfiles_riesgo_1a4_variables.xlsx`
- `05. Res/modelos/predicciones_promedio_segmentos_1a4.xlsx`
- `05. Res/modelos/metricas_clasificacion_logit.xlsx`
- `05. Res/modelos/sobredispersion_poisson.xlsx`
- `05. Res/modelos/comparativo_prima_pura.xlsx`
- `05. Res/modelos/predicciones_prima_pura_test.xlsx`
- `05. Res/modelos/particion_train_test_por_id.xlsx`
- `05. Res/modelos/modelos_finales.rds`
- coeficientes exportados por modelo

La particion de entrenamiento y prueba se realiza por `ID`, para evitar que un mismo identificador aparezca en ambos conjuntos. La evaluacion incluye metricas de regresion, metricas especificas del modelo logit, diagnostico de sobredispersion Poisson y comparacion de prediccion de prima pura/costo esperado por poliza.

Los archivos `indicadores_actuariales_segmentos.xlsx` e `indicadores_actuariales_anio.xlsx` responden directamente a los objetivos del proyecto. Incluyen frecuencia, proporcion de polizas con siniestro, severidad promedio, costo total, prima comercial, prima pura observada, prima pura sobre prima comercial, siniestralidad y resultado tecnico por segmento. El archivo anual permite revisar si los patrones se repiten en varios anos o si parecen puntuales.

Los archivos `perfiles_riesgo_bivariados.xlsx` y `perfiles_riesgo_bivariados_anio.xlsx` construyen perfiles mediante todas las combinaciones posibles de dos variables entre las variables de segmentacion disponibles. Cada perfil incluye indicadores de frecuencia, severidad, siniestralidad, factor de credibilidad actuarial, score de riesgo e indicadores numericos de comparacion. El libro `perfiles_riesgo_bivariados.xlsx` incluye hojas iniciales con los 10 perfiles mas riesgosos y los 10 perfiles mas seguros, considerando solo segmentos con factor de credibilidad actuarial mayor o igual a 0.5.

El archivo `perfiles_riesgo_1a4_variables.xlsx` extiende esa logica a perfiles de 1, 2, 3 y 4 variables. Incluye hojas con los 10 perfiles mas riesgosos y los 10 perfiles mas seguros para cada tamano de combinacion, ademas de hojas completas con todos los perfiles generados para cada tamano.

El factor de credibilidad actuarial se calcula con una aproximacion de fluctuacion limitada: `Z = min(1, sqrt(m / m0))`, donde `m` es el numero esperado de siniestros del segmento usando la frecuencia global de la cartera, y `m0 = (1.96 / 0.10)^2`. Esto equivale a credibilidad completa cuando el segmento tiene exposicion suficiente para esperar aproximadamente 384 siniestros, bajo un criterio de 95% de confianza y 10% de error relativo. Las tablas no incluyen interpretaciones textuales largas; se dejan identificadores de perfil e indicadores numericos.

El archivo `predicciones_promedio_segmentos_1a4.xlsx` compara, dentro de cada segmento o perfil, la frecuencia observada contra la frecuencia modelada, la probabilidad observada contra la probabilidad logit, la severidad observada contra la severidad predicha y el costo esperado observado contra el costo esperado modelado. Esta salida conecta directamente la segmentacion con los modelos predictivos.

## Compilacion sugerida

Desde `05. Res`:

```bash
pdflatex informe_final.tex
bibtex informe_final
pdflatex informe_final.tex
pdflatex informe_final.tex
pdflatex presentacion_10min.tex
```

Nota: en este entorno de Codex no hay `Rscript` disponible, por lo que los scripts quedan preparados para ejecutarse en una instalacion local de R.
