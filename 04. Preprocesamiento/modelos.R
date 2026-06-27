
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  broom,
  readr,
  openxlsx
)

if (!file.exists("../05. Res/base_modelo.rds")) {
  source("preprocesamiento.R", encoding = "UTF-8")
}

base_modelo <- readRDS("../05. Res/base_modelo.rds")

set.seed(2025)

# La particion se hace por ID para evitar que una misma poliza/cliente aparezca
# simultaneamente en entrenamiento y prueba.
ids <- unique(base_modelo$id)
ids_train <- sample(ids, size = floor(0.80 * length(ids)))

train <- base_modelo |>
  filter(id %in% ids_train)

test <- base_modelo |>
  filter(!id %in% ids_train)

particion <- tibble(
  conjunto = c("train", "test"),
  n_filas = c(nrow(train), nrow(test)),
  n_ids = c(n_distinct(train$id), n_distinct(test$id)),
  pct_filas = c(nrow(train), nrow(test)) / nrow(base_modelo)
)

vars_modelo <- c(
  "type_risk", "area_label", "grupo_edad", "grupo_experiencia",
  "grupo_potencia", "grupo_valor_vehiculo", "distribution_channel",
  "premium", "n_claims_history", "r_claims_history", "seniority"
)

form_frecuencia <- as.formula(
  paste("n_claims_year ~", paste(vars_modelo, collapse = " + "))
)

form_ocurrencia <- as.formula(
  paste("claim_flag ~", paste(vars_modelo, collapse = " + "))
)

form_severidad <- as.formula(
  paste("cost_claims_year ~", paste(vars_modelo, collapse = " + "))
)

modelo_poisson <- glm(
  form_frecuencia,
  family = poisson(link = "log"),
  data = train
)

modelo_quasipoisson <- glm(
  form_frecuencia,
  family = quasipoisson(link = "log"),
  data = train
)

modelo_binomial <- glm(
  form_ocurrencia,
  family = binomial(link = "logit"),
  data = train
)

train_sev <- train |>
  filter(cost_claims_year > 0, n_claims_year > 0)

test_sev <- test |>
  filter(cost_claims_year > 0, n_claims_year > 0)

modelo_gamma <- glm(
  form_severidad,
  family = Gamma(link = "log"),
  data = train_sev
)

modelo_lognormal <- lm(
  update(form_severidad, log1p(cost_claims_year) ~ .),
  data = train_sev
)

metricas_regresion <- function(obs, pred) {
  tibble(
    rmse = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    mae = mean(abs(obs - pred), na.rm = TRUE),
    media_obs = mean(obs, na.rm = TRUE),
    media_pred = mean(pred, na.rm = TRUE),
    sesgo = mean(pred - obs, na.rm = TRUE)
  )
}

auc_manual <- function(obs, prob) {
  ok <- !is.na(obs) & !is.na(prob)
  obs <- obs[ok]
  prob <- prob[ok]

  if (length(unique(obs)) < 2) {
    return(NA_real_)
  }

  rangos <- rank(prob, ties.method = "average")
  n_pos <- sum(obs == 1)
  n_neg <- sum(obs == 0)

  (sum(rangos[obs == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

metricas_clasificacion <- function(obs, prob, corte = 0.5) {
  pred <- as.integer(prob >= corte)

  tp <- sum(obs == 1 & pred == 1, na.rm = TRUE)
  tn <- sum(obs == 0 & pred == 0, na.rm = TRUE)
  fp <- sum(obs == 0 & pred == 1, na.rm = TRUE)
  fn <- sum(obs == 1 & pred == 0, na.rm = TRUE)

  eps <- 1e-15
  prob_clip <- pmin(pmax(prob, eps), 1 - eps)

  tibble(
    corte = corte,
    accuracy = (tp + tn) / (tp + tn + fp + fn),
    sensibilidad = if_else(tp + fn > 0, tp / (tp + fn), NA_real_),
    especificidad = if_else(tn + fp > 0, tn / (tn + fp), NA_real_),
    precision = if_else(tp + fp > 0, tp / (tp + fp), NA_real_),
    f1 = if_else(
      2 * tp + fp + fn > 0,
      2 * tp / (2 * tp + fp + fn),
      NA_real_
    ),
    auc = auc_manual(obs, prob),
    log_loss = -mean(obs * log(prob_clip) + (1 - obs) * log(1 - prob_clip), na.rm = TRUE),
    tasa_evento_obs = mean(obs, na.rm = TRUE),
    tasa_evento_pred = mean(prob, na.rm = TRUE)
  )
}

evaluar_sobredispersion <- function(modelo) {
  pearson <- residuals(modelo, type = "pearson")
  chi2 <- sum(pearson^2, na.rm = TRUE)
  gl <- df.residual(modelo)
  ratio <- chi2 / gl
  p_valor <- pchisq(chi2, df = gl, lower.tail = FALSE)

  tibble(
    modelo = "Poisson frecuencia",
    chi_cuadrado_pearson = chi2,
    gl = gl,
    ratio_sobredispersion = ratio,
    p_valor = p_valor,
    interpretacion = case_when(
      ratio > 1.5 ~ "Evidencia importante de sobredispersion",
      ratio > 1.2 ~ "Evidencia moderada de sobredispersion",
      ratio < 0.8 ~ "Posible subdispersion",
      TRUE ~ "Dispersion cercana a Poisson"
    )
  )
}

pred_poisson <- predict(modelo_poisson, newdata = test, type = "response")
pred_quasi <- predict(modelo_quasipoisson, newdata = test, type = "response")
pred_binomial <- predict(modelo_binomial, newdata = test, type = "response")
pred_gamma <- predict(modelo_gamma, newdata = test_sev, type = "response")
pred_lognormal <- expm1(predict(modelo_lognormal, newdata = test_sev))
pred_gamma_all <- predict(modelo_gamma, newdata = test, type = "response")
pred_lognormal_all <- expm1(predict(modelo_lognormal, newdata = test))

comparativo <- bind_rows(
  metricas_regresion(test$n_claims_year, pred_poisson) |>
    mutate(modelo = "Poisson frecuencia", variable = "n_claims_year", aic = AIC(modelo_poisson)),
  metricas_regresion(test$n_claims_year, pred_quasi) |>
    mutate(modelo = "Quasi-Poisson frecuencia", variable = "n_claims_year", aic = NA_real_),
  metricas_regresion(test$claim_flag, pred_binomial) |>
    mutate(modelo = "Logit ocurrencia", variable = "claim_flag", aic = AIC(modelo_binomial)),
  metricas_regresion(test_sev$cost_claims_year, pred_gamma) |>
    mutate(modelo = "Gamma severidad", variable = "cost_claims_year", aic = AIC(modelo_gamma)),
  metricas_regresion(test_sev$cost_claims_year, pred_lognormal) |>
    mutate(modelo = "Lognormal severidad", variable = "cost_claims_year", aic = AIC(modelo_lognormal))
) |>
  dplyr::select(modelo, variable, rmse, mae, media_obs, media_pred, sesgo, aic)

clasificacion_logit <- metricas_clasificacion(test$claim_flag, pred_binomial, corte = 0.5) |>
  mutate(modelo = "Logit ocurrencia") |>
  dplyr::select(modelo, everything())

sobredispersion_poisson <- evaluar_sobredispersion(modelo_poisson)

# Comparacion de prima pura / costo esperado por poliza.
# Costo anual agregado positivo.
test_prima_pura <- test |>
  mutate(
    pred_freq_poisson = pred_poisson,
    pred_freq_quasi = pred_quasi,
    pred_prob_siniestro = pred_binomial,
    pred_sev_gamma = pred_gamma_all,
    pred_sev_lognormal = pred_lognormal_all,
    prima_pura_poisson_gamma = pred_freq_poisson * pred_sev_gamma,
    prima_pura_quasi_gamma = pred_freq_quasi * pred_sev_gamma,
    prima_pura_logit_gamma = pred_prob_siniestro * pred_sev_gamma,
    prima_pura_logit_lognormal = pred_prob_siniestro * pred_sev_lognormal
  )

comparativo_prima_pura <- bind_rows(
  metricas_regresion(test_prima_pura$cost_claims_year, test_prima_pura$prima_pura_poisson_gamma) |>
    mutate(modelo = "Poisson x Gamma"),
  metricas_regresion(test_prima_pura$cost_claims_year, test_prima_pura$prima_pura_quasi_gamma) |>
    mutate(modelo = "Quasi-Poisson x Gamma"),
  metricas_regresion(test_prima_pura$cost_claims_year, test_prima_pura$prima_pura_logit_gamma) |>
    mutate(modelo = "Logit x Gamma"),
  metricas_regresion(test_prima_pura$cost_claims_year, test_prima_pura$prima_pura_logit_lognormal) |>
    mutate(modelo = "Logit x Lognormal")
) |>
  mutate(variable = "cost_claims_year") |>
  dplyr::select(modelo, variable, rmse, mae, media_obs, media_pred, sesgo)

dir.create("../05. Res/modelos", showWarnings = FALSE, recursive = TRUE)

guardar_xlsx <- function(tabla, archivo) {
  openxlsx::write.xlsx(
    x = as.data.frame(tabla),
    file = archivo,
    overwrite = TRUE,
    asTable = TRUE
  )
}

guardar_xlsx(particion, "../05. Res/modelos/particion_train_test_por_id.xlsx")
guardar_xlsx(comparativo, "../05. Res/modelos/comparativo_modelos.xlsx")
guardar_xlsx(clasificacion_logit, "../05. Res/modelos/metricas_clasificacion_logit.xlsx")
guardar_xlsx(sobredispersion_poisson, "../05. Res/modelos/sobredispersion_poisson.xlsx")
guardar_xlsx(comparativo_prima_pura, "../05. Res/modelos/comparativo_prima_pura.xlsx")
guardar_xlsx(
  test_prima_pura |>
    dplyr::select(
      id, date_last_renewal, cost_claims_year,
      pred_freq_poisson, pred_freq_quasi, pred_prob_siniestro,
      prima_pura_poisson_gamma, prima_pura_quasi_gamma,
      prima_pura_logit_gamma, prima_pura_logit_lognormal
    ),
  "../05. Res/modelos/predicciones_prima_pura_test.xlsx"
)
guardar_xlsx(tidy(modelo_poisson), "../05. Res/modelos/coeficientes_poisson.xlsx")
guardar_xlsx(tidy(modelo_quasipoisson), "../05. Res/modelos/coeficientes_quasipoisson.xlsx")
guardar_xlsx(tidy(modelo_binomial), "../05. Res/modelos/coeficientes_logit.xlsx")
guardar_xlsx(tidy(modelo_gamma), "../05. Res/modelos/coeficientes_gamma.xlsx")
guardar_xlsx(tidy(modelo_lognormal), "../05. Res/modelos/coeficientes_lognormal.xlsx")

saveRDS(
  list(
    poisson = modelo_poisson,
    quasipoisson = modelo_quasipoisson,
    logit = modelo_binomial,
    gamma = modelo_gamma,
    lognormal = modelo_lognormal,
    particion = particion,
    comparativo = comparativo,
    clasificacion_logit = clasificacion_logit,
    sobredispersion_poisson = sobredispersion_poisson,
    comparativo_prima_pura = comparativo_prima_pura
  ),
  "../05. Res/modelos/modelos_finales.rds"
)

print(particion)
print(comparativo)
print(clasificacion_logit)
print(sobredispersion_poisson)
print(comparativo_prima_pura)
