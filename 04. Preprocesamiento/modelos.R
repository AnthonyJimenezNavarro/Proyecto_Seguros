
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

base_modelo <- base_modelo |>
  mutate(
    grupo_antiguedad_vehiculo = cut(
      antiguedad_vehiculo,
      breaks = c(-Inf, 2, 5, 10, 15, Inf),
      labels = c("0-2", "3-5", "6-10", "11-15", ">15")
    )
  )

set.seed(20260627)

# La particion se hace por ID para evitar que una misma poliza aparezca en entrenamiento y prueba.
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
    indicador_sobredispersion_moderada = as.integer(ratio > 1.2),
    indicador_sobredispersion_importante = as.integer(ratio > 1.5),
    indicador_subdispersion = as.integer(ratio < 0.8)
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

resumen_predicciones_segmento <- function(data, variables) {
  data |>
    filter(if_all(all_of(variables), ~ !is.na(.x))) |>
    group_by(across(all_of(variables))) |>
    summarise(
      n_polizas = n(),
      n_ids = n_distinct(id),
      frecuencia_obs = mean(n_claims_year, na.rm = TRUE),
      frecuencia_pred_poisson = mean(pred_freq_poisson, na.rm = TRUE),
      frecuencia_pred_quasi = mean(pred_freq_quasi, na.rm = TRUE),
      prob_siniestro_obs = mean(claim_flag, na.rm = TRUE),
      prob_siniestro_pred_logit = mean(pred_prob_siniestro, na.rm = TRUE),
      severidad_obs = if_else(
        sum(n_claims_year, na.rm = TRUE) > 0,
        sum(cost_claims_year, na.rm = TRUE) / sum(n_claims_year, na.rm = TRUE),
        NA_real_
      ),
      severidad_pred_gamma = mean(pred_sev_gamma, na.rm = TRUE),
      severidad_pred_lognormal = mean(pred_sev_lognormal, na.rm = TRUE),
      costo_esperado_obs = mean(cost_claims_year, na.rm = TRUE),
      costo_esperado_pred_poisson_gamma = mean(prima_pura_poisson_gamma, na.rm = TRUE),
      costo_esperado_pred_quasi_gamma = mean(prima_pura_quasi_gamma, na.rm = TRUE),
      costo_esperado_pred_logit_gamma = mean(prima_pura_logit_gamma, na.rm = TRUE),
      costo_esperado_pred_logit_lognormal = mean(prima_pura_logit_lognormal, na.rm = TRUE),
      mae_costo_logit_gamma = mean(
        abs(cost_claims_year - prima_pura_logit_gamma),
        na.rm = TRUE
      ),
      mae_costo_logit_lognormal = mean(
        abs(cost_claims_year - prima_pura_logit_lognormal),
        na.rm = TRUE
      ),
      sesgo_costo_logit_gamma = mean(
        prima_pura_logit_gamma - cost_claims_year,
        na.rm = TRUE
      ),
      sesgo_costo_logit_lognormal = mean(
        prima_pura_logit_lognormal - cost_claims_year,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      n_variables = length(variables),
      variables_segmento = paste(variables, collapse = " + "),
      segmento = paste(c_across(all_of(variables)), collapse = " | ")
    ) |>
    ungroup() |>
    dplyr::select(
      n_variables, variables_segmento, segmento,
      n_polizas, n_ids,
      frecuencia_obs, frecuencia_pred_poisson, frecuencia_pred_quasi,
      prob_siniestro_obs, prob_siniestro_pred_logit,
      severidad_obs, severidad_pred_gamma, severidad_pred_lognormal,
      costo_esperado_obs,
      costo_esperado_pred_poisson_gamma,
      costo_esperado_pred_quasi_gamma,
      costo_esperado_pred_logit_gamma,
      costo_esperado_pred_logit_lognormal,
      mae_costo_logit_gamma,
      mae_costo_logit_lognormal,
      sesgo_costo_logit_gamma,
      sesgo_costo_logit_lognormal,
      all_of(variables)
    ) |>
    arrange(desc(costo_esperado_obs), desc(n_polizas))
}

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

resumen_actuarial_segmento <- function(data, variable, por_anio = FALSE) {
  grupos <- if (por_anio) c("anio_renovacion", variable) else variable

  data |>
    filter(!is.na(.data[[variable]])) |>
    group_by(across(all_of(grupos))) |>
    summarise(
      n_polizas = n(),
      n_ids = n_distinct(id),
      prima_comercial_total = sum(premium, na.rm = TRUE),
      prima_comercial_promedio = mean(premium, na.rm = TRUE),
      polizas_con_siniestro = sum(claim_flag == 1, na.rm = TRUE),
      proporcion_polizas_siniestro = polizas_con_siniestro / n_polizas,
      n_siniestros_total = sum(n_claims_year, na.rm = TRUE),
      frecuencia_reclamos = n_siniestros_total / n_polizas,
      costo_reclamos_total = sum(cost_claims_year, na.rm = TRUE),
      costo_reclamos_promedio_poliza = costo_reclamos_total / n_polizas,
      severidad_promedio = if_else(
        n_siniestros_total > 0,
        costo_reclamos_total / n_siniestros_total,
        NA_real_
      ),
      prima_pura_observada = frecuencia_reclamos * severidad_promedio,
      prima_pura_sobre_prima_comercial = prima_pura_observada / prima_comercial_promedio,
      siniestralidad = costo_reclamos_total / prima_comercial_total,
      resultado_tecnico = prima_comercial_total - costo_reclamos_total,
      recargo_comercial_sobre_pura = if_else(
        prima_pura_observada > 0,
        prima_comercial_promedio / prima_pura_observada - 1,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    rename(categoria = all_of(variable)) |>
    mutate(variable_segmento = variable, .before = 1) |>
    arrange(desc(siniestralidad))
}

resumen_estabilidad_anual <- function(tablas_anio) {
  bind_rows(tablas_anio) |>
    group_by(variable_segmento, categoria) |>
    summarise(
      n_anios = n_distinct(anio_renovacion),
      frecuencia_promedio_anual = mean(frecuencia_reclamos, na.rm = TRUE),
      frecuencia_sd_anual = sd(frecuencia_reclamos, na.rm = TRUE),
      frecuencia_cv_anual = frecuencia_sd_anual / frecuencia_promedio_anual,
      severidad_promedio_anual = mean(severidad_promedio, na.rm = TRUE),
      severidad_sd_anual = sd(severidad_promedio, na.rm = TRUE),
      severidad_cv_anual = severidad_sd_anual / severidad_promedio_anual,
      siniestralidad_promedio_anual = mean(siniestralidad, na.rm = TRUE),
      siniestralidad_sd_anual = sd(siniestralidad, na.rm = TRUE),
      siniestralidad_cv_anual = siniestralidad_sd_anual / siniestralidad_promedio_anual,
      anios_siniestralidad_sobre_100 = sum(siniestralidad > 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      lectura_patron = case_when(
        n_anios < 2 ~ "Informacion anual insuficiente",
        siniestralidad_cv_anual <= 0.25 ~ "Patron relativamente estable",
        siniestralidad_cv_anual <= 0.50 ~ "Patron con variacion moderada",
        TRUE ~ "Patron posiblemente puntual o volatil"
      )
    ) |>
    arrange(variable_segmento, desc(siniestralidad_promedio_anual))
}

resumen_actuarial_perfil2 <- function(data, variable_1, variable_2) {
  frecuencia_global <- sum(data$n_claims_year, na.rm = TRUE) / nrow(data)
  severidad_global <- sum(data$cost_claims_year, na.rm = TRUE) /
    sum(data$n_claims_year, na.rm = TRUE)
  siniestralidad_global <- sum(data$cost_claims_year, na.rm = TRUE) /
    sum(data$premium, na.rm = TRUE)
  siniestros_credibilidad_completa <- (qnorm(0.975) / 0.10)^2

  data |>
    filter(!is.na(.data[[variable_1]]), !is.na(.data[[variable_2]])) |>
    group_by(across(all_of(c(variable_1, variable_2)))) |>
    summarise(
      n_polizas = n(),
      n_ids = n_distinct(id),
      prima_comercial_total = sum(premium, na.rm = TRUE),
      prima_comercial_promedio = mean(premium, na.rm = TRUE),
      polizas_con_siniestro = sum(claim_flag == 1, na.rm = TRUE),
      proporcion_polizas_siniestro = polizas_con_siniestro / n_polizas,
      n_siniestros_total = sum(n_claims_year, na.rm = TRUE),
      frecuencia_reclamos = n_siniestros_total / n_polizas,
      costo_reclamos_total = sum(cost_claims_year, na.rm = TRUE),
      costo_reclamos_promedio_poliza = costo_reclamos_total / n_polizas,
      severidad_promedio = if_else(
        n_siniestros_total > 0,
        costo_reclamos_total / n_siniestros_total,
        NA_real_
      ),
      prima_pura_observada = frecuencia_reclamos * severidad_promedio,
      prima_pura_sobre_prima_comercial = prima_pura_observada / prima_comercial_promedio,
      siniestralidad = costo_reclamos_total / prima_comercial_total,
      resultado_tecnico = prima_comercial_total - costo_reclamos_total,
      recargo_comercial_sobre_pura = if_else(
        prima_pura_observada > 0,
        prima_comercial_promedio / prima_pura_observada - 1,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    rename(
      categoria_1 = all_of(variable_1),
      categoria_2 = all_of(variable_2)
    ) |>
    mutate(
      variable_1 = variable_1,
      variable_2 = variable_2,
      perfil = paste(categoria_1, categoria_2, sep = " | "),
      frecuencia_relativa = frecuencia_reclamos / frecuencia_global,
      severidad_relativa = severidad_promedio / severidad_global,
      siniestralidad_relativa = siniestralidad / siniestralidad_global,
      indicador_frecuencia_alta = as.integer(frecuencia_relativa >= 1.25),
      indicador_frecuencia_baja = as.integer(frecuencia_relativa <= 0.75),
      indicador_severidad_alta = as.integer(severidad_relativa >= 1.25),
      indicador_severidad_baja = as.integer(severidad_relativa <= 0.75),
      indicador_siniestralidad_alta = as.integer(siniestralidad_relativa >= 1.25),
      indicador_siniestralidad_baja = as.integer(siniestralidad_relativa <= 0.75),
      indicador_siniestralidad_critica = as.integer(siniestralidad >= 1)
    ) |>
    mutate(
      siniestros_esperados_global = n_polizas * frecuencia_global,
      siniestros_credibilidad_completa = siniestros_credibilidad_completa,
      factor_credibilidad = pmin(
        1,
        sqrt(siniestros_esperados_global / siniestros_credibilidad_completa)
      ),
      credibilidad_completa = as.integer(factor_credibilidad >= 1),
      score_frecuencia = coalesce(percent_rank(frecuencia_reclamos), 0),
      score_severidad = coalesce(percent_rank(severidad_promedio), 0),
      score_siniestralidad = coalesce(percent_rank(siniestralidad), 0),
      score_riesgo = 100 * (
        0.35 * score_frecuencia +
          0.35 * score_severidad +
          0.30 * score_siniestralidad
      )
    ) |>
    dplyr::select(
      variable_1, categoria_1, variable_2, categoria_2, perfil,
      n_polizas, n_ids,
      siniestros_esperados_global, siniestros_credibilidad_completa,
      factor_credibilidad, credibilidad_completa,
      frecuencia_reclamos, frecuencia_relativa,
      indicador_frecuencia_alta, indicador_frecuencia_baja,
      severidad_promedio, severidad_relativa,
      indicador_severidad_alta, indicador_severidad_baja,
      siniestralidad, siniestralidad_relativa,
      indicador_siniestralidad_alta, indicador_siniestralidad_baja,
      indicador_siniestralidad_critica,
      prima_comercial_promedio, prima_pura_observada,
      prima_pura_sobre_prima_comercial, resultado_tecnico,
      score_riesgo,
      everything()
    ) |>
    arrange(desc(score_riesgo), desc(n_polizas))
}

resumen_actuarial_perfil2_anio <- function(data, variable_1, variable_2) {
  data |>
    filter(!is.na(.data[[variable_1]]), !is.na(.data[[variable_2]])) |>
    group_by(anio_renovacion, across(all_of(c(variable_1, variable_2)))) |>
    summarise(
      n_polizas = n(),
      n_ids = n_distinct(id),
      prima_comercial_total = sum(premium, na.rm = TRUE),
      prima_comercial_promedio = mean(premium, na.rm = TRUE),
      polizas_con_siniestro = sum(claim_flag == 1, na.rm = TRUE),
      proporcion_polizas_siniestro = polizas_con_siniestro / n_polizas,
      n_siniestros_total = sum(n_claims_year, na.rm = TRUE),
      frecuencia_reclamos = n_siniestros_total / n_polizas,
      costo_reclamos_total = sum(cost_claims_year, na.rm = TRUE),
      severidad_promedio = if_else(
        n_siniestros_total > 0,
        costo_reclamos_total / n_siniestros_total,
        NA_real_
      ),
      prima_pura_observada = frecuencia_reclamos * severidad_promedio,
      siniestralidad = costo_reclamos_total / prima_comercial_total,
      resultado_tecnico = prima_comercial_total - costo_reclamos_total,
      .groups = "drop"
    ) |>
    rename(
      categoria_1 = all_of(variable_1),
      categoria_2 = all_of(variable_2)
    ) |>
    mutate(
      variable_1 = variable_1,
      variable_2 = variable_2,
      perfil = paste(categoria_1, categoria_2, sep = " | "),
      .before = 1
    ) |>
    arrange(anio_renovacion, desc(siniestralidad))
}

resumen_actuarial_perfil_n <- function(data, variables) {
  frecuencia_global <- sum(data$n_claims_year, na.rm = TRUE) / nrow(data)
  severidad_global <- sum(data$cost_claims_year, na.rm = TRUE) /
    sum(data$n_claims_year, na.rm = TRUE)
  siniestralidad_global <- sum(data$cost_claims_year, na.rm = TRUE) /
    sum(data$premium, na.rm = TRUE)
  siniestros_credibilidad_completa <- (qnorm(0.975) / 0.10)^2

  data |>
    filter(if_all(all_of(variables), ~ !is.na(.x))) |>
    group_by(across(all_of(variables))) |>
    summarise(
      n_polizas = n(),
      n_ids = n_distinct(id),
      prima_comercial_total = sum(premium, na.rm = TRUE),
      prima_comercial_promedio = mean(premium, na.rm = TRUE),
      polizas_con_siniestro = sum(claim_flag == 1, na.rm = TRUE),
      proporcion_polizas_siniestro = polizas_con_siniestro / n_polizas,
      n_siniestros_total = sum(n_claims_year, na.rm = TRUE),
      frecuencia_reclamos = n_siniestros_total / n_polizas,
      costo_reclamos_total = sum(cost_claims_year, na.rm = TRUE),
      costo_reclamos_promedio_poliza = costo_reclamos_total / n_polizas,
      severidad_promedio = if_else(
        n_siniestros_total > 0,
        costo_reclamos_total / n_siniestros_total,
        NA_real_
      ),
      prima_pura_observada = frecuencia_reclamos * severidad_promedio,
      prima_pura_sobre_prima_comercial = prima_pura_observada / prima_comercial_promedio,
      siniestralidad = costo_reclamos_total / prima_comercial_total,
      resultado_tecnico = prima_comercial_total - costo_reclamos_total,
      recargo_comercial_sobre_pura = if_else(
        prima_pura_observada > 0,
        prima_comercial_promedio / prima_pura_observada - 1,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      perfil = paste(c_across(all_of(variables)), collapse = " | ")
    ) |>
    ungroup() |>
    mutate(
      n_variables = length(variables),
      variables_perfil = paste(variables, collapse = " + "),
      frecuencia_relativa = frecuencia_reclamos / frecuencia_global,
      severidad_relativa = severidad_promedio / severidad_global,
      siniestralidad_relativa = siniestralidad / siniestralidad_global,
      indicador_frecuencia_alta = as.integer(frecuencia_relativa >= 1.25),
      indicador_frecuencia_baja = as.integer(frecuencia_relativa <= 0.75),
      indicador_severidad_alta = as.integer(severidad_relativa >= 1.25),
      indicador_severidad_baja = as.integer(severidad_relativa <= 0.75),
      indicador_siniestralidad_alta = as.integer(siniestralidad_relativa >= 1.25),
      indicador_siniestralidad_baja = as.integer(siniestralidad_relativa <= 0.75),
      indicador_siniestralidad_critica = as.integer(siniestralidad >= 1)
    ) |>
    mutate(
      siniestros_esperados_global = n_polizas * frecuencia_global,
      siniestros_credibilidad_completa = siniestros_credibilidad_completa,
      factor_credibilidad = pmin(
        1,
        sqrt(siniestros_esperados_global / siniestros_credibilidad_completa)
      ),
      credibilidad_completa = as.integer(factor_credibilidad >= 1),
      score_frecuencia = coalesce(percent_rank(frecuencia_reclamos), 0),
      score_severidad = coalesce(percent_rank(severidad_promedio), 0),
      score_siniestralidad = coalesce(percent_rank(siniestralidad), 0),
      score_riesgo = 100 * (
        0.35 * score_frecuencia +
          0.35 * score_severidad +
          0.30 * score_siniestralidad
      )
    ) |>
    dplyr::select(
      n_variables, variables_perfil, perfil,
      n_polizas, n_ids,
      siniestros_esperados_global, siniestros_credibilidad_completa,
      factor_credibilidad, credibilidad_completa,
      frecuencia_reclamos, frecuencia_relativa,
      indicador_frecuencia_alta, indicador_frecuencia_baja,
      severidad_promedio, severidad_relativa,
      indicador_severidad_alta, indicador_severidad_baja,
      siniestralidad, siniestralidad_relativa,
      indicador_siniestralidad_alta, indicador_siniestralidad_baja,
      indicador_siniestralidad_critica,
      prima_comercial_promedio, prima_pura_observada,
      prima_pura_sobre_prima_comercial, resultado_tecnico,
      score_riesgo,
      all_of(variables), everything()
    ) |>
    arrange(desc(score_riesgo), desc(n_polizas))
}

variables_segmentacion <- c(
  "grupo_edad",
  "type_risk",
  "grupo_antiguedad_vehiculo",
  "area_label",
  "grupo_experiencia",
  "grupo_potencia",
  "grupo_valor_vehiculo",
  "distribution_channel"
)

tablas_segmentos <- list(
  grupo_edad = resumen_actuarial_segmento(base_modelo, "grupo_edad"),
  tipo_riesgo = resumen_actuarial_segmento(base_modelo, "type_risk"),
  antiguedad_vehiculo = resumen_actuarial_segmento(base_modelo, "grupo_antiguedad_vehiculo"),
  zona = resumen_actuarial_segmento(base_modelo, "area_label"),
  experiencia = resumen_actuarial_segmento(base_modelo, "grupo_experiencia"),
  potencia = resumen_actuarial_segmento(base_modelo, "grupo_potencia"),
  valor_vehiculo = resumen_actuarial_segmento(base_modelo, "grupo_valor_vehiculo"),
  canal = resumen_actuarial_segmento(base_modelo, "distribution_channel")
)

tablas_segmentos_anio <- list(
  edad_anio = resumen_actuarial_segmento(base_modelo, "grupo_edad", por_anio = TRUE),
  riesgo_anio = resumen_actuarial_segmento(base_modelo, "type_risk", por_anio = TRUE),
  antiguedad_anio = resumen_actuarial_segmento(base_modelo, "grupo_antiguedad_vehiculo", por_anio = TRUE),
  zona_anio = resumen_actuarial_segmento(base_modelo, "area_label", por_anio = TRUE),
  experiencia_anio = resumen_actuarial_segmento(base_modelo, "grupo_experiencia", por_anio = TRUE),
  potencia_anio = resumen_actuarial_segmento(base_modelo, "grupo_potencia", por_anio = TRUE),
  valor_anio = resumen_actuarial_segmento(base_modelo, "grupo_valor_vehiculo", por_anio = TRUE),
  canal_anio = resumen_actuarial_segmento(base_modelo, "distribution_channel", por_anio = TRUE)
)

estabilidad_anual <- resumen_estabilidad_anual(tablas_segmentos_anio)

combinaciones_perfiles <- t(combn(variables_segmentacion, 2)) |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("variable_1", "variable_2")) |>
  mutate(nombre = sprintf("perfil_%02d", row_number()), .before = 1)

tablas_perfiles_bivariados <- pmap(
  combinaciones_perfiles,
  \(nombre, variable_1, variable_2) {
    resumen_actuarial_perfil2(base_modelo, variable_1, variable_2)
  }
) |>
  set_names(combinaciones_perfiles$nombre)

tablas_perfiles_bivariados_anio <- pmap(
  combinaciones_perfiles,
  \(nombre, variable_1, variable_2) {
    resumen_actuarial_perfil2_anio(base_modelo, variable_1, variable_2)
  }
) |>
  set_names(paste0(combinaciones_perfiles$nombre, "_anio"))

top_perfiles_riesgo <- bind_rows(tablas_perfiles_bivariados) |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(desc(score_riesgo), desc(n_polizas)) |>
  slice_head(n = 10)

top_perfiles_seguros <- bind_rows(tablas_perfiles_bivariados) |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(score_riesgo, desc(n_polizas)) |>
  slice_head(n = 10)

generar_perfiles_por_tamano <- function(data, variables, tamano) {
  combn(variables, tamano, simplify = FALSE) |>
    map_dfr(~ resumen_actuarial_perfil_n(data, .x))
}

perfiles_1_var <- generar_perfiles_por_tamano(base_modelo, variables_segmentacion, 1)
perfiles_2_var <- generar_perfiles_por_tamano(base_modelo, variables_segmentacion, 2)
perfiles_3_var <- generar_perfiles_por_tamano(base_modelo, variables_segmentacion, 3)
perfiles_4_var <- generar_perfiles_por_tamano(base_modelo, variables_segmentacion, 4)

generar_predicciones_por_tamano <- function(data, variables, tamano) {
  combn(variables, tamano, simplify = FALSE) |>
    map_dfr(~ resumen_predicciones_segmento(data, .x))
}

predicciones_1_var <- generar_predicciones_por_tamano(test_prima_pura, variables_segmentacion, 1)
predicciones_2_var <- generar_predicciones_por_tamano(test_prima_pura, variables_segmentacion, 2)
predicciones_3_var <- generar_predicciones_por_tamano(test_prima_pura, variables_segmentacion, 3)
predicciones_4_var <- generar_predicciones_por_tamano(test_prima_pura, variables_segmentacion, 4)

top_10_riesgosos_1_var <- perfiles_1_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(desc(score_riesgo), desc(n_polizas)) |>
  slice_head(n = 10)

top_10_seguros_1_var <- perfiles_1_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(score_riesgo, desc(n_polizas)) |>
  slice_head(n = 10)

top_10_riesgosos_2_var <- perfiles_2_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(desc(score_riesgo), desc(n_polizas)) |>
  slice_head(n = 10)

top_10_seguros_2_var <- perfiles_2_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(score_riesgo, desc(n_polizas)) |>
  slice_head(n = 10)

top_10_riesgosos_3_var <- perfiles_3_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(desc(score_riesgo), desc(n_polizas)) |>
  slice_head(n = 10)

top_10_seguros_3_var <- perfiles_3_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(score_riesgo, desc(n_polizas)) |>
  slice_head(n = 10)

top_10_riesgosos_4_var <- perfiles_4_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(desc(score_riesgo), desc(n_polizas)) |>
  slice_head(n = 10)

top_10_seguros_4_var <- perfiles_4_var |>
  filter(factor_credibilidad >= 0.5) |>
  arrange(score_riesgo, desc(n_polizas)) |>
  slice_head(n = 10)

dir.create("../05. Res/modelos", showWarnings = FALSE, recursive = TRUE)

guardar_xlsx <- function(tabla, archivo) {
  openxlsx::write.xlsx(
    x = as.data.frame(tabla),
    file = archivo,
    overwrite = TRUE,
    asTable = TRUE
  )
}

guardar_libro_xlsx <- function(tablas, archivo) {
  openxlsx::write.xlsx(
    x = lapply(tablas, as.data.frame),
    file = archivo,
    overwrite = TRUE,
    asTable = TRUE
  )
}

guardar_libro_xlsx(
  tablas_segmentos,
  "../05. Res/modelos/indicadores_actuariales_segmentos.xlsx"
)
guardar_libro_xlsx(
  c(tablas_segmentos_anio, list(estabilidad_anual = estabilidad_anual)),
  "../05. Res/modelos/indicadores_actuariales_anio.xlsx"
)
guardar_libro_xlsx(
  c(
    list(
      top_10_riesgosos = top_perfiles_riesgo,
      top_10_seguros = top_perfiles_seguros
    ),
    tablas_perfiles_bivariados
  ),
  "../05. Res/modelos/perfiles_riesgo_bivariados.xlsx"
)
guardar_libro_xlsx(
  tablas_perfiles_bivariados_anio,
  "../05. Res/modelos/perfiles_riesgo_bivariados_anio.xlsx"
)
guardar_libro_xlsx(
  list(
    top10_riesgosos_1var = top_10_riesgosos_1_var,
    top10_seguros_1var = top_10_seguros_1_var,
    top10_riesgosos_2var = top_10_riesgosos_2_var,
    top10_seguros_2var = top_10_seguros_2_var,
    top10_riesgosos_3var = top_10_riesgosos_3_var,
    top10_seguros_3var = top_10_seguros_3_var,
    top10_riesgosos_4var = top_10_riesgosos_4_var,
    top10_seguros_4var = top_10_seguros_4_var,
    perfiles_1_variable = perfiles_1_var,
    perfiles_2_variables = perfiles_2_var,
    perfiles_3_variables = perfiles_3_var,
    perfiles_4_variables = perfiles_4_var
  ),
  "../05. Res/modelos/perfiles_riesgo_1a4_variables.xlsx"
)
guardar_libro_xlsx(
  list(
    predicciones_1_variable = predicciones_1_var,
    predicciones_2_variables = predicciones_2_var,
    predicciones_3_variables = predicciones_3_var,
    predicciones_4_variables = predicciones_4_var
  ),
  "../05. Res/modelos/predicciones_promedio_segmentos_1a4.xlsx"
)
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
    comparativo_prima_pura = comparativo_prima_pura,
    tablas_segmentos = tablas_segmentos,
    tablas_segmentos_anio = tablas_segmentos_anio,
    estabilidad_anual = estabilidad_anual,
    tablas_perfiles_bivariados = tablas_perfiles_bivariados,
    tablas_perfiles_bivariados_anio = tablas_perfiles_bivariados_anio,
    top_perfiles_riesgo = top_perfiles_riesgo,
    top_perfiles_seguros = top_perfiles_seguros,
    perfiles_1_var = perfiles_1_var,
    perfiles_2_var = perfiles_2_var,
    perfiles_3_var = perfiles_3_var,
    perfiles_4_var = perfiles_4_var,
    predicciones_1_var = predicciones_1_var,
    predicciones_2_var = predicciones_2_var,
    predicciones_3_var = predicciones_3_var,
    predicciones_4_var = predicciones_4_var
  ),
  "../05. Res/modelos/modelos_finales.rds"
)

print(particion)
print(comparativo)
print(clasificacion_logit)
print(sobredispersion_poisson)
print(comparativo_prima_pura)
