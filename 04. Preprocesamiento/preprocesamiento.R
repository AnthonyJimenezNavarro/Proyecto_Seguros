
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  lubridate,
  janitor,
  readr
)

set.seed(20260627)

dir.create("../05. Res", showWarnings = FALSE, recursive = TRUE)

base_motor <- read_delim(
  "../02. Data/Motor vehicle insurance data(2).csv",
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ","),
  show_col_types = FALSE
) |>
  clean_names()

claims_type <- read_delim(
  "../02. Data/sample type claim(2).csv",
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ","),
  show_col_types = FALSE
) |>
  clean_names()

base_modelo <- base_motor |>
  mutate(
    across(
      c(date_birth, date_driving_licence, date_last_renewal,
        date_next_renewal, date_start_contract, date_lapse),
      ~ dmy(.x)
    ),
    edad_asegurado = as.numeric(interval(date_birth, date_last_renewal) / years(1)),
    experiencia_conduccion = as.numeric(interval(date_driving_licence, date_last_renewal) / years(1)),
    anio_renovacion = year(date_last_renewal),
    antiguedad_vehiculo = anio_renovacion - year_matriculation,
    claim_flag = as.integer(n_claims_year > 0),
    area_label = factor(area, levels = c(0, 1), labels = c("Rural", "Urbana")),
    type_risk = factor(
      type_risk,
      levels = c(1, 2, 3, 4),
      labels = c("Motocicleta", "Furgoneta", "Automovil particular", "Agricola")
    ),
    distribution_channel = factor(
      distribution_channel,
      levels = c(0, 1),
      labels = c("Agente", "Corredor")
    ),
    type_fuel = factor(type_fuel),
    payment = factor(payment),
    second_driver = factor(second_driver),
    grupo_edad = cut(
      edad_asegurado,
      breaks = c(-Inf, 25, 35, 45, 55, 65, Inf),
      labels = c("<=25", "26-35", "36-45", "46-55", "56-65", ">65")
    ),
    grupo_experiencia = cut(
      experiencia_conduccion,
      breaks = c(-Inf, 2, 5, 10, 20, Inf),
      labels = c("<=2", "3-5", "6-10", "11-20", ">20")
    ),
    grupo_potencia = cut(
      power,
      breaks = c(-Inf, 75, 100, 150, Inf),
      labels = c("Baja", "Media", "Alta", "Muy alta")
    ),
    grupo_valor_vehiculo = cut(
      value_vehicle,
      breaks = quantile(value_vehicle, probs = c(0, .25, .50, .75, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Bajo", "Medio-bajo", "Medio-alto", "Alto")
    )
  ) |>
  filter(
    !is.na(edad_asegurado),
    !is.na(experiencia_conduccion),
    edad_asegurado >= 18,
    experiencia_conduccion >= 0,
    antiguedad_vehiculo >= 0,
    premium > 0,
    cost_claims_year >= 0,
    n_claims_year >= 0
  )

claims_resumen <- claims_type |>
  group_by(id) |>
  summarise(
    n_tipos_reclamo = n_distinct(claims_type),
    costo_total_detallado = sum(cost_claims_by_type, na.rm = TRUE),
    costo_prom_detallado = mean(cost_claims_by_type, na.rm = TRUE),
    tipo_principal = claims_type[which.max(cost_claims_by_type)],
    .groups = "drop"
  ) |>
  mutate(
    costo_detallado = costo_total_detallado
  )

base_integrada <- base_modelo |>
  left_join(claims_resumen, by = "id") |>
  mutate(
    n_tipos_reclamo = replace_na(n_tipos_reclamo, 0L),
    costo_total_detallado = replace_na(costo_total_detallado, 0),
    costo_prom_detallado = replace_na(costo_prom_detallado, 0),
    costo_detallado = replace_na(costo_detallado, 0),
    tipo_principal = replace_na(tipo_principal, "sin reclamo")
  )

saveRDS(base_modelo, "../05. Res/base_modelo.rds")
saveRDS(base_integrada, "../05. Res/base_integrada.rds")
saveRDS(claims_type, "../05. Res/claims_type.rds")

write_csv(base_modelo, "../05. Res/base_modelo.csv")
write_csv(base_integrada, "../05. Res/base_integrada.csv")
