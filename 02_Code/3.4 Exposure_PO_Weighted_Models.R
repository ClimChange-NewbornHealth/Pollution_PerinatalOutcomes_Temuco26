# 3.4 Modelos de exposición — estimación ponderada (w_poststrat) -----
#
# Secuencia: (1) configuración y paquetes, (2) carga de datos ya ponderados,
# (3) preparación de data_model, (4) grilla de modelos, (5) logit ponderado,
# (6) Cox ponderado, (7) guardado de resultados en 03_Output/Weighted_analysis,
# (8) tablas resumen OR/HR (escala raw e IQR).
#
# Estimación: survey::svyglm(..., family = quasibinomial) y survey::svycoxph
# con diseño ids = ~1 y pesos = w_poststrat (Lumley, Complex Surveys).

## 1 Settings y paquetes ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")
source("02_Code/0.4 Functions_models.R")

# Opciones survey (diseño sin conglomerados explícitos)
options(survey.lonely.psu = "adjust")

out_dir <- "03_Output/Weighted_analysis"

## 2 Carga de datos (muestra con w_poststrat) ----
w <- rio::import("01_Input/Data_full_sample_exposure_analysis_weighted.RData") |> select(idbase, w_poststrat)
data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")

glimpse(w)
glimpse(data)

## 3 Preparación para modelos ----

data_model <- data |>
  left_join(w, by = "idbase") |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |> 
  dplyr::select(
    "idbase", "w_poststrat", "edad_gest", dplyr::starts_with("birth_"), "lbw", "tlbw", "sga",
    "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "mes_nac",
    "education", "health_insurance", "job", "first_birth", "para", "cesarea",
    dplyr::starts_with("pct1_"), dplyr::starts_with("t1_"), dplyr::starts_with("t2_"),
    dplyr::starts_with("t3_"), dplyr::starts_with("w20_"), dplyr::starts_with("tot_"),
    dplyr::starts_with("iqr_")
  ) |>
  dplyr::select(-dplyr::any_of(c("birth_extremely_preterm", "birth_term", "birth_posterm"))) |>
  dplyr::filter(!is.na(lbw | tlbw | sga)) |>
  dplyr::filter(edad_gest >= 28) |>
  dplyr::mutate(tstart = 27)

glimpse(data_model)
summary(data_model)

## 4 Grilla de modelos ----
dependent_vars <- c(
  colnames(data_model)[stringr::str_detect(colnames(data_model), pattern = "birth_.*")],
  "lbw", "tlbw", "sga"
)
dependent_vars

control_vars <- c(
  "edad_madre", "education", "health_insurance", "job", "first_birth",
  "sexo_rn", "a_nac", "mes_nac", "comuna"
)

time_periods <- c("pct1", "t1", "t2", "t3", "w20", "tot")
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")
model_types <- c("single", "pct1_t1_t2_t3", "t1_t2_t3")
exposure_scale <- c("raw", "iqr")

combinations_single <- expand.grid(
  dependent = dependent_vars,
  tiempo = time_periods,
  contaminante = contaminants,
  tipo = types,
  model_type = "single",
  adjustment = c("Unadjusted", "Adjusted"),
  exposure_scale = exposure_scale,
  stringsAsFactors = FALSE
)

combinations_single <- combinations_single |>
  dplyr::mutate(
    pref = dplyr::if_else(exposure_scale == "iqr", "iqr_", ""),
    predictor = paste0(pref, tiempo, "_", contaminante, "_", tipo)
  ) |>
  dplyr::select(-pref)

combinations_pct1_t1_t2_t3 <- expand.grid(
  dependent = dependent_vars,
  contaminante = contaminants,
  tipo = types,
  model_type = "pct1_t1_t2_t3",
  adjustment = c("Unadjusted", "Adjusted"),
  exposure_scale = exposure_scale,
  stringsAsFactors = FALSE
)

combinations_pct1_t1_t2_t3 <- combinations_pct1_t1_t2_t3 |>
  dplyr::mutate(
    pref = dplyr::if_else(exposure_scale == "iqr", "iqr_", ""),
    predictor = paste0(
      pref, "pct1_", contaminante, "_", tipo, " + ", pref, "t1_", contaminante, "_", tipo,
      " + ", pref, "t2_", contaminante, "_", tipo, " + ", pref, "t3_", contaminante, "_", tipo
    ),
    tiempo = "pct1_t1_t2_t3"
  ) |>
  dplyr::select(-pref)

combinations_t1_t2_t3 <- expand.grid(
  dependent = dependent_vars,
  contaminante = contaminants,
  tipo = types,
  model_type = "t1_t2_t3",
  adjustment = c("Unadjusted", "Adjusted"),
  exposure_scale = exposure_scale,
  stringsAsFactors = FALSE
)

combinations_t1_t2_t3 <- combinations_t1_t2_t3 |>
  dplyr::mutate(
    pref = dplyr::if_else(exposure_scale == "iqr", "iqr_", ""),
    predictor = paste0(
      pref, "t1_", contaminante, "_", tipo, " + ", pref, "t2_", contaminante, "_", tipo,
      " + ", pref, "t3_", contaminante, "_", tipo
    ),
    tiempo = "t1_t2_t3"
  ) |>
  dplyr::select(-pref)

combinations <- dplyr::bind_rows(
  combinations_single,
  combinations_pct1_t1_t2_t3,
  combinations_t1_t2_t3
)

available_predictors <- names(data_model)[grepl("(_PM25_|_Levo_|_K_)", names(data_model))]

combinations_single_valid <- combinations |>
  dplyr::filter(model_type == "single") |>
  dplyr::filter(predictor %in% available_predictors)

combinations_multi <- combinations |>
  dplyr::filter(model_type != "single") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    predictors_list = list(stringr::str_split(predictor, " \\+ ")[[1]]),
    all_exist = all(trimws(predictors_list) %in% available_predictors)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(all_exist) |>
  dplyr::select(-predictors_list, -all_exist)

combinations <- dplyr::bind_rows(combinations_single_valid, combinations_multi)

writexl::write_xlsx(
  combinations,
  path = file.path(out_dir, "List_models_exposure_PO_weighted.xlsx")
)

## 5 Modelos Cox ponderados (paralelo) ----
plan(multisession, workers = max(1L, parallel::detectCores() - 4L))
options(future.globals.maxSize = 1.5 * 1024^3)

tic()
results_list_cox <- future.apply::future_lapply(seq_len(nrow(combinations)), function(i) {
  message("Cox ponderado ", i, "/", nrow(combinations), " PID ", Sys.getpid())
  dep <- combinations$dependent[i]
  tiempo <- combinations$tiempo[i]
  contaminante <- combinations$contaminante[i]
  tipo <- combinations$tipo[i]
  pred <- combinations$predictor[i]
  model_type <- combinations$model_type[i]
  adj <- combinations$adjustment[i]
  exp_scale <- combinations$exposure_scale[i]

  res <- fit_cox_model_weighted(
    dependent = dep,
    predictor = pred,
    tiempo = tiempo,
    contaminante = contaminante,
    tipo = tipo,
    model_type = model_type,
    data = data_model,
    weight_var = "w_poststrat",
    adjustment = adj,
    time_start = "tstart"
  )
  res$exposure_scale <- exp_scale
  res
}, future.seed = TRUE)
toc()

plan(sequential)

## 6 Resultados agregados y guardado ----
results_cox <- dplyr::bind_rows(results_list_cox)

models_cox <- results_cox

save(
  results_list_cox,
  file = file.path(out_dir, "Exposure_models_PO_cox_weighted.RData")
)

writexl::write_xlsx(
  list(cox_models = results_cox),
  path = file.path(out_dir, "Exposure_models_PO_cox_weighted.xlsx")
)

## 7 Datos para tablas (misma lógica que 3.0, escala raw / IQR) ----
prepare_table_data <- function(models_df) {
  models_df |>
    dplyr::filter(
      (model_type == "single" & tiempo %in% c("tot", "w20")) |
        model_type == "t1_t2_t3"
    ) |>
    dplyr::arrange(dependent_var, contaminante, tipo, adjustment, model_type, term) |>
    dplyr::group_by(dependent_var, contaminante, tipo, adjustment, model_type, exposure_scale) |>
    dplyr::mutate(
      exposure = dplyr::case_when(
        model_type == "single" & (stringr::str_detect(term, "tot_") | stringr::str_detect(term, "iqr_tot_")) ~ "Overall",
        model_type == "single" & (stringr::str_detect(term, "w20_") | stringr::str_detect(term, "iqr_w20_")) ~ "W20",
        model_type == "t1_t2_t3" & dplyr::row_number() == 1L ~ "T1",
        model_type == "t1_t2_t3" & dplyr::row_number() == 2L ~ "T2",
        model_type == "t1_t2_t3" & dplyr::row_number() == 3L ~ "T3",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(exposure))
}

get_table_data_by_scale <- function(models_df, scale) {
  prepare_table_data(models_df) |>
    dplyr::filter(exposure_scale == scale)
}

## 8 Etiquetas de desenlaces ----
outcomes_order <- c(
  "birth_preterm",
  "birth_very_preterm",
  "birth_moderately_preterm",
  "birth_late_preterm",
  "lbw",
  "tlbw",
  "sga"
)

outcomes_labels <- c(
  birth_preterm = "Preterm birth",
  birth_very_preterm = "Very preterm birth",
  birth_moderately_preterm = "Moderately preterm birth",
  birth_late_preterm = "Late preterm birth",
  lbw = "Low birth weight",
  tlbw = "Very low birth weight",
  sga = "Small for gestational age"
)

exposure_order <- c("W20", "T1", "T2", "T3", "Overall")

## 9 Tablas combinadas OR / HR (12 columnas de exposición) ----
format_effect_ci <- function(estimate, conf_low, conf_high) {
  sprintf("%.3f (%.3f-%.3f)", estimate, conf_low, conf_high)
}

col_order <- c(
  "outcome", "exposure",
  "PM25_cs_Unadjusted", "PM25_cs_Adjusted",
  "Levo_cs_Unadjusted", "Levo_cs_Adjusted",
  "K_cs_Unadjusted", "K_cs_Adjusted",
  "PM25_sp_Unadjusted", "PM25_sp_Adjusted",
  "Levo_sp_Unadjusted", "Levo_sp_Adjusted",
  "K_sp_Unadjusted", "K_sp_Adjusted"
)

col_labels <- c(
  outcome = "Outcome",
  exposure = "Exposure",
  PM25_cs_Unadjusted = "PM 2.5 unadjusted CS",
  PM25_cs_Adjusted = "PM 2.5 adjusted CS",
  Levo_cs_Unadjusted = "Levo unadjusted CS",
  Levo_cs_Adjusted = "Levo adjusted CS",
  K_cs_Unadjusted = "K unadjusted CS",
  K_cs_Adjusted = "K adjusted CS",
  PM25_sp_Unadjusted = "PM 2.5 unadjusted SP",
  PM25_sp_Adjusted = "PM 2.5 adjusted SP",
  Levo_sp_Unadjusted = "Levo unadjusted SP",
  Levo_sp_Adjusted = "Levo adjusted SP",
  K_sp_Unadjusted = "K unadjusted SP",
  K_sp_Adjusted = "K adjusted SP"
)

build_table_all <- function(data_all) {
  data_all <- data_all |>
    dplyr::mutate(
      exposure = factor(exposure, levels = exposure_order),
      dependent_var = factor(dependent_var, levels = outcomes_order),
      value_fmt = format_effect_ci(estimate, conf.low, conf.high),
      col_name = paste0(contaminante, "_", tipo, "_", adjustment)
    )

  data_wide <- data_all |>
    dplyr::select(outcome = dependent_var, exposure, col_name, value_fmt) |>
    tidyr::pivot_wider(names_from = col_name, values_from = value_fmt) |>
    dplyr::arrange(outcome, exposure) |>
    dplyr::mutate(
      outcome = outcomes_labels[as.character(outcome)],
      exposure = as.character(exposure)
    )

  col_order_avail <- col_order[col_order %in% names(data_wide)]
  data_wide <- data_wide |> dplyr::select(dplyr::all_of(col_order_avail))

  for (i in seq_along(col_labels)) {
    old_nm <- names(col_labels)[i]
    new_nm <- col_labels[i]
    if (old_nm %in% names(data_wide)) {
      names(data_wide)[names(data_wide) == old_nm] <- new_nm
    }
  }

  data_wide
}

table_data_cox <- get_table_data_by_scale(models_cox, "raw")
tab_hr <- build_table_all(table_data_cox)

writexl::write_xlsx(
  list(HR = tab_hr),
  path = file.path(out_dir, "Tab_Exposure_PO_weighted.xlsx")
)

## 10 Misma estructura con exposición escalada por IQR ----
table_data_cox_iqr <- get_table_data_by_scale(models_cox, "iqr")
tab_hr_iqr <- build_table_all(table_data_cox_iqr)

writexl::write_xlsx(
  list(HR = tab_hr_iqr),
  path = file.path(out_dir, "Tab_Exposure_PO_IQR_weighted.xlsx")
)

