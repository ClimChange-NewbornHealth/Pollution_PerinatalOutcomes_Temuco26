# 3.3 Sensitivity models: Cox for preterm birth by cesarean ----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions_models.R")

control_vars <- c("edad_madre", "education", "health_insurance", "job", "first_birth", "sexo_rn", "a_nac", "mes_nac", "comuna")

## 1 Load data ----

data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")

## 2 Prepare data for the models ----

data_model <- data |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |>
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga",
         "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac",
         "education", "health_insurance", "job", "first_birth", "para", "cesarea",
         starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_"),
         starts_with("iqr_")
  ) |>
  select(-"birth_extremely_preterm", -"birth_term", -"birth_posterm") |>
  filter(!is.na(lbw | tlbw | sga)) |>
  filter(edad_gest >= 28) |>
  mutate(tstart = 27) |>
  filter(!is.na(cesarea))

## 3. Define grid: Cox models for birth_preterm only ----

dependent_vars <- "birth_preterm"
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")
model_types <- c("single", "pct1_t1_t2_t3", "t1_t2_t3")
exposure_scale <- c("raw", "iqr")

# Single models: tot, w20
combinations_single <- expand.grid(
  dependent = dependent_vars,
  tiempo = c("tot", "w20"),
  contaminante = contaminants,
  tipo = types,
  model_type = "single",
  adjustment = c("Unadjusted", "Adjusted"),
  exposure_scale = exposure_scale,
  stringsAsFactors = FALSE
) |>
  mutate(
    pref = if_else(exposure_scale == "iqr", "iqr_", ""),
    predictor = paste0(pref, tiempo, "_", contaminante, "_", tipo)
  ) |>
  select(-pref)

# t1_t2_t3 models
combinations_t1_t2_t3 <- expand.grid(
  dependent = dependent_vars,
  contaminante = contaminants,
  tipo = types,
  model_type = "t1_t2_t3",
  adjustment = c("Unadjusted", "Adjusted"),
  exposure_scale = exposure_scale,
  stringsAsFactors = FALSE
) |>
  mutate(
    pref = if_else(exposure_scale == "iqr", "iqr_", ""),
    predictor = paste0(pref, "t1_", contaminante, "_", tipo, " + ",
                      pref, "t2_", contaminante, "_", tipo, " + ",
                      pref, "t3_", contaminante, "_", tipo),
    tiempo = "t1_t2_t3"
  ) |>
  select(-pref)

available_predictors <- names(data_model)[grepl("(_PM25_|_Levo_|_K_)", names(data_model))]

combinations_single_valid <- combinations_single |>
  filter(predictor %in% available_predictors)

combinations_multi <- combinations_t1_t2_t3 |>
  rowwise() |>
  mutate(
    predictors_list = list(stringr::str_split(predictor, " \\+ ")[[1]]),
    all_exist = all(trimws(predictors_list) %in% available_predictors)
  ) |>
  ungroup() |>
  filter(all_exist) |>
  select(-predictors_list, -all_exist)

combinations <- bind_rows(combinations_single_valid, combinations_multi)

## 4. Run Cox models by cesarea (0 = Spontaneous, 1 = Cesarean) ----

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)

cesarea_levels <- c(0, 1)
cesarea_labels <- c("0" = "Spontaneous", "1" = "Cesarean")

results_cox_by_cesarea <- list()

for (ces_val in cesarea_levels) {
  data_sub <- data_model |> filter(cesarea == ces_val)
  message("Running Cox models for cesarea = ", ces_val, " (n = ", nrow(data_sub), ")")

  tic()
  results_list_cox <- future_lapply(seq_len(nrow(combinations)), function(i) {
    dep <- combinations$dependent[i]
    tiempo <- combinations$tiempo[i]
    contaminante <- combinations$contaminante[i]
    tipo <- combinations$tipo[i]
    pred <- combinations$predictor[i]
    model_type <- combinations$model_type[i]
    adj <- combinations$adjustment[i]
    exp_scale <- combinations$exposure_scale[i]

    res <- fit_cox_model(
      dependent = dep,
      predictor = pred,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      data = data_sub,
      adjustment = adj,
      time_start = "tstart"
    )
    res$exposure_scale <- exp_scale
    res$cesarea <- ces_val
    res
  }, future.seed = TRUE)
  toc()

  results_cox_by_cesarea[[as.character(ces_val)]] <- bind_rows(results_list_cox)
}

plan(sequential)

results_cox <- bind_rows(results_cox_by_cesarea)

## 5. Save results ----

save(results_cox, file = "03_Output/Models/Exposure_models_PO_cox_cesarea_stratified.RData")

writexl::write_xlsx(
  list(cox_models = results_cox),
  path = "03_Output/Models/Exposure_models_PO_cox_cesarea_stratified.xlsx"
)
message("Saved: 03_Output/Models/Exposure_models_PO_cox_cesarea_stratified.xlsx")

## 6. Generate tables (HR only, like 3.2 code) ----

# Prepare table data: filter single (tot, w20) and t1_t2_t3
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
        model_type == "single" & (stringr::str_detect(term, "tot_") | stringr::str_detect(term, "iqr_tot_"))  ~ "Overall",
        model_type == "single" & (stringr::str_detect(term, "w20_") | stringr::str_detect(term, "iqr_w20_")) ~ "W20",
        model_type == "t1_t2_t3" & dplyr::row_number() == 1 ~ "T1",
        model_type == "t1_t2_t3" & dplyr::row_number() == 2 ~ "T2",
        model_type == "t1_t2_t3" & dplyr::row_number() == 3 ~ "T3",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(exposure))
}

exposure_order <- c("W20", "T1", "T2", "T3", "Overall")

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

build_table_hr <- function(data_all) {
  data_all <- data_all |>
    dplyr::mutate(
      exposure = factor(exposure, levels = exposure_order),
      value_fmt = format_effect_ci(estimate, conf.low, conf.high),
      col_name = paste0(contaminante, "_", tipo, "_", adjustment)
    )

  data_wide <- data_all |>
    dplyr::select(outcome = dependent_var, exposure, col_name, value_fmt) |>
    tidyr::pivot_wider(
      names_from = col_name,
      values_from = value_fmt
    ) |>
    dplyr::arrange(outcome, exposure) |>
    dplyr::mutate(
      outcome = "Preterm birth",
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

# Tables by cesarea (raw scale)
dir.create("03_Output/Models/Tables", showWarnings = FALSE, recursive = TRUE)

tables_list <- list()
for (ces_val in cesarea_levels) {
  tab_data <- results_cox |>
    filter(cesarea == ces_val, exposure_scale == "raw") |>
    prepare_table_data()

  tab_hr <- build_table_hr(tab_data)
  tables_list[[cesarea_labels[as.character(ces_val)]]] <- tab_hr
}

writexl::write_xlsx(
  tables_list,
  path = "03_Output/Models/Tables/Tab_Exposure_PO_cox_cesarea_stratified.xlsx"
)
message("Saved: 03_Output/Models/Tables/Tab_Exposure_PO_cox_cesarea_stratified.xlsx")

# IQR version
tables_list_iqr <- list()
for (ces_val in cesarea_levels) {
  tab_data <- results_cox |>
    filter(cesarea == ces_val, exposure_scale == "iqr") |>
    prepare_table_data()

  tab_hr_iqr <- build_table_hr(tab_data)
  tables_list_iqr[[paste0(cesarea_labels[as.character(ces_val)], "_IQR")]] <- tab_hr_iqr
}

writexl::write_xlsx(
  tables_list_iqr,
  path = "03_Output/Models/Tables/Tab_Exposure_PO_cox_cesarea_stratified_IQR.xlsx"
)
message("Saved: 03_Output/Models/Tables/Tab_Exposure_PO_cox_cesarea_stratified_IQR.xlsx")
