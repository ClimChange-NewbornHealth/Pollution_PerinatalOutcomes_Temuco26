# 3.0 Tables exposure models -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")

## 1. Load results ----

models_logit <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "logit_models")
models_cox  <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "cox_models")

## 2. Prepare table data ----

prepare_table_data <- function(models_df) {
  models_df |>
    dplyr::filter(
      (model_type == "single" & tiempo %in% c("tot", "w20")) |
        model_type == "t1_t2_t3"
    ) |>
    dplyr::arrange(dependent_var, contaminante, tipo, adjustment, model_type, term) |>
    dplyr::group_by(dependent_var, contaminante, tipo, adjustment, model_type) |>
    dplyr::mutate(
      exposure = dplyr::case_when(
        model_type == "single" & stringr::str_detect(term, "^tot_")  ~ "Overall",
        model_type == "single" & stringr::str_detect(term, "^w20_") ~ "W20",
        model_type == "t1_t2_t3" & dplyr::row_number() == 1 ~ "T1",
        model_type == "t1_t2_t3" & dplyr::row_number() == 2 ~ "T2",
        model_type == "t1_t2_t3" & dplyr::row_number() == 3 ~ "T3",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(exposure))
}

table_data_logit <- prepare_table_data(models_logit)
table_data_cox  <- prepare_table_data(models_cox)

## 3. Outcome labels ----

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
  "birth_preterm"            = "Preterm birth",
  "birth_very_preterm"       = "Very preterm birth",
  "birth_moderately_preterm" = "Moderately preterm birth",
  "birth_late_preterm"       = "Late preterm birth",
  "lbw"                      = "Low birth weight",
  "tlbw"                     = "Very low birth weight",
  "sga"                      = "Small for gestational age"
)

exposure_order <- c("W20", "T1", "T2", "T3", "Overall")

## 4. Build combined table (all contaminants, 12 exposure columns) ----

# Format: "OR (IC)" or "HR (IC)" with 3 decimals, period as decimal separator
format_effect_ci <- function(estimate, conf_low, conf_high) {
  sprintf("%.3f (%.3f-%.3f)", estimate, conf_low, conf_high)
}

# Column order: PM2.5 cs, Levo cs, K cs, PM2.5 sp, Levo sp, K sp (each unadjusted, adjusted)
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
    tidyr::pivot_wider(
      names_from = col_name,
      values_from = value_fmt
    ) |>
    dplyr::arrange(outcome, exposure) |>
    dplyr::mutate(
      outcome = outcomes_labels[as.character(outcome)],
      exposure = as.character(exposure)
    )

  # Select and order columns
  col_order_avail <- col_order[col_order %in% names(data_wide)]
  data_wide <- data_wide |> dplyr::select(dplyr::all_of(col_order_avail))

  # Rename to final labels
  for (i in seq_along(col_labels)) {
    old_nm <- names(col_labels)[i]
    new_nm <- col_labels[i]
    if (old_nm %in% names(data_wide)) {
      names(data_wide)[names(data_wide) == old_nm] <- new_nm
    }
  }

  data_wide
}

## 5. Generate single Excel file (OR + HR sheets, 12 exposure columns) ----

tab_or <- build_table_all(table_data_logit)
tab_hr <- build_table_all(table_data_cox)

writexl::write_xlsx(
  list(OR = tab_or, HR = tab_hr),
  path = "03_Output/Models/Tables/Tab_Exposure_PO.xlsx"
)
message("Saved: 03_Output/Models/Tables/Tab_Exposure_PO.xlsx")
