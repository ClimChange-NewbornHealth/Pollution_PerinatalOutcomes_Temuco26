# 5.0 DLM Perinatal Outcomes - Model Estimation ----
# Distributed Lag Models (DLM) for perinatal outcomes.
# Estimates logit and Cox models per gestational week (1-39) using exposure + lagged exposure.
# Uses fit_logit_model and fit_cox_model from 0.4 Functions_models.R (Adjusted only).
# Saves results to RData for use in 5.1 DLM_PO_tables_plots.R

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")
source("02_Code/0.4 Functions_models.R")

## 1 Load data ----

data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")

## 2 Prepare data ----

# Select outcomes, controls, and weekly exposure columns (exclude preconception w-*)
data <- data |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |>
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga",
         "edad_madre", "education", "health_insurance", "job", "first_birth", "para", "cesarea",
         "sexo_rn", "a_nac", "estacion", "comuna", "mes_nac",
         matches("^w[0-9]+_"),
         matches("^iqr_w[0-9]+_"),
        ) |>
  select(-"birth_extremely_preterm", -"birth_term", -"birth_posterm") |>
  filter(!is.na(lbw | tlbw | sga)) |>
  filter(edad_gest >= 28)

# Reorder w20 columns after w19 (if present)
if ("w20_PM25_cs" %in% names(data)) {
  data <- data |>
    relocate(c("w20_PM25_cs", "w20_PM25_sp"), .after = "w19_PM25_sp") |>
    relocate(c("w20_Levo_cs", "w20_Levo_sp"), .after = "w19_Levo_sp") |>
    relocate(c("w20_K_cs", "w20_K_sp"), .after = "w19_K_sp")
}

## 3 Reshape wide to long for lag calculation ----

# Pivot weekly exposure columns to long: idbase, week, exposicion, contaminante, tipo (raw)
data_long <- data |>
  select(-starts_with("iqr_w")) |> 
  pivot_longer(
    cols = matches("^w[0-9]+_(PM25|Levo|K)_(cs|sp)$"),
    names_to = "col",
    values_to = "exposicion"
  ) |>
  mutate(
    week = as.numeric(stringr::str_extract(col, "[0-9]+")),
    contaminante = stringr::str_extract(col, "(PM25|Levo|K)"),
    tipo = stringr::str_extract(col, "(cs|sp)$")
  ) |>
  select(-col) 

# Pivot IQR-scaled weekly exposure columns (same structure)
data_long_iqr <- data |>
  select(-starts_with("w")) |> 
  pivot_longer(
    cols = matches("^iqr_w[0-9]+_(PM25|Levo|K)_(cs|sp)$"),
    names_to = "col",
    values_to = "exposicion"
  ) |>
  mutate(
    week = as.numeric(stringr::str_extract(col, "[0-9]+")),
    contaminante = stringr::str_extract(col, "(PM25|Levo|K)"),
    tipo = stringr::str_extract(col, "(cs|sp)$")
  ) |>
  select(-col)

## 4 Compute lagged exposure (weights from past weeks) ----
# Data are in long format here. For each idbase and contaminant-tipo, we compute
# exposicion_lagged = weighted sum of past exposures, with weights = 1 / (current_week - past_week).
# This section is documented in Spanish, line by line.

compute_lagged_exposure <- function(df) {
  df |>
    arrange(idbase, contaminante, tipo, week) |>
    group_by(idbase, contaminante, tipo) |>
    dplyr::mutate(
      # Para cada fila i, calculamos la exposición acumulada ponderada de semanas anteriores
      exposicion_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
        # Si estamos en semana 0, no hay semanas previas: retornamos NA
        if (week[i] == 0) return(NA_real_)
        # Índices de las filas correspondientes a semanas anteriores a la actual
        past_rows <- which(week < week[i])
        # Si no hay semanas previas, retornamos NA
        if (length(past_rows) == 0) return(NA_real_)
        # Pesos: inversamente proporcionales a la distancia temporal (semanas más recientes pesan más)
        # weight = 1 / (semana_actual - semana_pasada)
        weights <- 1 / (week[i] - week[past_rows])
        # Exposiciones en esas semanas pasadas
        exposures <- exposicion[past_rows]
        # Exposición lagged = suma ponderada (excluimos NA)
        sum(weights * exposures, na.rm = TRUE)
      })
    ) |>
    ungroup()
}

data_long <- compute_lagged_exposure(data_long)
data_long_iqr <- compute_lagged_exposure(data_long_iqr)

## 5 Reshape back to wide for modeling ----

# Pivot exposure and lagged exposure to wide: exposicion_1, exposicion_lagged_1, etc.
data_wide_expo <- data_long |>
  select(idbase, contaminante, tipo, week, exposicion, exposicion_lagged) |>
  pivot_wider(
    names_from = week,
    values_from = c(exposicion, exposicion_lagged),
    names_glue = "{.value}_{week}"
  )

data_wide_expo_iqr <- data_long_iqr |>
  select(idbase, contaminante, tipo, week, exposicion, exposicion_lagged) |>
  pivot_wider(
    names_from = week,
    values_from = c(exposicion, exposicion_lagged),
    names_glue = "{.value}_{week}"
  )

## 6 Define variables and grid ----

dependent_vars <- c(
  colnames(data)[stringr::str_detect(colnames(data), pattern = "birth_.*")],
  "lbw", "tlbw", "sga"
)
control_vars <- c("edad_madre", "education", "health_insurance", "job", "first_birth", "sexo_rn", "a_nac", "mes_nac", "comuna")
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")
exposure_scales <- c("raw", "iqr")  # raw = sin IQR, iqr = con IQR

# Analysis weeks: 1 to 39 only (exclude week 0)
weeks_analysis <- 1:37

## 7 Fit DLM models per contaminant, type, outcome, exposure scale ----

# Ensure control_vars exists for fit_logit_model and fit_cox_model
# (they read it from the calling environment)

results_logit <- list()
results_cox <- list()

tic("DLM model estimation")
for (scale in exposure_scales) {
  data_wide_expo_use <- if (scale == "raw") data_wide_expo else data_wide_expo_iqr

  for (contam in contaminants) {
    for (tipo_val in types) {

      # Build wide dataset for this contaminant-tipo
      wide_one <- data_wide_expo_use |>
        filter(contaminante == contam, tipo == tipo_val) |>
        select(-contaminante, -tipo)

      # Merge with outcomes and controls (one row per idbase)
      base_cols <- c("idbase", "edad_gest", dependent_vars, control_vars)
      data_model <- data |>
        select(any_of(base_cols)) |>
        distinct(idbase, .keep_all = TRUE) |>
        left_join(wide_one, by = "idbase") |>
        filter(!is.na(edad_madre), sexo_rn != "Indefinido")

      expo_cols <- paste0("exposicion_", weeks_analysis)
      lag_cols <- paste0("exposicion_lagged_", weeks_analysis)

      # Keep only weeks that exist in data
      expo_avail <- expo_cols[expo_cols %in% names(data_model)]
      lag_avail <- lag_cols[lag_cols %in% names(data_model)]
      common <- intersect(
        as.numeric(stringr::str_remove(expo_avail, "exposicion_")),
        as.numeric(stringr::str_remove(lag_avail, "exposicion_lagged_"))
      )
      common <- sort(common[common >= 1 & common <= 39])

      for (dep_var in dependent_vars) {

        key <- paste(dep_var, contam, tipo_val, scale, sep = "_")
        results_logit[[key]] <- data.frame()
        results_cox[[key]] <- data.frame()

        for (w in common) {
          exp_var <- paste0("exposicion_", w)
          lag_var <- paste0("exposicion_lagged_", w)
          if (!exp_var %in% names(data_model) || !lag_var %in% names(data_model)) next

          predictor <- paste(exp_var, lag_var, sep = " + ")
          tiempo <- paste0("w", w)

          # Logit (model_type != "single" so predictor is split into terms; we keep only exposicion_w)
          tbl_logit <- fit_logit_model(
            dependent = dep_var,
            predictor = predictor,
            tiempo = tiempo,
            contaminante = contam,
            tipo = tipo_val,
            model_type = "t1_t2_t3",
            data = data_model,
            adjustment = "Adjusted"
          )
          # Keep only the current-week exposure term (not the lagged)
          tbl_logit <- tbl_logit |> filter(term == exp_var)
          if (nrow(tbl_logit) > 0) {
            tbl_logit$week <- w
            tbl_logit$exposure_scale <- scale
            results_logit[[key]] <- rbind(results_logit[[key]], tbl_logit)
          }

          # Cox (model_type != "single" so predictor is split into terms; we keep only exposicion_w)
          tbl_cox <- fit_cox_model(
            dependent = dep_var,
            predictor = predictor,
            tiempo = tiempo,
            contaminante = contam,
            tipo = tipo_val,
            model_type = "t1_t2_t3",
            data = data_model,
            adjustment = "Adjusted"
          )
          tbl_cox <- tbl_cox |> filter(term == exp_var)
          if (nrow(tbl_cox) > 0) {
            tbl_cox$week <- w
            tbl_cox$exposure_scale <- scale
            results_cox[[key]] <- rbind(results_cox[[key]], tbl_cox)
          }
        }
      }
    }
  }
}
toc() #  3944,692 sec elapsed ~65 min approx

## 8 Save results ----

dlm_results <- list(
  results_logit = results_logit,
  results_cox = results_cox,
  dependent_vars = dependent_vars,
  contaminants = contaminants,
  types = types,
  exposure_scales = exposure_scales,
  weeks_analysis = weeks_analysis
)

save(dlm_results, file = "03_Output/DLM/DLM_PO_results.RData")
