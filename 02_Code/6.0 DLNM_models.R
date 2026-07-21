# 6.0 DLNM Perinatal Outcomes - PTB only ----
# Distributed Lag Non-linear Models (DLNM) with cohort cross-basis.
# Cox proportional hazards (aligned with 4.0 DLM_PO_estimation.R).
# Outcome: birth_preterm (PTB) only; 3 pollutants x cs/sp x raw/iqr.
# Four cross-basis specifications; gestational weeks 0-43; output weeks 1-43.
# Sample aligned with 4.0 (birth_preterm + lbw/tlbw/sga non-missing). Post-delivery NA -> 0 in Q.

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions_models.R")

if (!requireNamespace("dlnm", quietly = TRUE)) {
  install.packages("dlnm", repos = "https://cloud.r-project.org")
}
library(dlnm)

## 1 Load and prepare data (same as 4.0) ----

data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")

data <- data |>
  dplyr::mutate(mes_nac = lubridate::month(fecha_nac)) |>
  dplyr::select(
    "idbase", "edad_gest", "birth_preterm", "lbw", "tlbw", "sga",
    "edad_madre", "education", "health_insurance", "job", "first_birth", "para", "cesarea",
    "sexo_rn", "a_nac", "estacion", "comuna", "mes_nac",
    dplyr::matches("^w[0-9]+_"),
    dplyr::matches("^iqr_w[0-9]+_")
  ) |>
  dplyr::filter(!is.na(lbw | tlbw | sga)) |>
  dplyr::filter(!is.na(birth_preterm)) |>
  dplyr::filter(edad_gest >= 28) |>
  dplyr::mutate(tstart = 27)

if ("w20_PM25_cs" %in% names(data)) {
  data <- data |>
    dplyr::relocate(c("w20_PM25_cs", "w20_PM25_sp"), .after = "w19_PM25_sp") |>
    dplyr::relocate(c("w20_Levo_cs", "w20_Levo_sp"), .after = "w19_Levo_sp") |>
    dplyr::relocate(c("w20_K_cs", "w20_K_sp"), .after = "w19_K_sp")
}

## 2 Analysis grid ----

dependent_var <- "birth_preterm"
control_vars <- c(
  "edad_madre", "education", "health_insurance", "job", "first_birth",
  "sexo_rn", "a_nac", "mes_nac", "comuna"
)
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")
exposure_scales <- c("raw", "iqr")

gest_weeks <- 0:43
out_weeks <- 1:43

out_dir <- "03_Output/DLNM"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

lag_knots_trim <- list(fun = "ns", knots = c(13, 27))

crossbasis_specs <- list(
  lin_simple = list(
    label = "Linear exposure + NS lag (df=3)",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = 3)
  ),
  lin_knots1327 = list(
    label = "Linear exposure + NS lag (knots 13, 27)",
    argvar = list(fun = "lin"),
    arglag = lag_knots_trim
  ),
  bs2_knots1327 = list(
    label = "Quadratic B-spline exposure (df=3) + NS lag (knots 13, 27)",
    argvar = list("bs", degree = 2, df = 3),
    arglag = lag_knots_trim
  ),
  bs3_knots1327 = list(
    label = "Cubic B-spline exposure (df=4) + NS lag (knots 13, 27)",
    argvar = list("bs", degree = 3, df = 4),
    arglag = lag_knots_trim
  )
)

## 3 Helper functions ----

build_exposure_matrix <- function(data, contaminante, tipo, scale) {
  if (scale == "raw") {
    cols <- paste0("w", gest_weeks, "_", contaminante, "_", tipo)
  } else {
    cols <- paste0("iqr_w", gest_weeks, "_", contaminante, "_", tipo)
  }
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas de exposición: ", missing_cols[1])
  }
  Q <- as.matrix(data[, cols, drop = FALSE])
  # NA = weeks after delivery; treat as zero exposure in the cross-basis history
  Q[is.na(Q)] <- 0
  Q
}

prepare_analysis_data <- function(data) {
  data |>
    dplyr::filter(!is.na(.data[[dependent_var]])) |>
    dplyr::filter(!is.na(edad_madre), sexo_rn != "Indefinido") |>
    dplyr::filter(!is.na(edad_gest), !is.na(tstart)) |>
    dplyr::filter(.data$tstart < .data$edad_gest)
}

crosspred_contrast <- function(contaminante, Q, scale) {
  if (scale == "iqr") {
    list(cen = 0, at = 1)
  } else if (contaminante == "PM25") {
    cen <- stats::median(Q, na.rm = TRUE)
    list(cen = cen, at = cen + 10)
  } else {
    cen <- stats::median(Q, na.rm = TRUE)
    iqr <- stats::IQR(Q, na.rm = TRUE)
    if (!is.finite(iqr) || iqr <= 0) iqr <- 1
    list(cen = cen, at = cen + iqr)
  }
}

fit_dlnm_cox <- function(data, contaminante, tipo, scale, spec_id, spec) {
  df <- prepare_analysis_data(data)
  if (nrow(df) == 0) return(NULL)

  Q <- build_exposure_matrix(df, contaminante, tipo, scale)
  lag <- c(min(gest_weeks), max(gest_weeks))

  cb <- crossbasis(
    Q,
    lag = lag,
    argvar = spec$argvar,
    arglag = spec$arglag
  )

  available_controls <- control_vars[control_vars %in% names(df)]
  control_str <- paste(available_controls, collapse = " + ")
  fml <- stats::as.formula(
    paste0("survival::Surv(tstart, edad_gest, ", dependent_var, ") ~ cb + ", control_str)
  )

  mod <- tryCatch(
    survival::coxph(fml, data = df),
    error = function(e) NULL
  )
  if (is.null(mod)) return(NULL)

  contrast <- crosspred_contrast(contaminante, Q, scale)
  pred <- crosspred(cb, mod, cen = contrast$cen, at = contrast$at)

  lag_cols <- colnames(pred$matRRfit)
  lag_vals <- as.integer(sub("^lag", "", lag_cols))
  z975 <- stats::qnorm(0.975)
  log_fit <- as.numeric(pred$matfit[1, ])
  log_se <- as.numeric(pred$matse[1, ])
  rr <- as.numeric(pred$matRRfit[1, ])
  rr_l <- as.numeric(pred$matRRlow[1, ])
  rr_h <- as.numeric(pred$matRRhigh[1, ])

  res <- data.frame(
    Week = lag_vals,
    beta = log_fit,
    se = log_se,
    Lower = log_fit - z975 * log_se,
    Upper = log_fit + z975 * log_se,
    beta_exp = rr,
    Lower_exp = rr_l,
    Upper_exp = rr_h,
    stringsAsFactors = FALSE
  )
  res <- res[is.finite(res$beta_exp) & res$Week %in% out_weeks, , drop = FALSE]
  res$`No Obs` <- stats::nobs(mod)
  res$AIC <- stats::AIC(mod)
  res$BIC <- stats::BIC(mod)
  res$spec_id <- spec_id
  res$spec_label <- spec$label
  res$dependent <- dependent_var
  res$contaminante <- contaminante
  res$tipo <- tipo
  res$exposure_scale <- scale
  res$cen <- contrast$cen
  res$at <- contrast$at

  list(
    results = res,
    model = mod,
    crossbasis = cb,
    pred = pred,
    fit = data.frame(
      spec_id = spec_id,
      spec_label = spec$label,
      dependent = dependent_var,
      contaminante = contaminante,
      tipo = tipo,
      exposure_scale = scale,
      n = stats::nobs(mod),
      AIC = stats::AIC(mod),
      BIC = stats::BIC(mod),
      cen = contrast$cen,
      at = contrast$at,
      stringsAsFactors = FALSE
    )
  )
}

safe_sheet_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  substr(x, 1, 31)
}

## 4 Estimation ----

all_results <- list()
fit_table <- list()

tictoc::tic("DLNM Cox PTB estimation")
for (spec_id in names(crossbasis_specs)) {
  spec <- crossbasis_specs[[spec_id]]
  for (scale in exposure_scales) {
    for (contam in contaminants) {
      for (tipo_val in types) {
        model_key <- paste(spec_id, dependent_var, contam, tipo_val, scale, sep = "|")
        fit <- tryCatch(
          fit_dlnm_cox(
            data = data,
            contaminante = contam,
            tipo = tipo_val,
            scale = scale,
            spec_id = spec_id,
            spec = spec
          ),
          error = function(e) {
            warning("Fallo ", model_key, ": ", conditionMessage(e))
            NULL
          }
        )
        if (!is.null(fit)) {
          all_results[[model_key]] <- fit$results
          fit_table[[length(fit_table) + 1L]] <- fit$fit
        }
      }
    }
  }
}
tictoc::toc()

fit_df <- dplyr::bind_rows(fit_table)

## 5 Save results ----

dlnm_results <- list(
  all_results = all_results,
  fit_df = fit_df,
  dependent_var = dependent_var,
  contaminants = contaminants,
  types = types,
  exposure_scales = exposure_scales,
  crossbasis_specs = crossbasis_specs,
  out_weeks = out_weeks,
  gest_weeks = gest_weeks
)

save(dlnm_results, file = file.path(out_dir, "DLNM_PTB_results.RData"))

for (spec_id in names(crossbasis_specs)) {
  for (scale in exposure_scales) {
    spec_results <- all_results[
      grepl(paste0("^", spec_id, "\\|"), names(all_results)) &
        vapply(all_results, function(x) identical(x$exposure_scale[1], scale), logical(1))
    ]
    if (length(spec_results) == 0) next

    results_xlsx <- stats::setNames(
      spec_results,
      vapply(names(spec_results), function(s) {
        safe_sheet_name(sub(paste0("^", spec_id, "\\|", dependent_var, "\\|"), "", s))
      }, character(1))
    )

    scale_suffix <- if (scale == "raw") "" else "_IQR"
    writexl::write_xlsx(
      results_xlsx,
      path = file.path(out_dir, paste0("DLNM_PTB_", spec_id, scale_suffix, "_coef.xlsx"))
    )
  }
}

writexl::write_xlsx(
  list(Model_fit = fit_df),
  path = file.path(out_dir, "DLNM_PTB_model_fit_AIC_BIC.xlsx")
)

cat("DLNM Cox PTB analysis completed. Outputs in", out_dir, "\n")
