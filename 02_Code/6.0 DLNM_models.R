# 6.0 DLNM Perinatal Outcomes - PTB only ----
# Distributed Lag Non-linear Models (DLNM) with cohort cross-basis.
# Cox proportional hazards (aligned with 4.0 DLM_PO_estimation.R).
# Outcome: birth_preterm (PTB) only; 3 pollutants x cs/sp x IQR scale only.
# Eight cross-basis specifications; exposure lags 0-43; stored weeks 1-43; figures omit week 37.
# Post-delivery: exclude subjects with missing/non-finite in-utero exposure; then NA/Inf -> 0 in Q for crossbasis.

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
exposure_scales <- c("iqr")

gest_weeks <- 0:43
out_weeks <- 1:43

out_dir <- "03_Output/DLNM"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Eight cross-basis specifications (1 primary + sensitivity + secondary); gestational weeks 0-43.
# argvar: exposure-response shape (not temporal). arglag: gestational-week lag structure.
# Knots at weeks 13 and 27 = a priori flexibility near trimester transitions (continuous NS, not abrupt breaks).
# degree in argvar controls exposure-response only, not smoothness across gestational weeks.

primary_spec_id <- "lin_trimester"

lag_trimester <- list(
  fun = "ns",
  knots = c(13, 27)
)

crossbasis_specs <- list(
  lin_df3 = list(
    label = "Linear exposure + NS lag (df=3)",
    role = "sensitivity",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = 3)
  ),
  lin_trimester = list(
    label = "Linear exposure + NS lag (knots 13, 27)",
    role = "primary",
    argvar = list(fun = "lin"),
    arglag = lag_trimester
  ),
  lin_df4 = list(
    label = "Linear exposure + NS lag (df=4)",
    role = "sensitivity",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = 4)
  ),
  lin_df5 = list(
    label = "Linear exposure + NS lag (df=5)",
    role = "sensitivity",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = 5)
  ),
  lin_knots1224 = list(
    label = "Linear exposure + NS lag (knots 12, 24)",
    role = "sensitivity",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", knots = c(12, 24))
  ),
  lin_knots102030 = list(
    label = "Linear exposure + NS lag (knots 10, 20, 30)",
    role = "sensitivity",
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", knots = c(10, 20, 30))
  ),
  bs2_trimester = list(
    label = "Quadratic B-spline exposure (df=3) + NS lag (knots 13, 27)",
    role = "secondary_non_linear",
    argvar = list(fun = "bs", degree = 2, df = 3),
    arglag = lag_trimester
  ),
  bs3_trimester = list(
    label = "Cubic B-spline exposure (df=4) + NS lag (knots 13, 27)",
    role = "secondary_non_linear",
    argvar = list(fun = "bs", degree = 3, df = 4),
    arglag = lag_trimester
  )
)

## 3 Helper functions ----

summarize_cb_dimensions <- function(spec_id, spec, gest_weeks) {
  lag_rng <- c(min(gest_weeks), max(gest_weeks))
  Q <- matrix(0, nrow = 5, ncol = length(gest_weeks))
  cb <- crossbasis(
    Q,
    lag = lag_rng,
    argvar = spec$argvar,
    arglag = spec$arglag
  )
  lag_seq <- seq(lag_rng[1], lag_rng[2])
  df_var <- ncol(do.call(dlnm::onebasis, c(list(x = rep(1, 10)), spec$argvar)))
  df_lag <- ncol(do.call(dlnm::onebasis, c(list(x = lag_seq), spec$arglag)))
  out <- data.frame(
    spec_id = spec_id,
    spec_label = spec$label,
    role = spec$role,
    exposure_df = df_var,
    lag_df = df_lag,
    crossbasis_columns = ncol(cb),
    stringsAsFactors = FALSE
  )
  message(
    sprintf(
      "CB check [%s]: exposure_df=%d, lag_df=%d, total_columns=%d",
      spec_id, df_var, df_lag, ncol(cb)
    )
  )
  out
}

cb_dimension_table <- dplyr::bind_rows(lapply(
  names(crossbasis_specs),
  function(sid) summarize_cb_dimensions(sid, crossbasis_specs[[sid]], gest_weeks)
))
writexl::write_xlsx(
  list(Cross_basis_dimensions = cb_dimension_table),
  path = file.path(out_dir, "DLNM_PTB_crossbasis_dimensions.xlsx")
)

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
  Q[!is.finite(Q)] <- NA
  Q
}

# Keep subjects with observed exposure through gestational age (in utero); exclude incomplete histories.
filter_in_utero_exposure <- function(df, Q, gest_weeks) {
  ok <- vapply(
    seq_len(nrow(df)),
    function(i) {
      g <- floor(df$edad_gest[i])
      if (g < 1L) {
        return(FALSE)
      }
      lag_idx <- which(gest_weeks < g)
      if (length(lag_idx) == 0L) {
        return(FALSE)
      }
      all(is.finite(Q[i, lag_idx]))
    },
    logical(1)
  )
  if (!any(ok)) {
    return(list(data = df[0, , drop = FALSE], Q = Q[0, , drop = FALSE], n_excluded = nrow(df)))
  }
  list(
    data = df[ok, , drop = FALSE],
    Q = Q[ok, , drop = FALSE],
    n_excluded = sum(!ok)
  )
}

prepare_q_for_crossbasis <- function(Q) {
  Q <- Q
  Q[is.na(Q) | !is.finite(Q)] <- 0
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
  hist <- filter_in_utero_exposure(df, Q, gest_weeks)
  df <- hist$data
  Q <- prepare_q_for_crossbasis(hist$Q)
  if (nrow(df) == 0) return(NULL)
  rownames(df) <- seq_len(nrow(df))

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

  fit_warnings <- character()
  warn_env <- new.env(parent = emptyenv())
  warn_env$msgs <- character()
  mod <- withCallingHandlers(
    tryCatch(
      survival::coxph(fml, data = df, na.action = stats::na.omit),
      error = function(e) NULL
    ),
    warning = function(w) {
      warn_env$msgs <- c(warn_env$msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  fit_warnings <- warn_env$msgs
  if (is.null(mod)) return(NULL)

  converged <- all(is.finite(stats::coef(mod)))
  contrast <- crosspred_contrast(contaminante, Q, scale)
  pred <- tryCatch(
    crosspred(cb, mod, cen = contrast$cen, at = contrast$at),
    error = function(e) NULL
  )
  if (is.null(pred)) return(NULL)

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
  res <- res[is.finite(res$beta_exp) & res$Week %in% out_weeks & res$Week != 37L, , drop = FALSE]
  res$`No Obs` <- stats::nobs(mod)
  res$AIC <- stats::AIC(mod)
  res$BIC <- stats::BIC(mod)
  res$spec_id <- spec_id
  res$spec_label <- spec$label
  res$spec_role <- spec$role
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
      spec_role = spec$role,
      is_primary = identical(spec_id, primary_spec_id),
      dependent = dependent_var,
      contaminante = contaminante,
      tipo = tipo,
      exposure_scale = scale,
      n = stats::nobs(mod),
      n_params = length(stats::coef(mod)),
      logLik = as.numeric(stats::logLik(mod)),
      AIC = stats::AIC(mod),
      BIC = stats::BIC(mod),
      converged = converged,
      warnings = paste(unique(fit_warnings), collapse = " | "),
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

n_expected <- length(crossbasis_specs) * length(exposure_scales) * length(contaminants) * length(types)
message(
  "DLNM fits completed: ", nrow(fit_df), " / ", n_expected,
  " (exposure scale: ", paste(exposure_scales, collapse = ", "), ")"
)

# Primary model is lin_trimester (a priori); fit_df supports sensitivity comparison — not AIC-based selection.

## 5 Save results ----

dlnm_results <- list(
  all_results = all_results,
  fit_df = fit_df,
  cb_dimension_table = cb_dimension_table,
  primary_spec_id = primary_spec_id,
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
  list(
    Model_fit = fit_df,
    Cross_basis_dimensions = cb_dimension_table
  ),
  path = file.path(out_dir, "DLNM_PTB_model_fit_AIC_BIC.xlsx")
)

cat("DLNM Cox PTB analysis completed. Outputs in", out_dir, "\n")
