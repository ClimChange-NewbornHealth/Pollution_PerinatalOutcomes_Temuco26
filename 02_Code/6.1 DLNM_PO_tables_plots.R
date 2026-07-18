# 6.1 DLNM PTB - Tables, Figures and Model Fit ----
# Loads DLNM Cox results from 6.0 (birth_preterm only).
# Figures: 3 cols (PM2.5, Levoglucosan, K) x 2 rows (FS, LUR) per DLNM spec.
# Aesthetic aligned with Fig2_DLM_preterm in 5.0 Figures_model_paper.R.

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")

out_dir <- "03_Output/DLNM"
fig_dir <- file.path(out_dir, "Figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

## 1 Load DLNM results ----

load(file.path(out_dir, "DLNM_PTB_results.RData"))

all_results <- dlnm_results$all_results
fit_df <- dlnm_results$fit_df
dependent_var <- dlnm_results$dependent_var
contaminants <- dlnm_results$contaminants
types <- dlnm_results$types
exposure_scales <- dlnm_results$exposure_scales
crossbasis_specs <- dlnm_results$crossbasis_specs
out_weeks <- dlnm_results$out_weeks

tipo_labels <- c("cs" = "FS", "sp" = "LUR")
cont_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")
plot_order <- c("PM25_cs", "Levo_cs", "K_cs", "PM25_sp", "Levo_sp", "K_sp")

format_effect_ci <- function(estimate, conf_low, conf_high) {
  sprintf("%.3f (%.3f-%.3f)", estimate, conf_low, conf_high)
}

get_result <- function(spec_id, cont, tp, scale) {
  key <- paste(spec_id, dependent_var, cont, tp, scale, sep = "|")
  all_results[[key]]
}

to_plot_format <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  tbl |>
    dplyr::transmute(
      week = as.integer(Week),
      estimate = beta_exp,
      conf.low = Lower_exp,
      conf.high = Upper_exp,
      log_hr = beta,
      log_hr_conf.low = Lower,
      log_hr_conf.high = Upper
    )
}

safe_sheet_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  substr(x, 1, 31)
}

## 2 Plot function (same aesthetic as 5.0 Fig2 DLM) ----

plot_dlnm_ptb_panel <- function(data_one, panel_label, y_label = "HR (95% CI)", ref_line = 1) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  data_one <- data_one |> dplyr::filter(week <= 37)

  y_vals <- c(data_one$estimate, data_one$conf.low, data_one$conf.high)
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  min_range <- if (ref_line == 0) 0.05 else 0.1
  max_dist <- max(ref_line - y_min, y_max - ref_line, min_range)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  ggplot2::ggplot(data_one, ggplot2::aes(x = week, y = estimate)) +
    ggplot2::geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      width = 0.3, color = "black"
    ) +
    ggplot2::geom_point(size = 2, color = "black") +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 39, by = 3)) +
    ggplot2::labs(y = y_label, x = "Gestational week", title = panel_label) +
    ggplot2::theme_light(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 8),
      plot.margin = ggplot2::margin(4, 4, 4, 4, "pt")
    )
}

build_ptb_figure <- function(spec_id, scale, effect = "HR") {
  use_log <- identical(effect, "logHR")
  ref_line <- if (use_log) 0 else 1
  y_label <- if (use_log) "log(HR) (95% CI)" else "HR (95% CI)"

  plots_list <- list()
  for (cont in contaminants) {
    for (tp in types) {
      tbl <- get_result(spec_id, cont, tp, scale)
      data_one <- to_plot_format(tbl)
      if (!is.null(data_one)) {
        if (use_log) {
          data_one <- data_one |>
            dplyr::mutate(
              estimate = log_hr,
              conf.low = log_hr_conf.low,
              conf.high = log_hr_conf.high
            )
        }
        p <- plot_dlnm_ptb_panel(
          data_one,
          panel_label = paste(cont_labels[cont], "-", tipo_labels[tp]),
          y_label = y_label,
          ref_line = ref_line
        )
        plots_list[[paste(cont, tp, sep = "_")]] <- p
      }
    }
  }

  plots_ordered <- plots_list[plot_order]
  plots_ordered <- plots_ordered[!vapply(plots_ordered, is.null, logical(1))]
  if (length(plots_ordered) == 0) return(NULL)

  ggpubr::ggarrange(
    plotlist = plots_ordered,
    ncol = 3,
    nrow = 2,
    align = "hv"
  )
}

## 3 Summary tables (3 pollutants per outcome-tipo) ----

build_summary_table <- function(spec_id, tp, scale) {
  to_cell <- function(d) {
    dplyr::transmute(
      d,
      Week = as.integer(Week),
      cell = sprintf(
        "%.3f (%.3f-%.3f)",
        .data$beta_exp,
        .data$Lower_exp,
        .data$Upper_exp
      )
    )
  }

  pm <- get_result(spec_id, "PM25", tp, scale)
  le <- get_result(spec_id, "Levo", tp, scale)
  kk <- get_result(spec_id, "K", tp, scale)

  pmf <- if (!is.null(pm)) dplyr::rename(to_cell(pm), PM25 = cell) else NULL
  lef <- if (!is.null(le)) dplyr::rename(to_cell(le), Levo = cell) else NULL
  kkf <- if (!is.null(kk)) dplyr::rename(to_cell(kk), K = cell) else NULL

  out <- tibble::tibble(Week = out_weeks)
  if (!is.null(pmf)) out <- dplyr::left_join(out, pmf, by = "Week")
  if (!is.null(lef)) out <- dplyr::left_join(out, lef, by = "Week")
  if (!is.null(kkf)) out <- dplyr::left_join(out, kkf, by = "Week")
  out
}

## 4 DLM reference fit (4.0) for model comparison ----

compute_dlm_ptb_fit_stats <- function() {
  cache_path <- file.path(out_dir, "DLM_PTB_fit_reference.RData")
  if (file.exists(cache_path)) {
    dlm_fit <- get("dlm_fit", envir = load(cache_path))
    return(dlm_fit)
  }

  source("02_Code/0.4 Functions_models.R", local = TRUE)
  control_vars <<- c(
    "edad_madre", "education", "health_insurance", "job", "first_birth",
    "sexo_rn", "a_nac", "mes_nac", "comuna"
  )

  data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")
  data <- data |>
    dplyr::mutate(mes_nac = lubridate::month(fecha_nac)) |>
    dplyr::select(
      "idbase", "edad_gest", "birth_preterm",
      "edad_madre", "education", "health_insurance", "job", "first_birth",
      "sexo_rn", "a_nac", "comuna", "mes_nac",
      dplyr::matches("^w[0-9]+_"),
      dplyr::matches("^iqr_w[0-9]+_")
    ) |>
    dplyr::filter(!is.na(birth_preterm), edad_gest >= 28) |>
    dplyr::mutate(tstart = 27)

  compute_lagged_exposure <- function(df) {
    df |>
      dplyr::arrange(idbase, contaminante, tipo, week) |>
      dplyr::group_by(idbase, contaminante, tipo) |>
      dplyr::mutate(
        exposicion_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
          if (week[i] == 0) return(NA_real_)
          past_rows <- which(week < week[i])
          if (length(past_rows) == 0) return(NA_real_)
          weights <- 1 / (week[i] - week[past_rows])
          sum(weights * exposicion[past_rows], na.rm = TRUE)
        })
      ) |>
      dplyr::ungroup()
  }

  build_wide <- function(data_in, col_pattern) {
    data_long <- data_in |>
      tidyr::pivot_longer(
        cols = dplyr::matches(col_pattern),
        names_to = "col",
        values_to = "exposicion"
      ) |>
      dplyr::mutate(
        week = as.numeric(stringr::str_extract(col, "[0-9]+")),
        contaminante = stringr::str_extract(col, "(PM25|Levo|K)"),
        tipo = stringr::str_extract(col, "(cs|sp)$")
      ) |>
      dplyr::select(-col)

    data_long <- compute_lagged_exposure(data_long)
    data_long |>
      dplyr::select(idbase, contaminante, tipo, week, exposicion, exposicion_lagged) |>
      tidyr::pivot_wider(
        names_from = week,
        values_from = c(exposicion, exposicion_lagged),
        names_glue = "{.value}_{week}"
      )
  }

  data_wide_raw <- build_wide(
    data |> dplyr::select(-dplyr::starts_with("iqr_w")),
    "^w[0-9]+_(PM25|Levo|K)_(cs|sp)$"
  )
  data_wide_iqr <- build_wide(
    data |> dplyr::select(-dplyr::starts_with("w")),
    "^iqr_w[0-9]+_(PM25|Levo|K)_(cs|sp)$"
  )

  weeks_analysis <- 1:37
  base_cols <- c("idbase", "edad_gest", "tstart", "birth_preterm", control_vars)
  rows <- list()

  for (scale in exposure_scales) {
    wide_expo <- if (scale == "raw") data_wide_raw else data_wide_iqr
    for (contam in contaminants) {
      for (tipo_val in types) {
        wide_one <- wide_expo |>
          dplyr::filter(contaminante == contam, tipo == tipo_val) |>
          dplyr::select(-contaminante, -tipo)

        data_model <- data |>
          dplyr::select(dplyr::any_of(base_cols)) |>
          dplyr::distinct(idbase, .keep_all = TRUE) |>
          dplyr::left_join(wide_one, by = "idbase") |>
          dplyr::filter(!is.na(edad_madre), sexo_rn != "Indefinido")

        aic_vals <- c()
        bic_vals <- c()
        n_obs <- NA_integer_

        for (w in weeks_analysis) {
          exp_var <- paste0("exposicion_", w)
          lag_var <- paste0("exposicion_lagged_", w)
          if (!exp_var %in% names(data_model) || !lag_var %in% names(data_model)) next

          predictor <- paste(exp_var, lag_var, sep = " + ")
          available_controls <- control_vars[control_vars %in% names(data_model)]
          fml <- stats::as.formula(
            paste0(
              "survival::Surv(tstart, edad_gest, birth_preterm) ~ ",
              predictor, " + ", paste(available_controls, collapse = " + ")
            )
          )

          df_w <- data_model |>
            dplyr::filter(
              !is.na(birth_preterm), !is.na(edad_gest), !is.na(tstart),
              tstart < edad_gest,
              !is.na(.data[[exp_var]]), !is.na(.data[[lag_var]])
            )

          if (nrow(df_w) < 10) next
          mod <- tryCatch(survival::coxph(fml, data = df_w), error = function(e) NULL)
          if (is.null(mod)) next

          aic_vals <- c(aic_vals, stats::AIC(mod))
          bic_vals <- c(bic_vals, stats::BIC(mod))
          n_obs <- stats::nobs(mod)
        }

        if (length(aic_vals) == 0) next
        rows[[length(rows) + 1L]] <- tibble::tibble(
          spec_id = "dlm_4.0",
          spec_label = "DLM weekly Cox (4.0): current week + inverse-distance lag",
          dependent = dependent_var,
          contaminante = contam,
          tipo = tipo_val,
          exposure_scale = scale,
          n = n_obs,
          AIC = mean(aic_vals, na.rm = TRUE),
          BIC = mean(bic_vals, na.rm = TRUE),
          n_weekly_models = length(aic_vals),
          cen = NA_real_,
          at = NA_real_
        )
      }
    }
  }

  dlm_fit <- dplyr::bind_rows(rows)
  save(dlm_fit, file = cache_path)
  dlm_fit
}

extract_dlm_reference_fit <- function() {
  tryCatch(
    compute_dlm_ptb_fit_stats(),
    error = function(e) {
      warning("No se pudo calcular ajuste DLM de referencia: ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

build_paper_fit_table <- function(fit_df_all) {
  pollutant_labels <- c(PM25 = "PM2.5", Levo = "Levoglucosan", K = "Potassium")
  exposure_type_labels <- c(cs = "Fixed site (cs)", sp = "Land-use regression (sp)")
  scale_labels <- c(raw = "Raw", iqr = "IQR-scaled")
  model_family_labels <- c(
    lin_simple = "DLNM (cohort cross-basis)",
    lin_knots1327 = "DLNM (cohort cross-basis)",
    bs2_knots1327 = "DLNM (cohort cross-basis)",
    bs3_knots1327 = "DLNM (cohort cross-basis)",
    dlm_4.0 = "DLM weekly Cox (primary analysis)"
  )
  spec_order <- c(
    "lin_simple", "lin_knots1327", "bs2_knots1327", "bs3_knots1327", "dlm_4.0"
  )

  fit_df_all |>
    dplyr::mutate(
      Outcome = "Preterm birth (<37 weeks)",
      Model = unname(model_family_labels[.data$spec_id]),
      `Model specifications` = dplyr::if_else(
        .data$spec_id == "dlm_4.0",
        paste0(
          .data$spec_label,
          " (AIC and BIC averaged over ",
          .data$n_weekly_models,
          " gestational-week models)."
        ),
        .data$spec_label
      ),
      Pollutant = unname(pollutant_labels[.data$contaminante]),
      `Exposure model` = unname(exposure_type_labels[.data$tipo]),
      `Exposure scale` = unname(scale_labels[.data$exposure_scale]),
      AIC = sprintf("%.3f", .data$AIC),
      BIC = sprintf("%.3f", .data$BIC),
      spec_ord = match(.data$spec_id, spec_order),
      poll_ord = match(.data$contaminante, names(pollutant_labels)),
      tipo_ord = match(.data$tipo, names(exposure_type_labels)),
      scale_ord = match(.data$exposure_scale, names(scale_labels))
    ) |>
    dplyr::arrange(.data$scale_ord, .data$tipo_ord, .data$poll_ord, .data$spec_ord) |>
    dplyr::select(
      Outcome,
      Model,
      `Model specifications`,
      Pollutant,
      `Exposure model`,
      `Exposure scale`,
      AIC,
      BIC
    )
}

## 5 Generate tables and figures for all DLNM specs ----

list_figures <- list()

for (spec_id in names(crossbasis_specs)) {
  for (scale in exposure_scales) {
    scale_suffix <- if (scale == "raw") "" else "_IQR"

    # Per pollutant-tipo sheets
    sheets_list <- list()
    for (cont in contaminants) {
      for (tp in types) {
        tbl <- get_result(spec_id, cont, tp, scale)
        if (is.null(tbl) || nrow(tbl) == 0) next

        tab <- tbl |>
          dplyr::arrange(Week) |>
          dplyr::mutate(
            `HR (95% CI)` = format_effect_ci(beta_exp, Lower_exp, Upper_exp)
          ) |>
          dplyr::select(Week, `HR (95% CI)`)

        sheets_list[[paste(cont, tp, sep = "_")]] <- tab
      }
    }

    if (length(sheets_list) > 0) {
      tab_path <- file.path(out_dir, paste0("Tab_DLNM_PTB_", spec_id, scale_suffix, ".xlsx"))
      writexl::write_xlsx(sheets_list, path = tab_path)
      message("Saved: ", tab_path)
    }

    # Summary: 3 pollutants x cs/sp
    summary_list <- list(
      FS = build_summary_table(spec_id, "cs", scale),
      LUR = build_summary_table(spec_id, "sp", scale)
    )
    sum_path <- file.path(out_dir, paste0("Tab_DLNM_PTB_summary_", spec_id, scale_suffix, ".xlsx"))
    writexl::write_xlsx(summary_list, path = sum_path)
    message("Saved: ", sum_path)

    # Figures (HR and logHR)
    for (eff in c("HR", "logHR")) {
      fig <- build_ptb_figure(spec_id, scale, effect = eff)
      if (is.null(fig)) next

      eff_suffix <- if (eff == "HR") "HR" else "logHR"
      fig_path <- file.path(
        fig_dir,
        paste0("Fig_DLNM_PTB_", spec_id, "_", eff_suffix, scale_suffix, ".png")
      )
      ggplot2::ggsave(
        fig_path,
        plot = fig,
        res = 300,
        width = 30,
        height = 14,
        units = "cm",
        device = ragg::agg_png
      )
      message("Saved: ", fig_path)
      list_figures[[paste(spec_id, eff, scale, sep = "_")]] <- fig
    }
  }
}

## 6 Model fit table with DLM reference ----

fit_dlm_ref <- extract_dlm_reference_fit()
fit_df_all <- dplyr::bind_rows(
  fit_df |> dplyr::mutate(n_weekly_models = NA_integer_),
  fit_dlm_ref
)

paper_fit <- build_paper_fit_table(fit_df_all)
writexl::write_xlsx(
  list(
    Model_fit = fit_df_all,
    Model_fit_paper = paper_fit
  ),
  path = file.path(out_dir, "DLNM_PTB_model_fit_AIC_BIC.xlsx")
)
message("Saved: DLNM_PTB_model_fit_AIC_BIC.xlsx")

save(list_figures, file = file.path(out_dir, "Figures_compiled_PTB.RData"))

cat("DLNM PTB tables and figures completed. Outputs in", out_dir, "\n")
