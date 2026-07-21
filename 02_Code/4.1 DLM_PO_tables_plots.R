# 6.1 DLM Perinatal Outcomes - Tables and Plots ----
# Loads DLM results from 5.0, creates Excel tables and compiled figures.
# Tables: one sheet per outcome-exposure, week effects only.
# Figures: one panel per contaminant-tipo, each subplot = one outcome (2 cols x 4 rows).

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")

## 1 Load DLM results ----

load("03_Output/DLM/DLM_PO_results.RData")

results_logit <- dlm_results$results_logit
results_cox <- dlm_results$results_cox
dependent_vars <- dlm_results$dependent_vars
contaminants <- dlm_results$contaminants
types <- dlm_results$types
exposure_scales <- if ("exposure_scales" %in% names(dlm_results)) dlm_results$exposure_scales else "raw"

# Figures: weeks 1–36 on x-axis; y-limits from CI range over weeks <= 36 only
plot_week_max <- 36L

save_dlm_png <- function(filename, plot, width, height) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename,
    plot = plot,
    res = 300,
    width = width,
    height = height,
    units = "cm",
    device = ragg::agg_png,
    background = "white"
  )
}

# Fig1 uses common.legend (ggarrange becomes a grob bundle); ggsave/print mis-render it
save_fig1_png <- function(filename, plot, width, height) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  w_px <- round(width / 2.54 * 300)
  h_px <- round(height / 2.54 * 300)
  grDevices::png(filename, width = w_px, height = h_px, res = 300, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(ggpubr::as_ggplot(plot))
  invisible(filename)
}

theme_plot_white <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )
}

## 2 Outcome labels and order ----

outcomes_order <- c(
  "birth_preterm",
  "birth_very_preterm",
  "birth_moderately_preterm",
  "birth_late_preterm",
  "lbw",
  "tlbw",
  "sga"
)

# Outcome labels (aligned with Data_dictionary_PO_pollution.md)
outcomes_labels <- c(
  "birth_preterm"            = "Preterm birth (<37 weeks)",
  "birth_very_preterm"       = "Very preterm birth (28–31 weeks)",
  "birth_moderately_preterm" = "Moderately preterm birth (32–33 weeks)",
  "birth_late_preterm"       = "Late preterm birth (34–36 weeks)",
  "lbw"                      = "Low birth weight (<2500 g)",
  "tlbw"                     = "Very low birth weight (<1500 g)",
  "sga"                      = "Small for gestational age (SGA)"
)

# Panel labels: letter + outcome name for each subplot
panel_labels <- setNames(
  paste0(LETTERS[seq_along(outcomes_order)], ". ", outcomes_labels[outcomes_order]),
  outcomes_order
)

## 3 Build Excel sheets (one per outcome-exposure) ----

format_effect_ci <- function(estimate, conf_low, conf_high) {
  sprintf("%.3f (%.3f-%.3f)", estimate, conf_low, conf_high)
}

# Helper: get result by key, supporting both "dep_cont_tp_scale" and legacy "dep_cont_tp"
get_result <- function(results_list, dep, cont, tp, scale) {
  key_scale <- paste(dep, cont, tp, scale, sep = "_")
  key_legacy <- paste(dep, cont, tp, sep = "_")
  res <- results_list[[key_scale]]
  if (is.null(res) && scale == "raw") res <- results_list[[key_legacy]]
  res
}

for (scale in exposure_scales) {
  sheets_list <- list()

  for (dep in dependent_vars) {
    for (cont in contaminants) {
      for (tp in types) {
        tbl_logit <- get_result(results_logit, dep, cont, tp, scale)
        tbl_cox <- get_result(results_cox, dep, cont, tp, scale)

        if (is.null(tbl_logit) || nrow(tbl_logit) == 0) next
        if (is.null(tbl_cox) || nrow(tbl_cox) == 0) next

        tbl_logit <- tbl_logit |> dplyr::arrange(week)
        tbl_cox <- tbl_cox |> dplyr::arrange(week)

        # Merge by week
        tab <- dplyr::full_join(
          tbl_logit |> select(week, estimate, conf.low, conf.high) |> rename(OR = estimate, OR_low = conf.low, OR_high = conf.high),
          tbl_cox |> select(week, estimate, conf.low, conf.high) |> rename(HR = estimate, HR_low = conf.low, HR_high = conf.high),
          by = "week"
        ) |>
          dplyr::arrange(week) |>
          dplyr::mutate(
            `OR (95% CI)` = format_effect_ci(OR, OR_low, OR_high),
            `HR (95% CI)` = format_effect_ci(HR, HR_low, HR_high)
          ) |>
          dplyr::select(Week = week, `OR (95% CI)`, `HR (95% CI)`)

        # Short names for Excel (max 31 chars); avoid truncation/dedup
        dep_short <- c(
          birth_preterm = "preterm", birth_very_preterm = "vpreterm",
          birth_moderately_preterm = "mpreterm", birth_late_preterm = "lpreterm",
          lbw = "lbw", tlbw = "tlbw", sga = "sga"
        )[dep]
        sheet_name <- paste(dep_short, cont, tp, sep = "_")
        sheets_list[[sheet_name]] <- tab
      }
    }
  }

  outfile <- if (scale == "raw") "03_Output/DLM/Tab_DLM_PO.xlsx" else "03_Output/DLM/Tab_DLM_PO_IQR.xlsx"
  writexl::write_xlsx(sheets_list, path = outfile)
  message("Saved: ", outfile)
}

## 4 DLM plot function (week on x, effect on y) ----

plot_dlm_outcome <- function(data_one_outcome, y_var, ymin_var, ymax_var,
                             ref_line, y_label, panel_label, show_legend = FALSE) {

  if (is.null(data_one_outcome) || nrow(data_one_outcome) == 0) return(NULL)

  data_plot <- data_one_outcome |>
    dplyr::filter(week <= plot_week_max) |>
    dplyr::filter(
      is.finite(.data[[y_var]]),
      is.finite(.data[[ymin_var]]),
      is.finite(.data[[ymax_var]])
    )

  if (nrow(data_plot) == 0) return(NULL)

  y_vals <- c(data_plot[[ymin_var]], data_plot[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  min_range <- if (ref_line == 0) 0.05 else 0.1
  max_dist <- max(ref_line - y_min, y_max - ref_line, min_range)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  ggplot(data_plot, aes(x = week, y = .data[[y_var]])) +
    geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.3, color = "black"
    ) +
    geom_point(size = 2, color = "black") +
    scale_y_continuous(
      limits = y_limits,
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    scale_x_continuous(
      breaks = seq(1, plot_week_max, by = 3),
      limits = c(0.5, plot_week_max + 0.5)
    ) +
    labs(y = y_label, x = "Gestational week", title = panel_label) +
    theme_light(base_size = 10) +
    theme_plot_white() +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      plot.margin = margin(4, 4, 4, 4, "pt")
    )
}

## 5 Build and save figures ----

# Ensure log columns exist for logit and cox
ensure_log_cols <- function(tbl, log_est = "log_or", log_lo = "log_or_conf.low", log_hi = "log_or_conf.high") {
  if (!log_est %in% names(tbl)) {
    tbl <- tbl |>
      dplyr::mutate(
        !!rlang::sym(log_est) := log(estimate),
        !!rlang::sym(log_lo) := log(conf.low),
        !!rlang::sym(log_hi) := log(conf.high)
      )
  }
  tbl
}

list_log_scale <- list()
list_ratio_scale <- list()
# Raw: original filenames; IQR: _iqr suffix
scale_suffix <- c(raw = "", iqr = "_iqr")

tic("Time plotes DLM:")
for (scale in exposure_scales) {
  sfx <- scale_suffix[scale]

  for (cont in contaminants) {
    for (tp in types) {

      # --- Logit: log(OR) ---
      plots_logit_log <- list()
      for (i in seq_along(outcomes_order)) {
        dep <- outcomes_order[i]
        data_one <- get_result(results_logit, dep, cont, tp, scale)
        if (!is.null(data_one) && nrow(data_one) > 0) {
          data_one <- ensure_log_cols(data_one, "log_or", "log_or_conf.low", "log_or_conf.high")
          p <- plot_dlm_outcome(
            data_one,
            y_var = "log_or",
            ymin_var = "log_or_conf.low",
            ymax_var = "log_or_conf.high",
            ref_line = 0,
            y_label = "log(OR) (95% CI)",
            panel_label = panel_labels[dep],
            show_legend = (i == 1)
          )
          plots_logit_log[[i]] <- p
        } else {
          plots_logit_log[[i]] <- NULL
        }
      }

      plots_logit_log <- plots_logit_log[!vapply(plots_logit_log, is.null, logical(1))]
      if (length(plots_logit_log) > 0) {
        fig_logit_log <- ggpubr::ggarrange(
          plotlist = plots_logit_log,
          ncol = 2,
          nrow = 4,
          align = "hv",
          bg = "white"
        )
        list_log_scale[[paste0("logit_", cont, "_", tp, "_", scale)]] <- fig_logit_log

        outfile <- sprintf("03_Output/DLM/Plots_logit/OR_%s_%s%s.png", cont, tp, sfx)
        save_dlm_png(outfile, fig_logit_log, width = 20, height = 24)
        message("Saved: ", outfile)
      }

      # --- Logit: OR ---
      plots_logit_or <- list()
      for (i in seq_along(outcomes_order)) {
        dep <- outcomes_order[i]
        data_one <- get_result(results_logit, dep, cont, tp, scale)
        if (!is.null(data_one) && nrow(data_one) > 0) {
          p <- plot_dlm_outcome(
            data_one,
            y_var = "estimate",
            ymin_var = "conf.low",
            ymax_var = "conf.high",
            ref_line = 1,
            y_label = "OR (95% CI)",
            panel_label = panel_labels[dep],
            show_legend = (i == 1)
          )
          plots_logit_or[[i]] <- p
        } else {
          plots_logit_or[[i]] <- NULL
        }
      }

      plots_logit_or <- plots_logit_or[!vapply(plots_logit_or, is.null, logical(1))]
      if (length(plots_logit_or) > 0) {
        fig_logit_or <- ggpubr::ggarrange(
          plotlist = plots_logit_or,
          ncol = 2,
          nrow = 4,
          align = "hv",
          bg = "white"
        )
        list_ratio_scale[[paste0("logit_", cont, "_", tp, "_", scale)]] <- fig_logit_or

        outfile <- sprintf("03_Output/DLM/Plots_logit/OR_%s_%s_ratio%s.png", cont, tp, sfx)
        save_dlm_png(outfile, fig_logit_or, width = 20, height = 24)
        message("Saved: ", outfile)
      }

      # --- Cox: log(HR) ---
      plots_cox_log <- list()
      for (i in seq_along(outcomes_order)) {
        dep <- outcomes_order[i]
        data_one <- get_result(results_cox, dep, cont, tp, scale)
        if (!is.null(data_one) && nrow(data_one) > 0) {
          data_one <- ensure_log_cols(data_one, "log_hr", "log_hr_conf.low", "log_hr_conf.high")
          p <- plot_dlm_outcome(
            data_one,
            y_var = "log_hr",
            ymin_var = "log_hr_conf.low",
            ymax_var = "log_hr_conf.high",
            ref_line = 0,
            y_label = "log(HR) (95% CI)",
            panel_label = panel_labels[dep],
            show_legend = (i == 1)
          )
          plots_cox_log[[i]] <- p
        } else {
          plots_cox_log[[i]] <- NULL
        }
      }

      plots_cox_log <- plots_cox_log[!vapply(plots_cox_log, is.null, logical(1))]
      if (length(plots_cox_log) > 0) {
        fig_cox_log <- ggpubr::ggarrange(
          plotlist = plots_cox_log,
          ncol = 2,
          nrow = 4,
          align = "hv",
          bg = "white"
        )
        list_log_scale[[paste0("cox_", cont, "_", tp, "_", scale)]] <- fig_cox_log

        outfile <- sprintf("03_Output/DLM/Plots_cox/HR_%s_%s%s.png", cont, tp, sfx)
        save_dlm_png(outfile, fig_cox_log, width = 20, height = 24)
        message("Saved: ", outfile)
      }

      # --- Cox: HR ---
      plots_cox_hr <- list()
      for (i in seq_along(outcomes_order)) {
        dep <- outcomes_order[i]
        data_one <- get_result(results_cox, dep, cont, tp, scale)
        if (!is.null(data_one) && nrow(data_one) > 0) {
          p <- plot_dlm_outcome(
            data_one,
            y_var = "estimate",
            ymin_var = "conf.low",
            ymax_var = "conf.high",
            ref_line = 1,
            y_label = "HR (95% CI)",
            panel_label = panel_labels[dep],
            show_legend = (i == 1)
          )
          plots_cox_hr[[i]] <- p
        } else {
          plots_cox_hr[[i]] <- NULL
        }
      }

      plots_cox_hr <- plots_cox_hr[!vapply(plots_cox_hr, is.null, logical(1))]
      if (length(plots_cox_hr) > 0) {
        fig_cox_hr <- ggpubr::ggarrange(
          plotlist = plots_cox_hr,
          ncol = 2,
          nrow = 4,
          align = "hv",
          bg = "white"
        )
        list_ratio_scale[[paste0("cox_", cont, "_", tp, "_", scale)]] <- fig_cox_hr

        outfile <- sprintf("03_Output/DLM/Plots_cox/HR_%s_%s_ratio%s.png", cont, tp, sfx)
        save_dlm_png(outfile, fig_cox_hr, width = 20, height = 24)
        message("Saved: ", outfile)
      }
    }
  }
}
toc() # Time plotes DLM: 65,575 sec elapsed

## 6 Save compiled figure lists ----

save(list_log_scale, list_ratio_scale,
  file = "03_Output/DLM/Plots_compiled_figures.RData"
)

## 7 Fig1 Cox preterm (paper; white background) ----

fig_paper_dir <- "03_Output/Fig_paper"
dir.create(fig_paper_dir, recursive = TRUE, showWarnings = FALSE)

models_cox <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "cox_models")
if (!"exposure_scale" %in% names(models_cox)) {
  models_cox <- models_cox |>
    dplyr::mutate(exposure_scale = dplyr::if_else(stringr::str_detect(term, "^iqr_"), "iqr", "raw"))
}

prepare_plot_data_cox <- function(models_df) {
  models_df |>
    dplyr::filter(
      (model_type == "single" & tiempo == "tot") |
        model_type == "t1_t2_t3"
    ) |>
    dplyr::arrange(dependent_var, contaminante, tipo, adjustment, model_type, term) |>
    dplyr::group_by(dependent_var, contaminante, tipo, adjustment, model_type, exposure_scale) |>
    dplyr::mutate(
      exposure = dplyr::case_when(
        model_type == "t1_t2_t3" & dplyr::row_number() == 1 ~ "T1",
        model_type == "t1_t2_t3" & dplyr::row_number() == 2 ~ "T2",
        model_type == "t1_t2_t3" & dplyr::row_number() == 3 ~ "T3",
        model_type == "single" & (stringr::str_detect(term, "tot_") | stringr::str_detect(term, "iqr_tot_")) ~ "Overall",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(exposure)) |>
    dplyr::mutate(
      exposure = factor(exposure, levels = c("T1", "T2", "T3", "Overall")),
      adjustment = factor(adjustment, levels = c("Unadjusted", "Adjusted"))
    )
}

ensure_log_hr <- function(df) {
  if (!"log_hr" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        log_hr = log(estimate),
        log_hr_conf.low = log(conf.low),
        log_hr_conf.high = log(conf.high)
      )
  }
  df
}

plot_cox_single <- function(data_one, y_var, ymin_var, ymax_var, ref_line, y_label, panel_label, show_legend = FALSE) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  pd <- ggplot2::position_dodge(width = 0.6)
  rect_t1t3 <- data.frame(xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf)
  rect_overall <- data.frame(xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf)

  y_vals <- c(data_one[[y_var]], data_one[[ymin_var]], data_one[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  min_range <- if (ref_line == 0) 0.05 else 0.1
  max_dist <- max(ref_line - y_min, y_max - ref_line, min_range)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  ggplot2::ggplot(data_one, ggplot2::aes(y = .data[[y_var]], x = exposure, color = adjustment, shape = adjustment)) +
    ggplot2::geom_rect(
      data = rect_t1t3,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "white"
    ) +
    ggplot2::geom_rect(
      data = rect_overall,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey95", alpha = 0.7
    ) +
    ggplot2::geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.25, position = pd
    ) +
    ggplot2::geom_point(size = 2, position = pd) +
    ggplot2::scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    ggplot2::scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0)) +
    ggplot2::labs(y = y_label, x = NULL, title = panel_label) +
    ggplot2::theme_light(base_size = 10) +
    theme_plot_white() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0),
      legend.position = if (show_legend) "top" else "none",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 8, angle = 0),
      axis.ticks.y = ggplot2::element_line(),
      plot.margin = ggplot2::margin(4, 4, 4, 4, "pt")
    )
}

cont_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")
effect_params <- list(
  logHR = list(
    y_var = "log_hr", ymin = "log_hr_conf.low", ymax = "log_hr_conf.high",
    ref = 0, label = "log(HR) (95% CI)", suffix = "logHR"
  ),
  HR = list(
    y_var = "estimate", ymin = "conf.low", ymax = "conf.high",
    ref = 1, label = "HR (95% CI)", suffix = "HR"
  )
)

for (scale in exposure_scales) {
  plot_data <- prepare_plot_data_cox(models_cox) |>
    dplyr::filter(exposure_scale == scale, dependent_var == "birth_preterm")
  plot_data <- if ("log_hr" %in% names(plot_data)) plot_data else ensure_log_hr(plot_data)

  scale_suffix <- if (scale == "raw") "_raw" else "_iqr"

  for (eff in names(effect_params)) {
    ep <- effect_params[[eff]]
    order_plots <- c("PM25_cs", "Levo_cs", "K_cs", "PM25_sp", "Levo_sp", "K_sp")
    plots_list <- vector("list", length(order_plots))
    names(plots_list) <- order_plots
    for (cont in contaminants) {
      for (tp in types) {
        key <- paste(cont, tp, sep = "_")
        data_one <- plot_data |> dplyr::filter(contaminante == cont, tipo == tp)
        plots_list[[key]] <- plot_cox_single(
          data_one,
          y_var = ep$y_var,
          ymin_var = ep$ymin,
          ymax_var = ep$ymax,
          ref_line = ep$ref,
          y_label = ep$label,
          panel_label = paste(cont_labels[cont], "-", if (tp == "cs") "FS" else "LUR"),
          show_legend = FALSE
        )
      }
    }

    plots_ordered <- plots_list[order_plots]
    plots_ordered <- plots_ordered[!vapply(plots_ordered, is.null, logical(1))]

    if (length(plots_ordered) > 0) {
      fig <- ggpubr::ggarrange(
        plotlist = plots_ordered,
        ncol = 3,
        nrow = 2,
        common.legend = TRUE,
        legend = "top",
        bg = "white"
      )
      fpath <- file.path(fig_paper_dir, paste0("Fig1_Cox_preterm_", ep$suffix, scale_suffix, ".png"))
      save_fig1_png(fpath, fig, width = 30, height = 14)
      message("Saved: ", fpath)
    }
  }
}
