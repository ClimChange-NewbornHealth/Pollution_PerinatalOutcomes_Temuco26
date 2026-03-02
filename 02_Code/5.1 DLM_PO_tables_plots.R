# 5.1 DLM Perinatal Outcomes - Tables and Plots ----
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

outcomes_labels <- c(
  "birth_preterm"            = "Preterm birth",
  "birth_very_preterm"       = "Very preterm birth",
  "birth_moderately_preterm" = "Moderately preterm birth",
  "birth_late_preterm"       = "Late preterm birth",
  "lbw"                      = "Low birth weight (LBW)",
  "tlbw"                     = "Low birth weight of term (tLBW)",
  "sga"                      = "Small for gestational age (SGA)"
)

panel_labels <- setNames(
  paste0(LETTERS[seq_along(outcomes_order)], ". ", outcomes_labels[outcomes_order]),
  outcomes_order
)

## 3 Build Excel sheets (one per outcome-exposure) ----

format_effect_ci <- function(estimate, conf_low, conf_high) {
  sprintf("%.3f (%.3f-%.3f)", estimate, conf_low, conf_high)
}

sheets_list <- list()

for (dep in dependent_vars) {
  for (cont in contaminants) {
    for (tp in types) {
      key <- paste(dep, cont, tp, sep = "_")

      tbl_logit <- results_logit[[key]]
      tbl_cox <- results_cox[[key]]

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

writexl::write_xlsx(sheets_list, path = "03_Output/DLM/Tab_DLM_PO.xlsx")
message("Saved: 03_Output/DLM/Tab_DLM_PO.xlsx")

## 4 DLM plot function (week on x, effect on y) ----

plot_dlm_outcome <- function(data_one_outcome, y_var, ymin_var, ymax_var,
                             ref_line, y_label, panel_label, show_legend = FALSE) {

  if (is.null(data_one_outcome) || nrow(data_one_outcome) == 0) return(NULL)

  y_vals <- c(data_one_outcome[[ymin_var]], data_one_outcome[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  max_dist <- max(ref_line - y_min, y_max - ref_line)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  data_one_outcome <- data_one_outcome |> filter(week<=37)

  ggplot(data_one_outcome, aes(x = week, y = .data[[y_var]])) +
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
    scale_x_continuous(breaks = seq(1, 39, by = 3)) +
    labs(y = y_label, x = "Gestational week", title = panel_label) +
    theme_light(base_size = 10) +
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

tic("Time plotes DLM:")
for (cont in contaminants) {
  for (tp in types) {

    # --- Logit: log(OR) ---
    plots_logit_log <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      key <- paste(dep, cont, tp, sep = "_")
      data_one <- results_logit[[key]]
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

    plots_logit_log <- plots_logit_log[!sapply(plots_logit_log, is.null)]
    if (length(plots_logit_log) > 0) {
      fig_logit_log <- ggpubr::ggarrange(
        plotlist = plots_logit_log,
        ncol = 2,
        nrow = 4,
        align = "hv"
      )
      list_log_scale[[paste0("logit_", cont, "_", tp)]] <- fig_logit_log

      outfile <- sprintf("03_Output/DLM/Plots_logit/OR_%s_%s.png", cont, tp)
      dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
      ggplot2::ggsave(outfile,
        plot = fig_logit_log,
        res = 300,
        width = 20,
        height = 24,
        units = "cm",
        device = ragg::agg_png
      )
      message("Saved: ", outfile)
    }

    # --- Logit: OR ---
    plots_logit_or <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      key <- paste(dep, cont, tp, sep = "_")
      data_one <- results_logit[[key]]
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

    plots_logit_or <- plots_logit_or[!sapply(plots_logit_or, is.null)]
    if (length(plots_logit_or) > 0) {
      fig_logit_or <- ggpubr::ggarrange(
        plotlist = plots_logit_or,
        ncol = 2,
        nrow = 4,
        align = "hv"
      )
      list_ratio_scale[[paste0("logit_", cont, "_", tp)]] <- fig_logit_or

      outfile <- sprintf("03_Output/DLM/Plots_logit/OR_%s_%s_ratio.png", cont, tp)
      ggplot2::ggsave(outfile,
        plot = fig_logit_or,
        res = 300,
        width = 20,
        height = 24,
        units = "cm",
        device = ragg::agg_png
      )
      message("Saved: ", outfile)
    }

    # --- Cox: log(HR) ---
    plots_cox_log <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      key <- paste(dep, cont, tp, sep = "_")
      data_one <- results_cox[[key]]
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

    plots_cox_log <- plots_cox_log[!sapply(plots_cox_log, is.null)]
    if (length(plots_cox_log) > 0) {
      fig_cox_log <- ggpubr::ggarrange(
        plotlist = plots_cox_log,
        ncol = 2,
        nrow = 4,
        align = "hv"
      )
      list_log_scale[[paste0("cox_", cont, "_", tp)]] <- fig_cox_log

      outfile <- sprintf("03_Output/DLM/Plots_cox/HR_%s_%s.png", cont, tp)
      dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
      ggplot2::ggsave(outfile,
        plot = fig_cox_log,
        res = 300,
        width = 20,
        height = 24,
        units = "cm",
        device = ragg::agg_png
      )
      message("Saved: ", outfile)
    }

    # --- Cox: HR ---
    plots_cox_hr <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      key <- paste(dep, cont, tp, sep = "_")
      data_one <- results_cox[[key]]
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

    plots_cox_hr <- plots_cox_hr[!sapply(plots_cox_hr, is.null)]
    if (length(plots_cox_hr) > 0) {
      fig_cox_hr <- ggpubr::ggarrange(
        plotlist = plots_cox_hr,
        ncol = 2,
        nrow = 4,
        align = "hv"
      )
      list_ratio_scale[[paste0("cox_", cont, "_", tp)]] <- fig_cox_hr

      outfile <- sprintf("03_Output/DLM/Plots_cox/HR_%s_%s_ratio.png", cont, tp)
      ggplot2::ggsave(outfile,
        plot = fig_cox_hr,
        res = 300,
        width = 20,
        height = 24,
        units = "cm",
        device = ragg::agg_png
      )
      message("Saved: ", outfile)
    }
  }
}
toc() # Time plotes DLM: 17,549 sec elapsed

## 6 Save compiled figure lists ----

save(list_log_scale, list_ratio_scale,
  file = "03_Output/DLM/Plots_compiled_figures.RData"
)
