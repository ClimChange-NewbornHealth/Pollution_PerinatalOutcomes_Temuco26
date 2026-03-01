# 4.0 Plot exposure models -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")

## 1. Load results ----

models_logit <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "logit_models")
models_cox  <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "cox_models")

## 2. Prepare plot data ----

# Exposure categories: Overall (tot), Week 20 (w20), Trimesters (t1, t2, t3 from t1_t2_t3 model)
# For t1_t2_t3: term is repeated per outcome, so assign by row order (1st=t1, 2nd=t2, 3rd=t3)
prepare_plot_data <- function(models_df) {
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
        model_type == "single" & stringr::str_detect(term, "^w20_") ~ "Week 20",
        model_type == "t1_t2_t3" & dplyr::row_number() == 1 ~ "Trimester 1",
        model_type == "t1_t2_t3" & dplyr::row_number() == 2 ~ "Trimester 2",
        model_type == "t1_t2_t3" & dplyr::row_number() == 3 ~ "Trimester 3",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(exposure)) |>
    dplyr::mutate(
      exposure = factor(exposure,
        levels = c("Week 20", "Overall", "Trimester 1", "Trimester 2", "Trimester 3")
      ),
      adjustment = factor(adjustment, levels = c("Unadjusted", "Adjusted"))
    )
}

plot_data_logit <- prepare_plot_data(models_logit)
plot_data_cox  <- prepare_plot_data(models_cox)

# Ensure log columns exist (compute from OR/HR if missing, e.g. from older Excel)
if (!"log_or" %in% names(plot_data_logit)) {
  plot_data_logit <- plot_data_logit |>
    dplyr::mutate(
      log_or = log(estimate),
      log_or_conf.low = log(conf.low),
      log_or_conf.high = log(conf.high)
    )
}
if (!"log_hr" %in% names(plot_data_cox)) {
  plot_data_cox <- plot_data_cox |>
    dplyr::mutate(
      log_hr = log(estimate),
      log_hr_conf.low = log(conf.low),
      log_hr_conf.high = log(conf.high)
    )
}

## 3. Outcomes and panel labels ----

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
  "tlbw"                     = "Very low birth weight (tLBW)",
  "sga"                      = "Small for gestational age (SGA)"
)

panel_labels <- setNames(
  paste0(LETTERS[seq_along(outcomes_order)], ". ", outcomes_labels[outcomes_order]),
  outcomes_order
)

## 4. Single-outcome plot (for compilation with ggarrange) ----

# Creates one panel per outcome with y limits from data range for that outcome
# show_legend: use TRUE for first panel only (for ggarrange common.legend)
plot_single_outcome <- function(data_one_outcome, y_var, ymin_var, ymax_var,
                                ref_line, y_label, panel_label, show_legend = FALSE) {

  if (nrow(data_one_outcome) == 0) return(NULL)

  pd <- position_dodge(width = 0.6)
  rect_data <- data.frame(xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf)

  y_vals <- c(data_one_outcome[[ymin_var]], data_one_outcome[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  # Symmetric limits around reference line (proportional above and below no-effect)
  max_dist <- max(ref_line - y_min, y_max - ref_line)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  ggplot(data_one_outcome, aes(y = .data[[y_var]], x = exposure, color = adjustment, shape = adjustment)) +
    geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey95", alpha = 0.7
    ) +
    geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.25,
      position = pd
    ) +
    geom_point(size = 2, position = pd) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(
      limits = y_limits,
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    scale_x_discrete(expand = c(0.05, 0)) +
    labs(y = y_label, x = NULL, title = panel_label) +
    theme_light(base_size = 10) +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0),
      legend.position = if (show_legend) "top" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8, angle = 0),
      axis.ticks.y = element_line(),
      plot.margin = margin(4, 4, 4, 4, "pt")
    )
}

## 5. Build and compile figures ----

contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")

# Lists to store compiled figures for future use
list_log_scale <- list()   # log(OR) and log(HR)
list_ratio_scale <- list() # OR and HR

for (cont in contaminants) {
  for (tp in types) {

    # --- Logit: log(OR) ---
    data_logit <- plot_data_logit |>
      dplyr::filter(contaminante == cont, tipo == tp)

    plots_logit_log <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      data_one <- data_logit |> dplyr::filter(dependent_var == dep)
      p <- plot_single_outcome(
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
    }

    # Remove NULLs and compile
    plots_logit_log <- plots_logit_log[!sapply(plots_logit_log, is.null)]
    if (length(plots_logit_log) > 0) {
      fig_logit_log <- ggpubr::ggarrange(
        plotlist = plots_logit_log,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top"
      )
      list_log_scale[[paste0("logit_", cont, "_", tp)]] <- fig_logit_log

      outfile <- sprintf("03_Output/Models/Plots_logit/OR_%s_%s.png", cont, tp)
      ggsave(outfile,
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
      data_one <- data_logit |> dplyr::filter(dependent_var == dep)
      p <- plot_single_outcome(
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
    }

    plots_logit_or <- plots_logit_or[!sapply(plots_logit_or, is.null)]
    if (length(plots_logit_or) > 0) {
      fig_logit_or <- ggpubr::ggarrange(
        plotlist = plots_logit_or,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top"
      )
      list_ratio_scale[[paste0("logit_", cont, "_", tp)]] <- fig_logit_or

      outfile <- sprintf("03_Output/Models/Plots_logit/OR_%s_%s_ratio.png", cont, tp)  # OR (ratio scale)
      ggsave(outfile,
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
    data_cox <- plot_data_cox |>
      dplyr::filter(contaminante == cont, tipo == tp)

    plots_cox_log <- list()
    for (i in seq_along(outcomes_order)) {
      dep <- outcomes_order[i]
      data_one <- data_cox |> dplyr::filter(dependent_var == dep)
      p <- plot_single_outcome(
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
    }

    plots_cox_log <- plots_cox_log[!sapply(plots_cox_log, is.null)]
    if (length(plots_cox_log) > 0) {
      fig_cox_log <- ggpubr::ggarrange(
        plotlist = plots_cox_log,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top"
      )
      list_log_scale[[paste0("cox_", cont, "_", tp)]] <- fig_cox_log

      outfile <- sprintf("03_Output/Models/Plots_cox/HR_%s_%s.png", cont, tp)
      ggsave(outfile,
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
      data_one <- data_cox |> dplyr::filter(dependent_var == dep)
      p <- plot_single_outcome(
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
    }

    plots_cox_hr <- plots_cox_hr[!sapply(plots_cox_hr, is.null)]
    if (length(plots_cox_hr) > 0) {
      fig_cox_hr <- ggpubr::ggarrange(
        plotlist = plots_cox_hr,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top"
      )
      list_ratio_scale[[paste0("cox_", cont, "_", tp)]] <- fig_cox_hr

      outfile <- sprintf("03_Output/Models/Plots_cox/HR_%s_%s_ratio.png", cont, tp)  # HR (ratio scale)
      ggsave(outfile,
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

## 6. Save figure lists for future use ----

save(list_log_scale, list_ratio_scale,
  file = "03_Output/Models/Plots_compiled_figures.RData"
)
