# 5.0 Final figures for paper ----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")

outfile <- "03_Output/Fig_paper"
dir.create(outfile, showWarnings = FALSE, recursive = TRUE)

## 1. Load results ----

models_cox <- rio::import("03_Output/Models/Exposure_models_PO_logit_cox.xlsx", sheet = "cox_models")
if (!"exposure_scale" %in% names(models_cox)) {
  models_cox <- models_cox |>
    dplyr::mutate(exposure_scale = if_else(stringr::str_detect(term, "^iqr_"), "iqr", "raw"))
}

load("03_Output/DLM/DLM_PO_results.RData")
results_cox_dlm <- dlm_results$results_cox
dependent_vars_dlm <- dlm_results$dependent_vars
contaminants <- dlm_results$contaminants
types <- dlm_results$types
exposure_scales <- if ("exposure_scales" %in% names(dlm_results)) dlm_results$exposure_scales else c("raw", "iqr")

## 2. Prepare plot data (Cox) ----

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

get_result_dlm <- function(results_list, dep, cont, tp, scale) {
  key <- paste(dep, cont, tp, scale, sep = "_")
  res <- results_list[[key]]
  if (is.null(res) && scale == "raw") res <- results_list[[paste(dep, cont, tp, sep = "_")]]
  res
}

## 3. Plot functions (from 3.1 and 4.1) ----

# Cox: exposure categories on x, log(HR) or HR on y
plot_cox_single <- function(data_one, y_var, ymin_var, ymax_var, ref_line, y_label, panel_label, show_legend = FALSE) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  pd <- position_dodge(width = 0.6)
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

  ggplot(data_one, aes(y = .data[[y_var]], x = exposure, color = adjustment, shape = adjustment)) +
    geom_rect(data = rect_t1t3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "white"
    ) +
    geom_rect(data = rect_overall, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey95", alpha = 0.7
    ) +
    geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.25, position = pd
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

# Cox multi-contaminant: color = contaminant (for Fig 3)
# Ensure narrow CIs (e.g. PM2.5 near 0) are visible; moderate dodge for separation
plot_cox_multi_cont <- function(data_one, y_var, ymin_var, ymax_var, ref_line, y_label, panel_label, show_legend = FALSE) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  pd <- position_dodge(width = 0.45)
  rect_t1t3 <- data.frame(xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf)
  rect_overall <- data.frame(xmin = 3.5, xmax = 4.5, ymin = -Inf, ymax = Inf)

  y_vals <- c(data_one[[y_var]], data_one[[ymin_var]], data_one[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  max_dist <- max(ref_line - y_min, y_max - ref_line, 0.05)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  cont_colors <- c("PM25" = "#E41A1C", "Levo" = "#FF7F00", "K" = "#984EA3")

  ggplot(data_one, aes(y = .data[[y_var]], x = exposure, color = contaminante, shape = contaminante)) +
    geom_rect(data = rect_t1t3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "white"
    ) +
    geom_rect(data = rect_overall, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey95", alpha = 0.7
    ) +
    geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.2, position = pd
    ) +
    geom_point(size = 2, position = pd) +
    scale_color_manual(values = cont_colors, labels = c(PM25 = "PM2.5", Levo = "Levoglucosan", K = "K")) +
    scale_shape_manual(values = c(PM25 = 16, Levo = 17, K = 15), labels = c(PM25 = "PM2.5", Levo = "Levoglucosan", K = "K")) +
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
      legend.text = element_text(size = 8),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8, angle = 0),
      axis.ticks.y = element_line(),
      plot.margin = margin(4, 4, 4, 4, "pt")
    )
}

# DLM: week on x, log(HR) or HR on y
plot_dlm_single <- function(data_one, y_var, ymin_var, ymax_var, ref_line, y_label, panel_label, show_legend = FALSE) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  data_one <- data_one |> dplyr::filter(week <= 37)

  y_vals <- c(data_one[[y_var]], data_one[[ymin_var]], data_one[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  min_range <- if (ref_line == 0) 0.05 else 0.1
  max_dist <- max(ref_line - y_min, y_max - ref_line, min_range)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  ggplot(data_one, aes(x = week, y = .data[[y_var]])) +
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

# DLM multi-contaminant: color = contaminant (for Fig 4)
# Use position_dodge so 3 points per week stay at their week but don't overlap
plot_dlm_multi_cont <- function(data_one, y_var, ymin_var, ymax_var, ref_line, y_label, panel_label, show_legend = FALSE) {
  if (is.null(data_one) || nrow(data_one) == 0) return(NULL)

  data_one <- data_one |> dplyr::filter(week <= 37)

  y_vals <- c(data_one[[ymin_var]], data_one[[ymax_var]])
  y_vals <- y_vals[is.finite(y_vals)]
  if (length(y_vals) == 0) return(NULL)

  y_min <- min(y_vals)
  y_max <- max(y_vals)
  min_range <- if (ref_line == 0) 0.05 else 0.1
  max_dist <- max(ref_line - y_min, y_max - ref_line, min_range)
  y_limits <- c(ref_line - max_dist, ref_line + max_dist)

  cont_colors <- c("PM25" = "#E41A1C", "Levo" = "#FF7F00", "K" = "#984EA3")
  pd <- position_dodge(width = 0.8)

  ggplot(data_one, aes(x = week, y = .data[[y_var]], color = contaminante, group = contaminante)) +
    geom_hline(yintercept = ref_line, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.2, linewidth = 0.5, position = pd
    ) +
    geom_point(size = 1.5, position = pd) +
    scale_color_manual(values = cont_colors, labels = c(PM25 = "PM2.5", Levo = "Levoglucosan", K = "K")) +
    scale_y_continuous(
      limits = y_limits,
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    scale_x_continuous(breaks = seq(1, 37, by = 3), limits = c(0.5, 37.5)) +
    labs(y = y_label, x = "Gestational week", title = panel_label) +
    theme_light(base_size = 10) +
    theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0),
      legend.position = if (show_legend) "top" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      plot.margin = margin(4, 4, 4, 4, "pt")
    )
}

## 4. Labels ----

outcomes_preterm <- c(
  "birth_preterm",
  "birth_very_preterm",
  "birth_moderately_preterm",
  "birth_late_preterm"
)

outcomes_labels <- c(
  "birth_preterm"            = "Preterm birth (<37 weeks)",
  "birth_very_preterm"       = "Very preterm birth (28–31 weeks)",
  "birth_moderately_preterm" = "Moderately preterm birth (32–33 weeks)",
  "birth_late_preterm"       = "Late preterm birth (34–36 weeks)"
)

tipo_labels <- c("cs" = "FS", "sp" = "LUR")
cont_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

# Effect scale params: logHR vs HR
effect_params <- list(
  logHR = list(y_var = "log_hr", ymin = "log_hr_conf.low", ymax = "log_hr_conf.high", ref = 0, label = "log(HR) (95% CI)", suffix = "logHR"),
  HR = list(y_var = "estimate", ymin = "conf.low", ymax = "conf.high", ref = 1, label = "HR (95% CI)", suffix = "HR")
)

## 5. Figure 1: Cox preterm only - 3 cols (contaminants) x 2 rows (CS, LUR) ----

for (scale in exposure_scales) {
  plot_data <- prepare_plot_data_cox(models_cox) |>
    dplyr::filter(exposure_scale == scale, dependent_var == "birth_preterm")
  plot_data <- if ("log_hr" %in% names(plot_data)) plot_data else ensure_log_hr(plot_data)

  scale_suffix <- if (scale == "raw") "_raw" else "_iqr"

  for (eff in names(effect_params)) {
    ep <- effect_params[[eff]]

    plots_list <- list()
    for (cont in contaminants) {
      for (tp in types) {
        data_one <- plot_data |> dplyr::filter(contaminante == cont, tipo == tp)
        p <- plot_cox_single(
          data_one,
          y_var = ep$y_var,
          ymin_var = ep$ymin,
          ymax_var = ep$ymax,
          ref_line = ep$ref,
          y_label = ep$label,
          panel_label = paste(cont_labels[cont], "-", tipo_labels[tp]),
          show_legend = (cont == contaminants[1] && tp == types[1])
        )
        plots_list[[paste(cont, tp, sep = "_")]] <- p
      }
    }

    order_plots <- c("PM25_cs", "Levo_cs", "K_cs", "PM25_sp", "Levo_sp", "K_sp")
    plots_ordered <- plots_list[order_plots]
    plots_ordered <- plots_ordered[!sapply(plots_ordered, is.null)]

    if (length(plots_ordered) > 0) {
      fig <- ggpubr::ggarrange(
        plotlist = plots_ordered,
        ncol = 3,
        nrow = 2,
        common.legend = TRUE,
        legend = "top"
      )
      fpath <- file.path(outfile, paste0("Fig1_Cox_preterm_", ep$suffix, scale_suffix, ".png"))
      ggplot2::ggsave(fpath, plot = fig, res = 300, width = 30, height = 14, units = "cm", device = ragg::agg_png)
      message("Saved: ", fpath)
    }
  }
}

## 6. Figure 2: DLM preterm only - 3 cols x 2 rows ----

for (scale in exposure_scales) {
  scale_suffix <- if (scale == "raw") "_raw" else "_iqr"

  for (eff in names(effect_params)) {
    ep <- effect_params[[eff]]
    need_log <- (eff == "logHR")

    plots_list <- list()
    for (cont in contaminants) {
      for (tp in types) {
        data_one <- get_result_dlm(results_cox_dlm, "birth_preterm", cont, tp, scale)
        if (!is.null(data_one) && nrow(data_one) > 0) {
          if (need_log) data_one <- ensure_log_hr(data_one)
          p <- plot_dlm_single(
            data_one,
            y_var = ep$y_var,
            ymin_var = ep$ymin,
            ymax_var = ep$ymax,
            ref_line = ep$ref,
            y_label = ep$label,
            panel_label = paste(cont_labels[cont], "-", tipo_labels[tp]),
            show_legend = FALSE
          )
          plots_list[[paste(cont, tp, sep = "_")]] <- p
        } else {
          plots_list[[paste(cont, tp, sep = "_")]] <- NULL
        }
      }
    }

    order_plots <- c("PM25_cs", "Levo_cs", "K_cs", "PM25_sp", "Levo_sp", "K_sp")
    plots_ordered <- plots_list[order_plots]
    plots_ordered <- plots_ordered[!sapply(plots_ordered, is.null)]

    if (length(plots_ordered) > 0) {
      fig <- ggpubr::ggarrange(
        plotlist = plots_ordered,
        ncol = 3,
        nrow = 2,
        align = "hv"
      )
      fpath <- file.path(outfile, paste0("Fig2_DLM_preterm_", ep$suffix, scale_suffix, ".png"))
      ggplot2::ggsave(fpath, plot = fig, res = 300, width = 30, height = 14, units = "cm", device = ragg::agg_png)
      message("Saved: ", fpath)
    }
  }
}

## 7. Figure 3: Cox 4 preterm outcomes - 4 rows x 2 cols (CS | LUR) ----

for (scale in exposure_scales) {
  plot_data <- prepare_plot_data_cox(models_cox) |>
    dplyr::filter(exposure_scale == scale, dependent_var %in% outcomes_preterm) |>
    dplyr::filter(adjustment == "Adjusted")
  plot_data <- if ("log_hr" %in% names(plot_data)) plot_data else ensure_log_hr(plot_data)

  scale_suffix <- if (scale == "raw") "_raw" else "_iqr"

  for (eff in names(effect_params)) {
    ep <- effect_params[[eff]]

    plots_list <- list()
    for (dep in outcomes_preterm) {
      for (tp in types) {
        data_one <- plot_data |> dplyr::filter(dependent_var == dep, tipo == tp)
        p <- plot_cox_multi_cont(
          data_one,
          y_var = ep$y_var,
          ymin_var = ep$ymin,
          ymax_var = ep$ymax,
          ref_line = ep$ref,
          y_label = ep$label,
        panel_label = outcomes_labels[dep],
        show_legend = (dep == "birth_very_preterm" && tp == types[1])
      )
      plots_list[[paste(dep, tp, sep = "_")]] <- p
    }
  }

    # Row order: very, moderately, late preterm first; preterm birth (overall) at bottom
    order_plots <- c(
      "birth_very_preterm_cs", "birth_very_preterm_sp",
      "birth_moderately_preterm_cs", "birth_moderately_preterm_sp",
      "birth_late_preterm_cs", "birth_late_preterm_sp",
      "birth_preterm_cs", "birth_preterm_sp"
    )
    plots_ordered <- plots_list[order_plots]
    plots_ordered <- plots_ordered[!sapply(plots_ordered, is.null)]

    if (length(plots_ordered) > 0) {
      fig <- ggpubr::ggarrange(
        plotlist = plots_ordered,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top"
      )
      fpath <- file.path(outfile, paste0("Fig3_Cox_preterm_outcomes_", ep$suffix, scale_suffix, ".png"))
      ggplot2::ggsave(fpath, plot = fig, res = 300, width = 20, height = 24, units = "cm", device = ragg::agg_png)
      message("Saved: ", fpath)
    }
  }
}

## 8. Figure 4: DLM 4 preterm outcomes - 4 rows x 2 cols (CS | LUR) ----

for (scale in exposure_scales) {
  scale_suffix <- if (scale == "raw") "_raw" else "_iqr"

  for (eff in names(effect_params)) {
    ep <- effect_params[[eff]]
    need_log <- (eff == "logHR")

    plots_list <- list()
    for (dep in outcomes_preterm) {
      for (tp in types) {
        data_list <- list()
        for (cont in contaminants) {
          d <- get_result_dlm(results_cox_dlm, dep, cont, tp, scale)
          if (!is.null(d) && nrow(d) > 0) {
            if (need_log) d <- ensure_log_hr(d)
            d$contaminante <- cont
            data_list[[cont]] <- d
          }
        }
        data_one <- if (length(data_list) > 0) dplyr::bind_rows(data_list) else NULL

        p <- plot_dlm_multi_cont(
          data_one,
          y_var = ep$y_var,
          ymin_var = ep$ymin,
          ymax_var = ep$ymax,
          ref_line = ep$ref,
          y_label = ep$label,
          panel_label = outcomes_labels[dep],
          show_legend = (dep == "birth_very_preterm" && tp == types[1])
        )
        plots_list[[paste(dep, tp, sep = "_")]] <- p
      }
    }

    # Row order: very, moderately, late preterm first; preterm birth (overall) at bottom
    order_plots <- c(
      "birth_very_preterm_cs", "birth_very_preterm_sp",
      "birth_moderately_preterm_cs", "birth_moderately_preterm_sp",
      "birth_late_preterm_cs", "birth_late_preterm_sp",
      "birth_preterm_cs", "birth_preterm_sp"
    )
    plots_ordered <- plots_list[order_plots]
    plots_ordered <- plots_ordered[!sapply(plots_ordered, is.null)]

    if (length(plots_ordered) > 0) {
      fig <- ggpubr::ggarrange(
        plotlist = plots_ordered,
        ncol = 2,
        nrow = 4,
        common.legend = TRUE,
        legend = "top",
        align = "hv"
      )
      fpath <- file.path(outfile, paste0("Fig4_DLM_preterm_outcomes_", ep$suffix, scale_suffix, ".png"))
      ggplot2::ggsave(fpath, plot = fig, res = 300, width = 30, height = 24, units = "cm", device = ragg::agg_png)
      message("Saved: ", fpath)
    }
  }
}

