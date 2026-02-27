# 3.0 Plot exposure models -----

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
  "lbw"                      = "Low birth weight",
  "tlbw"                     = "Very low birth weight",
  "sga"                      = "Small for gestational age"
)

panel_labels <- setNames(
  paste0(LETTERS[seq_along(outcomes_order)], ". ", outcomes_labels[outcomes_order]),
  outcomes_order
)

## 4. Plotting function ----

plot_exposure_effects <- function(plot_data, y_label, contaminante_val, tipo_val) {

  data_sub <- plot_data |>
    dplyr::filter(contaminante == contaminante_val, tipo == tipo_val) |>
    dplyr::mutate(
      dependent_var = factor(dependent_var, levels = outcomes_order)
    )

  if (nrow(data_sub) == 0) return(NULL)

  pd <- position_dodge(width = 0.6)

  rect_data <- data.frame(xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf)

  # Use log scale columns for plot (log_or or log_hr depending on model type)
  y_var <- if ("log_or" %in% names(data_sub)) "log_or" else "log_hr"
  ymin_var <- if ("log_or" %in% names(data_sub)) "log_or_conf.low" else "log_hr_conf.low"
  ymax_var <- if ("log_or" %in% names(data_sub)) "log_or_conf.high" else "log_hr_conf.high"

  ggplot(data_sub, aes(y = .data[[y_var]], x = exposure, color = adjustment, shape = adjustment)) +
    geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE, fill = "grey95", alpha = 0.7
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = .data[[ymin_var]], ymax = .data[[ymax_var]]),
      width = 0.25,
      position = pd
    ) +
    geom_point(size = 2, position = pd) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(
      n.breaks = 6,
      labels = scales::label_number(decimal.mark = ".")
    ) +
    scale_x_discrete(expand = c(0.05, 0)) +
    labs(y = y_label, x = NULL) +
    facet_wrap(~dependent_var, ncol = 2, scales = "free",
      labeller = as_labeller(panel_labels)
    ) +
    theme_light(base_size = 10) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 9),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 9, color = "black", face = "bold", hjust = 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 8, angle = 0),
      axis.ticks.y = element_line(),
      plot.margin = margin(4, 4, 4, 4, "pt"),
      panel.spacing = unit(0.5, "lines")
    )
}

## 5. Save plots - Logit models ----

contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")

for (cont in contaminants) {
  for (tp in types) {
    g <- plot_exposure_effects(
      plot_data_logit,
      y_label = "log(OR) (95% CI)",
      contaminante_val = cont,
      tipo_val = tp
    )
    if (!is.null(g)) {
      outfile <- sprintf("03_Output/Models/Plots_logit/OR_%s_%s.png", cont, tp)
      ggsave(outfile,
        plot = g,
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

## 6. Save plots - Cox models ----

for (cont in contaminants) {
  for (tp in types) {
    g <- plot_exposure_effects(
      plot_data_cox,
      y_label = "log(HR) (95% CI)",
      contaminante_val = cont,
      tipo_val = tp
    )
    if (!is.null(g)) {
      outfile <- sprintf("03_Output/Models/Plots_cox/HR_%s_%s.png", cont, tp)
      ggsave(outfile,
        plot = g,
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
