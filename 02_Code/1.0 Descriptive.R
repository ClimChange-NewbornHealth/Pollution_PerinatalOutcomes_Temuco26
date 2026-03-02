# 1.0 Descriptives analysis -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

outfile <- "03_Output/Descriptives"

## Note perinatal outcomes definitions (no execute):

# Refs p10 weeks 28-42 (Alarcón & Pittaluga)
#ref_p10 <- tribble(
#  ~edad_gest, ~p10,
#    28,  945.7,
#    29, 1092.1,
#    30, 1258.2,
#    31, 1439.2,
#    32, 1630.8,
#    33, 1828.7,
#    34, 2028.6,
#    35, 2226.0,
#    36, 2416.7,
#    37, 2562.2,
#    38, 2760.2,
#    39, 2904.2,
#    40, 3024.1,
#    41, 3115.3,
#    42, 3173.5,
#    43,    NA_real_, 
#    44,    NA_real_
#)

#data <- data |> 
#  left_join(ref_p10, by = "edad_gest") |> 
#  mutate(p10 = if_else(is.na(p10), quantile(peso_rn, probs = 0.1, na.rm = TRUE), p10)) |> 
#  mutate(birth_preterm = if_else(edad_gest < 37, 1, 0)) |>
#  mutate(birth_extremely_preterm = if_else(edad_gest < 28, 1, 0)) |> 
#  mutate(birth_very_preterm = if_else(edad_gest >= 28 & edad_gest <32, 1, 0)) |> 
#  mutate(birth_moderately_preterm = if_else(edad_gest >= 32 & edad_gest <33, 1, 0)) |> 
#  mutate(birth_late_preterm = if_else(edad_gest >= 34 & edad_gest <37, 1, 0)) |> 
#  mutate(birth_term = if_else(edad_gest >= 37 & edad_gest <42, 1, 0)) |> 
#  mutate(birth_posterm = if_else(edad_gest >= 42, 1, 0)) |> 
#  mutate(lbw = if_else(peso_rn < 2500, 1, 0)) |> 
#  mutate(tlbw = if_else(peso_rn < 2500 & edad_gest >= 37, 1, 0)) |> 
#  mutate(sga = if_else(peso_rn < p10, 1, 0)) |> 
#  select(-p10) 

## 1. Load perinatal outcomes data ----
data <- rio::import("01_Input/Data_full_sample_exposure.RData")
glimpse(data)

data_des <- data |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |> 
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga", 
         "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac",
         starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_")) |> 
  select(-"birth_extremely_preterm", -"birth_term", -"birth_posterm") |> 
  filter(!is.na(lbw | tlbw | sga)) |> 
  filter(edad_gest >= 28)

glimpse(data_des)

# Labels outcomes 
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
  "birth_preterm"            = "A. Preterm birth",
  "birth_very_preterm"       = "B. Very preterm birth",
  "birth_moderately_preterm" = "C. Moderately preterm birth",
  "birth_late_preterm"       = "D. Late preterm birth",
  "lbw"                      = "E. Low birth weight",
  "tlbw"                     = "F. Very low birth weight",
  "sga"                      = "G. Small for gestational age"
)


## 2. Perinatal outcomes time trend ----

# Estimate perinatal outcomes (overall, base code unchanged)
po_trends <- data_des |> 
  mutate(month_nac = lubridate::as_date(paste0(a_nac, "-", mes_nac, "-01"))) |> 
  group_by(month_nac) |>
  summarise(
    n_births = n(),
    birth_preterm = mean(birth_preterm, na.rm = TRUE),
    birth_very_preterm = mean(birth_very_preterm, na.rm = TRUE),
    birth_moderately_preterm = mean(birth_moderately_preterm, na.rm = TRUE),
    birth_late_preterm = mean(birth_late_preterm, na.rm = TRUE),
    lbw = mean(lbw, na.rm = TRUE),
    tlbw = mean(tlbw, na.rm = TRUE),
    sga = mean(sga, na.rm = TRUE)
  ) 

# Plot trends (overall)
po_trends_plot  <- po_trends |> 
  select(-n_births) |>
  filter(month_nac >= "2009-11-01") |> 
  pivot_longer(-month_nac, names_to = "outcome") |>
  mutate(label = recode(outcome, !!!outcomes_labels)) |> 
  ggplot(aes(x = month_nac, y = value*100)) +
  geom_line(color = "#08519c") +
  geom_point(color = "#08519c", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color="gray30", alpha=0.5, linewidth=0.5) +
  labs(
    y = "Prevalence (per 100)",
    x = NULL,
  ) +
  facet_wrap(~label, ncol = 2, scales = "free") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 5) +
  theme_light() +
  theme(plot.title = element_text(size = 11, hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text = element_text(color = "black", size = 10, face = "bold", hjust = 0),
        panel.grid = element_blank()
  )

ggsave(paste0(outfile, "/PO_time_trends.png"),
        plot = po_trends_plot,
        res = 300,
        width = 20,
        height = 20,
        scale = 1.25,
        units = "cm",
        device = ragg::agg_png
      )

## 2.5 Exposure levels table by group ----

# Exposure variables: w20, t1, t2, t3, overall (tot) for each pollutant (PM25, Levo, K) and type (cs, sp)
exposure_vars <- names(data_des)[grepl("^(w20|t1|t2|t3|tot)_(PM25|Levo|K)_(cs|sp)$", names(data_des))]

# Groups: All sample + 7 perinatal outcomes (those with outcome = 1)
group_labels <- c(
  "All" = "All sample",
  "birth_preterm" = "Preterm birth",
  "birth_very_preterm" = "Very preterm birth",
  "birth_moderately_preterm" = "Moderately preterm birth",
  "birth_late_preterm" = "Late preterm birth",
  "lbw" = "Low birth weight",
  "tlbw" = "Very low birth weight",
  "sga" = "Small for gestational age"
)

# Format: mean (SD) with 2 decimals, period as separator
format_mean_sd <- function(m, s) {
  sprintf("%.2f (%.2f)", m, s)
}

# Build table: rows = exposure, cols = groups
tab_exposure_list <- list()
for (g in names(group_labels)) {
  if (g == "All") {
    dat <- data_des
  } else {
    dat <- data_des |> filter(.data[[g]] == 1)
  }
  means <- sapply(exposure_vars, function(v) mean(dat[[v]], na.rm = TRUE))
  sds   <- sapply(exposure_vars, function(v) sd(dat[[v]], na.rm = TRUE))
  tab_exposure_list[[group_labels[g]]] <- format_mean_sd(means, sds)
}

tab_exposure <- as.data.frame(tab_exposure_list, row.names = exposure_vars)
tab_exposure <- tab_exposure |> tibble::rownames_to_column("Exposure")

# Split into Contaminant, Exposure (window), and Type
pollutant_lab <- c("PM25" = "PM2.5", "Levo" = "Levo", "K" = "K")
window_lab   <- c("w20" = "w20", "t1" = "T1", "t2" = "T2", "t3" = "T3", "tot" = "Overall")
group_cols   <- setNames(make.names(group_labels), NULL)  # as.data.frame converts names
tab_exposure <- tab_exposure |>
  mutate(
    Contaminant = stringr::str_extract(Exposure, "(PM25|Levo|K)"),
    Type        = stringr::str_extract(Exposure, "(cs|sp)$"),
    Exposure    = stringr::str_extract(Exposure, "^(w20|t1|t2|t3|tot)")
  ) |>
  mutate(
    Contaminant = pollutant_lab[Contaminant],
    Exposure    = window_lab[Exposure]
  ) |>
  select(Contaminant, Exposure, Type, all_of(group_cols))

# Restore readable column names for Excel
names(tab_exposure)[-(1:3)] <- as.character(group_labels)

# Ensure period as decimal separator (options already set in 0.1 Settings)
writexl::write_xlsx(list(Exposure_levels = tab_exposure), path = paste0(outfile, "/Exposure_levels_by_group.xlsx"))

## 3. Iteration by comuna ----

# Comuna labels: TEM = Temuco, PLC = Padre las casas
comuna_labels <- c("TEM" = "Temuco", "PLC" = "Padre las casas")

# Estimate perinatal outcomes by comuna
po_trends_by_comuna <- data_des |> 
  mutate(month_nac = lubridate::as_date(paste0(a_nac, "-", mes_nac, "-01"))) |> 
  group_by(month_nac, comuna) |>
  summarise(
    n_births = n(),
    birth_preterm = mean(birth_preterm, na.rm = TRUE),
    birth_very_preterm = mean(birth_very_preterm, na.rm = TRUE),
    birth_moderately_preterm = mean(birth_moderately_preterm, na.rm = TRUE),
    birth_late_preterm = mean(birth_late_preterm, na.rm = TRUE),
    lbw = mean(lbw, na.rm = TRUE),
    tlbw = mean(tlbw, na.rm = TRUE),
    sga = mean(sga, na.rm = TRUE),
    .groups = "drop"
  )

# Save tables to Excel (one sheet per comuna)
sheets_comuna <- list()
for (com in names(comuna_labels)) {
  tab <- po_trends_by_comuna |>
    filter(comuna == com) |>
    select(-comuna)
  sheets_comuna[[comuna_labels[com]]] <- tab
}
# Add overall sheet
po_trends_export <- po_trends
sheets_comuna[["Overall"]] <- po_trends_export

dir.create(outfile, recursive = TRUE, showWarnings = FALSE)
writexl::write_xlsx(sheets_comuna, path = paste0(outfile, "/PO_time_trends_by_comuna.xlsx"))
message("Saved: ", outfile, "/PO_time_trends_by_comuna.xlsx")

# Plot by comuna: one plot per comuna, facet in one column, then ggarrange
plot_by_comuna <- function(data_trends, comuna_code, comuna_name) {
  data_trends |>
    select(-n_births) |>
    filter(month_nac >= "2009-11-01") |>
    pivot_longer(-month_nac, names_to = "outcome") |>
    mutate(label = recode(outcome, !!!outcomes_labels)) |>
    ggplot(aes(x = month_nac, y = value * 100)) +
    geom_line(color = "#08519c") +
    geom_point(color = "#08519c", size = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "gray30", alpha = 0.5, linewidth = 0.5) +
    labs(
      title = comuna_name,
      y = "Prevalence (per 100)",
      x = NULL
    ) +
    facet_wrap(~label, ncol = 1, scales = "free") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(n.breaks = 5) +
    theme_light() +
    theme(
      plot.title = element_text(size = 11, hjust = 0, face = "bold"),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(color = "black", size = 10, face = "bold", hjust = 0),
      panel.grid = element_blank()
    )
}

plot_temuco <- po_trends_by_comuna |>
  filter(comuna == "TEM") |>
  select(-comuna) |>
  plot_by_comuna("TEM", "Temuco")

plot_plc <- po_trends_by_comuna |>
  filter(comuna == "PLC") |>
  select(-comuna) |>
  plot_by_comuna("PLC", "Padre las casas")

# Combine with ggarrange
po_trends_comuna_plot <- ggpubr::ggarrange(
  plot_temuco,
  plot_plc,
  ncol = 2,
  nrow = 1,
  widths = c(1, 1)
)

ggsave(paste0(outfile, "/PO_time_trends_by_comuna.png"),
       plot = po_trends_comuna_plot,
       res = 300,
       width = 24,
       height = 28,
       scale = 1.25,
       units = "cm",
       device = ragg::agg_png
)
  