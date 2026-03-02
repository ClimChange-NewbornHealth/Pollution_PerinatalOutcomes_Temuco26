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

# Estimate perinatal outcomes 
po_trends <- data_des |> 
  mutate(month_nac = lubridate::as_date(paste0(a_nac, "-", mes_nac, "-01"))) |> 
  group_by(month_nac) |> # comuna
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

# Plot trends 
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
  