# 2.0 Descriptives analysis -----

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
data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")
data_geo <- rio::import("01_Input/Data_full_sample_exposure_geo.RData")
glimpse(data)
glimpse(data_geo)

data_des <- data |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |> 
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga", 
         "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac",
         "education", "health_insurance", "job", "first_birth", "para", "cesarea",
         starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_")) |> 
  select(-"birth_extremely_preterm", -"birth_term", -"birth_posterm") |> 
  filter(!is.na(lbw | tlbw | sga)) |> 
  filter(edad_gest >= 28) |> 
  mutate(cesarea = factor(cesarea, levels = c(0, 1), labels = c("Spontaneous", "Cesarean"))) 

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

po_trends_plot

ggsave(paste0(outfile, "/PO_time_trends.png"),
        plot = po_trends_plot,
        res = 300,
        width = 20,
        height = 20,
        scale = 1.25,
        units = "cm",
        device = ragg::agg_png
      )

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
writexl::write_xlsx(sheets_comuna, path = paste0(outfile, "/PO_time_trends_by_comuna.xlsx"))


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

po_trends_comuna_plot

ggsave(paste0(outfile, "/PO_time_trends_by_comuna.png"),
       plot = po_trends_comuna_plot,
       res = 300,
       width = 24,
       height = 28,
       scale = 1.25,
       units = "cm",
       device = ragg::agg_png
)
  

## 4 Exposure levels table by group ----

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

## 5. Descriptive stats ----

table_des <- data_des |>
  select(
    starts_with("birth_"), lbw, tlbw, sga,
    edad_gest, sexo_rn, 
    edad_madre, education, health_insurance, job, first_birth, para, cesarea,
    comuna, a_nac, mes_nac
    ) 

# Groups: All sample + 7 perinatal outcomes
group_labels_des <- c(
  "All" = "All sample",
  "birth_preterm" = "Preterm birth",
  "birth_very_preterm" = "Very preterm birth",
  "birth_moderately_preterm" = "Moderately preterm birth",
  "birth_late_preterm" = "Late preterm birth",
  "lbw" = "Low birth weight",
  "tlbw" = "Very low birth weight",
  "sga" = "Small for gestational age"
)

# Build descriptives: numeric = Mean (SD), categorical = XX.X% (n=XX)
# Format with 1 decimal, period as separator
format_pct <- function(pct, n) sprintf("%.1f%% (n=%d)", pct, n)
format_mean_sd_des <- function(m, s) sprintf("%.1f (SD=%.1f)", m, s)

# Define row structure from full sample (all variables to describe by outcome)
sexo_levels <- sort(unique(na.omit(as.character(table_des$sexo_rn))))
education_levels <- sort(unique(na.omit(as.character(table_des$education))))
health_insurance_levels <- sort(unique(na.omit(as.character(table_des$health_insurance))))
job_levels <- sort(unique(na.omit(as.character(table_des$job))))
first_birth_levels <- sort(unique(na.omit(as.character(table_des$first_birth))))
cesarea_levels <- sort(unique(na.omit(as.character(table_des$cesarea))))
comuna_levels <- sort(unique(na.omit(as.character(table_des$comuna))))
a_nac_levels <- sort(unique(na.omit(table_des$a_nac)))
mes_levels <- sort(unique(na.omit(table_des$mes_nac)))

rows_var <- c(
  "N",
  "edad_gest", "edad_madre", "para",
  rep("sexo_rn", length(sexo_levels)),
  rep("education", length(education_levels)),
  rep("health_insurance", length(health_insurance_levels)),
  rep("job", length(job_levels)),
  rep("first_birth", length(first_birth_levels)),
  rep("cesarea", length(cesarea_levels)),
  rep("comuna", length(comuna_levels)),
  rep("a_nac", length(a_nac_levels)),
  rep("mes_nac", length(mes_levels))
)
rows_char <- c(
  "",
  "Mean (SD)", "Mean (SD)", "Mean (SD)",
  sexo_levels,
  education_levels,
  health_insurance_levels,
  job_levels,
  first_birth_levels,
  cesarea_levels,
  comuna_levels,
  as.character(a_nac_levels),
  as.character(mes_levels)
)

# For each group, compute value for each row
tab_des_list <- list(Variable = rows_var, Characteristic = rows_char)

for (g in names(group_labels_des)) {
  dat <- if (g == "All") table_des else table_des |> filter(.data[[g]] == 1)
  n_g <- nrow(dat)
  vals <- character(length(rows_var))
  i <- 0L

  # N
  i <- i + 1L
  vals[i] <- sprintf("N=%d", n_g)

  # edad_gest
  i <- i + 1L
  vals[i] <- format_mean_sd_des(mean(dat$edad_gest, na.rm = TRUE), sd(dat$edad_gest, na.rm = TRUE))

  # edad_madre
  i <- i + 1L
  vals[i] <- format_mean_sd_des(mean(dat$edad_madre, na.rm = TRUE), sd(dat$edad_madre, na.rm = TRUE))

  # para
  i <- i + 1L
  vals[i] <- format_mean_sd_des(mean(dat$para, na.rm = TRUE), sd(dat$para, na.rm = TRUE))

  # sexo_rn
  for (lev in sexo_levels) {
    i <- i + 1L
    n_lev <- sum(dat$sexo_rn == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # education
  for (lev in education_levels) {
    i <- i + 1L
    n_lev <- sum(dat$education == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # health_insurance
  for (lev in health_insurance_levels) {
    i <- i + 1L
    n_lev <- sum(dat$health_insurance == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # job
  for (lev in job_levels) {
    i <- i + 1L
    n_lev <- sum(dat$job == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # first_birth
  for (lev in first_birth_levels) {
    i <- i + 1L
    n_lev <- sum(dat$first_birth == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # cesarea
  for (lev in cesarea_levels) {
    i <- i + 1L
    n_lev <- sum(dat$cesarea == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # comuna
  for (lev in comuna_levels) {
    i <- i + 1L
    n_lev <- sum(dat$comuna == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # a_nac
  for (lev in a_nac_levels) {
    i <- i + 1L
    n_lev <- sum(dat$a_nac == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  # mes_nac
  for (lev in mes_levels) {
    i <- i + 1L
    n_lev <- sum(dat$mes_nac == lev, na.rm = TRUE)
    vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
  }

  tab_des_list[[group_labels_des[g]]] <- vals
}

tab_descriptives <- as.data.frame(tab_des_list, stringsAsFactors = FALSE)
# Restore readable column names (as.data.frame converts spaces to dots)
names(tab_descriptives)[-(1:2)] <- as.character(group_labels_des)

writexl::write_xlsx(list(Descriptives = tab_descriptives), path = paste0(outfile, "/Table_descriptives.xlsx"))

## 6. Table Birth ----

des_births <- table_des |> 
  select(starts_with("birth_"), "lbw", "tlbw", "sga") |>
  pivot_longer(everything(), names_to = "Outcome", values_to = "Prevalence") |> 
  group_by(Outcome) |> 
  summarise(
    Prevalence_pct = mean(Prevalence, na.rm = TRUE)*100,
    n = sum(Prevalence, na.rm = TRUE)
  ) 

writexl::write_xlsx(list(Descriptives = des_births), path = paste0(outfile, "/Table_births_descriptives.xlsx"))

## 7. Correlation exposure metrics ----

# Correlate t1, t2, t3, overall within each contaminant (cs and sp separately)
# Table: contaminante, tiempo | t1, t2, t3, overall (cs) | t1, t2, t3, overall (sp)
contaminants <- c("PM25", "Levo", "K")
time_windows <- c("t1", "t2", "t3", "overall")

format_cor <- function(x) sprintf("%.3f", x)
format_pval <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

tab_cor_list <- list()
tab_corr_pval_list <- list()
for (cont in contaminants) {
  vars_cs <- paste0(c("t1", "t2", "t3", "tot"), "_", cont, "_cs")
  vars_sp <- paste0(c("t1", "t2", "t3", "tot"), "_", cont, "_sp")

  dat_cs <- data_des |> select(all_of(vars_cs))
  dat_sp <- data_des |> select(all_of(vars_sp))

  cor_cs <- cor(dat_cs, use = "pairwise.complete.obs")
  cor_sp <- cor(dat_sp, use = "pairwise.complete.obs")

  for (i in seq_along(time_windows)) {
    tw <- time_windows[i]
    row_cs <- format_cor(cor_cs[i, ])
    row_sp <- format_cor(cor_sp[i, ])
    tab_cor_list <- c(tab_cor_list, list(data.frame(
      contaminante = cont,
      tiempo = tw,
      t1_cs = row_cs[1],
      t2_cs = row_cs[2],
      t3_cs = row_cs[3],
      overall_cs = row_cs[4],
      t1_sp = row_sp[1],
      t2_sp = row_sp[2],
      t3_sp = row_sp[3],
      overall_sp = row_sp[4],
      stringsAsFactors = FALSE
    )))

    # P-values from cor.test (same structure as correlations)
    pval_cs <- numeric(4)
    pval_sp <- numeric(4)
    for (j in 1:4) {
      ct_cs <- tryCatch(cor.test(dat_cs[[i]], dat_cs[[j]], exact = FALSE),
        error = function(e) list(p.value = NA_real_))
      ct_sp <- tryCatch(cor.test(dat_sp[[i]], dat_sp[[j]], exact = FALSE),
        error = function(e) list(p.value = NA_real_))
      pval_cs[j] <- ct_cs$p.value
      pval_sp[j] <- ct_sp$p.value
    }
    row_pval_cs <- format_pval(pval_cs)
    row_pval_sp <- format_pval(pval_sp)
    tab_corr_pval_list <- c(tab_corr_pval_list, list(data.frame(
      contaminante = cont,
      tiempo = tw,
      t1_cs = row_pval_cs[1],
      t2_cs = row_pval_cs[2],
      t3_cs = row_pval_cs[3],
      overall_cs = row_pval_cs[4],
      t1_sp = row_pval_sp[1],
      t2_sp = row_pval_sp[2],
      t3_sp = row_pval_sp[3],
      overall_sp = row_pval_sp[4],
      stringsAsFactors = FALSE
    )))
  }
}

tab_correlations <- bind_rows(tab_cor_list)
tab_corr_pval <- bind_rows(tab_corr_pval_list)

# Columns: contaminante, tiempo | t1, t2, t3, overall (cs) | t1, t2, t3, overall (sp)
names(tab_correlations) <- c("contaminante", "tiempo",
  "t1_cs", "t2_cs", "t3_cs", "overall_cs",
  "t1_sp", "t2_sp", "t3_sp", "overall_sp")
names(tab_corr_pval) <- names(tab_correlations)

tab_correlations
tab_corr_pval

writexl::write_xlsx(
  list(Correlations = tab_correlations, P_values = tab_corr_pval),
  path = paste0(outfile, "/Table_correlations_exposure.xlsx")
)

# Scatter plots: t1 vs t2, t1 vs t3, t1 vs overall, t2 vs t3, t2 vs overall, t3 vs overall
# Two panels (cs, sp), facet: 6 columns (trimestres) x 3 rows (contaminants), contaminant legend on the right
pairs_list <- list(
  c("t1", "t2"), c("t1", "t3"), c("t2", "t3"), c("t1", "tot"), c("t2", "tot"), c("t3", "tot")
)
pair_labels <- c(
  "T1 vs T2", "T1 vs T3", "T2 vs T3", "T1 vs Overall", "T2 vs Overall", "T3 vs Overall"
)
contaminant_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

make_cor_label <- function(x, y, data) {
  test <- cor.test(data[[x]], data[[y]], exact = FALSE)
  r <- formatC(test$estimate, format = "f", digits = 2, decimal.mark = ".")
  p <- sub(",", ".", format.pval(test$p.value, digits = 3, eps = .001), fixed = TRUE)
  paste0("r = ", r, ", p", p)
}

build_scatter_data <- function(type_suffix) {
  out_list <- list()
  for (cont in contaminants) {
    for (k in seq_along(pairs_list)) {
      x_var <- paste0(pairs_list[[k]][1], "_", cont, type_suffix)
      y_var <- paste0(pairs_list[[k]][2], "_", cont, type_suffix)
      dat <- data_des |>
        select(x = all_of(x_var), y = all_of(y_var)) |>
        filter(complete.cases(x, y))
      if (nrow(dat) < 10) next
      fit <- lm(y ~ x, data = dat)
      dat$resid <- abs(residuals(fit))
      dat$intensity <- 1 - (dat$resid / max(dat$resid, na.rm = TRUE))
      dat$contaminant <- factor(contaminant_labels[cont], levels = contaminant_labels)
      dat$pair <- factor(pair_labels[k], levels = pair_labels)
      out_list[[paste(cont, k, sep = "_")]] <- dat
    }
  }
  bind_rows(out_list)
}

build_label_data <- function(type_suffix) {
  out_list <- list()
  for (cont in contaminants) {
    for (k in seq_along(pairs_list)) {
      x_var <- paste0(pairs_list[[k]][1], "_", cont, type_suffix)
      y_var <- paste0(pairs_list[[k]][2], "_", cont, type_suffix)
      dat <- data_des |> select(all_of(c(x_var, y_var)))
      dat <- dat[complete.cases(dat), , drop = FALSE]
      if (nrow(dat) < 10) next
      lab <- make_cor_label(x_var, y_var, dat)
      x_pos <- max(dat[[x_var]], na.rm = TRUE)
      y_pos <- if (type_suffix == "_cs") {
        if (cont == "PM25") 100 else if (cont == "Levo") 1.5 else 1.75
      } else {
        if (cont == "PM25") 300 else if (cont == "Levo") 1.75 else 4
      }
      out_list[[paste(cont, k, sep = "_")]] <- data.frame(
        contaminant = factor(contaminant_labels[cont], levels = contaminant_labels),
        pair = factor(pair_labels[k], levels = pair_labels),
        label = lab,
        x_pos = x_pos,
        y_pos = y_pos
      )
    }
  }
  bind_rows(out_list)
}

plot_scatter_one_contaminant <- function(data_plot, data_labels) {
  ggplot(data_plot, aes(x = x, y = y, color = intensity)) +
    geom_point(alpha = 0.7, size = 1.2) +
    scale_color_viridis_c(option = "plasma", limits = c(0, 1), guide = "none") +
    geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha = 0.3, linewidth = 1, se = TRUE, inherit.aes = FALSE, aes(x = x, y = y)) +
    geom_text(
      data = data_labels,
      aes(x = x_pos, y = y_pos, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 1,
      size = 3.5
    ) +
    facet_grid(contaminant ~ pair, scales = "free", switch = "y") +
    scale_x_continuous(labels = scales::label_number(decimal.mark = "."), limits = c(0, NA), expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = "."), limits = c(0, NA), expand = c(0.02, 0)) +
    labs(x = NULL, y = NULL) +
    theme_light(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      strip.text.x = element_text(size = 10, color = "black"),
      strip.text.y.left = element_text(angle = 90, size = 10, color = "black", face = "bold"),
      #strip.text.y = element_text(angle = 0, size = 10, color = "black", face = "bold"),
      strip.placement = "outside",
      strip.background = element_rect(fill = "white", color = "gray95"),
      legend.position = "none"
    )
}

data_cs <- build_scatter_data("_cs")
data_sp <- build_scatter_data("_sp")
labels_cs <- build_label_data("_cs")
labels_sp <- build_label_data("_sp")

# One figure per contaminant row, then combine into 3-row figure for cs and sp
plots_cs <- lapply(contaminant_labels, function(cont_label) {
  dat <- filter(data_cs, contaminant == cont_label)
  lab <- filter(labels_cs, contaminant == cont_label)
  if (nrow(dat) == 0) return(NULL)
  plot_scatter_one_contaminant(dat, lab)
})
plots_cs <- plots_cs[!sapply(plots_cs, is.null)]

plots_sp <- lapply(contaminant_labels, function(cont_label) {
  dat <- filter(data_sp, contaminant == cont_label)
  lab <- filter(labels_sp, contaminant == cont_label)
  if (nrow(dat) == 0) return(NULL)
  plot_scatter_one_contaminant(dat, lab)
})
plots_sp <- plots_sp[!sapply(plots_sp, is.null)]

fig_scatter_cs <- ggpubr::ggarrange(
  plotlist = plots_cs,
  nrow = length(plots_cs),
  ncol = 1,
  heights = rep(1, length(plots_cs))
)

fig_scatter_cs 

fig_scatter_sp <- ggpubr::ggarrange(
  plotlist = plots_sp,
  nrow = length(plots_sp),
  ncol = 1,
  heights = rep(1, length(plots_sp))
)

fig_scatter_sp

ggsave(
  paste0(outfile, "/Scatter_correlations_exposure_cs.png"),
  plot = fig_scatter_cs,
  width = 36,
  height = 7 * length(plots_cs),
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

ggsave(
  paste0(outfile, "/Scatter_correlations_exposure_sp.png"),
  plot = fig_scatter_sp,
  width = 36,
  height = 7 * length(plots_sp),
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

## 8. Maps exposure ----

if (!inherits(data_geo, "sf")) {
  data_geo <- st_as_sf(data_geo, sf_column_name = "geometry")
}
data_geo_we <- data_geo |>
  left_join(
    select(data, idbase, starts_with("t1_"), starts_with("t2_"), starts_with("t3_"), starts_with("tot_")),
    by = "idbase"
  )
data_geo_wgs <- st_transform(data_geo_we, 4326)

# Palettes per contaminant (low -> high)
palette_PM25 <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
palette_Levo <- c("#FFFFFF", "#D4EDDA", "#28A745", "#1E5631", "#0D2818")
palette_K <- c("#FFFFFF", "#E8DAEF", "#9B59B6", "#5B2C6F", "#1A0A1A")

# Period and contaminant labels
map_periods <- c("t1" = "T1", "t2" = "T2", "t3" = "T3", "tot" = "Overall")
map_contaminants <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "K")

# Pivot to long format for mapping
pivot_geo_long <- function(geo_sf, type_suffix) {
  vars <- paste0(rep(c("t1", "t2", "t3", "tot"), each = 3), "_",
                 rep(c("PM25", "Levo", "K"), 4), type_suffix)
  geo_sf |>
    select(idbase, geometry, any_of(vars)) |>
    tidyr::pivot_longer(cols = any_of(vars), names_to = "var", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(
      period = factor(map_periods[sub("_.*", "", var)], levels = c("T1", "T2", "T3", "Overall")),
      contaminant = factor(
        unname(map_contaminants[sapply(strsplit(var, "_"), `[`, 2)]),
        levels = c("PM2.5", "Levoglucosan", "K")
      )
    )
}

data_map_cs <- pivot_geo_long(data_geo_wgs, "_cs")
data_map_sp <- pivot_geo_long(data_geo_wgs, "_sp")

# Unified limits per contaminant
get_map_limits <- function(data_map) {
  data_map |> group_by(contaminant) |> summarise(min_val = min(value, na.rm = TRUE), max_val = max(value, na.rm = TRUE), .groups = "drop")
}

limits_cs <- get_map_limits(data_map_cs)
limits_sp <- get_map_limits(data_map_sp)

# Plot one contaminant row (4 maps: T1, T2, T3, Overall)
# map_base: ggmap or SpatRaster object for base layer; NULL for no base
plot_map_row <- function(data_map, limits_df, palette, map_base = NULL) {
  cont <- as.character(unique(data_map$contaminant))
  lims <- limits_df |> filter(contaminant == cont)
  r_min <- lims$min_val
  r_max <- lims$max_val

  if (!is.null(map_base) && inherits(map_base, "ggmap")) {
    p <- ggmap(map_base) +
      geom_sf(data = data_map, aes(color = value), size = 0.5, alpha = 0.85, inherit.aes = FALSE)
  } else if (!is.null(map_base) && inherits(map_base, "SpatRaster") && requireNamespace("tidyterra", quietly = TRUE)) {
    p <- ggplot() +
      tidyterra::geom_spatraster_rgb(data = map_base) +
      geom_sf(data = data_map, aes(color = value), size = 0.5, alpha = 0.85)
  } else {
    p <- ggplot(data_map) +
      geom_sf(aes(color = value), size = 0.2, alpha = 0.8)
  }
  p +
  scale_color_gradientn(
      colors = palette,
      limits = c(r_min, r_max),
      name = NULL,
      na.value = "grey90",
      breaks = seq(r_min, r_max, length.out = 5),
      labels = function(x) format(round(x, 1), nsmall = 1, decimal.mark = "."),
      guide = guide_colorbar(
        barwidth = 12,
        barheight = 0.4,
        nbin = 5,
        label.position = "bottom",
        title = NULL
      )
    ) +
    facet_grid(contaminant ~ period) +
    #labs(title = cont) +
    theme_light(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 7),
      strip.text.x = element_text(size = 10 , color = "black"),
      strip.text.y = element_text(angle = 0, size = 10, color = "black", face = "bold"),
      strip.placement = "outside",
      strip.background = element_rect(fill = "white", color = "white"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0, face = "bold", size = 10),
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
}

# Build map figures: CS and SP separately (4 cols x 3 rows)
build_map_figure <- function(data_map, limits_df, map_base = NULL) {
  plots_list <- list()
  for (cont in c("PM2.5", "Levoglucosan", "K")) {
    dat <- filter(data_map, contaminant == cont)
    if (nrow(dat) == 0) next
    pal <- switch(cont,
      "PM2.5" = palette_PM25,
      "Levoglucosan" = palette_Levo,
      "K" = palette_K,
      palette_K
    )
    plots_list[[cont]] <- plot_map_row(dat, limits_df, pal, map_base = map_base)
  }

  ggpubr::ggarrange(
    plotlist = plots_list,
    nrow = length(plots_list),
    ncol = 1,
    heights = rep(1, length(plots_list)),
    common.legend = FALSE,
    align = "v"
  )
}

fig_maps_cs <- build_map_figure(data_map_cs, limits_cs)
fig_maps_sp <- build_map_figure(data_map_sp, limits_sp)

ggsave(
  paste0(outfile, "/Maps_Exposure_cs.png"),
  plot = fig_maps_cs,
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

ggsave(
  paste0(outfile, "/Maps_Exposure_sp.png"),
  plot = fig_maps_sp,
  width = 30,
  height = 20,
  units = "cm",
  res = 300,
  device = ragg::agg_png
)

## 9. Base map download (Temuco - Padre las Casas) ----

bbox_data <- st_bbox(data_geo_wgs)
pad_x <- (bbox_data["xmax"] - bbox_data["xmin"]) * 0.08
pad_y <- (bbox_data["ymax"] - bbox_data["ymin"]) * 0.08
bbox <- c(
  left   = as.numeric(bbox_data["xmin"] - pad_x),
  bottom = as.numeric(bbox_data["ymin"] - pad_y),
  right  = as.numeric(bbox_data["xmax"] + pad_x),
  top    = as.numeric(bbox_data["ymax"] + pad_y)
)
map_base_temuco <- NULL
for (z in c(14, 13, 12, 11, 10)) {
  map_base_temuco <- tryCatch(
    get_stadiamap(bbox, zoom = z, maptype = "stamen_toner_lite"),
    error = function(e) NULL
  )
  if (!is.null(map_base_temuco)) {
    message("Stadia Maps: zoom ", z, " OK")
    break
  }
}
if (is.null(map_base_temuco)) {
  message("get_stadiamap failed, using maptiles")
  if (requireNamespace("maptiles", quietly = TRUE)) {
    library(maptiles)
    map_base_temuco <- get_tiles(data_geo_wgs, provider = "OpenStreetMap", zoom = 15, crop = TRUE)
  }
}

# Save standalone base map (optional reference)
if (!is.null(map_base_temuco)) {
  if (inherits(map_base_temuco, "SpatRaster")) {
    png(paste0(outfile, "/Map_base_Temuco_PadreLasCasas.png"), width = 35, height = 35, units = "cm", res = 600)
    terra::plotRGB(map_base_temuco)
    dev.off()
  } else {
    p <- ggmap(map_base_temuco)
    ggsave(paste0(outfile, "/Map_base_Temuco_PadreLasCasas.png"), plot = p, width = 35, height = 35, units = "cm", dpi = 600, device = ragg::agg_png)
  }
}

## 10. Maps exposure with base map ----

fig_maps_cs_basemap <- build_map_figure(data_map_cs, limits_cs, map_base = map_base_temuco)
fig_maps_sp_basemap <- build_map_figure(data_map_sp, limits_sp, map_base = map_base_temuco)

# Higher dpi for basemap figures to preserve raster quality in panels
ggsave(
  paste0(outfile, "/Maps_Exposure_cs_basemap.png"),
  plot = fig_maps_cs_basemap,
  width = 30,
  height = 20,
  units = "cm",
  dpi = 600,
  device = ragg::agg_png
)

ggsave(
  paste0(outfile, "/Maps_Exposure_sp_basemap.png"),
  plot = fig_maps_sp_basemap,
  width = 30,
  height = 20,
  units = "cm",
  dpi = 600,
  device = ragg::agg_png
)
