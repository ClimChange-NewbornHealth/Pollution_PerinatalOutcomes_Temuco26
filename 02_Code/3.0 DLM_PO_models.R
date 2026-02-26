# Code 6.1 DLM Malformaciones - Full Models ----
# Aplicación iterativa de DLM para todas las combinaciones de:
# - Variables dependientes: malf, malf_card_bin
# - Contaminantes: PM25, Levo, K
# - Tipos: cs, sp
# Total: 12 modelos

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")

## Open Data ---- 
data_full <- rio::import("Output/Data_malf_exposure_long.RData") 

# Variables
dependent_vars <- c("malf", "malf_card_bin")
contaminantes <- c("PM25", "Levo", "K")
tipos <- c("cs", "sp")
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")

## Preparamos los datos base ---- 
data_full_long <- data_full |> 
  filter(str_detect(tiempo, "w")) |>
  mutate(week = as.integer(str_extract(tiempo, "(?<=w)-?\\d+"))) |> 
  mutate(week_pct = as.integer(str_extract(tiempo, "(?<=w)-?\\d+"))+13) |> 
  arrange(malf, idbase, week)

## Función para procesar un modelo ----
process_dlm_model <- function(dep_var, contam, tipo_val, data_long, control_vars) {
  
  # Filtramos datos
  data_filtered <- data_long |> 
    filter(tipo == tipo_val & contaminante == contam) |>
    filter(week >= 0) |>
    select(all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion")))
  
  if (nrow(data_filtered) == 0) {
    return(list(results = NULL, plot = NULL))
  }
  
  # Transformamos a formato wide para exposición
  data_wide <- data_filtered |> 
    select(all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |> 
    pivot_wider(
      names_from = week, 
      values_from = "exposicion", 
      names_prefix = "exposicion_"
    )
  
  # Calculamos lagged exposures
  data_lagged <- data_filtered |> 
    select(all_of(c("idbase", dependent_vars, control_vars, "week", "exposicion"))) |> 
    arrange(idbase, week) |> 
    group_by(idbase) |> 
    dplyr::mutate(exposicion_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
      if (week[i] == 0) return(NA_real_)
      past_rows <- which(week < week[i])
      if (length(past_rows) == 0) return(NA_real_)
      weights <- 1 / (week[i] - week[past_rows])
      exposures <- exposicion[past_rows]
      sum(weights * exposures, na.rm = TRUE)
    }))
  
  setDT(data_lagged)
  
  # Lagged en formato wide
  data_lagged_wide <- data_lagged |> 
    select(idbase, week, exposicion_lagged) |>
    pivot_wider(
      names_from = week,
      values_from = exposicion_lagged,
      names_prefix = "exposicion_lagged_"
    ) |> 
    ungroup()
  
  # Unimos datos
  data_model <- left_join(data_wide, 
                          data_lagged_wide,
                          by = "idbase") |> 
    arrange(idbase) |> 
    drop_na(edad_madre) |> 
    filter(sexo_rn != "Indefinido")
  
  setDT(data_model)
  
  # Seleccionamos variables de exposición
  expo <- grep("^exposicion_\\d+$", names(data_model), value = TRUE)
  expo_lag <- grep("^exposicion_lagged_\\d+$", names(data_model), value = TRUE)
  
  if (length(expo) == 0 || length(expo_lag) == 0) {
    return(list(results = NULL, plot = NULL))
  }
  
  # Extraemos números de semana
  expo_weeks <- as.numeric(gsub("exposicion_", "", expo))
  expo_lag_weeks <- as.numeric(gsub("exposicion_lagged_", "", expo_lag))
  
  # Semanas comunes (donde existe el lag)
  common_weeks <- intersect(expo_weeks, expo_lag_weeks)
  common_weeks <- common_weeks[common_weeks >= 2 & common_weeks <= 39]
  
  if (length(common_weeks) == 0) {
    return(list(results = NULL, plot = NULL))
  }
  
  # Generamos vectores de trabajo
  expo_vars <- paste0("exposicion_", common_weeks)
  expo_vars_lag <- paste0("exposicion_lagged_", common_weeks)
  
  # Estimamos modelos
  res_combo <- data.table()
  
  for (i in seq_along(expo_vars)) {
    
    exp <- expo_vars[i]
    lag <- expo_vars_lag[i]
    control_str <- paste(control_vars, collapse = " + ")
    
    formula_str <- paste0(
      dep_var, " ~ ",
      exp, " + ", lag,
      " + ", control_str
    )
    
    fml <- as.formula(formula_str)
    mod <- glm(fml, data = data_model, family = binomial(link = "logit"))
    
    # Intervalos perfilados (escala logit)
    ci_prof <- suppressMessages(
      confint(mod, parm = exp)
    )

    dat <- data.frame(
      Week = common_weeks[i],
      Exposure = exp,
      Lagged = lag,
      `No Obs` = mod$df.null + 1,
      beta = unname(mod$coefficients[exp]),
      se = summary(mod)$coefficients[exp, "Std. Error"],
      Lower = ci_prof[1],
      Upper = ci_prof[2],
      AIC = AIC(mod),
      BIC = BIC(mod)
    )
    
    # CI and exp(beta)
    #dat$Lower <- dat$beta - (qnorm(0.975) * dat$se)
    #dat$Upper <- dat$beta + (qnorm(0.975) * dat$se)
    dat$beta_exp <- exp(dat$beta)
    dat$Lower_exp <- exp(dat$Lower)
    dat$Upper_exp <- exp(dat$Upper)
    
    res_combo <- rbind(res_combo, dat)
  }
  
  # Creamos gráfico
  g <- ggplot(res_combo, aes(x = Week, y = beta)) +
    geom_point() +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    #scale_y_continuous(limits = c(0.95, 1.05)) +
    #scale_y_continuous(limits = c(-1.5, 1.5)) +
    scale_x_continuous(breaks = seq(1, 41, by = 3)) +
    labs(
      x = "Gestational Weeks",
      y = "Log(OR) (95% CI)"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(list(results = res_combo, plot = g))
}

## Estimamos todos los modelos ----
all_results <- list()
all_plots <- list()

# Etiquetas para contaminantes
contam_labels <- c("PM25" = "PM2.5", "Levo" = "Levoglucosan", "K" = "Potasio")
dep_labels <- c("malf" = "Malformación General", "malf_card_bin" = "Malformación Cardíaca")

# Panel labels
panel_labels <- LETTERS[1:12]

# Orden correcto: primero todas las combinaciones de malf, luego malf_card_bin
# Dentro de cada dep_var: PM25-CS, PM25-SP, Levo-CS, Levo-SP, K-CS, K-SP
idx <- 1
for (dep_var in dependent_vars) {
  for (contam in contaminantes) {
    for (tipo_val in tipos) {
      
      model_name <- paste(dep_var, contam, tipo_val, sep = "_")
      
      result <- process_dlm_model(dep_var, contam, tipo_val, data_full_long, control_vars)
      
      if (!is.null(result$results)) {
        all_results[[model_name]] <- result$results
        
        # Agregamos título al gráfico
        plot_title <- paste0(
          panel_labels[idx], ". Exposición ", contam_labels[contam], 
          " - ", dep_labels[dep_var], " (", toupper(tipo_val), ")"
        )
        
        plot_with_title <- result$plot + 
          labs(title = plot_title) +
          theme(plot.title = element_text(size = 10, face = "bold"))
        
        all_plots[[model_name]] <- plot_with_title
        
        idx <- idx + 1
      }
    }
  }
}

## Vemos los resultados
all_plots
all_results

## Guardamos resultados en Excel ----
if (length(all_results) > 0) {
  write_xlsx(all_results, "Output/DLM/Malf_DLM_results_all_models.xlsx")
}

## Guardamos gráficos individuales ----
for (i in seq_along(all_plots)) {
  model_name <- names(all_plots)[i]
  filename <- paste0("Output/DLM/Malf_DLM_", model_name, ".png")
  
  ggsave(filename, all_plots[[i]], 
         res = 300, width = 15, height = 10, units = "cm",
         scaling = 0.9, device = ragg::agg_png)
}

## Creamos panel con todos los gráficos ----
if (length(all_plots) > 0) {
  # Reorganizamos los plots en el orden correcto para el panel
  # Orden: primero todas las combinaciones de malf, luego malf_card_bin
  # Dentro de cada dep_var: PM25-CS, PM25-SP, Levo-CS, Levo-SP, K-CS, K-SP
  plot_order <- c()
  for (dep_var in dependent_vars) {
    for (contam in contaminantes) {
      for (tipo_val in tipos) {
        model_name <- paste(dep_var, contam, tipo_val, sep = "_")
        if (model_name %in% names(all_plots)) {
          plot_order <- c(plot_order, model_name)
        }
      }
    }
  }
  
  plots_ordered <- all_plots[plot_order]
  
  # Creamos el panel: 2 columnas, 6 filas
  panel_plot <- ggarrange(
    plotlist = plots_ordered,
    ncol = 2,
    nrow = 6,
    align = "hv"
  )
  
  ggsave("Output/DLM/Malf_DLM_panel_all_models.png", panel_plot,
         res = 300, width = 30, height = 60, units = "cm",
         scaling = 0.9, device = ragg::agg_png)
}

