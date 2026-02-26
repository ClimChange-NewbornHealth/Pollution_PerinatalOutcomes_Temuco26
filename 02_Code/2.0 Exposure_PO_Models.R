# 5.0 Exposure models -----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

## 1. Cargamos los datos ----

data_full_wide <- rio::import("Output/Data_malf_exposure_wide.RData")
#data_full_long <- rio::import("Output/Data_malf_exposure_long.RData")
glimpse(data_full_wide)

## 2. Preparamos los datos para modelado ----

# Filtramos datos con valores válidos en variables dependientes
data_model <- data_full_wide |>
  filter(!is.na(malf))

## 3. Definimos variables y creamos grilla de modelos ----

# Variables dependientes
dependent_vars <- c("malf", colnames(data_full_wide)[str_detect(colnames(data_full_wide), pattern = "malf_.*_bin")])
dependent_vars

# Variables de control para modelos ajustados
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion")

test <- data_model |>
  select(all_of(control_vars))

# Períodos de tiempo de interés
time_periods <- c("pct1", "t1", "t2", "t3", "w20", "tot")

# Contaminantes y tipos
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp")

# Tipos de modelos
model_types <- c("single", "pct1_t1_t2_t3", "t1_t2_t3")

# Creamos grilla de modelos individuales (single)
combinations_single <- expand.grid(
  dependent = dependent_vars,
  tiempo = time_periods,
  contaminante = contaminants,
  tipo = types,
  model_type = "single",
  adjustment = c("Unadjusted", "Adjusted"),
  stringsAsFactors = FALSE
)

# Creamos nombre de predictor para modelos single
combinations_single <- combinations_single |>
  mutate(
    predictor = paste0(tiempo, "_", contaminante, "_", tipo)
  )

# Creamos grilla de modelos con pct1 + t1 + t2 + t3
combinations_pct1_t1_t2_t3 <- expand.grid(
  dependent = dependent_vars,
  contaminante = contaminants,
  tipo = types,
  model_type = "pct1_t1_t2_t3",
  adjustment = c("Unadjusted", "Adjusted"),
  stringsAsFactors = FALSE
)

combinations_pct1_t1_t2_t3 <- combinations_pct1_t1_t2_t3 |>
  mutate(
    predictor = paste0("pct1_", contaminante, "_", tipo, " + t1_", contaminante, "_", tipo, 
                      " + t2_", contaminante, "_", tipo, " + t3_", contaminante, "_", tipo),
    tiempo = "pct1_t1_t2_t3"
  )

# Creamos grilla de modelos con t1 + t2 + t3
combinations_t1_t2_t3 <- expand.grid(
  dependent = dependent_vars,
  contaminante = contaminants,
  tipo = types,
  model_type = "t1_t2_t3",
  adjustment = c("Unadjusted", "Adjusted"),
  stringsAsFactors = FALSE
)

combinations_t1_t2_t3 <- combinations_t1_t2_t3 |>
  mutate(
    predictor = paste0("t1_", contaminante, "_", tipo, " + t2_", contaminante, "_", tipo, 
                      " + t3_", contaminante, "_", tipo),
    tiempo = "t1_t2_t3"
  )

# Combinamos todas las grillas
combinations <- bind_rows(
  combinations_single,
  combinations_pct1_t1_t2_t3,
  combinations_t1_t2_t3
)

# Verificamos que los predictores existan en los datos
available_predictors <- names(data_model)[grepl("(_PM25_|_Levo_|_K_)", names(data_model))]

# Para modelos single, verificamos que el predictor exista
combinations_single_valid <- combinations |>
  filter(model_type == "single") |>
  filter(predictor %in% available_predictors)

# Para modelos múltiples, verificamos que todos los predictores existan
combinations_multi <- combinations |>
  filter(model_type != "single") |>
  rowwise() |>
  mutate(
    predictors_list = list(stringr::str_split(predictor, " \\+ ")[[1]]),
    all_exist = all(trimws(predictors_list) %in% available_predictors)
  ) |>
  ungroup() |>
  filter(all_exist) |>
  select(-predictors_list, -all_exist)

# Combinamos resultados válidos
combinations <- bind_rows(combinations_single_valid, combinations_multi)

# Guardamos la grilla de modelos
writexl::write_xlsx(
  combinations, 
  path = "Output/Models/List_models_exposure_malf.xlsx"
)

## 4. Función para estimar modelos logísticos ----

fit_logit_model <- function(dependent, predictor, tiempo, contaminante, tipo, 
                           model_type, data, conf.level = 0.95, adjustment = "Adjusted") {
  
  # Extraemos lista de predictores individuales
  if (model_type == "single") {
    predictors_list <- predictor
  } else {
    predictors_list <- trimws(stringr::str_split(predictor, " \\+ ")[[1]])
  }
  
  # Verificamos que todos los predictores existan en los datos
  missing_predictors <- predictors_list[!predictors_list %in% names(data)]
  if (length(missing_predictors) > 0) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = 0
    ))
  }
  
  # Filtramos datos con valores válidos en variable dependiente y todos los predictores
  data_subset <- data |>
    dplyr::filter(!is.na(.data[[dependent]]))
  
  # Verificamos valores válidos en todos los predictores
  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }
  
  # Si no hay datos suficientes, retornamos NA
  if (nrow(data_subset) < 10) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    ))
  }
  
  # Construimos fórmula según ajuste
  if (identical(adjustment, "Adjusted")) {
    # Verificamos que las variables de control existan
    available_controls <- control_vars[control_vars %in% names(data_subset)]
    
    rhs <- if (length(available_controls) > 0) {
      paste(
        paste(predictors_list, collapse = " + "),
        paste("+", paste(available_controls, collapse = " + "))
      )
    } else {
      paste(predictors_list, collapse = " + ")
    }
  } else {
    rhs <- paste(predictors_list, collapse = " + ")
  }
  
  # Construimos fórmula
  fml <- as.formula(paste0(dependent, " ~ ", rhs))
  
  # Estimamos modelo
  model_fit <- tryCatch({
    glm(fml, data = data_subset, family = binomial(link = "logit"))
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(model_fit)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    ))
  }
  
  # Extraemos resultados
  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  # Filtramos solo los términos de exposición (predictores)
  tbl_exposure <- tbl[tbl$term %in% predictors_list, ]
  
  if (nrow(tbl_exposure) > 0) {
    tbl_exposure <- tbl_exposure |>
      dplyr::mutate(
        or = exp(estimate),
        conf.low = exp(estimate - z * std.error),
        conf.high = exp(estimate + z * std.error),
        estimate = or,
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset)
      ) |>
      dplyr::select(term, estimate, std.error, statistic, p.value, 
                    conf.low, conf.high, dependent_var, predictor, 
                    tiempo, contaminante, tipo, model_type, adjustment, n)
  } else {
    # Si no encontramos términos de exposición, creamos fila con NAs
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset)
    )
  }
  
  rm(model_fit); gc()
  
  return(tbl_exposure)
}

## 5. Estimamos todos los modelos de forma paralela ----

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)

tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  message("Iteración ", i, "/", nrow(combinations), " en PID ", Sys.getpid())
  
  dep <- combinations$dependent[i]
  tiempo <- combinations$tiempo[i]
  contaminante <- combinations$contaminante[i]
  tipo <- combinations$tipo[i]
  pred <- combinations$predictor[i]
  model_type <- combinations$model_type[i]
  adj <- combinations$adjustment[i]
  
  fit_logit_model(
    dependent = dep,
    predictor = pred,
    tiempo = tiempo,
    contaminante = contaminante,
    tipo = tipo,
    model_type = model_type,
    data = data_model,
    adjustment = adj
  )
}, future.seed = TRUE)
toc() # 22 sec

plan(sequential)

## 6. Consolidamos y guardamos resultados ----

# Combinamos todos los resultados
results_logit <- bind_rows(results_list) |> 
  mutate(term = predictor)

# Guardamos resultados
save(results_list, file = "Output/Models/Exposure_models_malf.RData")

writexl::write_xlsx(
  results_logit, 
  path = "Output/Models/Exposure_models_malf.xlsx"
)

# Test de prueba de algunos modelos 
m1 <- glm(
  malf ~ pct1_PM25_cs + t1_PM25_cs + t2_PM25_cs + t3_PM25_cs +
  #edad_madre + sexo_rn + a_nac + estacion, 
  family = binomial(link = logit), data = data_full_wide)

summary(m1)
exp(m1$coefficients[-1])
exp(confint.default(m1)) 


m2 <- glm(
  malf ~ t1_PM25_cs + t2_PM25_cs + t3_PM25_cs +
  edad_madre + sexo_rn + a_nac + estacion, 
  family = binomial(link = logit), data = data_full_wide)

summary(m2)
exp(m2$coefficients[-1])
exp(confint.default(m2)) 


m3 <- glm(
  malf ~ tot_PM25_cs +
  edad_madre + sexo_rn + a_nac + estacion, 
  family = binomial(link = logit), data = data_full_wide)

summary(m3)
exp(m3$coefficients[-1])
exp(confint.default(m3)) 

m1 <- broom::tidy(m1, conf.int = TRUE, exponentiate = TRUE)
m2 <- broom::tidy(m2, conf.int = TRUE, exponentiate = TRUE)
m3 <- broom::tidy(m3, conf.int = TRUE, exponentiate = TRUE)