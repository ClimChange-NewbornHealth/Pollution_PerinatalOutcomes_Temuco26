# 2.0 Exposure models -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")
source("02_Code/0.4 Functions_models.R")

## 1 Load data ----

data <- rio::import("01_Input/Data_full_sample_exposure.RData")
glimpse(data)

## 2 Prepare data for the models ----

# Filtramos datos con valores válidos en variables dependientes
data_model <- data |>
  mutate(mes_nac = lubridate::month(fecha_nac)) |> 
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga", 
         "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac",
         starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_")) |> 
  filter(!is.na(lbw | tlbw | sga)) |> 
  filter(edad_gest >= 28)

glimpse(data_model)
summary(data_model)

## 3. Define and create a grid models ----

# Dependente vars
dependent_vars <- c(colnames(data_model)[str_detect(colnames(data_model), pattern = "birth_.*")], "lbw", "tlbw", "sga")
dependent_vars

# Covariantes
control_vars <- c("edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac")

# Time independent variables
time_periods <- c("pct1", "t1", "t2", "t3", "w20", "tot")

# Contaminants 
contaminants <- c("PM25", "Levo", "K")
types <- c("cs", "sp") # Spatial models 

# Model types 
model_types <- c("single", "pct1_t1_t2_t3", "t1_t2_t3")

# Grid to single models 
combinations_single <- expand.grid(
  dependent = dependent_vars,
  tiempo = time_periods,
  contaminante = contaminants,
  tipo = types,
  model_type = "single",
  adjustment = c("Unadjusted", "Adjusted"),
  stringsAsFactors = FALSE
)

# Predictor trimester 
combinations_single <- combinations_single |>
  mutate(
    predictor = paste0(tiempo, "_", contaminante, "_", tipo)
  )

# Grid with pct1 + t1 + t2 + t3 (Unadjusted and adjusted models)
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

# Grid with t1 + t2 + t3
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

# All grids in an object 
combinations <- bind_rows(
  combinations_single,
  combinations_pct1_t1_t2_t3,
  combinations_t1_t2_t3
)

# Predictor in the data
available_predictors <- names(data_model)[grepl("(_PM25_|_Levo_|_K_)", names(data_model))]

# Predictor in the data for the single models
combinations_single_valid <- combinations |>
  filter(model_type == "single") |>
  filter(predictor %in% available_predictors)

# Predictor in the data for the multiple models
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

# All models 
combinations <- bind_rows(combinations_single_valid, combinations_multi)

# Save grid with all models 
writexl::write_xlsx(
  combinations, 
  path = "03_Output/Models/List_models_exposure_PO.xlsx"
)

## 4. Functions models (logit) ----

fit_logit_model # Logit models 
fit_cox_model # Cox models

## 5. Parallel computation modelling ----

### Logit models ------
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

### Cox models ------



## 6. Join and save the results ----

# Combinamos todos los resultados
results_logit <- bind_rows(results_list) |> 
  mutate(term = predictor)

# Guardamos resultados
save(results_list, file = "Output/Models/Exposure_models_malf.RData")

writexl::write_xlsx(
  results_logit, 
  path = "Output/Models/Exposure_models_malf.xlsx"
)

## 7. Test results models  ----

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