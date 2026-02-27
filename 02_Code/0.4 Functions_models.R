# Functions models -----

# Fit logit models function (parametric approach)  -----
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

# Cox proportional hazards models function (semiparametric approach) -----

fit_cox_model <- function(dependent, predictor, tiempo, contaminante, tipo,
                         model_type, data, time_var = "edad_gest",
                         conf.level = 0.95, adjustment = "Adjusted") {

  # Extract individual predictors list
  if (model_type == "single") {
    predictors_list <- predictor
  } else {
    predictors_list <- trimws(stringr::str_split(predictor, " \\+ ")[[1]])
  }

  # Verify all predictors exist in data
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

  # Filter data with valid values in dependent, time variable, and all predictors
  data_subset <- data |>
    dplyr::filter(!is.na(.data[[dependent]]), !is.na(.data[[time_var]]))

  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }

  # If insufficient data, return NA
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

  # Build formula according to adjustment
  if (identical(adjustment, "Adjusted")) {
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

  # Build formula: Surv(time_var, event) ~ predictors
  fml <- as.formula(paste0("Surv(", time_var, ", ", dependent, ") ~ ", rhs))

  # Fit Cox model
  model_fit <- tryCatch({
    survival::coxph(fml, data = data_subset)
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

  # Extract results (HR when exponentiate = TRUE)
  tbl <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = conf.level)

  # Filter only exposure terms (predictors)
  tbl_exposure <- tbl[tbl$term %in% predictors_list, ]

  if (nrow(tbl_exposure) > 0) {
    tbl_exposure <- tbl_exposure |>
      dplyr::mutate(
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

