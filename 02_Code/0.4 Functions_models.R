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
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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
        or_conf.low = exp(estimate - z * std.error),
        or_conf.high = exp(estimate + z * std.error),
        log_or = estimate,
        log_or_conf.low = estimate - z * std.error,
        log_or_conf.high = estimate + z * std.error,
        estimate = or,
        conf.low = or_conf.low,
        conf.high = or_conf.high,
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset)
      ) |>
      dplyr::select(term, estimate, conf.low, conf.high, log_or, log_or_conf.low, log_or_conf.high,
                    std.error, statistic, p.value, dependent_var, predictor,
                    tiempo, contaminante, tipo, model_type, adjustment, n)
  } else {
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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

# time_start: nombre de columna con inicio de riesgo (entrada retardada); NULL = Surv(t_stop, evento)
fit_cox_model <- function(
    dependent, predictor, tiempo, contaminante, tipo,
    model_type, data, time_var = "edad_gest",
    time_start = NULL,
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
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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

  use_delayed <- !is.null(time_start) &&
    is.character(time_start) && nzchar(time_start) && time_start %in% names(data)
  if (use_delayed) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[time_start]])) |>
      dplyr::filter(.data[[time_start]] < .data[[time_var]])
  }

  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }

  # If insufficient data, return NA
  if (nrow(data_subset) < 10) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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

  surv_lhs <- if (use_delayed) {
    paste0("Surv(", time_start, ", ", time_var, ", ", dependent, ")")
  } else {
    paste0("Surv(", time_var, ", ", dependent, ")")
  }
  fml <- stats::as.formula(paste0(surv_lhs, " ~ ", rhs))

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
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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
        hr = estimate,
        hr_conf.low = conf.low,
        hr_conf.high = conf.high,
        log_hr = log(estimate),
        log_hr_conf.low = log(conf.low),
        log_hr_conf.high = log(conf.high),
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset)
      ) |>
      dplyr::select(term, estimate, conf.low, conf.high, log_hr, log_hr_conf.low, log_hr_conf.high,
                    std.error, statistic, p.value, dependent_var, predictor,
                    tiempo, contaminante, tipo, model_type, adjustment, n)
  } else {
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
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

# Logit ponderado (post-estratificación u otros pesos de frecuencia) — survey::svyglm ----
fit_logit_model_weighted <- function(
    dependent, predictor, tiempo, contaminante, tipo,
    model_type, data,
    weight_var = "w_poststrat",
    conf.level = 0.95,
    adjustment = "Adjusted") {
  if (model_type == "single") {
    predictors_list <- predictor
  } else {
    predictors_list <- trimws(stringr::str_split(predictor, " \\+ ")[[1]])
  }

  missing_predictors <- predictors_list[!predictors_list %in% names(data)]
  if (length(missing_predictors) > 0 || !weight_var %in% names(data)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = 0L,
      sum_w = NA_real_
    ))
  }

  data_subset <- data |>
    dplyr::filter(!is.na(.data[[dependent]])) |>
    dplyr::filter(!is.na(.data[[weight_var]]), .data[[weight_var]] > 0)

  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }

  if (nrow(data_subset) < 10 || sum(data_subset[[weight_var]], na.rm = TRUE) < 1) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum(data_subset[[weight_var]], na.rm = TRUE)
    ))
  }

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

  fml <- stats::as.formula(paste0(dependent, " ~ ", rhs))
  wform <- stats::as.formula(paste0("~", weight_var))
  des <- survey::svydesign(ids = ~1, weights = wform, data = data_subset)

  model_fit <- tryCatch(
    survey::svyglm(fml, design = des, family = stats::quasibinomial()),
    error = function(e) NULL
  )

  if (is.null(model_fit)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum(data_subset[[weight_var]], na.rm = TRUE)
    ))
  }

  tbl <- broom::tidy(model_fit, conf.int = TRUE, conf.level = conf.level)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  tbl_exposure <- tbl[tbl$term %in% predictors_list, ]
  sum_w <- sum(data_subset[[weight_var]], na.rm = TRUE)

  if (nrow(tbl_exposure) > 0) {
    tbl_exposure <- tbl_exposure |>
      dplyr::mutate(
        or = exp(.data$estimate),
        or_conf.low = exp(.data$estimate - z * .data$std.error),
        or_conf.high = exp(.data$estimate + z * .data$std.error),
        log_or = .data$estimate,
        log_or_conf.low = .data$estimate - z * .data$std.error,
        log_or_conf.high = .data$estimate + z * .data$std.error,
        estimate = .data$or,
        conf.low = .data$or_conf.low,
        conf.high = .data$or_conf.high,
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset),
        sum_w = sum_w
      ) |>
      dplyr::select(
        term, estimate, conf.low, conf.high, log_or, log_or_conf.low, log_or_conf.high,
        std.error, statistic, p.value, dependent_var, predictor,
        tiempo, contaminante, tipo, model_type, adjustment, n, sum_w
      )
  } else {
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_or = NA_real_,
      log_or_conf.low = NA_real_,
      log_or_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum_w
    )
  }

  rm(model_fit, des)
  gc()
  return(tbl_exposure)
}

# Cox ponderado — survey::svycoxph ----
fit_cox_model_weighted <- function(
    dependent, predictor, tiempo, contaminante, tipo,
    model_type, data,
    time_var = "edad_gest",
    time_start = NULL,
    weight_var = "w_poststrat",
    conf.level = 0.95,
    adjustment = "Adjusted") {
  if (model_type == "single") {
    predictors_list <- predictor
  } else {
    predictors_list <- trimws(stringr::str_split(predictor, " \\+ ")[[1]])
  }

  missing_predictors <- predictors_list[!predictors_list %in% names(data)]
  if (length(missing_predictors) > 0 || !weight_var %in% names(data)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = 0L,
      sum_w = NA_real_
    ))
  }

  data_subset <- data |>
    dplyr::filter(!is.na(.data[[dependent]]), !is.na(.data[[time_var]])) |>
    dplyr::filter(!is.na(.data[[weight_var]]), .data[[weight_var]] > 0)

  use_delayed <- !is.null(time_start) &&
    is.character(time_start) && nzchar(time_start) && time_start %in% names(data_subset)
  if (use_delayed) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[time_start]])) |>
      dplyr::filter(.data[[time_start]] < .data[[time_var]])
  }

  for (pred in predictors_list) {
    data_subset <- data_subset |>
      dplyr::filter(!is.na(.data[[pred]]))
  }

  if (nrow(data_subset) < 10 || sum(data_subset[[weight_var]], na.rm = TRUE) < 1) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum(data_subset[[weight_var]], na.rm = TRUE)
    ))
  }

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

  surv_lhs <- if (use_delayed) {
    paste0("Surv(", time_start, ", ", time_var, ", ", dependent, ")")
  } else {
    paste0("Surv(", time_var, ", ", dependent, ")")
  }
  fml <- stats::as.formula(paste0(surv_lhs, " ~ ", rhs))
  wform <- stats::as.formula(paste0("~", weight_var))
  des <- survey::svydesign(ids = ~1, weights = wform, data = data_subset)

  model_fit <- tryCatch(
    survey::svycoxph(fml, design = des),
    error = function(e) NULL
  )

  if (is.null(model_fit)) {
    return(data.frame(
      term = predictor,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum(data_subset[[weight_var]], na.rm = TRUE)
    ))
  }

  tbl <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = conf.level)
  tbl_exposure <- tbl[tbl$term %in% predictors_list, ]
  sum_w <- sum(data_subset[[weight_var]], na.rm = TRUE)

  if (nrow(tbl_exposure) > 0) {
    tbl_exposure <- tbl_exposure |>
      dplyr::mutate(
        log_hr = log(.data$estimate),
        log_hr_conf.low = log(.data$conf.low),
        log_hr_conf.high = log(.data$conf.high),
        dependent_var = dependent,
        predictor = predictor,
        tiempo = tiempo,
        contaminante = contaminante,
        tipo = tipo,
        model_type = model_type,
        adjustment = adjustment,
        n = nrow(data_subset),
        sum_w = sum_w
      ) |>
      dplyr::select(
        term, estimate, conf.low, conf.high, log_hr, log_hr_conf.low, log_hr_conf.high,
        std.error, statistic, p.value, dependent_var, predictor,
        tiempo, contaminante, tipo, model_type, adjustment, n, sum_w
      )
  } else {
    tbl_exposure <- data.frame(
      term = predictors_list[1],
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      log_hr = NA_real_,
      log_hr_conf.low = NA_real_,
      log_hr_conf.high = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      dependent_var = dependent,
      predictor = predictor,
      tiempo = tiempo,
      contaminante = contaminante,
      tipo = tipo,
      model_type = model_type,
      adjustment = adjustment,
      n = nrow(data_subset),
      sum_w = sum_w
    )
  }

  rm(model_fit, des)
  gc()
  return(tbl_exposure)
}

