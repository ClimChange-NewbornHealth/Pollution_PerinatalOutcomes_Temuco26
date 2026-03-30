# 1.0 Process data -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

## Open data ----

# Birth data
data <- rio::import("01_Input/data_1992_2020.RData") |> janitor::clean_names()
glimpse(data)

# Spatial data
mun <- chilemapas::codigos_territoriales |> 
  mutate(
        code_mun=as.numeric(codigo_comuna), # mun: 9101 (TEM) - 9112 (PLC); reg: 9  
        code_reg=as.numeric(codigo_region), 
        code_prov=as.numeric(codigo_provincia)
        ) |>
  rename(
        name_mun=nombre_comuna, 
        name_prov=nombre_provincia, 
        name_reg=nombre_region) |>
  select(code_mun, code_prov, code_reg, name_mun, name_prov, name_reg)
glimpse(mun)

# Analysis data 
data_tem <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData") |> 
  mutate(mes_nac = lubridate::month(fecha_nac)) |> 
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga", "peso_rn",
         "edad_madre", "sexo_rn", "a_nac", "estacion", "comuna", "a_nac", "mes_nac",
         "education", "health_insurance", "job", "first_birth", "para", "cesarea",
         starts_with("pct1_"), starts_with("t1_"), starts_with("t2_"),
         starts_with("t3_"), starts_with("w20_"), starts_with("tot_")) |> 
  select(-"birth_extremely_preterm", -"birth_term", -"birth_posterm") |> 
  filter(!is.na(lbw | tlbw | sga)) |> 
  filter(edad_gest >= 28) |> 
  mutate(cesarea = factor(cesarea, levels = c(0, 1), labels = c("Spontaneous", "Cesarean"))) 

glimpse(data_tem)

## Edit data and obtain distribution ----

births_tem <- data |> 
  filter(ano_nac %in% c(2009:2016)) |> 
  filter(comuna %in% c(9101, 9112)) |> 
  ########################
  # Conversar con estela estos filtros pues no fueron aplicados a la data original 
  # filter(age_mom>=12 & age_mom<=50)
  filter(tipo_parto==1) |> # Simple (Aplica para este caso?)
  ########################
  mutate(id=1:n()) |> 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac)) |>
  mutate(tbw=if_else(peso==9999, NA_real_, peso),
         weeks=if_else(semanas==99, NA_real_, semanas)) |> 
  filter(weeks >= 28) |> 
  mutate(
    #size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    #age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    #educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    #job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
    ) |> 
  mutate(
    educ_group_mom = case_when(
      educ_mom == 1 ~ 3, # College
      educ_mom == 2 ~ 2, # Secondary
      educ_mom == 3 ~ 2, # Secondary
      educ_mom %in% c(4, 5) ~ 1, # None or primary
      TRUE ~ NA_real_, #Unknow
    ), 
    educ_group_mom = factor(
      educ_group_mom,
      levels = c(1, 2, 3),
      labels = c("None or primary", "Secondary", "Higher")
    ),
    job_mom = if_else(job_mom == 3, 1, job_mom),
    job_mom = factor(job_mom, levels = c(2, 1), labels = c("Employed", "Unemployed"))
  ) |>
  mutate(sex = factor(sexo, levels = c(1, 2), labels = c("Male", "Female"))) |>
  mutate(year=year(date_nac)) |>
  mutate(month=month(date_nac)) |>
  rename(code_mun=comuna) |> 
  left_join(mun, by="code_mun") |>
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) |>
  mutate(birth_late_preterm = if_else(weeks >= 34 & weeks <37, 1, 0)) |> 
  mutate(birth_moderately_preterm = if_else(weeks >= 32 & weeks <33, 1, 0)) |> 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) |> 
  mutate(lbw = if_else(peso < 2500, 1, 0)) |> 
  select(
    id, date_nac, year, weeks, tbw, sex, age_mom,
    educ_mom, educ_group_mom, job_mom,
    code_mun, name_mun, code_prov, name_prov, code_reg, name_reg,
    birth_preterm, birth_late_preterm, birth_moderately_preterm, birth_very_preterm, lbw
  )

glimpse(births_tem) # 40414 - 22 NA (0.05%)

births_tem <- births_tem |> 
  drop_na() # 40392

glimpse(births_tem) # 40392
glimpse(data_tem) # 15398 (38.12%)

save(births_tem, file=paste0("01_Input/", "Data_births_deis_tem_plc", ".RData"))

# -----------------------------------------------------------------------------
# 2.0 Ponderador por post-estratificación (opción A: celda completa) -------
#
# Objetivo: que la muestra (data_tem), al ponderar cada fila con w_poststrat,
# reproduzca en la población de referencia (births_tem) la distribución CONJUNTA
# de tres variables: edad materna (en grupos), nivel educativo y situación laboral.
#
# Pasos:
#   1) Definir los mismos grupos de edad en muestra y población (cut con mismos cortes).
#   2) Alinear educación y trabajo a los mismos niveles (factores con mismos levels).
#   3) En la población: contar n_pop por cada celda (edad × educ × trabajo).
#   4) En la muestra: contar n_samp por la misma celda.
#   5) Peso de post-estratificación: w_i = n_pop_c / n_samp_c para toda observación i
#      que cae en la celda c (constante dentro de la celda).
#   6) Si una celda tiene observaciones en la muestra pero n_pop = 0 en la población,
#      no hay masa poblacional objetivo: asignamos peso 1 y registramos advertencia.
#   7) Pegar w_poststrat a data_tem y guardar; las tablas descriptivas ponderadas
#      usan sum(w) como "tamaño" efectivo total y medias/porcentajes ponderados.
# -----------------------------------------------------------------------------

outfile_w <- "03_Output/Weighted_analysis"
lev_educ_post <- c("None or primary", "Secondary", "Higher")
lev_job_post <- c("Employed", "Unemployed")

# Misma función de estratos de edad en ambas bases (evita desalineación por cortes distintos)
make_age_strata_mom <- function(x) {
  cut(
    as.numeric(x),
    breaks = c(0, 19, 24, 29, 34, 39, Inf),
    labels = c("<=19", "20-24", "25-29", "30-34", "35-39", "40+"),
    right = TRUE
  )
}

# Tabla poblacional: conteos por celda (objetivo conjunto opción A)
pop_cells <- births_tem |>
  dplyr::transmute(
    age_strata = make_age_strata_mom(age_mom),
    educ_strata = factor(educ_group_mom, levels = lev_educ_post),
    job_strata = factor(job_mom, levels = lev_job_post)
  ) |>
  dplyr::count(age_strata, educ_strata, job_strata, name = "n_pop")

# Muestra: mismas celdas
samp_cells <- data_tem |>
  dplyr::transmute(
    age_strata = make_age_strata_mom(edad_madre),
    educ_strata = factor(education, levels = lev_educ_post),
    job_strata = factor(job, levels = lev_job_post)
  ) |>
  dplyr::count(age_strata, educ_strata, job_strata, name = "n_samp")

# Cruce población–muestra: factor de celda = N_pop / n_muestra
weight_lookup <- dplyr::full_join(pop_cells, samp_cells, by = c("age_strata", "educ_strata", "job_strata")) |>
  dplyr::mutate(
    n_pop = tidyr::replace_na(n_pop, 0L),
    n_samp = tidyr::replace_na(n_samp, 0L),
    w_cell = dplyr::case_when(
      n_samp > 0 & n_pop > 0 ~ n_pop / n_samp,
      n_samp > 0 & n_pop == 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

bad_cells <- weight_lookup |> dplyr::filter(n_samp > 0 & n_pop == 0)
if (nrow(bad_cells) > 0) {
  message(
    "Advertencia post-estratificación: ",
    nrow(bad_cells),
    " celda(s) con muestra pero sin conteo poblacional (n_pop=0); peso fijado en 1."
  )
}
weight_lookup <- weight_lookup |>
  dplyr::mutate(w_cell = dplyr::if_else(is.na(w_cell) & n_samp > 0, 1, w_cell))

# Pegar w_poststrat: mismas celdas que en weight_lookup; quitar columnas auxiliares al final
data_tem <- data_tem |>
  dplyr::mutate(
    age_strata = make_age_strata_mom(edad_madre),
    educ_strata = factor(education, levels = lev_educ_post),
    job_strata = factor(job, levels = lev_job_post)
  ) |>
  dplyr::left_join(
    dplyr::select(weight_lookup, age_strata, educ_strata, job_strata, w_cell),
    by = c("age_strata", "educ_strata", "job_strata")
  ) |>
  dplyr::mutate(w_poststrat = tidyr::replace_na(w_cell, 1)) |>
  dplyr::select(-w_cell, -age_strata, -educ_strata, -job_strata)

save(
  data_tem,
  file = file.path("01_Input", "Data_full_sample_exposure_analysis_weighted.RData")
)

# 2.1 Descriptive analysis (sample vs population) -----

prep_descriptive <- function(dat, is_sample = TRUE) {
  if (is_sample) {
    dat |>
      dplyr::transmute(
        gest = edad_gest,
        sex_var = as.character(sexo_rn),
        peso = peso_rn,
        edad_m = edad_madre,
        educ = as.character(education),
        job_var = as.character(job),
        mun = as.character(comuna),
        yr = a_nac,
        mo = mes_nac,
        birth_preterm,
        birth_very_preterm,
        birth_moderately_preterm,
        birth_late_preterm
      )
  } else {
    dat |>
      dplyr::transmute(
        gest = weeks,
        sex_var = as.character(sex),
        peso = tbw,
        edad_m = age_mom,
        educ = as.character(educ_group_mom),
        job_var = as.character(job_mom),
        mun = as.character(name_mun),
        yr = year,
        mo = lubridate::month(date_nac),
        birth_preterm,
        birth_very_preterm,
        birth_moderately_preterm,
        birth_late_preterm
      )
  }
}

# Misma estructura que prep_descriptive(is_sample=TRUE) más columna w (ponderador)
prep_descriptive_weighted <- function(dat) {
  dat |>
    dplyr::transmute(
      w = w_poststrat,
      gest = edad_gest,
      sex_var = as.character(sexo_rn),
      peso = peso_rn,
      edad_m = edad_madre,
      educ = as.character(education),
      job_var = as.character(job),
      mun = as.character(comuna),
      yr = a_nac,
      mo = mes_nac,
      birth_preterm,
      birth_very_preterm,
      birth_moderately_preterm,
      birth_late_preterm
    )
}

# Media y DE con pesos de frecuencia (post-estratificación)
wtd_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[ok], w[ok])
}

wtd_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  xx <- x[ok]
  ww <- w[ok]
  m <- sum(ww * xx) / sum(ww)
  sqrt(sum(ww * (xx - m)^2) / sum(ww))
}

group_ptb <- c(
  "All" = "Full sample",
  "birth_preterm" = "PTB",
  "birth_very_preterm" = "Very PTB",
  "birth_moderately_preterm" = "Moderate PTB",
  "birth_late_preterm" = "Late PTB"
)

format_pct <- function(pct, n) sprintf("%.1f%% (n=%d)", pct, n)
format_mean_sd <- function(m, s) sprintf("%.1f (%.1f)", m, s)

build_ptb_descriptive_table <- function(table_dat) {
  sex_levels <- sort(unique(na.omit(table_dat$sex_var)))
  educ_levels <- sort(unique(na.omit(table_dat$educ)))
  job_levels <- sort(unique(na.omit(table_dat$job_var)))
  mun_levels <- sort(unique(na.omit(table_dat$mun)))
  yr_levels <- sort(unique(na.omit(table_dat$yr)))
  mo_levels <- sort(unique(na.omit(table_dat$mo)))

  rows_var <- c(
    "N",
    "gest", "peso", "edad_m",
    rep("sex_var", length(sex_levels)),
    rep("educ", length(educ_levels)),
    rep("job_var", length(job_levels)),
    rep("mun", length(mun_levels)),
    rep("yr", length(yr_levels)),
    rep("mo", length(mo_levels))
  )
  rows_char <- c(
    "",
    "Gestational age (weeks), Mean (SD)",
    "Birth weight (g), Mean (SD)",
    "Maternal age (years), Mean (SD)",
    sex_levels,
    educ_levels,
    job_levels,
    mun_levels,
    as.character(yr_levels),
    as.character(mo_levels)
  )

  tab_list <- list(Variable = rows_var, Characteristic = rows_char)

  for (g in names(group_ptb)) {
    dat <- if (g == "All") table_dat else dplyr::filter(table_dat, .data[[g]] == 1)
    n_g <- nrow(dat)
    vals <- character(length(rows_var))
    i <- 0L

    i <- i + 1L
    vals[i] <- sprintf("N=%d", n_g)

    i <- i + 1L
    vals[i] <- format_mean_sd(mean(dat$gest, na.rm = TRUE), sd(dat$gest, na.rm = TRUE))

    i <- i + 1L
    vals[i] <- format_mean_sd(mean(dat$peso, na.rm = TRUE), sd(dat$peso, na.rm = TRUE))

    i <- i + 1L
    vals[i] <- format_mean_sd(mean(dat$edad_m, na.rm = TRUE), sd(dat$edad_m, na.rm = TRUE))

    for (lev in sex_levels) {
      i <- i + 1L
      n_lev <- sum(dat$sex_var == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }
    for (lev in educ_levels) {
      i <- i + 1L
      n_lev <- sum(dat$educ == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }
    for (lev in job_levels) {
      i <- i + 1L
      n_lev <- sum(dat$job_var == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }
    for (lev in mun_levels) {
      i <- i + 1L
      n_lev <- sum(dat$mun == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }
    for (lev in yr_levels) {
      i <- i + 1L
      n_lev <- sum(dat$yr == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }
    for (lev in mo_levels) {
      i <- i + 1L
      n_lev <- sum(dat$mo == lev, na.rm = TRUE)
      vals[i] <- format_pct(100 * n_lev / n_g, n_lev)
    }

    tab_list[[group_ptb[g]]] <- vals
  }

  out <- as.data.frame(tab_list, stringsAsFactors = FALSE)
  names(out)[-(1:2)] <- as.character(group_ptb)
  out
}

# Tabla descriptiva con la misma plantilla que la muestra cruda, pero estadísticos ponderados
# (suma de pesos en lugar de n; medias/DE y % con peso w en cada subgrupo PTB)
format_pct_wt <- function(w_vec, idx) {
  sw <- sum(w_vec, na.rm = TRUE)
  if (sw <= 0) {
    return("—")
  }
  w_sub <- sum(w_vec[idx], na.rm = TRUE)
  sprintf("%.1f%% (sum w=%.0f)", 100 * w_sub / sw, w_sub)
}

build_ptb_descriptive_table_weighted <- function(table_dat) {
  sex_levels <- sort(unique(na.omit(table_dat$sex_var)))
  educ_levels <- sort(unique(na.omit(table_dat$educ)))
  job_levels <- sort(unique(na.omit(table_dat$job_var)))
  mun_levels <- sort(unique(na.omit(table_dat$mun)))
  yr_levels <- sort(unique(na.omit(table_dat$yr)))
  mo_levels <- sort(unique(na.omit(table_dat$mo)))

  rows_var <- c(
    "N",
    "gest", "peso", "edad_m",
    rep("sex_var", length(sex_levels)),
    rep("educ", length(educ_levels)),
    rep("job_var", length(job_levels)),
    rep("mun", length(mun_levels)),
    rep("yr", length(yr_levels)),
    rep("mo", length(mo_levels))
  )
  rows_char <- c(
    "",
    "Gestational age (weeks), Mean (SD)",
    "Birth weight (g), Mean (SD)",
    "Maternal age (years), Mean (SD)",
    sex_levels,
    educ_levels,
    job_levels,
    mun_levels,
    as.character(yr_levels),
    as.character(mo_levels)
  )

  tab_list <- list(Variable = rows_var, Characteristic = rows_char)

  for (g in names(group_ptb)) {
    dat <- if (g == "All") {
      table_dat
    } else {
      dplyr::filter(table_dat, .data[[g]] == 1)
    }
    w <- dat$w
    sw <- sum(w, na.rm = TRUE)
    vals <- character(length(rows_var))
    i <- 0L

    i <- i + 1L
    vals[i] <- sprintf("Sum w=%.0f (n filas=%d)", sw, nrow(dat))

    i <- i + 1L
    vals[i] <- format_mean_sd(wtd_mean(dat$gest, w), wtd_sd(dat$gest, w))

    i <- i + 1L
    vals[i] <- format_mean_sd(wtd_mean(dat$peso, w), wtd_sd(dat$peso, w))

    i <- i + 1L
    vals[i] <- format_mean_sd(wtd_mean(dat$edad_m, w), wtd_sd(dat$edad_m, w))

    for (lev in sex_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$sex_var == lev & !is.na(dat$sex_var))
    }
    for (lev in educ_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$educ == lev & !is.na(dat$educ))
    }
    for (lev in job_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$job_var == lev & !is.na(dat$job_var))
    }
    for (lev in mun_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$mun == lev & !is.na(dat$mun))
    }
    for (lev in yr_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$yr == lev & !is.na(dat$yr))
    }
    for (lev in mo_levels) {
      i <- i + 1L
      vals[i] <- format_pct_wt(w, dat$mo == lev & !is.na(dat$mo))
    }

    tab_list[[group_ptb[g]]] <- vals
  }

  out <- as.data.frame(tab_list, stringsAsFactors = FALSE)
  names(out)[-(1:2)] <- as.character(group_ptb)
  out
}

tab_sample <- build_ptb_descriptive_table(prep_descriptive(data_tem, is_sample = TRUE))
tab_population <- build_ptb_descriptive_table(prep_descriptive(births_tem, is_sample = FALSE))

outcome_dist <- c(
  "birth_preterm",
  "birth_very_preterm",
  "birth_moderately_preterm",
  "birth_late_preterm"
)
outcome_labels <- c(
  birth_preterm = "Preterm birth (<37 wk)",
  birth_very_preterm = "Very preterm (28–31 wk)",
  birth_moderately_preterm = "Moderately preterm (32–33 wk)",
  birth_late_preterm = "Late preterm (34–36 wk)"
)

fmt_n_pct <- function(v, n_tot) {
  n_ev <- sum(v == 1, na.rm = TRUE)
  sprintf("n=%d (%.1f%%)", n_ev, 100 * n_ev / n_tot)
}

# Misma idea que fmt_n_pct pero usando suma de pesos entre casos con outcome = 1
fmt_n_pct_wt <- function(v, w) {
  sw <- sum(w, na.rm = TRUE)
  if (sw <= 0) {
    return("—")
  }
  w_ev <- sum(w * as.numeric(v == 1), na.rm = TRUE)
  sprintf("sum w=%.0f (%.1f%%)", w_ev, 100 * w_ev / sw)
}

tab_sample_weighted <- build_ptb_descriptive_table_weighted(prep_descriptive_weighted(data_tem))

tab_preterm_dist <- tibble::tibble(
  Outcome = unname(outcome_labels[outcome_dist]),
  `Sample (data_tem)` = vapply(outcome_dist, function(vn) fmt_n_pct(data_tem[[vn]], nrow(data_tem)), character(1)),
  `Sample (ponderado)` = vapply(outcome_dist, function(vn) fmt_n_pct_wt(data_tem[[vn]], data_tem$w_poststrat), character(1)),
  `Population (births_tem)` = vapply(outcome_dist, function(vn) fmt_n_pct(births_tem[[vn]], nrow(births_tem)), character(1))
)

# Resumen en Excel del método (texto breve + estadísticos de los pesos)
wg <- data_tem$w_poststrat
sum_w <- sum(wg, na.rm = TRUE)
ess_kish <- sum_w^2 / sum(wg^2, na.rm = TRUE)
info_ponderador <- tibble::tibble(
  Tema = c(
    "Qué es w_poststrat",
    "Objetivo estadístico (opción A)",
    "Fórmula por celda",
    "Celdas problemáticas",
    "Descriptivos ponderados",
    "Suma de pesos y N efectivo",
    "Datos guardados"
  ),
  Detalle = c(
    "Ponderador de post-estratificación pegado a cada fila de la muestra de análisis.",
    "Igualar la distribución CONJUNTA de edad materna (6 grupos), educación (3 niveles) y empleo (2 categorías) a la de births_tem (DEIS, mismo filtro espacio-tiempo).",
    "Para cada celda c: w_i = N_pop,c / n_muestra,c para todas las observaciones i en c.",
    "Si había nacimientos en la muestra en una celda con n_pop=0 en la población de referencia, se asignó w=1 y se emitió advertencia en consola.",
    "En la hoja Sample_weighted, la primera fila muestra sum(w) y n filas; medias/DE y % usan los pesos; en categorías, 'sum w' es la masa ponderada en esa categoría.",
    sprintf(
      "sum(w) = %.0f (≈ tamaño poblacional de referencia en las celdas cubiertas); n filas muestra = %d; N efectivo (Kish) ≈ %.0f; min/máx peso = %.3f / %.3f",
      sum_w, nrow(data_tem), ess_kish, min(wg, na.rm = TRUE), max(wg, na.rm = TRUE)
    ),
    "01_Input/Data_full_sample_exposure_analysis_weighted.RData contiene data_tem con la columna w_poststrat."
  )
)

writexl::write_xlsx(
  list(
    Info_ponderador = info_ponderador,
    Sample = tab_sample,
    Sample_weighted = tab_sample_weighted,
    Population = tab_population,
    Preterm_distribution = tab_preterm_dist,
    Celdas_poststrat = as.data.frame(weight_lookup)
  ),
  path = file.path(outfile_w, "Table_descriptives_sample_population.xlsx")
)

