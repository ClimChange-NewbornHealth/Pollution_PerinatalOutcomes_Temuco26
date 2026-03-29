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
  select("idbase", "edad_gest", starts_with("birth_"), "lbw", "tlbw", "sga", 
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
  mutate(sex=factor(sexo, levels=c(1,2), labels=c("Male", "Female"))) |> 
  mutate(year=year(date_nac)) |>
  rename(code_mun=comuna) |> 
  left_join(mun, by="code_mun") |>
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) |>
  mutate(birth_late_preterm = if_else(weeks >= 34 & weeks <37, 1, 0)) |> 
  mutate(birth_moderately_preterm = if_else(weeks >= 32 & weeks <33, 1, 0)) |> 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) |> 
  mutate(lbw = if_else(peso < 2500, 1, 0)) |> 
  select(id, date_nac, year, weeks, tbw, sex, age_mom, educ_mom, job_mom, 
    code_mun, name_mun, code_prov, name_prov, code_reg, name_reg,
    birth_preterm, birth_late_preterm, birth_moderately_preterm, birth_very_preterm, lbw)

glimpse(births_tem) # 40414 - 22 NA (0.05%)

births_tem <- births_tem |> 
  drop_na() # 40392

glimpse(births_tem) # 40392
glimpse(data_tem) # 15398 (38.12%)

save(births_tem, file=paste0("01_Input/", "Data_births_deis_tem_plc", ".RData"))

# 2.0 Descriptive analysis -----
