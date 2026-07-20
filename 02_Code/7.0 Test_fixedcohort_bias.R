# 7.0 Sensitivity models -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions_models.R")

## 1 Load data ----

data <- rio::import("01_Input/Data_full_sample_exposure_analysis.RData")
glimpse(data)

## 2 Validate data with fixed cohort bias ----

summary(data$fecha_ini)
summary(data$fecha_nac)
summary(data$edad_gest)

# Calculate week time 

date_last_week <- as.Date("2016-12-31") - weeks(max(data$edad_gest)) 
date_last_week # "2016-03-05"

nrow(data)
table(data$birth_preterm) # 1480 PTB 

data_fcb <- data |> 
  mutate(date_val = fecha_ini + weeks(edad_gest-1)) |>
  mutate(date_bin = if_else(fecha_nac != date_val, 1, 0))

table(data_fcb$date_bin) # No tenemos FCB. 

#filter(fecha_ini >= as.Date("2009-01-01")) |> 
#filter(fecha_ini <= date_last_week)


