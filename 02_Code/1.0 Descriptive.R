# 1.0 Descriptives analysis -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

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
