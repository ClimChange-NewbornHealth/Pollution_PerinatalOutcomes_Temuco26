# 1.0 Process data -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

## Open data ----

data <- rio::import("01_Input/Data_full_sample_exposure.RData")
glimpse(data)

data_aux <- rio::import("01_Input/new_covariates_base_eb_emod.xlsx") |> 
  select(idbase, estudios, estudios2, prevision, prevision2, job, npartos, para, cesarea, lon, lat)
glimpse(data_aux)
summary(data_aux)

# Check variables 
table(data_aux$prevision, useNA = "ifany") 
table(data_aux$prevision2, useNA = "ifany") # This
table(data_aux$estudios, useNA = "ifany") 
table(data_aux$estudios2, useNA = "ifany") #This 

# Adjust ID and merge data
data_aux <- data_aux |> 
  mutate(idbase = as.character(idbase)) |> 
  mutate(idbase = paste0("ID-", idbase))

# Check id's
length(unique(data$idbase)) 
length(unique(data_aux$idbase)) 
table(unique(data$idbase) %in% unique(data_aux$idbase), useNA = "ifany") # All id's in data are in data_aux

# Merge data -----

data <- data |> 
  left_join(data_aux, by = "idbase") |> 
  relocate(estudios, estudios2, prevision, prevision2, job, npartos, para, cesarea, lon, lat, .after = "comuna")

glimpse(data)

# Edit new variables 
data <- data |> 
  mutate(
    education = factor(estudios2, labels = c("None or primary", "Secondary", "Higher")),
    health_insurance = factor(prevision2, labels = c("Public", "Private")), 
    job = factor(job, labels = c("Unemployed", "Employed")),
    first_birth = if_else(para==0, 1, 0), 
    first_birth = factor(first_birth, labels = c("No", "Yes"))
) |> 
  relocate(education, health_insurance, job, first_birth, .after = para)

glimpse(data)

# Spatial points data -----

data_geo <- data |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 32719,   # UTM zone 19S (coordinates in Excel are in zone 19S, not 18S)
    remove = FALSE
  ) |> 
  relocate(geometry, .after = lat) |> 
  select(idbase, geometry)

glimpse(data_geo)

# IQR Metrics per contaminant -----

exposure_vars <- data |> 
  st_drop_geometry() |>
  select(
    starts_with(c("pct1", "t1", "t2", "t3", "tot", "w"))
  ) |> 
  colnames()

exposure_vars # 360 cols

# exposure_vars/IQR(exposure_vars)
iqr_vals <- data |>
  summarise(across(all_of(exposure_vars), ~ IQR(.x, na.rm = TRUE))) |>
  as.list()

iqr_vals # Calculate IQR per variable 

writexl::write_xlsx(iqr_vals |> data.frame(), paste0("01_Input/", "Data_IQR_ref_values.xlsx"))

data <- data |> 
  mutate(across(all_of(exposure_vars), ~ .x / iqr_vals[[cur_column()]], .names = "iqr_{.col}"))

glimpse(data)

# Save data ----
save(data, file=paste0("01_Input/", "Data_full_sample_exposure_analysis", ".RData"))
save(data_geo, file=paste0("01_Input/", "Data_full_sample_exposure_geo", ".RData"))



