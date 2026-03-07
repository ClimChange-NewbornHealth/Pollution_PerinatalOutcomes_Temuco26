# 1.0 Process data -----

## Settings ----
source("02_Code/0.1 Settings.R")
source("02_Code/0.2 Packages.R")
source("02_Code/0.3 Functions.R")

outfile <- "03_Output/Descriptives"

## Open data ----

data <- rio::import("01_Input/Data_full_sample_exposure.RData")
glimpse(data)
