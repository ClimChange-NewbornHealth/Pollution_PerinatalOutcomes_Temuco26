# Air Pollution and Perinatal Outcomes in Temuco–Padre Las Casas, Chile (2009–2016)

[![GitHub Repo stars](https://img.shields.io/github/stars/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub last commit](https://img.shields.io/github/last-commit/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub language count](https://img.shields.io/github/languages/count/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub top language](https://img.shields.io/github/languages/top/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub License](https://img.shields.io/github/license/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)

## :moneybag: Funding

**Fondecyt Nº 11240322**: Climate change and urban health: how air pollution, temperature, and city structure relate to preterm birth.

## :busts_in_silhouette: Research Team

:mailbox_with_mail: **Estela Blanco** (<estela.blanco@uc.cl>) - **Principal Investigator**

:mailbox_with_mail: **José Daniel Conejeros** (<jdconejeros@uc.cl>) - **Research Assistant / Repository Manager**

**Research Collaborators**: 

## :pushpin: Publication

*To be updated.*

---

## :dart: Project Overview

### Background

Residential wood burning in southern Chile, particularly in the Temuco–Padre Las Casas conurbation (Araucanía Region), contributes to severe wintertime air pollution. Fine particulate matter (PM₂.₅) and biomass burning tracers such as levoglucosan and potassium are associated with adverse perinatal outcomes. Evidence on the association between air pollution and perinatal health in this region remains limited.

### Objective

To evaluate the association between exposure to air pollution during pregnancy and the risk of adverse perinatal outcomes among births in Temuco and Padre Las Casas, Chile, from 2009 to 2016.

### Methods

We conducted a population-based retrospective cohort study using:

- **Study Population**: Births in Temuco and Padre Las Casas (Araucanía Region), Chile (2009–2016)
- **Sample Size**: 15,557 births after exclusion criteria (gestational age ≥28 weeks)
- **Exposure**: Air pollution during pregnancy:
  - **PM₂.₅** (μg/m³): Fine particulate matter
  - **Levoglucosan**: Biomass burning tracer
  - **Potassium (K)**: Biomass burning tracer
  - **Spatial resolution**: City-scale (`_cs`) and spatial (`_sp`)
- **Exposure Windows**:
  - Week 20 of pregnancy
  - Whole pregnancy (Overall)
  - Trimesters 1, 2, and 3 (mutually adjusted)
- **Exposure Scale**: Raw (μg/m³ or original units) and IQR-scaled (exposure / IQR) for comparability across pollutants
- **Outcomes**:
  - Preterm birth (<37 weeks), with subcategories: very preterm, moderately preterm, late preterm
  - Low birth weight (<2500 g)
  - Very low birth weight (<1500 g)
  - Small for gestational age (SGA)
- **Statistical Analysis**:
  - **Exposure models**: Logistic regression (OR) and Cox proportional hazards models (HR) with gestational age as the time scale
  - **Distributed Lag Models (DLM)**: Per-week models (weeks 1–39) with current-week exposure and weighted lagged exposure (weights = 1 / temporal distance); logit and Cox, adjusted for covariates
- **Covariates**: Maternal age, education, health insurance, employment, first birth, infant sex, birth year, birth month, municipality

### Key Findings

*To be updated upon completion of analysis.*

---

## R Code Structure

### Setup and Utilities (`02_Code/0.x`)

- `0.1 Settings.R` - Global settings and options
- `0.2 Packages.R` - Package installation and loading
- `0.3 Functions.R` - Custom functions
- `0.4 Functions_models.R` - Model fitting functions (logistic and Cox)

### Data Processing

- `1.0 Process_data.R` - Merges birth and covariate data; computes IQR-scaled exposure variables (`iqr_*`); saves `Data_full_sample_exposure_analysis.RData`

### Descriptive Analysis

- `2.0 Descriptive.R` - Descriptive statistics for perinatal outcomes and exposure

### Exposure Models (`02_Code/3.x`)

- `3.0 Exposure_PO_Models.R` - Logistic and Cox models for perinatal outcomes by exposure window (tot, w20, trimesters). Estimates both **raw** and **IQR-scaled** exposure versions. Saves to `Exposure_models_PO_logit_cox.xlsx`
- `3.1 Exposure_PO_Plots_Models.R` - Forest plots of OR and HR (log and ratio scale) by pollutant and outcome. Generates **raw** and **IQR** versions
- `3.2 Exposure_PO_Tables_Models.R` - Excel tables of OR and HR by pollutant and outcome. Generates `Tab_Exposure_PO.xlsx` (raw) and `Tab_Exposure_PO_IQR.xlsx` (IQR)

### Distributed Lag Models (DLM) (`02_Code/4.x`)

- `4.0 DLM_PO_estimation.R` - DLM estimation: logit and Cox models per gestational week (1–37) with exposure + lagged exposure. Estimates both **raw** and **IQR-scaled** exposure versions. Uses `fit_logit_model` and `fit_cox_model` from `0.4 Functions_models.R`. Saves results to `03_Output/DLM/DLM_PO_results.RData`
- `4.1 DLM_PO_tables_plots.R` - DLM tables (Excel, one sheet per outcome–exposure) and figures (panels by contaminant–type). Generates **raw** and **IQR** versions

---

## :chart_with_upwards_trend: Principal Findings

### Exposure Models: Association Between Air Pollution and Perinatal Outcomes

**Logistic models (OR)** and **Cox models (HR)** by pollutant (PM₂.₅, Levoglucosan, K) and spatial type (cs, sp). Results are produced in two exposure scales: **raw** (original units) and **IQR** (exposure / IQR).

Outputs:
- `03_Output/Models/Exposure_models_PO_logit_cox.xlsx` - OR and HR results (raw and IQR models)
- `03_Output/Models/Tables/Tab_Exposure_PO.xlsx` - Formatted tables (raw)
- `03_Output/Models/Tables/Tab_Exposure_PO_IQR.xlsx` - Formatted tables (IQR)
- `03_Output/Models/Plots_logit/` - OR panels (log and ratio scale): `*_raw.png`, `*_iqr.png`
- `03_Output/Models/Plots_cox/` - HR panels (log and ratio scale): `*_raw.png`, `*_iqr.png`

*Note*: Each figure shows seven perinatal outcomes across exposure windows (Week 20, Overall, Trimesters 1–3). Unadjusted and adjusted models are displayed.

### Distributed Lag Models (DLM): Week-Specific Effects

**DLM** estimates the association between exposure at each gestational week (1–37) and perinatal outcomes, controlling for lagged exposure (weighted sum of past weeks). Results are produced in two exposure scales: **raw** and **IQR**.

Outputs:
- `03_Output/DLM/DLM_PO_results.RData` - Estimated coefficients and CIs (logit and Cox, raw and IQR)
- `03_Output/DLM/Tab_DLM_PO.xlsx` - Excel tables (raw; one sheet per outcome–exposure; Week, OR, HR with 95% CI)
- `03_Output/DLM/Tab_DLM_PO_IQR.xlsx` - Excel tables (IQR)
- `03_Output/DLM/Plots_logit/` - OR by week (log and ratio scale): original filenames (raw), `*_iqr.png` (IQR)
- `03_Output/DLM/Plots_cox/` - HR by week (log and ratio scale): original filenames (raw), `*_iqr.png` (IQR)

*Note*: Each figure is a panel (2×4) where each subplot corresponds to one perinatal outcome, with gestational week on the x-axis.

---

## :file_folder: Data Availability

### Input Data

1. **Birth and Exposure Data**: `01_Input/Data_full_sample_exposure.RData`
   - Perinatal outcomes linked to air pollution exposure during pregnancy
   - Variables: Gestational age, birth weight, maternal characteristics, exposure by week/trimester
   - See `01_Input/Data_dictionary_PO_pollution.md` for full variable descriptions

2. **Analysis Dataset** (created by `1.0 Process_data.R`): `01_Input/Data_full_sample_exposure_analysis.RData`
   - Merged with covariates (education, health insurance, job, etc.)
   - Includes IQR-scaled exposure variables (`iqr_[window]_[pollutant]_[type]`)

### Data Access

**Restricted access.** Data requests may be submitted to **Estela Blanco** (estela.blanco@uc.cl), Professor at College and Public Health, Pontificia Universidad Católica de Chile.

**Note**: Due to data privacy regulations, individual-level birth records cannot be publicly shared. Aggregated results and code are available in this repository.

---

## :computer: Reproducibility

### System Requirements

- R version ≥4.0.0
- Required packages (automatically installed via `0.2 Packages.R`):
  - Data manipulation: `tidyverse`, `data.table`, `rio`
  - Survival analysis: `survival`
  - Parallel computing: `future`, `furrr`, `future.apply`
  - Visualization: `ggplot2`, `ggpubr`, `ragg`, `scales`
  - And more (see `0.2 Packages.R` for complete list)

### Running the Analysis

The analysis pipeline follows this sequence:

1. **Setup** (run first):
   ```r
   source("02_Code/0.1 Settings.R")
   source("02_Code/0.2 Packages.R")
   source("02_Code/0.3 Functions.R")
   source("02_Code/0.4 Functions_models.R")
   ```

2. **Data Processing** (creates analysis dataset with IQR-scaled exposures):
   ```r
   source("02_Code/1.0 Process_data.R")
   ```

3. **Descriptive Analysis**:
   ```r
   source("02_Code/2.0 Descriptive.R")
   ```

4. **Exposure Models** (Logistic and Cox, raw + IQR):
   ```r
   source("02_Code/3.0 Exposure_PO_Models.R")
   ```

5. **Exposure Tables and Plots** (raw + IQR):
   ```r
   source("02_Code/3.1 Exposure_PO_Plots_Models.R")
   source("02_Code/3.2 Exposure_PO_Tables_Models.R")
   ```

6. **Distributed Lag Models (DLM)** (raw + IQR):
   ```r
   source("02_Code/4.0 DLM_PO_estimation.R")   # Estimation (~15–20 min for raw + IQR)
   source("02_Code/4.1 DLM_PO_tables_plots.R") # Tables and figures
   ```

### Notes on Computation Time

- **Data processing** (`1.0 Process_data.R`): ~10–30 seconds
- **Exposure models** (`3.0 Exposure_PO_Models.R`): ~2–3 minutes (parallelized; raw + IQR)
- **Exposure tables** (`3.2 Exposure_PO_Tables_Models.R`): ~5 seconds
- **Exposure plots** (`3.1 Exposure_PO_Plots_Models.R`): ~15–20 seconds
- **DLM estimation** (`4.0 DLM_PO_estimation.R`): ~15–20 minutes (37 weeks × 6 pollutant–type × 7 outcomes × 2 models × 2 scales)
- **DLM tables and plots** (`4.1 DLM_PO_tables_plots.R`): ~2–3 minutes

---

## :open_book: Codebook

See `01_Input/Data_dictionary_PO_pollution.md` for a detailed data dictionary. Summary of key variables:

### Perinatal Outcomes

- `birth_preterm`: Preterm birth (<37 weeks)
- `birth_very_preterm`: Very preterm (28–31 weeks)
- `birth_moderately_preterm`: Moderately preterm (32–33 weeks)
- `birth_late_preterm`: Late preterm (34–36 weeks)
- `lbw`: Low birth weight (<2500 g)
- `tlbw`: Very low birth weight (<1500 g)
- `sga`: Small for gestational age

### Exposure Variables

- `[window]_[pollutant]_[type]`: e.g., `tot_PM25_cs`, `w20_Levo_sp`, `t1_K_cs` (raw)
- `iqr_[window]_[pollutant]_[type]`: IQR-scaled (exposure / IQR), e.g., `iqr_tot_PM25_cs`, `iqr_w20_Levo_sp`
- **Windows**: `pct1` (first trimester), `t1`, `t2`, `t3`, `w20`, `tot`
- **Pollutants**: PM25, Levo, K
- **Types**: cs (city-scale), sp (spatial)

### Covariates

- `edad_madre`: Maternal age
- `education`: Maternal education (None or primary, Secondary, Higher)
- `health_insurance`: Health insurance type (Public, Private)
- `job`: Employment status (Unemployed, Employed)
- `first_birth`: First birth (No, Yes)
- `sexo_rn`: Infant sex
- `a_nac`: Birth year
- `mes_nac`: Birth month
- `comuna`: Municipality (TEM, PLC)

---

## :microscope: Methods Detail

### Exposure Windows

- **Week 20**: Average exposure during gestational week 20
- **Overall**: Whole-pregnancy average exposure
- **Trimesters 1–3**: Trimester-specific averages; mutually adjusted models include all three trimesters simultaneously
- **DLM (weeks 1–37)**: Per-week exposure; each model includes current-week exposure and lagged exposure (weighted sum of past weeks, weights = 1 / temporal distance)

### Model Outputs

- **Exposure models**: OR and log(OR) with 95% CI (logistic); HR and log(HR) with 95% CI (Cox). Raw and IQR versions. Results saved to `03_Output/Models/Exposure_models_PO_logit_cox.xlsx`, `Tab_Exposure_PO.xlsx`, `Tab_Exposure_PO_IQR.xlsx`
- **DLM**: Week-specific OR and HR (weeks 1–37) with 95% CI. Raw and IQR versions. Results saved to `03_Output/DLM/DLM_PO_results.RData`, `Tab_DLM_PO.xlsx`, `Tab_DLM_PO_IQR.xlsx`

### Exclusion Criteria

- Gestational age <28 weeks
- Missing values in outcome variables (lbw, tlbw, sga)

---

## :file_cabinet: Repository Structure

```
Pollution_PerinatalOutcomes_Temuco26/
├── 01_Input/                              # Input data
│   ├── Data_full_sample_exposure.RData
│   ├── Data_full_sample_exposure_analysis.RData   # Created by 1.0 (includes iqr_* vars)
│   ├── new_covariates_base_eb_emod.xlsx
│   ├── Data_IQR_ref_values.xlsx
│   └── Data_dictionary_PO_pollution.md
├── 02_Code/                               # Analysis scripts
│   ├── 0.1 Settings.R
│   ├── 0.2 Packages.R
│   ├── 0.3 Functions.R
│   ├── 0.4 Functions_models.R
│   ├── 1.0 Process_data.R                 # Data merge, IQR scaling
│   ├── 2.0 Descriptive.R                  # Descriptive statistics
│   ├── 3.0 Exposure_PO_Models.R           # Exposure models (raw + IQR)
│   ├── 3.1 Exposure_PO_Plots_Models.R    # Exposure plots (raw + IQR)
│   ├── 3.2 Exposure_PO_Tables_Models.R   # Exposure tables (raw + IQR)
│   ├── 4.0 DLM_PO_estimation.R            # DLM estimation (raw + IQR)
│   └── 4.1 DLM_PO_tables_plots.R          # DLM tables and figures (raw + IQR)
├── 03_Output/
│   ├── Descriptives/                      # From 2.0 Descriptive.R
│   ├── Models/
│   │   ├── Exposure_models_PO_logit_cox.xlsx
│   │   ├── List_models_exposure_PO.xlsx
│   │   ├── Tables/
│   │   │   ├── Tab_Exposure_PO.xlsx       # Raw
│   │   │   └── Tab_Exposure_PO_IQR.xlsx  # IQR
│   │   ├── Plots_logit/                   # OR figures (*_raw.png, *_iqr.png)
│   │   ├── Plots_cox/                     # HR figures (*_raw.png, *_iqr.png)
│   │   └── Plots_compiled_figures.RData
│   └── DLM/
│       ├── DLM_PO_results.RData
│       ├── Tab_DLM_PO.xlsx                # Raw
│       ├── Tab_DLM_PO_IQR.xlsx            # IQR
│       ├── Plots_logit/                   # OR by week (raw, *_iqr.png)
│       ├── Plots_cox/                     # HR by week (raw, *_iqr.png)
│       └── Plots_compiled_figures.RData
└── README.md
```

---

## :warning: Important Notes

### Data Privacy

Individual-level birth records are confidential and cannot be shared publicly due to Chilean data protection regulations. Data access requests should be directed to Estela Blanco (estela.blanco@uc.cl).

### Geographic Context

Temuco–Padre Las Casas is known for high wintertime air pollution from residential wood burning. Levoglucosan and potassium are used as tracers of biomass combustion, complementing PM₂.₅ measurements.

---

## :email: Contact

For questions about the code or methodology:
- **Estela Blanco**: <estela.blanco@uc.cl>
- **José Daniel Conejeros**: <jdconejeros@uc.cl>

For data access inquiries:
- **Estela Blanco**: <estela.blanco@uc.cl>

---

## :page_facing_up: License

This project is licensed under the terms specified in the LICENSE file.

---

## :handshake: Acknowledgments

This research was supported by Fondecyt de Iniciación en Investigación Nº 11240322.
