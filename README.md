# Air Pollution and Perinatal Outcomes in Temuco–Padre Las Casas, Chile (2009–2016)

[![GitHub Repo stars](https://img.shields.io/github/stars/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub last commit](https://img.shields.io/github/last-commit/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub language count](https://img.shields.io/github/languages/count/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub top language](https://img.shields.io/github/languages/top/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)
[![GitHub License](https://img.shields.io/github/license/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)](https://github.com/ClimChange-NewbornHealth/Pollution_PerinatalOutcomes_Temuco26)

## :moneybag: Funding

**Fondecyt Nº 11240322**: Climate change and urban health: how air pollution, temperature, and city structure relate to preterm birth

## :busts_in_silhouette: Research Team

:mailbox_with_mail: **Estela Blanco** (<estela.blanco@uc.cl>) - **Principal Investigator**

:mailbox_with_mail: **José Daniel Conejeros** (<jdconejeros@uc.cl>) - **Research Assistant / Repository Manager**

**Research Collaborators**: Álvaro González-Reyes, Pamela Smith, Paola Rubilar, & Pablo Sarricolea

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
- **Outcomes**:
  - Preterm birth (<37 weeks), with subcategories: very preterm, moderately preterm, late preterm
  - Low birth weight (<2500 g)
  - Very low birth weight (<1500 g)
  - Small for gestational age (SGA)
- **Statistical Analysis**: Logistic regression (OR) and Cox proportional hazards models (HR) with gestational age as the time scale
- **Covariates**: Maternal age, infant sex, birth year, birth month, municipality

### Key Findings

*To be updated upon completion of analysis.*

---

## R Code Structure

### Setup and Utilities

- `0.1 Settings.R` - Global settings and options
- `0.2 Packages.R` - Package installation and loading
- `0.3 Functions.R` - Custom functions
- `0.4 Functions_models.R` - Model fitting functions (logistic and Cox)

### Descriptive Analysis

- `1.0 Descriptive.R` - Descriptive statistics for perinatal outcomes and exposure

### Exposure Models

- `2.0 Exposure_PO_Models.R` - Logistic and Cox models for perinatal outcomes by exposure window
- `3.0 Exposure_PO_Plots_Models.R` - Forest plots of OR and HR (log scale) by pollutant and outcome

### Distributed Lag Models (DLM)

- `4.0 DLM_PO_models.R` - *Pending*. Distributed lag models for perinatal outcomes (to be estimated).

---

## :chart_with_upwards_trend: Principal Findings

### Figure. Association Between Air Pollution and Perinatal Outcomes

**Logistic models (OR)** and **Cox models (HR)** by pollutant (PM₂.₅, Levoglucosan, K) and spatial type (cs, sp).

Outputs are saved in:
- `03_Output/Models/Plots_logit/` - OR panels (log scale)
- `03_Output/Models/Plots_cox/` - HR panels (log scale)

*Note*: Each figure shows seven perinatal outcomes (preterm birth, very preterm, moderately preterm, late preterm, low birth weight, very low birth weight, small for gestational age) across exposure windows (Week 20, Overall, Trimesters 1–3). Unadjusted and adjusted models are displayed.

---

## :file_folder: Data Availability

### Input Data

1. **Birth and Exposure Data**: `01_Input/Data_full_sample_exposure.RData`
   - Perinatal outcomes linked to air pollution exposure during pregnancy
   - Variables: Gestational age, birth weight, maternal characteristics, exposure by week/trimester
   - See `01_Input/Data_dictionary_PO_pollution.md` for full variable descriptions

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

2. **Descriptive Analysis**:
   ```r
   source("02_Code/1.0 Descriptive.R")
   ```

3. **Exposure Models** (Logistic and Cox):
   ```r
   source("02_Code/2.0 Exposure_PO_Models.R")
   ```

4. **Visualization**:
   ```r
   source("02_Code/3.0 Exposure_PO_Plots_Models.R")
   ```

5. **Distributed Lag Models** (pending):
   ```r
   # source("02_Code/4.0 DLM_PO_models.R")  # To be run when DLM analysis is complete
   ```

### Notes on Computation Time

- **Exposure models** (`2.0 Exposure_PO_Models.R`): ~20–30 seconds (parallelized)
- **Plots** (`3.0 Exposure_PO_Plots_Models.R`): ~10 seconds

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

- `[window]_[pollutant]_[type]`: e.g., `tot_PM25_cs`, `w20_Levo_sp`, `t1_K_cs`
- **Windows**: `pct1` (first trimester), `t1`, `t2`, `t3`, `w20`, `tot`
- **Pollutants**: PM25, Levo, K
- **Types**: cs (city-scale), sp (spatial)

### Covariates

- `edad_madre`: Maternal age
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

### Model Outputs

- **OR** and **log(OR)** with 95% CI (logistic models)
- **HR** and **log(HR)** with 95% CI (Cox models)
- Results saved to `03_Output/Models/Exposure_models_PO_logit_cox.xlsx`

### Exclusion Criteria

- Gestational age <28 weeks
- Missing values in outcome variables (lbw, tlbw, sga)

---

## :file_cabinet: Repository Structure

```
Pollution_PerinatalOutcomes_Temuco26/
├── 01_Input/                        # Input data
│   ├── Data_full_sample_exposure.RData
│   └── Data_dictionary_PO_pollution.md
├── 02_Code/                         # Analysis scripts
│   ├── 0.1 Settings.R
│   ├── 0.2 Packages.R
│   ├── 0.3 Functions.R
│   ├── 0.4 Functions_models.R
│   ├── 1.0 Descriptive.R
│   ├── 2.0 Exposure_PO_Models.R
│   ├── 3.0 Exposure_PO_Plots_Models.R
│   └── 4.0 DLM_PO_models.R         # Pending
├── 03_Output/
│   └── Models/
│       ├── Exposure_models_PO_logit_cox.xlsx
│       ├── Plots_logit/             # OR figures
│       └── Plots_cox/               # HR figures
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
