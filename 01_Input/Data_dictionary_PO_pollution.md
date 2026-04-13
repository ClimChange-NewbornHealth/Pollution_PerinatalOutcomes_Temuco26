# Data Dictionary: Perinatal Outcomes and Pollution Exposure

## Data Access

**Restricted access.** Data requests may be submitted to **Estela Blanco** (estela.blanco@uc.cl), Professor at College and Public Health, Pontificia Universidad Católica de Chile.

---

## Overview

This document describes the datasets used in the analysis pipeline:

1. **`Data_full_sample_exposure_analysis.RData`** – Main analysis dataset. Combines perinatal outcomes from births in Temuco and Padre Las Casas (Araucanía Region, Chile) with air pollution exposure estimates during pregnancy. Created by `1.0 Process_data.R` from `Data_full_sample_exposure.RData` merged with covariates; includes IQR-scaled exposure variables. Study period: **2009–2016**.

2. **`Data_full_sample_exposure_geo.RData`** – Geographic coordinates for spatial analysis. One row per birth; links `idbase` to point geometry.

| Attribute | Data_full_sample_exposure_analysis | Data_full_sample_exposure_geo |
|-----------|-----------------------------------|-------------------------------|
| **Object name** | `data` | `data_geo` |
| **Format** | data.frame | sf (simple features) data.frame |
| **Observations** | 15,557 | 15,557 |
| **Variables** | 786 | 2 |
| **CRS (geo)** | — | EPSG:32719 (WGS 84 / UTM zone 19S) |

---

## 1. Identification and Dates

| Variable | Type | Description |
|----------|------|-------------|
| `idbase` | character | Unique identifier for each birth record (e.g., "ID-3013") |
| `fecha_ini` | POSIXct | Pregnancy start date (last menstrual period) |
| `fecha_nac` | POSIXct | Birth date |
| `a_nac` | integer | Year of birth (2009–2016) |

---

## 2. Geographic and Maternal Covariates

| Variable | Type | Description |
|----------|------|-------------|
| `comuna` | factor | Municipality of birth: PLC (Padre Las Casas), TEM (Temuco) |
| `edad_madre` | integer | Maternal age at delivery (years; range 13–51) |
| `education` | factor | Maternal education: None or primary, Secondary, Higher |
| `health_insurance` | factor | Health insurance type: Public, Private |
| `job` | factor | Employment status: Unemployed, Employed |
| `first_birth` | factor | First birth: No, Yes |
| `estudios` | character | Raw education variable (source) |
| `estudios2` | numeric | Education code (0–2) |
| `prevision` | character | Raw health insurance variable (source) |
| `prevision2` | numeric | Health insurance code (0–1) |
| `npartos` | numeric | Number of deliveries (0–8) |
| `para` | numeric | Parity (0–8) |
| `cesarea` | numeric | Cesarean delivery: 0 (no), 1 (yes) |
| `parto` | factor | Delivery type: cesárea (cesarean), vaginal |
| `estacion` | factor | Season at pregnancy start: verano (summer), otono (autumn), invierno (winter), primavera (spring) |
| `estacion_bin` | numeric | Binary: pregnancy started in winter (1) or not (0) |
| `diabetes` | factor | Maternal diabetes diagnosis: 0 (no), 1 (yes) |
| `lon` | numeric | Longitude (UTM zone 19S; range ~181089–194038) |
| `lat` | numeric | Latitude (UTM zone 19S; range ~5701608–5711955) |

---

## 3. Gestational and Birth Characteristics

| Variable | Type | Description |
|----------|------|-------------|
| `edad_gest` | numeric | Gestational age at birth (weeks; range 19–43) |
| `sexo_rn` | character | Infant sex: Masculino, Femenino, Indefinido |
| `peso_rn` | numeric | Birth weight (grams; range 260–5650) |
| `talla_rn` | numeric | Birth length (cm); mostly missing |
| `circunf_rn` | numeric | Head circumference at birth (cm; range 17–53.5) |
| `adec_rn` | factor | Weight-for-gestational-age: AEG (LGA), GEG (AGA), PEG (SGA), and other categories |

---

## 4. Perinatal Outcomes (Binary)

| Variable | Type | Description |
|----------|------|-------------|
| `birth_preterm` | numeric | Preterm birth (<37 weeks): 0/1 |
| `birth_extremely_preterm` | numeric | Extremely preterm (<28 weeks): 0/1 |
| `birth_very_preterm` | numeric | Very preterm (28–31 weeks): 0/1 |
| `birth_moderately_preterm` | numeric | Moderately preterm (32–33 weeks): 0/1 |
| `birth_late_preterm` | numeric | Late preterm (34–36 weeks): 0/1 |
| `birth_term` | numeric | Term birth (37–41 weeks): 0/1 |
| `birth_posterm` | numeric | Post-term birth (>41 weeks): 0/1 |
| `lbw` | numeric | Low birth weight (<2500 g): 0/1 |
| `tlbw` | numeric | Very low birth weight (<1500 g): 0/1 |
| `sga` | numeric | Small for gestational age: 0/1 |

---

## 5. Clinical and Neonatal Indicators

| Variable | Type | Description |
|----------|------|-------------|
| `apgar1` | factor | Apgar score at 1 minute (0–10) |
| `apgar5` | factor | Apgar score at 5 minutes (0–10) |
| `cond_egreso` | factor | Discharge status: Fallecido (deceased), Vivo (alive) |

---

## 6. Congenital Malformations

| Variable | Type | Description |
|----------|------|-------------|
| `malf_mult` | factor | Multiple malformations: 0/1 |
| `genopatia` | factor | Genetic disorder: 0/1 |
| `malf_num` | factor | Total number of malformations |
| `malf_nerv` | factor | Nervous system malformation |
| `malf_ococ` | factor | Ocular and craniofacial malformation |
| `malf_card` | factor | Cardiac malformation |
| `malf_resp` | factor | Respiratory system malformation |
| `malf_dig` | factor | Digestive system malformation |
| `malf_og` | factor | Genital malformation |
| `malf_uri` | factor | Urinary system malformation |
| `malf_om` | factor | Musculoskeletal malformation |
| `malf_otra` | factor | Other congenital malformations |

*Note: Malformation variables are largely missing in this dataset.*

---

## 7. Air Pollution Exposure Variables

Exposure is estimated at two spatial resolutions:
- **`_cs`**: City-scale (or central station)
- **`_sp`**: Spatial (e.g., satellite-derived or spatially interpolated)

### 7.1 Pollutants

| Code | Pollutant | Unit | Description |
|------|-----------|------|-------------|
| **PM25** | PM₂.₅ | μg/m³ | Fine particulate matter (≤2.5 μm) |
| **Levo** | Levoglucosan | — | Biomass burning tracer |
| **K** | Potassium | — | Biomass burning tracer |

### 7.2 Exposure Windows (Raw)

| Prefix | Description |
|--------|-------------|
| `pct1_` | First trimester |
| `t1_`, `t2_`, `t3_` | Trimesters 1, 2, and 3 |
| `w20_` | Week 20 of pregnancy |
| `tot_` | Whole pregnancy |
| `wX_` | Week-specific exposure (X = gestational week) |

### 7.3 IQR-Scaled Exposure Variables

Variables prefixed with `iqr_` are exposure divided by the interquartile range (IQR) of that variable. This scaling allows comparability across pollutants and windows. Created in `1.0 Process_data.R`.

| Pattern | Description |
|---------|-------------|
| `iqr_pct1_*`, `iqr_t1_*`, `iqr_t2_*`, `iqr_t3_*` | IQR-scaled trimester exposures |
| `iqr_tot_*` | IQR-scaled whole-pregnancy exposure |
| `iqr_w20_*` | IQR-scaled week 20 exposure |
| `iqr_wX_*` | IQR-scaled week-specific exposure |

Examples: `iqr_tot_PM25_cs`, `iqr_w20_Levo_sp`, `iqr_t1_K_cs`

### 7.4 Week-Specific Variables (Raw)

Weekly exposure variables follow the pattern `wX_POLLUTANT_cs` and `wX_POLLUTANT_sp`:

- **Preconception**: `w-12` to `w-1` (weeks before last menstrual period)
- **Pregnancy**: `w0` to `w43` (gestational weeks)

Examples: `w0_PM25_cs`, `w20_Levo_sp`, `w-3_K_cs`

*Note: Week 20 (`w20_*`) is present; weekly variables span w-12 through w43.*

### 7.5 Summary Exposure Variables

| Variable pattern | Description |
|------------------|-------------|
| `pct1_PM25_cs`, `pct1_PM25_sp` | First-trimester PM₂.₅ |
| `t1_*`, `t2_*`, `t3_*` | Trimester 1, 2, 3 averages |
| `w20_*` | Week 20 exposure |
| `tot_*` | Whole-pregnancy average |
| `iqr_*` | IQR-scaled counterparts of above |

---

## 8. Geographic Data (Data_full_sample_exposure_geo.RData)

| Variable | Type | Description |
|----------|------|-------------|
| `idbase` | character | Unique identifier; links to analysis dataset |
| `geometry` | sfc_POINT | Point coordinates (UTM zone 19S, EPSG:32719) |

Use `st_join()` or `left_join(..., by = "idbase")` to merge with the analysis dataset for spatial analyses.

---

## 9. Model Covariates (control_vars)

The adjusted models use the following covariates:

```r
control_vars <- c("edad_madre", "education", "health_insurance", "job", "first_birth", "sexo_rn", "a_nac", "mes_nac", "comuna")
```

Note: `mes_nac` (birth month) is derived from `fecha_nac` in the modeling scripts.

---

## 10. Data Quality Notes

- **Missing data**: `talla_rn` is fully missing; malformation variables are mostly missing; `cond_egreso` is fully missing.
- **Exposure NAs**: Week-specific exposures have more NAs for later gestational weeks (fewer full-term pregnancies reach those weeks) and for preconception weeks (depending on data availability).
- **Delivery type**: 1,224 records have missing `parto`.
- **Birth outcomes**: 11 records missing `peso_rn`, `lbw`, and `sga`; 5 missing `tlbw`.
- **Analysis sample**: Models exclude gestational age <28 weeks and records with missing outcome variables.

---

## 11. Geographic Context

The study area (Temuco–Padre Las Casas) is known for high wintertime air pollution from residential wood burning. Levoglucosan and potassium are used as tracers of biomass combustion, complementing PM₂.₅ measurements.
