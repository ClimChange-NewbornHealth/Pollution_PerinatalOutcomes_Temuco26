# Data Dictionary: Perinatal Outcomes and Pollution Exposure

## Data Access

**Restricted access.** Data requests may be submitted to **Estela Blanco** (estela.blanco@uc.cl), Professor at College and Public Health, Pontificia Universidad Católica de Chile.

---

## Overview

This document describes the dataset contained in `Data_full_sample_exposure.RData`. The data combines perinatal outcomes from births in Temuco and Padre Las Casas (Araucanía Region, Chile) with air pollution exposure estimates during pregnancy. The study period spans births from **2009 to 2016**.

| Attribute | Value |
|-----------|-------|
| **Object name** | `data_exp` |
| **Format** | R data.frame |
| **Observations** | 15,557 |
| **Variables** | 407 |
| **Source file** | `Data_full_sample_exposure.RData` |

---

## 1. Identification and Dates

| Variable | Type | Description |
|----------|------|-------------|
| `idbase` | character | Unique identifier for each birth record (e.g., "ID-3013") |
| `fecha_ini` | POSIXct | Pregnancy start date |
| `fecha_nac` | POSIXct | Birth date |
| `a_nac` | integer | Year of birth (2009–2016) |

---

## 2. Geographic and Maternal Covariates

| Variable | Type | Description |
|----------|------|-------------|
| `comuna` | factor | Municipality of birth: PLC (Padre Las Casas), TEM (Temuco) |
| `parto` | factor | Delivery type: cesárea (cesarean), vaginal |
| `edad_madre` | integer | Maternal age at delivery (years) |
| `diabetes` | factor | Maternal diabetes diagnosis: 0 (no), 1 (yes) |
| `estacion` | factor | Season at pregnancy start: verano (summer), otono (autumn), invierno (winter), primavera (spring) |
| `estacion_bin` | numeric | Binary: pregnancy started in winter (1) or not (0) |

---

## 3. Gestational and Birth Characteristics

| Variable | Type | Description |
|----------|------|-------------|
| `edad_gest` | numeric | Gestational age at birth (weeks) |
| `sexo_rn` | character | Infant sex: Masculino, Femenino |
| `peso_rn` | numeric | Birth weight (grams) |
| `talla_rn` | numeric | Birth length (cm); mostly missing |
| `circunf_rn` | numeric | Head circumference at birth (cm) |
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

### 7.2 Exposure Windows

| Prefix | Description |
|--------|-------------|
| `pct1_` | First trimester (percentile or average) |
| `t1_`, `t2_`, `t3_` | Trimesters 1, 2, and 3 |
| `w20_` | Week 20 of pregnancy |
| `tot_` | Whole pregnancy |
| `wX_` | Week-specific exposure (X = gestational week) |

### 7.3 Week-Specific Variables

Weekly exposure variables follow the pattern `wX_POLLUTANT_cs` and `wX_POLLUTANT_sp`:

- **Preconception**: `w-12` to `w-1` (weeks before last menstrual period)
- **Pregnancy**: `w0` to `w43` (gestational weeks)

Examples: `w0_PM25_cs`, `w20_Levo_sp`, `w-3_K_cs`

### 7.4 Summary Exposure Variables

| Variable pattern | Description |
|------------------|-------------|
| `pct1_PM25_cs`, `pct1_PM25_sp` | First-trimester PM₂.₅ |
| `pct1_Levo_cs`, `pct1_Levo_sp` | First-trimester levoglucosan |
| `pct1_K_cs`, `pct1_K_sp` | First-trimester potassium |
| `t1_*`, `t2_*`, `t3_*` | Trimester 1, 2, 3 averages |
| `w20_*` | Week 20 exposure |
| `tot_*` | Whole-pregnancy average |

---

## 8. Data Quality Notes

- **Missing data**: `talla_rn` is fully missing; malformation variables are mostly missing; `cond_egreso` is fully missing.
- **Exposure NAs**: Week-specific exposures have more NAs for later gestational weeks (fewer full-term pregnancies reach those weeks) and for preconception weeks (depending on data availability).
- **Delivery type**: 1,224 records have missing `parto`.
- **Birth outcomes**: 11 records missing `peso_rn`, `lbw`, and `sga`; 5 missing `tlbw`.

---

## 9. Geographic Context

The study area (Temuco–Padre Las Casas) is known for high wintertime air pollution from residential wood burning. Levoglucosan and potassium are used as tracers of biomass combustion, complementing PM₂.₅ measurements.
