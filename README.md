# Sea level rise weakens mussel–cordgrass facilitation in salt marshes

## Repository overview

This repository hosts the **R scripts** and **raw data** for analyzing how **sea level rise (via relative elevation / inundation)** alters the restoration benefit of **ribbed mussel–cordgrass facilitation** in Gulf Coast salt marshes. The workflow reproduces the core statistical analyses and figures, including the study-site map, biweekly growth trajectories, linear mixed-effects models, single-predictor regressions, and piecewise structural equation modeling (SEM).

For full methods and results, please refer to our study:

> **"Sea level rise weakens the restoration benefit of mussel–cordgrass facilitation in salt marshes"**
> Accepted for publication in *Journal of Applied Ecology*

**Corresponding author:** Brian J. Roberts — [broberts@lumcon.edu](mailto:broberts@lumcon.edu)

---

## What's in this repository

### R scripts

- **`R code 1 - Map - 0623.R`**
  Generates the study-site map (Louisiana with parish borders and the field site marked; Figure 1a).
  *Output:* `map.pdf`. Uses **tigris** to download US Census TIGER/Line boundaries, so an internet connection is required on first run.

- **`R code 2 - Biweekly data analysis - 0623.R`**
  Aggregates the biweekly non-destructive monitoring to the experimental-unit level and fits per-treatment simple linear regressions of growth metrics (stem height, live leaves, live stems, mortality rate) against days deployed.
  *Outputs:* `Table S4.docx`, `Figure 2.pdf`.

- **`R code 3 - Final data analysis - 0623.R`**
  Analyzes end-of-season (destructive sampling) plant and soil traits: linear mixed-effects models (`trait ~ Level * Treat + (1 | Organ)`, Type II Wald tests; Table S5), within-elevation regressions of traits against mussel density (Table S8), and trait-response figures (Figures 3, S3, S4, S6).
  *Outputs:* `Table S5.docx`, `Table S8.docx`, `Figure 3.pdf`, appendix panel figures, `Figure S6.pdf`.

- **`R code 4 - SEM analysis - 0623.R`**
  Builds the variable correlation matrix and fits the piecewise SEM linking inundation frequency and mussel biomass to soil properties and plant biomass / C:N responses (Table S9).
  *Outputs:* `correlation_matrix.pdf`, `Table S9.docx`.

- **`R code 5 - Single effect analysis - 0623.R`**
  Fits pooled single-predictor regressions of each plant/soil trait against relative elevation (Table S6) and against mussel density (Table S7), with the corresponding panel figures (Figures S1, S2).
  *Outputs:* `Figure S1 ... .pdf`, `Figure S2 ... .pdf`, `Table S6.docx`, `Table S7.docx`.

### Data files

- **`Raw data - Biweekly data - 0623.xlsx`**
  Biweekly non-destructive plant monitoring measurements.
- **`Raw data - Final data - 0623.xlsx`**
  End-of-season plant / soil / mussel measurements used for final trait analyses and the SEM.
- **`Raw data - README - 0623.docx`**
  Data dictionary / variable definitions for the raw datasets, including units and notes.

### License

- **`LICENSE.txt`** (MIT)

---

## How to reproduce

### 1. Requirements

Analyses were run in **R** (>= 4.2). Install the required packages once:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "purrr", "broom", "tibble", "stringr", "rlang",
  "ggplot2", "cowplot", "scales", "sf", "tigris",
  "lme4", "car", "piecewiseSEM", "corrplot", "Hmisc",
  "flextable", "officer"
))
```

### 2. Set the working directory

Each analysis script (scripts 2–5) reads the Excel files from, and writes its outputs to, a working directory set near the top of the file:

```r
setwd("…/GitHub")   # change this to wherever you cloned this repository
```

Before running, **change this `setwd()` path to the location of this repository on your machine**. Script 1 (map) has no `setwd()` and writes `map.pdf` to the current working directory.

### 3. Run

The five scripts are independent (each reads the raw data fresh) and can be run in any order.

| Script | Main manuscript outputs |
|---|---|
| `R code 1 - Map` | Figure 1a (map) |
| `R code 2 - Biweekly data analysis` | Figure 2, Table S4 |
| `R code 3 - Final data analysis` | Figures 3, S3, S4, S6; Tables S5, S8 |
| `R code 4 - SEM analysis` | SEM (Figure 4 basis), correlation matrix, Table S9 |
| `R code 5 - Single effect analysis` | Figures S1, S2; Tables S6, S7 |

---

## Data availability

The raw data needed to reproduce all analyses are included in this repository. For the published version, the data and code will also be archived in a permanent repository with a citable DOI (e.g., Dryad for data, Zenodo for code): **DOI to be added upon publication.**
