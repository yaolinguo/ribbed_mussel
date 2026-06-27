## Repository overview

This repository hosts the **R scripts** and **raw data** for analyzing how sea level rise alters the restoration benefit of ribbed mussel–cordgrass facilitation in Gulf Coast salt marshes. The workflow reproduces the core statistical analyses and figures.

For full methods and results, please refer to our study:

> **"Sea level rise weakens the restoration benefit of mussel–cordgrass facilitation in salt marshes"**
> *Journal of Applied Ecology*

**Corresponding author:** Brian J. Roberts — [broberts@lumcon.edu](mailto:broberts@lumcon.edu)

---

## What's in this repository

### R scripts

- **`R code 1 - Map - 0626.R`**
  Generates the study-site map.

- **`R code 2 - Biweekly data analysis - 0626.R`**
  Aggregates the biweekly non-destructive monitoring to the experimental-unit level and fits per-treatment simple linear regressions of growth metrics against days deployed.

- **`R code 3 - Final data analysis - 0626.R`**
  Analyzes end-of-season (destructive sampling) plant and soil traits: linear mixed-effects models, within-elevation regressions of traits against mussel density, and trait-response figures.

- **`R code 4 - SEM analysis - 0626.R`**
  Builds the variable correlation matrix and fits the piecewise SEM linking inundation frequency and mussel biomass to soil properties and plant biomass / C:N responses.

- **`R code 5 - Single effect analysis - 0626.R`**
  Fits pooled single-predictor regressions of each plant/soil trait against relative elevation and against mussel density, with the corresponding panel figures.

### Data files

- **`Raw data - Biweekly data - 0626.xlsx`**
  Biweekly non-destructive plant monitoring measurements.
  
- **`Raw data - Final data - 0626.xlsx`**
  End-of-season plant / soil / mussel measurements used for final trait analyses and the SEM.
  
- **`Raw data - README - 0626.docx`**
  Data dictionary / variable definitions for the raw datasets.
