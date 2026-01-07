## Repository Overview

This repository hosts the **R scripts** and raw data for analyzing how **sea level rise (via relative elevation / inundation)** alters the restoration benefit of **ribbed mussel–cordgrass facilitation** in Gulf Coast salt marshes. The workflow reproduces the core statistical analyses and figures, including biweekly growth trajectories, mixed-effects models, single-predictor regressions, and piecewise structural equation modeling (SEM).

For full methods and results, please refer to our study:

> **"Sea level rise weakens the restoration benefit of mussel–cordgrass facilitation in salt marshes"**  
> To be submitted to *Journal of Applied Ecology*

**Corresponding author:**  
Brian J. Roberts — [broberts@lumcon.edu](mailto:broberts@lumcon.edu)

---

## What’s in this repository

### R scripts
- **`CSAP R code 1 - Map - 0107.R`**  
  Generates the study map / spatial figure components.
- **`CSAP R code 2 - Biweekly data analysis - 0107.R`**  
  Analyzes biweekly (time-series) plant monitoring data across elevation × mussel density treatments.
- **`CSAP R code 3 - Final data analysis - 0107.R`**  
  Analyzes end-of-season (destructive sampling) plant/soil traits and fits mixed-effects models.
- **`CSAP R code 4 - SEM - 0107.R`**  
  Fits piecewise SEM linking inundation/mussel biomass, soil properties, and plant responses.
- **`CSAP R code 5 - Single effect analysis - 0107.R`**  
  Runs single-predictor regressions and/or stratified regressions (e.g., within elevation levels).

### Data files
- **`Raw data - Biweekly - 0107.xlsx`**  
  Biweekly non-destructive plant monitoring measurements.
- **`Raw data - Final - 0107.xlsx`**  
  End-of-season plant/soil/mussel measurements used for final trait analyses and SEM.
- **`Raw data - README - 0107.docx`**  
  Data dictionary / variable definitions for the raw datasets (Excel sheets), including units and notes.

### License
- **`LICENSE.txt`** (MIT)
