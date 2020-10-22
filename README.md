# REBA: Rapid EOO-Based Assessment

This repository contains code, data, and figures that support:

Levin, M., J.B. Meek, B. Boom, S.M. Kross, and E.A. Eskew. In review. Leveraging publicly available data to conduct rapid assessments of extinction risk.

--- 

### Repository Structure

- [`/data`](/data) contains raw and intermediate data files necessary for the analysis
  - [`/fit_models`](/data/fit_models) contains the fit Bayesian model objects
  - [`/gbif_miscellaneous`](/data/gbif_miscellaneous) contains metadata associated with the [GBIF](https://www.gbif.org/) data pull
  - [`/IUCN`](/data/IUCN) contains assessment, plant type, and plant threat data from the [IUCN](https://www.iucn.org/)
  - [`/modeling_data`](/data/modeling_data) contains datasets for statistical modeling
  - [`/NatureServe`](/data/NatureServe) contains [NatureServe](https://www.natureserve.org/) data on plant conservation status, for comparison
  - [`/rcat`](/data/rcat) contains conservation metrics, as calculated with [`rCAT`](https://cran.r-project.org/web/packages/rCAT/index.html)

- [`/outputs`](/outputs) contains all figures output from the data visualization script

- [`/R`](/R) contains code for functions that are sourced in the analysis scripts

- [`/scripts`](/scripts) contains the primary analysis scripts, ordered sequentially by number
  - [`01_data_acquisition.R`](/scripts/01_data_acquisition.R) uses [`rgbif`](https://github.com/ropensci/rgbif) to harvest occurrence records
  - [`02_data_cleaning.R`](/scripts/02_data_cleaning.R) cleans the resulting occurrence data
  - [`03_data_analysis.R`](/scripts/03_data_analysis.R) uses [`rCAT`](https://cran.r-project.org/web/packages/rCAT/index.html) and custom methods to generate conservation metrics for each species
  - [`04_generate_modeling_data.R`](/scripts/04_generate_modeling_data.R) cleans and organizes the data for statistical modeling purposes
  - [`05_statistical_modeling.R`](/scripts/05_statistical_modeling.R) fits Bayesian models
  - [`06_data_visualization.R`](/scripts/06_data_visualization.R) generates all relevant figures
