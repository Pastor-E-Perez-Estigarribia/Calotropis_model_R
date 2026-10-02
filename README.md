# Modelling the Invasive Trajectory of *Calotropis procera* in South America

This repository contains the complete reproducible geospatial workflow and R source code for the manuscript: 
**"Modelling the invasive trajectory of *Calotropis procera* in South America: new records and climate-driven distribution projections"** (submitted to *Scientific Reports*).

It integrates optimized Ecological Niche Models (SDMs) with CMIP6 climate projections through a novel discrete-time kinematic framework to analyze the velocity, acceleration, and dynamic topology of the species' expansion across South American drylands.

## 📌 Features & Workflow

1. **Spatial Data Depuration**: Spatial autocorrelation filtering and bias reduction utilizing nearest neighbor analysis and the `spThin` package.
2. **Algorithm Optimization**: Hyperparameter tuning for the `maxnet` algorithm (Maxent) via a composite scoring system balancing validation AUC, omission rates, and spatial overfitting (`ENMeval`).
3. **Climate-Driven Projections**: Habitat suitability forecasting under the SSP2-4.5 mitigation scenario through to 2070, utilizing a multi-model CMIP6 ensemble.
4. **Kinematic Invasion Analysis**: Implementation of finite difference operators to classify the spatio-temporal dynamics of the invasion front (e.g., *Runaway expansion*, *Asymptotic saturation*, *Continuous retraction*).

## 📂 Repository Structure

* `Data/` - Contains raw and thinned occurrence points (e.g., GBIF, field records) and baseline bioclimatic variables (WorldClim v2.1). *(Note: Heavy raster files are excluded via `.gitignore`)*.
* `Output/` - Directory for generated model predictions, evaluation tables, and high-resolution map figures (PDF/SVG/PNG).
* `Scripts/` - Core R scripts:
  * `Dis_C_pro.R`: Main pipeline for occurrence processing, background sampling, and ENM evaluation.
  * `correlation_pipeline_terra.R`: Collinearity analysis and dimensionality reduction for environmental predictors.
  * `Analisis_de_estabilidad.R`: Kinematic evaluation (velocity and acceleration tensors) and spatial topology mapping.

## 🛠 Prerequisites & Dependencies

The analyses were conducted in R. Core dependencies include:
* **Geospatial Processing**: `terra`, `sf`, `raster`, `spatstat.geom`, `rnaturalearth`
* **SDM & Optimization**: `ENMeval`, `maxnet`, `dismo`, `spThin`
* **Data Manipulation & Visualization**: `tidyverse`, `ggplot2`, `viridis`, `patchwork`

## 👤 Author
**Pastor E. Pérez-Estigarribia**  
Universidad Nacional de Asunción