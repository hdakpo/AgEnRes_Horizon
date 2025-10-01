# AgEnRes_Horizon
This repository contains the R codes used in AgEnRes Deliverable 2.1 to disaggregate non-renewable energy use (fertilizer, fuels, electricity) across crops in EU farming systems. The code is organized into clear pipelines for data preparation, outlier detection, and econometric estimation, ensuring transparency and reproducibility of results.

#  Main Folders
## Fertilizer
Contains all scripts related to nitrogen fertilizer allocation at the crop level.
## Fuels & electricity
Contains all scripts related to the allocation of motor fuels and electricity across crops.

# Subfolders (common to both main folders)
1. Data preparation
-  Scripts to harmonize raw FADN microdata (2017–2022).
-  Construction of cropped Utilized Agricultural Area (SAUC) and crop shares.
-  Generation of standardized RDS files used as inputs in the pipeline.
3.	Outliers detection
 -  Routines to identify and treat abnormal values in fertilizer, fuel, and electricity use.
 -  Implements three complementary methods (wBACON, DBSCAN, IQR) to flag suspicious farm-year observations.
 -  Produces visual diagnostics (scatterplots, boxplots) before and after cleaning.
4.	Estimation
-  Econometric modeling of crop-level allocations using Random Parameter models estimated with SAEM, combined with Bayesian simulation.
-  Integration of principal component analysis (PCA) to handle collinearity in crop shares.
-  Outputs farm-level and crop-level input use predictions (per hectare), along with diagnostics and model fit statistics.

# Outputs
The scripts collectively reproduce the methodological pipeline described in Deliverable 2.1, generating:
-  Cleaned, harmonized input data per country.
-  Outlier-filtered datasets with visual and statistical checks.
-  Final econometric estimates of input allocation (fertilizer, fuels, electricity) at crop level across EU-27 arable farms.
