This repository contains replication code for Schneider et al. 2022 "Assessing the affordability of nutrient-adequate diets". 
Created and last updated by Kate R. Schneider on June 09, 2022.

The paper makes use of datasets calculated using household survey data, individual nutrient requirements, food composition data, and market price data. The replication files to create the analysis datasets provided here are on GitHub (see below). This dataset contains the specific datasets needed to replicate the analysis in the paper "Assessing the Affordability of nutrient-adequate diets." Published by Schneider et al (2022) in the American Journal of Agricultural Economics. 

The replication code requires Stata, and was created in Stata version 15.1 and revised in Stata 17.0.

All of the analysis, figures, and tables presented in the paper can be replicated with the single .do file provided. The following input datasets are needed to replicate the analysis in the paper: 

CPIDataforCoNA - contains the market price data needed to compute the least-cost diets.
HHNeedsIndivLevel_r - contains household nutrient requirements (shared requirements) at the individual level. 
MalawiIHPS_DietQualCost_CoNAiCoNAseries_r - contains the least-cost diet results for individualized and shared diets at the monthly level over the full time series.
MalawiIHPS_DietQualCost_FoodHHExp - household food expenditure computed from household surveys. Data are at the food item level.
MalawiIHPS_DietQualCost_FoodNUT_HHExp - household food expenditure computed from household surveys. Data are at the food item by nutrient level.
MalawiIHPS_DietQualCost_HH_r - household survey data with the shared least-cost diet result in the month the household was surveyed. Data are at the household level.
MalawiIHPS_DietQualCost_iCoNA - household survey data with the individualized least-cost diet result in the month the household was surveyed. Data are at the household level.
MalawiIHPS_DietQualCost_PID_r - household survey data at the individual level, including nutrient requirements. 
PPP_Denton - monthly level data to smooth international annual PPP price conversions over the year.

These files also include MalawiIHPS_DietQualCost_seasonality - the dataset created in the analysis of the seasonality results following the replication code.

Replication code and all datasets to create the analysis datasets used in this paper are available on Harvard Dataverse: https://doi.org/10.7910/DVN/AXDJZ8

Replication code and raw data to create the datasets analyzed here are available on GitHub at https://github.com/KateSchneider-FoodPol/Kate-Schneider-MalawiDietQualCostHHs-2021
Replicating the creation of these files requires Stata (code created in version 15.1), R, and access to a high power computer to transform the input dataset required for the linear programming (do file #4), run the linear programming in R (syntax script #5), and transform the results (do file #6). 

