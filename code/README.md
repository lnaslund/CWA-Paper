# Code
Code should be run in the order corresponding to the leading number in the file name.

*`1-319_cleaning.R`: Loads raw 319 project data from the `data` folder, fixes obvious date entry errors, and summarizes the total spending in $USD for projects with end dates between 2007 and 2019 for each state and for each state and year combination. The summarized data are written to the `clean_data` folder inside the `data` folder.

*`2-formatting_lake_data.R`: Loads raw NLA data from the 2007, 2012, and 2017 assessments, harmonizes units, removes observations with data quality flags other than non-detects, merges data across surveys, and outputs total nitrogen and total phosphorus data into the `clean_data` folder. Non-detects are set equal to the method detection limit.

*`3-formatting_NRSA_stream_rivers.R`: Loads raw NRSA data from the 2008-2009, 2013-2014, and 2018-2019 assessments, harmonizes units, removes observations with data quality flags other than non-detects, merges data across surveys, and outputs total nitrogen, total phosphorus data into the `clean_data` folder. Non-detects are set equal to the method detection limit.

*`4-summarizing_predictors_of_loading.R`: Loads nutrient loading predictor variables, normalizes them by state area, z-scores them, and outputs them to `alternative_loading_predictors.csv` in the `clean_data` folder.

*`5-summarizing_TMDL_data.R`: Loads raw TMDL data, summarizes it by state and year, and outputs to the `clean_data` folder.

*`6-trend_analysis.R`: Loads cleaned nutrient data and runs a simple linear regression and a weighted linear regression using `trend_analysis()` in the `spsurvey` package to quantify changes in nutrient concentrations in each state over time. The script writes these model outputs to the `clean_data` folder, generates Figure 2 and 3 of state trends in nutrients, and determines the correlation among different nutrient trends (Table 1).

*`7-model_fitting.R`: Loads the cleaned nutrient trend data, loading variables, and policy variables, determines the best model structure with respect to the loading variables for each nutrient-waterbody combination using bootstrapped nutrient trend estimates for each state, z scores the policy variables, and fits models with the policy variables and loading variable structure determined above using bootstrapped nutrient trend estimates for each state. 

*`8-figures.R`: Generates and outputs Figure 1 describing the 319 spending, TMDL site visits, and nutrient criteria by state, Figure 4 depicting the parameter estimates of the loading variables that best described trends in nutrient concentrations among states, and Figure 5 depicting the parameter estimates of the effect of policy variables on trends in nutrient concentrations. Generates summary of effect sizes of policy variables in Table 3.

*`9-supplemental_model_fitting.R`: Fits the models in `7-model_fitting.R` without the nutrient loading predictor variables and calculates the correlations among policy and loading variables. Results presented in the supplemental. 

*`10-trend_analysis_median.R`: Calculates the median trends in nutrient concentrations. Results presented in the supplemental. 

*`11-supplemental_figures.R`: Generates figures of median trends in concentrations by waterbody and nutrient and the relationships between mean and median concentration trends. Figures presented in the supplemental. 

*`12-model_fitting_medians.R`: Fits the models in `7-model_fitting.R` with trends in median nutrient concentrations. Results presented in the supplemental. 
