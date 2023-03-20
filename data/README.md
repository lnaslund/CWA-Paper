# Raw Data

## NRSA and NLA data
All water chemistry data from the National Aquatic Resource Surveys were downloaded from the [EPA website](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys) on April 7, 2022. 

## Policy variables
### 319 grant data
All data pertaining to 319 grants were obtained from the public-facing [EPA GRTS database](https://ofmpub.epa.gov/apex/grts/f?p=109:5000::::::) using a guest login. 
We downloaded Project Summary Reports and Project Budget Reports from 1996-2019 in the Interactive Reports tab on April 1, 2022. 
The downloaded data used for analyses are located in the `grts_project-budget-report` and `grts_project-summary-report` folders.

### Nutrient criteria data
Data concerning the extent and duration of state numeric nutrient criteria were accessed from the [EPA website](https://www.epa.gov/nutrient-policy-data/state-progress-toward-developing-numeric-nutrient-water-quality-criteria) on April 7, 2022.

### TMDL data
Data describing the sites assessed were accessed from the [EPA website](https://www.epa.gov/waterdata/waters-geospatial-data-downloads#NationalGeospatialDatasets) on March 16, 2022. We used the "Pre-2015 305(b) Waters As Assessed Reach Indexed Dataset Archive" which describes all waters assessed within a state, both those that were found to be impaired and those that met their designated uses. 
The downloaded data used for analyses are located in TMDL_data.csv and data summarized at the state level are in
"TMDL_data_summary.csv"

## Nutrient loading predictors
To control for changes in nutrient loading which may drive patterns in nutrient concentrations across states, we developed a global linear model using variables we expected to correlate with nutrient loading to predict state trends in nutrient concentrations. We conducted model selection using Akaike Information Criteria for small sample sizes (AICc) on all models that are a subset of the global model using the `dredge` function in the MuMIn package (Bartón 2022).


### Population data
Population data summarized at the state level were obtained from the US census. 

Table 1. Annual Estimates of the Resident Population: April 1, 2010 to July 1, 2019 (PEPANNRES)
Source: U.S. Census Bureau, Population Division 
Release Date: December 2019

We used the census data from 2010 and the estimated population estimates produced by the census for the following years.
Data were downloaded on August 26, 2022
data are titled "census_population_estimates.csv"

### Land cover data 
Land cover data were obtained from the [National Land Cover Database](https://www.mrlc.gov/data) on April 7, 2022.

### Feed and fertilizer data
Feed and fertilizer data were obtained from the United States Department of Agriculture [Census of Agriculture](https://www.nass.usda.gov/AgCensus/) on April 7, 2022.

# Derived Data
All derived data are located in the `clean_data` folder

