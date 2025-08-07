# Set WD:
setwd('/Users/gustavovital/Documents/GitHub/Forecasting-Inflation')

# PREPARE ENVIRONMENT ====
source('requirement.R')
source('get_data.R')
source('covid_get_data.R')

# ESTIMATE VARS MODELS (COVID) ====
source('covid_VAR_montly_oos.R')
source('covid_VAR_quarterly_oos.R')
source('covid_VAR_statistical_oos.R')

# UNIT ROOT ====
source('unit_root_tests.R')

# WRANGLING RESULTS ====
source('covid_VAR_compound.R')

# RESULTS ====
source('results.R')
