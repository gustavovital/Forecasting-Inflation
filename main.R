# Set WD:
setwd('/Users/gustavovital/Documents/GitHub/Forecasting-Inflation')

# PREPARE ENVIRONMENT ====
source('requirement.R')
source('functions.R')
source('get_data.R')

# ESTIMATE VARS MODELS (NO COVID)====
source('VAR_montly.R')
source('VAR_quarterly.R')
source('VAR_statistical.R')


