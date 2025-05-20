rm(list = ls())

library(tidyverse)
library(ggplot2)


forecast_compound <- readRDS('data/forecast_compound.rds')

# forecast_q <- readRDS('data/forecast_quarterly_oos.rds')
# forecast_c1 <- readRDS('data/forecast_class_I.rds')
# forecast_c2 <- readRDS('data/forecast_class_II.rds')

# Graficos de Compound ====
forecast_compound

