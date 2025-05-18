rm(list = ls())

library(tidyverse)
library(ggplot2)

data_montly_d <- readRDS("data/data_montly_d.rds")
forecast_m <- readRDS('data/forecast_montly.rds')
forecast_q <- readRDS('data/forecast_quarter.rds')

forecast_m_wide <- forecast_m %>%
  pivot_wider(
    names_from = model,
    values_from = c(mean, lower, upper)
  )

forecast_q_wide <- forecast_q %>%
  pivot_wider(
    names_from = model,
    values_from = c(mean, lower, upper)
  )






forecast_q_wide %>%
  dplyr::filter(component == 'p_livre') %>% 
  ggplot(aes(x = horizon)) +
  
  geom_line(aes(y = mean_VAR_I), size=.2, color = 'dodgerblue2') +
  geom_line(aes(y = mean_VAR_II), size=.2, color = 'dodgerblue3') +
  geom_line(aes(y = mean_VAR_III), size=.2, color = 'dodgerblue4') +
  
  geom_line(aes(y = mean_BVAR_I), size=.2, color = 'tomato2') + 
  geom_line(aes(y = mean_BVAR_II), size=.2, color = 'tomato3') + 
  geom_line(aes(y = mean_BVAR_III), size=.2, color = 'tomato4') +
  geom_line(aes(y = mean_VECM), size=.2, color = 'darkgreen') +
  
  geom_line(aes(y = `mean_COMPONENT I`), color = "black", size = 0.8) +
  # geom_ribbon(aes(ymin = `lower_COMPONENT I`, ymax = `upper_COMPONENT I`),
  #             fill = "black", alpha = 0.15) +
  
  # Component II
  geom_line(aes(y = `mean_COMPONENT II`), color = "orange", size = 0.8) +
  # geom_ribbon(aes(ymin = `lower_COMPONENT II`, ymax = `upper_COMPONENT II`),
  #             fill = "orange", alpha = 0.15) +
  
  labs(
    title = "Forecasts for p_livre with Confidence Intervals",
    y = "Forecasted Value",
    x = "Horizon"
  ) +
  theme_minimal()
