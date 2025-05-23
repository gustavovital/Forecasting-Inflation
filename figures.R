rm(list = ls())

library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)

forecast_compound <- readRDS('data/forecast_compound_diff.rds')
forecast_compound_accum <- readRDS('data/forecast_compound_acc.rds')

# p_livre <- readRDS('data/p_livre.rds')
# forecast_q <- readRDS('data/forecast_quarterly_oos.rds')
# forecast_c1 <- readRDS('data/forecast_class_I.rds')
# forecast_c2 <- readRDS('data/forecast_class_II.rds')


# Graficos de Compound ====
forecast_compound_accum %>% 
  mutate(line_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.1)) %>%
  mutate(point_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.2)) %>%
  ggplot(aes(x = as.Date(date), y = mean, 
             color = forecast_model, 
             group = interaction(forecast_model, COMPOUND))) +
  geom_line(aes(size = line_width)) + 
  geom_point(aes(size = point_width)) +
  scale_size_identity() +  # This ensures the size values are used as-is
  labs(title = "Forecast Model Performance",
       x = "Date", 
       y = "Mean Value",
       color = "Forecast Model") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))



# Supondo que `p_livre_df` tenha colunas: date, p_livre
p_livre %>%
  arrange(date) %>%
  mutate(p_livre_real_acc = p_livre - lag(p_livre, 12)) 
  
  