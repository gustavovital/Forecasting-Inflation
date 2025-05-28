rm(list = ls())

library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)


# forecast_compound <- readRDS('data/forecast_compound_diff.rds')
forecast_compound_ipca <- readRDS('data/forecast_compound_ipca.rds')
COVID_forecast_compound_ipca <- readRDS('data/COVID_forecast_compound_ipca.rds')


forecast_compound_ipca %>%
  mutate(line_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, .5),
         point_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.2),
         alpha_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.05)) %>%
  ggplot(aes(x = as.Date(date), y = mean,
             group = interaction(forecast_model, COMPOUND))) +
  
  # Plot Free Prices as a bold black line
  geom_line(data = subset(forecast_compound_ipca, COMPOUND == "Free Prices"),
            aes(x = as.Date(date), y = mean), 
            color = "black", size = 1.2) +
  
  # Plot the rest as usual
  geom_line(aes(color = forecast_model, size = line_width, alpha = alpha_width)) +
  geom_point(aes(color = forecast_model, size = point_width)) +
  
  scale_size_identity() +
  labs(title = NULL,
       x = "Date",
       y = NULL,
       color = "Forecast Model") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# covid plot ====
COVID_forecast_compound_ipca %>%
  mutate(line_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 2),
         point_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.2),
         alpha_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.05)) %>%
  ggplot(aes(x = as.Date(date), y = mean,
             group = interaction(forecast_model, COMPOUND))) +
  
  # Plot Free Prices as a bold black line
  geom_line(data = subset(forecast_compound_ipca, COMPOUND == "Free Prices"),
            aes(x = as.Date(date), y = mean), 
            color = "black", size = 1.2) +
  
  # Plot the rest as usual
  geom_line(aes(color = forecast_model, size = line_width, alpha = alpha_width)) +
  geom_point(aes(color = forecast_model, size = point_width)) +
  
  scale_size_identity() +
  labs(title = NULL,
       x = "Date",
       y = NULL,
       color = "Forecast Model") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

# by forecast ====
# (1) ahead ====
COVID_forecast_compound_ipca %>%
  filter(COMPOUND == 'Free Prices' | COMPOUND == 'AVERAGE MODEL') %>% 
  group_by(forecast_model, COMPOUND) %>%
  arrange(date) %>%
  slice(1) %>%
  ungroup()

teste <- COVID_forecast_compound_ipca %>%
  group_by(COMPOUND, forecast_model) %>%  # keep both grouping dimensions
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(COMPOUND, forecast_model)

  
COVID_forecast_compound_ipca %>%
  filter(COMPOUND %in% c("Free Prices", "AVERAGE MODEL")) 

FP <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'Free Prices')

AV <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'AVERAGE MODEL') %>% 
  group_by(COMPOUND, forecast_model) %>%  # keep both grouping dimensions
  arrange(date) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(COMPOUND, forecast_model)

avfp <- bind_rows(FP, AV)

avfp %>% 
  ggplot(aes(x = date)) +
  geom_line() # continuar daqui

