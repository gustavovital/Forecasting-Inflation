rm(list = ls())

library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)


forecast_compound <- readRDS('data/forecast_compound_diff.rds')
forecast_compound_ipca <- readRDS('data/forecast_compound_ipca.rds')
# forecast_compound_accum <- readRDS('data/forecast_compound_acc.rds')

# p_livre <- readRDS('data/p_livre.rds')
# forecast_q <- readRDS('data/forecast_quarterly_oos.rds')
# forecast_c1 <- readRDS('data/forecast_class_I.rds')
# forecast_c2 <- readRDS('data/forecast_class_II.rds')


# Graficos de Compound ====
# forecast_compound_ipca %>% 
#   mutate(line_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 2)) %>%
#   mutate(point_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.2)) %>%
#   mutate(alpha_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.05)) %>%
#   mutate(alpha_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.05)) %>%
#   ggplot(aes(x = as.Date(date), y = mean, 
#              color = forecast_model, 
#              group = interaction(forecast_model, COMPOUND))) +
#   geom_line(aes(size = line_width, alpha = alpha_width)) + 
#   geom_point(aes(size = point_width)) +
#   scale_size_identity() +  # This ensures the size values are used as-is
#   labs(title = NULL,
#        x = "Date", 
#        y = NULL,
#        color = "Forecast Model") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold")) 

forecast_compound_ipca %>%
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
