rm(list = ls())

library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)

# forecast_compound <- readRDS('data/forecast_compound_diff.rds')
# forecast_compound_ipca <- readRDS('data/forecast_compound_ipca.rds')
COVID_forecast_compound_ipca <- readRDS('data/COVID_forecast_compound_ipca.rds')
## define plots functions ====

plot_forecast <- function(data, lags, compound) {
  
  # Filter and prepare data
  FP <- data %>% 
    filter(COMPOUND %in% 'Free Prices')
  
  CP <- data %>% 
    filter(COMPOUND %in% compound) %>% 
    group_by(COMPOUND, forecast_model) %>%
    arrange(date) %>%
    slice(lags) %>%
    ungroup() %>%
    arrange(COMPOUND, forecast_model)
  
  FPCP <- bind_rows(FP, CP)
  FPCP <- FPCP %>% dplyr::filter(date <= as.Date('2025-01-01')) 
  
  # Create named vectors for manual scales
  color_values <- c("Free Prices" = "black", "Other" = "black")
  names(color_values)[2] <- compound  # Dynamically name the second element
  
  fill_values <- c("Free Prices" = "gray60", "Other" = "gray60")
  names(fill_values)[2] <- compound
  
  linetype_values <- c("Free Prices" = "solid", "Other" = "dotted")
  names(linetype_values)[2] <- compound
  
  # Create custom quarterly breaks and labels
  date_breaks <- seq.Date(from = min(FPCP$date, na.rm = TRUE),
                         to = max(FPCP$date, na.rm = TRUE),
                         by = "quarter")
  
  date_labels <- function(x) {
    quarters <- c("Q1", "Q2", "Q3", "Q4")
    paste0(year(x), quarters[quarter(x)])
  }
  
  # Create the plot with both confidence intervals
  p <- FPCP %>% 
    ggplot(aes(x = date, y = mean, color = COMPOUND, linetype = COMPOUND)) +
    # Add 95% CI (wider band)
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = COMPOUND), 
                alpha = 0.15, linetype = 0) +
    # Add 80% CI (narrower band)
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = COMPOUND), 
                alpha = 0.25, linetype = 0) +
    # Add mean forecast line
    geom_line(size = 1) +
    # Custom scales
    scale_color_manual(values = color_values) +
    scale_fill_manual(values = fill_values) +
    scale_linetype_manual(values = linetype_values) +
    # Labels and titles
    labs(title = "Forecast with Confidence Intervals",
         subtitle = "Dark band: 80% CI | Light band: 95% CI",
         x = NULL, y = NULL,
         color = "Series", linetype = "Series", fill = "Series") +
    # Quarterly date scale
    scale_x_date(breaks = date_breaks,
                 labels = date_labels,
                 minor_breaks = NULL) +  # Remove minor breaks for cleaner look
    # Theme settings
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
          legend.position = "bottom",
          plot.subtitle = element_text(color = "gray40", size = 10),
          panel.grid.minor = element_blank())
  
  return(p)
}


# FORECAST PLOTS BY QUARTER ====
unique(COVID_forecast_compound_ipca$COMPOUND)
# AVERAGE MODEL ====
plot_forecast(COVID_forecast_compound_ipca, 1, 'AVERAGE MODEL')


forecast_compound_ipca %>%
  mutate(line_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, .5),
         point_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.2),
         alpha_width = ifelse(COMPOUND == "AVERAGE MODEL", 1, 0.5)) %>%
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

avfp %>% 
  ggplot(aes(x = date, y = mean, color = COMPOUND, fill = COMPOUND)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  labs(title = "Forecast with Confidence Intervals",
       x = "Date", y = "Forecast (%)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## para 2 periodo ====
FP <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'Free Prices')

AV2 <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'AVERAGE MODEL') %>% 
  group_by(COMPOUND, forecast_model) %>% 
  arrange(date) %>%
  slice(2) %>%  # <<< Segunda previsão
  ungroup() %>%
  arrange(COMPOUND, forecast_model)

avfp2 <- bind_rows(FP, AV2)

avfp2 %>% 
  ggplot(aes(x = date, y = mean, color = COMPOUND, fill = COMPOUND)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  labs(title = "Forecast with Confidence Intervals - 2nd Step Ahead",
       x = "Date", y = "Forecast (%)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## para 3 periodo ====
FP <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'Free Prices')

AV3 <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'AVERAGE MODEL') %>% 
  group_by(COMPOUND, forecast_model) %>% 
  arrange(date) %>%
  slice(3) %>%  # <<< Segunda previsão
  ungroup() %>%
  arrange(COMPOUND, forecast_model)

avfp3 <- bind_rows(FP, AV3)

avfp3 %>% 
  ggplot(aes(x = date, y = mean, color = COMPOUND, fill = COMPOUND)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  labs(title = "Forecast with Confidence Intervals - 2nd Step Ahead",
       x = "Date", y = "Forecast (%)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## para 4 periodo ====
FP <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'Free Prices')

AV4 <- COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% 'AVERAGE MODEL') %>% 
  group_by(COMPOUND, forecast_model) %>% 
  arrange(date) %>%
  slice(4) %>%  # <<< Segunda previsão
  ungroup() %>%
  arrange(COMPOUND, forecast_model)

avfp4 <- bind_rows(FP, AV4)

avfp4 %>% 
  ggplot(aes(x = date, y = mean, color = COMPOUND, fill = COMPOUND)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
  labs(title = "Forecast with Confidence Intervals - 2nd Step Ahead",
       x = "Date", y = "Forecast (%)") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
