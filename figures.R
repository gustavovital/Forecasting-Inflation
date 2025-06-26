rm(list = ls())
source('functions.R')

library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(patchwork)

# forecast_compound <- readRDS('data/forecast_compound_diff.rds')
# forecast_compound_ipca <- readRDS('data/forecast_compound_ipca.rds')
COVID_forecast_compound_ipca <- readRDS('data/COVID_forecast_compound_ipca.rds')
## define plots functions ====

# plot_all_forecasts <- function(data, lags) {
#   # Extract Free Prices (actuals)
#   FP <- data %>% 
#     dplyr::filter(COMPOUND == 'Free Prices') %>%
#     arrange(date)
#   
#   # Extract all forecast models (excluding Free Prices)
#   forecasts <- data %>%
#     dplyr::filter(COMPOUND != 'Free Prices') %>%
#     group_by(COMPOUND, forecast_model) %>%
#     arrange(date) %>%
#     slice(lags) %>%  # Use the same lag selection logic
#     ungroup()
#   
#   # Combine data for plotting
#   plot_data <- bind_rows(FP, forecasts) %>%
#     dplyr::filter(date <= as.Date('2025-01-01'))
#   
#   # Generate distinct colors for each model
#   n_models <- n_distinct(forecasts$COMPOUND)
#   model_colors <- c("Free Prices" = "black", 
#                     scales::hue_pal()(n_models))
#   names(model_colors) <- c("Free Prices", unique(forecasts$COMPOUND))
#   
#   # Create plot
#   p <- plot_data %>%
#     ggplot(aes(
#       x = date, 
#       y = mean, 
#       color = COMPOUND,
#       linetype = COMPOUND,
#       group = COMPOUND,
#       linewidth = COMPOUND
#     )) +
#     # Plot all lines
#     geom_line(size = 1) +
#     
#     # Confidence intervals (now with black fill)
#     geom_ribbon(
#       data = plot_data %>% dplyr::filter(COMPOUND == "AVERAGE MODEL"),
#       aes(ymin = lower_95, ymax = upper_95),
#       fill = "black",  # Hardcoded black for 95% CI
#       alpha = 0.10,
#       linetype = 0
#     ) +
#     geom_ribbon(
#       data = plot_data %>% dplyr::filter(COMPOUND == "AVERAGE MODEL"),
#       aes(ymin = lower_80, ymax = upper_80),
#       fill = "black",  # Hardcoded black for 80% CI
#       alpha = 0.10,
#       linetype = 0
#     ) +
#     
#     # Color scale (Free Prices and AVERAGE MODEL both black)
#     scale_color_manual(
#       values = c(
#         "Free Prices" = "black",
#         "AVERAGE MODEL" = "tomato3",  # Force black
#         model_colors[!names(model_colors) %in% c("Free Prices", "AVERAGE MODEL")]
#       )
#     ) +
#     scale_linewidth_manual(values = c(
#       "Free Prices" = 1,  
#       "AVERAGE MODEL" = 1,  
#       "BVAR - M" = 0.4,  
#       "BVAR - Q" = 0.4,  
#       "CLASS I" = 0.4, 
#       "CLASS II" = 0.4, 
#       "VAR/VECM - M" = 0.4, 
#       "VAR/VECM - Q" = 0.4
#     )) +
#     # Line types (unchanged)
#     scale_linetype_manual(
#       values = c(
#         "Free Prices" = "solid",  
#         "AVERAGE MODEL" = "dashed",  # Matches Free Prices
#         "BVAR - M" = "dotted",  
#         "BVAR - Q" = "dotted",  
#         "CLASS I" = "dotted", 
#         "CLASS II" = "dotted", 
#         "VAR/VECM - M" = "dotted", 
#         "VAR/VECM - Q" = "dotted"
#       )
#     ) +
#     
#     # Remove fill legend (no longer needed)
#     labs(
#       title = "Forecast Models vs Free Prices",
#       subtitle = "AVERAGE MODEL (black) with 80%/95% CIs. Other models dashed.",
#       x = NULL, y = NULL,
#       color = "Model", 
#       linetype = "Model"
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       legend.position = "bottom",
#       legend.box = "horizontal"
#     )
#   
#   
#   return(p)
# }

plot_forecast_error <- function(data, lags, compound) {
  
  # Filter and prepare data
  FP <- data %>% 
    dplyr::filter(COMPOUND == 'Free Prices') %>%
    arrange(date)
  
  CP <- data %>% 
    dplyr::filter(COMPOUND == compound) %>% 
    group_by(COMPOUND, forecast_model) %>%
    arrange(date) %>%
    slice(lags) %>%
    ungroup() %>%
    arrange(date)
  
  # Check if we can calculate metrics
  if(nrow(FP) > 0 && nrow(CP) > 0) {
    # Align dates for comparison - ensure we have matching dates with complete cases
    merged_data <- inner_join(
      FP %>% dplyr::select(date, FP_mean = mean),
      CP %>% dplyr::select(date, CP_mean = mean),
      by = "date"
    )
    
    if(nrow(merged_data) > 0) {
      # Calculate error metrics
      errors <- merged_data$CP_mean - merged_data$FP_mean
      error_metrics <- data.frame(
        MSE = mean(errors^2, na.rm = TRUE),
        RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
        MAE = mean(abs(errors), na.rm = TRUE),
        MAPE = mean(2 * abs(errors) / (abs(merged_data$FP_mean) + abs(merged_data$CP_mean)), na.rm = TRUE)) %>% # add * 100 to percentage 
        mutate(across(everything(), ~ ifelse(is.nan(.) | is.infinite(.), NA, round(., 4))))
      
      # Print metrics
      cat("\nForecast Evaluation Metrics for:", compound, "\n")
      cat("----------------------------------------\n")
      print(error_metrics)
      cat("\nNumber of observations used:", nrow(merged_data), "\n")
      cat("\n")
    } else {
      warning("No matching dates with complete cases between Free Prices and ", compound)
    }
  } else {
    if(nrow(FP) == 0) warning("No Free Prices data available")
    if(nrow(CP) == 0) warning("No forecast data available for ", compound)
  }
  
  # Prepare data for plotting
  FPCP <- bind_rows(
    FP %>% mutate(COMPOUND = "Free Prices"),
    CP %>% mutate(COMPOUND = compound)
  ) %>% 
    dplyr::filter(date <= as.Date('2025-01-01')) 
  
  # Create plot only if we have data
  if(nrow(FPCP) > 0) {
    color_values <- c("Free Prices" = "black", "Other" = "black")
    names(color_values)[2] <- compound
    
    fill_values <- c("Free Prices" = "gray60", "Other" = "gray60")
    names(fill_values)[2] <- compound
    
    linetype_values <- c("Free Prices" = "solid", "Other" = "dotted")
    names(linetype_values)[2] <- compound
    
    date_breaks <- seq.Date(
      from = min(FPCP$date, na.rm = TRUE),
      to = max(FPCP$date, na.rm = TRUE),
      by = "quarter"
    )
    
    date_labels <- function(x) {
      quarters <- c("Q1", "Q2", "Q3", "Q4")
      paste0(year(x), " ", quarters[quarter(x)])
    }
    
    p <- FPCP %>% 
      ggplot(aes(x = date, y = mean, color = COMPOUND, linetype = COMPOUND)) +
      geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = COMPOUND), 
                  alpha = 0.15, linetype = 0) +
      geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = COMPOUND), 
                  alpha = 0.25, linetype = 0) +
      geom_line(size = .6) +
      scale_color_manual(values = color_values) +
      scale_fill_manual(values = fill_values) +
      scale_linetype_manual(values = linetype_values) +
      labs(title = paste(compound),
           subtitle = "Dark band: 80% CI | Light band: 95% CI",
           x = NULL, y = NULL,
           color = NULL, linetype = NULL, fill = NULL) +
      scale_x_date(breaks = date_breaks,
                   labels = date_labels,
                   minor_breaks = NULL) +
      theme_minimal(base_size = 8) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            legend.position = "bottom",
            plot.subtitle = element_text(color = "gray40", size = 8),
            panel.grid.minor = element_blank())
    
    return(p)
  } else {
    warning("No data available for plotting")
    return(NULL)
  }
}

# FORECAST PLOTS BY QUARTER ====
# unique(COVID_forecast_compound_ipca$COMPOUND)
# AVERAGE MODEL ====
ci_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'CLASS I')
cii_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'CLASS II')
vm_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'VAR/VECM - M')
bv_m_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'BVAR - M')
vq_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'VAR/VECM - Q')
bv_q_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'BVAR - Q')
av_p <- plot_forecast_error(COVID_forecast_compound_ipca, 1, 'AVERAGE MODEL')


(ci_p + cii_p) / (vm_p + bv_m_p) / (vq_p + bv_q_p) / av_p


plot_all_forecasts(COVID_forecast_compound_ipca, 1)

#### no idea about the code below...
compare_forecast_values <- function(data, lags, models) {
  library(dplyr)
  
  # Actual free prices
  FP <- data %>%
    filter(COMPOUND == "Free Prices") %>%
    arrange(date) %>%
    select(date, actual = mean)
  
  # Loop over each model and extract forecasted values along with confidence intervals
  forecast_data <- lapply(models, function(model_name) {
    data %>%
      filter(COMPOUND == model_name) %>%
      group_by(COMPOUND, forecast_model) %>%
      arrange(date) %>%
      slice(lags) %>%
      ungroup() %>%
      select(date, forecast = mean, lower_80, upper_80, lower_95, upper_95) %>%
      mutate(model = model_name)
  }) %>%
    bind_rows()
  
  # Merge with actual free prices
  comparison <- inner_join(forecast_data, FP, by = "date") %>%
    select(date, model, forecast, lower_80, upper_80, lower_95, upper_95, actual)
  
  return(comparison)
}


print(compare_forecast_values(COVID_forecast_compound_ipca, lags = 4, 
                        models = c("AVERAGE MODEL", "CLASS I", "CLASS II", "VAR/VECM - M", "BVAR - M", "VAR/VECM - Q", "BVAR - Q")), n = 100000)


print(compare_forecast_values(COVID_forecast_compound_ipca, lags = 4, 
                        models = c("AVERAGE MODEL")), n = 100000)




# data <- COVID_forecast_compound_ipca
# lags <- 1
# 
# FP <- data %>% 
#   dplyr::filter(COMPOUND == 'Free Prices') %>%
#   arrange(date)
# 
# # Extract all forecast models (excluding Free Prices)
# forecasts <- data %>%
#   dplyr::filter(COMPOUND != 'Free Prices') %>%
#   group_by(COMPOUND, forecast_model) %>%
#   arrange(date) %>%
#   slice(lags) %>%  # Use the same lag selection logic
#   ungroup()
# 
# 
#   
# # Combine data for plotting
# plot_data <- bind_rows(FP, forecasts) %>%
#   dplyr::filter(date <= as.Date('2025-01-01'))
#   
# # Generate distinct colors for each model
# n_models <- n_distinct(forecasts$COMPOUND)
# model_colors <- c("Free Prices" = "black", 
#                   scales::hue_pal()(n_models))
# names(model_colors) <- c("Free Prices", unique(forecasts$COMPOUND))
#   
#   # Create plot
# plot_data %>%
#   ggplot(aes(
#     x = date, 
#     y = mean, 
#     color = COMPOUND,
#     linetype = COMPOUND,
#     group = COMPOUND,
#     linewidth = COMPOUND
#   )) +
#   # Plot all lines
#   geom_line(size = 1) +
#   
#   # Confidence intervals (now with black fill)
#   geom_ribbon(
#     data = plot_data %>% dplyr::filter(COMPOUND == "AVERAGE MODEL"),
#     aes(ymin = lower_95, ymax = upper_95),
#     fill = "black",  # Hardcoded black for 95% CI
#     alpha = 0.10,
#     linetype = 0
#   ) +
#   geom_ribbon(
#     data = plot_data %>% dplyr::filter(COMPOUND == "AVERAGE MODEL"),
#     aes(ymin = lower_80, ymax = upper_80),
#     fill = "black",  # Hardcoded black for 80% CI
#     alpha = 0.10,
#     linetype = 0
#   ) +
#   
#   # Color scale (Free Prices and AVERAGE MODEL both black)
#   scale_color_manual(
#     values = c(
#       "Free Prices" = "black",
#       "AVERAGE MODEL" = "tomato3",  # Force black
#       model_colors[!names(model_colors) %in% c("Free Prices", "AVERAGE MODEL")]
#     )
#   ) +
#   scale_linewidth_manual(values = c(
#     "Free Prices" = 1,  
#     "AVERAGE MODEL" = 1,  
#     "BVAR - M" = 0.4,  
#     "BVAR - Q" = 0.4,  
#     "CLASS I" = 0.4, 
#     "CLASS II" = 0.4, 
#     "VAR/VECM - M" = 0.4, 
#     "VAR/VECM - Q" = 0.4
#   )) +
#   # Line types (unchanged)
#   scale_linetype_manual(
#     values = c(
#       "Free Prices" = "solid",  
#       "AVERAGE MODEL" = "dashed",  # Matches Free Prices
#       "BVAR - M" = "dotted",  
#       "BVAR - Q" = "dotted",  
#       "CLASS I" = "dotted", 
#       "CLASS II" = "dotted", 
#       "VAR/VECM - M" = "dotted", 
#       "VAR/VECM - Q" = "dotted"
#     )
#   ) +
#   
#   # Remove fill legend (no longer needed)
#   labs(
#     title = "Forecast Models vs Free Prices",
#     subtitle = "AVERAGE MODEL (black) with 80%/95% CIs. Other models dashed.",
#     x = NULL, y = NULL,
#     color = "Model", 
#     linetype = "Model"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "bottom",
#     legend.box = "horizontal"
#   )
