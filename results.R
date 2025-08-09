rm(list = ls())
source('requirement.R')

COVID_forecast_compound_ipca <- readRDS('data/COVID_forecast_compound_diff.rds')
common_horizon <- read_rds('data/common_horizon.rds')
start_forecast <- common_horizon$end %m-% years(1)

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
  
  
  FPCP <- bind_rows(
    FP %>% mutate(COMPOUND = "Free Prices"),
    CP %>% mutate(COMPOUND = compound)
  ) 
  #  %>% 
  #   dplyr::filter(date <= as.Date('2025-01-01')) 
  
  
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
for(forecast in c(1,2,4)){
  
  ci_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'CLASS I')
  cii_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'CLASS II')
  vm_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'VAR/VECM - M')
  bv_m_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'BVAR - M')
  vq_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'VAR/VECM - Q')
  bv_q_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'BVAR - Q')
  av_p <- plot_forecast_error(COVID_forecast_compound_ipca, forecast, 'AVERAGE MODEL')

  plot((ci_p + cii_p) / (vm_p + bv_m_p) / (vq_p + bv_q_p) / av_p)

}

COVID_forecast_compound_ipca %>% 
  filter(COMPOUND %in% c('Free Prices', 'AVERAGE MODEL'),
         forecast_model %in% c('Free Prices', 'VI')) %>% 
  bind_rows(tibble(
    forecast_model = max(COVID_forecast_compound_ipca$forecast_model),
    date = dplyr::last(COVID_forecast_compound_ipca$date[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    mean = dplyr::last(COVID_forecast_compound_ipca$mean[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    lower_95 = dplyr::last(COVID_forecast_compound_ipca$mean[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    upper_95 = dplyr::last(COVID_forecast_compound_ipca$mean[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    lower_80 = dplyr::last(COVID_forecast_compound_ipca$mean[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    upper_80 = dplyr::last(COVID_forecast_compound_ipca$mean[COVID_forecast_compound_ipca$COMPOUND == 'Free Prices']),
    COMPOUND = 'AVERAGE MODEL')) %>% 
  ggplot(aes(date, color = COMPOUND)) + 
  geom_line(aes(y=mean)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95, fill = COMPOUND), 
              alpha = 0.15, linetype = 0) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, fill = COMPOUND), 
              alpha = 0.25, linetype = 0) 



