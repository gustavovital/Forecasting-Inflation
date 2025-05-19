rm(list = ls())

library(tidyverse)
library(ggplot2)

data_montly_d <- readRDS("data/data_montly_d.rds")
forecast_m <- readRDS('data/forecast_montly.rds')
forecast_q <- readRDS('data/forecast_quarter.rds')
forecast_q <- readRDS('data/forecast_quarter.rds')
forecast_c1 <- readRDS('data/forecast_class_I.rds')
forecast_c2 <- readRDS('data/forecast_class_II.rds')


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




forecast_s %>% 
  ggplot(aes(x = horizon)) + 
  geom_line(aes(y = mean, color = model_id)) +
  theme(legend.position = "none")

forecast_s %>%
  ggplot(aes(x = horizon, y = mean, group = model_id)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_polar() +
  labs(
    title = "Forecasts for p_livre across models",
    x = "Horizon",
    y = "Forecasted Value"
  )
  
  

  


forecast_s %>%
  ggplot(aes(x = horizon, group = model_id, y = mean)) + 
  geom_line(aes(color = class)) +
  theme(legend.position = "none")
  
  
  
  
  
ggplot(aes(x = horizon, y = mean, group = model_id)) +
  geom_line(alpha = 0.5, size = 0.4) +
  theme_minimal() +
  labs(
    title = "Forecasts for p_livre across models",
    x = "Horizon",
    y = "Forecasted Value"
  ) +
  theme(legend.position = "none")


forecast_s %>%
  
  ggplot(aes(x = horizon)) + 
  geom_point()


library(ggplot2)

# Make sure model_id is a factor or integer (for grouping lines)
forecast_s <- forecast_s %>%
  mutate(model_id = as.factor(model_id))

# Plot
ggplot(forecast_s, aes(x = horizon, y = mean, group = model_id)) +
  geom_line(alpha = 0.3, size = 0.3) +
  facet_wrap(~ class) +
  labs(
    title = "Forecasts for p_livre across VAR models",
    x = "Horizon",
    y = "Forecasted Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

forecast_c1 <- forecast_c1 %>% 
  mutate(model_id = as.factor(model_id))

forecast_c1 %>% 
  ggplot(aes(x = horizon, y = mean, group = model_id, colour = class)) +
  geom_line(alpha = 0.3, size = 0.3) +
  theme(legend.position = "none")

forecast_c2 %>% 
  ggplot(aes(x = horizon, y = mean, group = model_id, colour = class)) +
  geom_line(alpha = 0.3, size = 0.3) +
  theme(legend.position = "none")


forecast_c1 %>%
  ggplot(aes(x = horizon, y = mean, group = model_id)) +
  geom_line(
    aes(
      color = class,
      size = class  # Map size to class directly
    ),
    alpha = 0.6
  ) +
  scale_size_manual(values = c("CLASS I" = 0.06, "COMPONENT CLASS I" = .8)) +  # Adjust these values
  scale_color_manual(values = c("CLASS I" = "gray60", "COMPONENT CLASS I" = "darkred")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Forecasts for p_livre (Class I models)",
    x = "Horizon",
    y = "Forecasted Value"
  )

forecast_c2 %>%
  ggplot(aes(x = horizon, y = mean, group = model_id)) +
  geom_line(
    aes(
      color = class,
      size = class  # Map size to class directly
    ),
    alpha = 0.6
  ) +
  scale_size_manual(values = c("CLASS II" = 0.06, "COMPONENT CLASS II" = .8)) +  # Adjust these values
  scale_color_manual(values = c("CLASS II" = "gray60", "COMPONENT CLASS II" = "darkred")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Forecasts for p_livre (Class I models)",
    x = "Horizon",
    y = "Forecasted Value"
  )
