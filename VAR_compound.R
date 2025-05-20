rm(list = ls())
library(tidyverse)

forecast_m <- readRDS('data/forecast_monthly_oos.rds')
forecast_q <- readRDS('data/forecast_quarterly_oos.rds')
forecast_c1 <- readRDS('data/forecast_class_I.rds')
forecast_c2 <- readRDS('data/forecast_class_II.rds')

# Wrangling ====
compound_mv <- forecast_m %>%
  filter(model %in% c('VAR_I', 'VAR_2', 'VAR_3', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_mb <- forecast_m %>%
  filter(!model %in% c('VAR_I', 'VAR_2', 'VAR_3', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  ) 

compound_qv <- forecast_q %>%
  filter(model %in% c('VAR_I', 'VAR_2', 'VAR_3', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  ) 

compound_qb <- forecast_q %>%
  filter(!model %in% c('VAR_I', 'VAR_2', 'VAR_3', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  ) 

compound_c1 <- forecast_c1 %>% filter(class == 'COMPONENT I')
compound_c2 <- forecast_c2 %>% filter(class == 'COMPONENT I')

# Create compound VAR ====
compound_c1$COMPOUND <- "CLASS I"
compound_c2$COMPOUND <- "CLASS II"
compound_mv$COMPOUND <- "Montly VAR VECM"
compound_mb$COMPOUND <- "Montly BVAR"
compound_qv$COMPOUND <- "Quarterly VAR VECM"
compound_qb$COMPOUND <- "Quarterly BVAR"

compound_var <- tibble(
     date = c(compound_c1$date, compound_c2$date, compound_mv$date, compound_mb$date, compound_qv$date, compound_qb$date),
     mean = c(compound_c1$mean, compound_c2$mean, compound_mv$mean, compound_mb$mean, compound_qv$mean, compound_qb$mean),
     `Forecast Model` = c(compound_c1$forecast_model, compound_c2$forecast_model, compound_mv$forecast_model, compound_mb$forecast_model, compound_qv$forecast_model, compound_qb$forecast_model),
     Compound = c(compound_c1$COMPOUND, compound_c2$COMPOUND, compound_mv$COMPOUND, compound_mb$COMPOUND, compound_qv$COMPOUND, compound_qb$COMPOUND)
)

compound <- compound_var %>% 
  group_by(date, `Forecast Model`) %>% 
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    .groups = "drop"
  ) 

compound$Compound <- 'AVERAGE MODEL'
compound_var <- bind_rows(compound_var, compound)

saveRDS(compound_var, file = "data/forecast_compound.rds")

# Acumulado ====
p_livre_obs <- readRDS("data/data_montly_l.rds") %>%
  dplyr::select(date, p_livre) 

compound_accum <- compound_var %>%
  arrange(Compound, `Forecast Model`, date) %>%
  group_by(Compound, `Forecast Model`) %>%
  mutate(
    mean_acc  = cumsum(mean),
  ) %>%
  ungroup()

saveRDS(p_livre_obs, file = "data/p_livre.rds")
saveRDS(compound_accum, file = "data/forecast_compound_accum.rds")
