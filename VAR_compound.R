rm(list = ls())
library(tidyverse)

# montly data ====
# forecast_m <- readRDS('data/forecast_m.rds')
forecast_m_t <- readRDS('data/forecast_m_t.rds')
# forecast_m_acc <- readRDS('data/forecast_m_acc.rds')
forecast_m_acc_t <- readRDS('data/forecast_m_acc_t.rds')

# quarter data ====
forecast_t <- readRDS('data/forecast_t.rds')
forecast_t_acc <- readRDS('data/forecast_t_acc.rds')

# stat data class I ====
# forecast_c1 <- readRDS('data/forecast_c1.rds')
forecast_c1_t <- readRDS('data/forecast_c1_t.rds')
# forecast_c1_acc <- readRDS('data/forecast_c1_acc.rds')
forecast_c1_acc_t <- readRDS('data/forecast_c1_acc_t.rds')

# stat data class II ====
# forecast_c2 <- readRDS('data/forecast_c2.rds')
forecast_c2_t <- readRDS('data/forecast_c2_t.rds')
# forecast_c2_acc <- readRDS('data/forecast_c2_acc.rds')
forecast_c2_acc_t <- readRDS('data/forecast_c2_acc_t.rds')

# WRANGLING DATA ====
# MONTLY ====
compound_m_t_VAR <- forecast_m_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_m_t_BVAR <- forecast_m_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# QUARTERLY DATA ====
compound_t_VAR <- forecast_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_t_BVAR <- forecast_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# STATISTICAL DATA ====
compound_c1_t <- forecast_c1_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_c2_t <- forecast_c2_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )


# ACCUMULATED DATA WRANGLING ====
# MONTLY ====
compound_m_t_VAR_acc <- forecast_m_acc_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date, model) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_m_t_BVAR_acc <- forecast_m_acc_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date, model) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# QUARTERLY DATA ====
compound_t_VAR_acc <- forecast_t_acc %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date, model) %>%
  summarise(
    mean  = median(mean_acc, na.rm = TRUE),
    lower = median(lower_acc, na.rm = TRUE),
    upper = median(upper_acc, na.rm = TRUE),
    .groups = "drop"
  )

compound_t_BVAR_acc <- forecast_t_acc %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean_acc, na.rm = TRUE),
    lower = median(lower_acc, na.rm = TRUE),
    upper = median(upper_acc, na.rm = TRUE),
    .groups = "drop"
  )

# STATISTICAL DATA ====
compound_c1_t_acc <- forecast_c1_acc_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_c2_t_acc <- forecast_c2_acc_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# Create compound VAR ====
compound_c1_t$COMPOUND <- "CLASS I"
compound_c2_t$COMPOUND <- "CLASS II"
compound_m_t_VAR$COMPOUND <- "VAR VECM - M"
compound_m_t_BVAR$COMPOUND <- "BVAR - M"
compound_t_VAR$COMPOUND <- "VAR/VECM - Q"
compound_t_BVAR$COMPOUND <- "BVAR - Q"

# Accumulated 
compound_c1_t_acc$COMPOUND <- "CLASS I"
compound_c2_t_acc$COMPOUND <- "CLASS II"
compound_m_t_VAR_acc$COMPOUND <- "VAR VECM - M"
compound_m_t_BVAR_acc$COMPOUND <- "BVAR - M"
compound_t_VAR_acc$COMPOUND <- "VAR/VECM - Q"
compound_t_BVAR_acc$COMPOUND <- "BVAR - Q"


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
