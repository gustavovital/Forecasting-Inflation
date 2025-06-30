rm(list = ls())
library(tidyverse)

# montly data ====
# forecast_m <- readRDS('data/forecast_m.rds')
forecast_m_t <- readRDS('data/forecast_m_t.rds')
# forecast_m_acc <- readRDS('data/forecast_m_acc.rds')
# forecast_m_acc_t <- readRDS('data/forecast_m_acc_t.rds')

# quarter data ====
forecast_t <- readRDS('data/forecast_t.rds')
# forecast_t_acc <- readRDS('data/forecast_t_acc.rds')

# stat data class I ====
# forecast_c1 <- readRDS('data/forecast_c1.rds')
forecast_c1_t <- readRDS('data/forecast_c1_t.rds')
# forecast_c1_acc <- readRDS('data/forecast_c1_acc.rds')
# forecast_c1_acc_t <- readRDS('data/forecast_c1_acc_t.rds')

# stat data class II ====
# forecast_c2 <- readRDS('data/forecast_c2.rds')
forecast_c2_t <- readRDS('data/forecast_c2_t.rds')
# forecast_c2_acc <- readRDS('data/forecast_c2_acc.rds')
# forecast_c2_acc_t <- readRDS('data/forecast_c2_acc_t.rds')

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
    mean  = median(quarterly_mean, na.rm = TRUE),
    lower = median(quarterly_lower, na.rm = TRUE),
    upper = median(quarterly_upper, na.rm = TRUE),
    .groups = "drop"
  )

compound_c2_t <- forecast_c2_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(quarterly_mean, na.rm = TRUE),
    lower = median(quarterly_lower, na.rm = TRUE),
    upper = median(quarterly_upper, na.rm = TRUE),
    .groups = "drop"
  )


# ACCUMULATED DATA WRANGLING ====
# MONTLY ====
# compound_m_t_VAR_acc <- forecast_m_acc_t %>%
#   filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean, na.rm = TRUE),
#     lower = median(lower, na.rm = TRUE),
#     upper = median(upper, na.rm = TRUE),
#     .groups = "drop"
#   )

# compound_m_t_BVAR_acc <- forecast_m_acc_t %>%
#   filter(model %in% c('BVAR_I', 'BVAR_II', 'BVAR_III')) %>% 
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean, na.rm = TRUE),
#     lower = median(lower, na.rm = TRUE),
#     upper = median(upper, na.rm = TRUE),
#     .groups = "drop"
#   )

# QUARTERLY DATA ====
# compound_t_VAR_acc <- forecast_t_acc %>%
#   filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean_acc, na.rm = TRUE),
#     lower = median(lower_acc, na.rm = TRUE),
#     upper = median(upper_acc, na.rm = TRUE),
#     .groups = "drop"
#   )

# compound_t_BVAR_acc <- forecast_t_acc %>%
#   filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean_acc, na.rm = TRUE),
#     lower = median(lower_acc, na.rm = TRUE),
#     upper = median(upper_acc, na.rm = TRUE),
#     .groups = "drop"
#   )

# STATISTICAL DATA ====
# compound_c1_t_acc <- forecast_c1_acc_t %>%
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean, na.rm = TRUE),
#     lower = median(lower, na.rm = TRUE),
#     upper = median(upper, na.rm = TRUE),
#     .groups = "drop"
#   )

# compound_c2_t_acc <- forecast_c2_acc_t %>%
#   group_by(forecast_model, date) %>%
#   summarise(
#     mean  = median(mean, na.rm = TRUE),
#     lower = median(lower, na.rm = TRUE),
#     upper = median(upper, na.rm = TRUE),
#     .groups = "drop"
#   )

# Create compound VAR ====
compound_c1_t$COMPOUND <- "CLASS I"
compound_c2_t$COMPOUND <- "CLASS II"
compound_m_t_VAR$COMPOUND <- "VAR VECM - M"
compound_m_t_BVAR$COMPOUND <- "BVAR - M"
compound_t_VAR$COMPOUND <- "VAR/VECM - Q"
compound_t_BVAR$COMPOUND <- "BVAR - Q"

compound_diff <- bind_rows(compound_c1_t, 
                           compound_c2_t,
                           compound_m_t_VAR,
                           compound_m_t_BVAR,
                           compound_t_VAR,
                           compound_t_BVAR)
# Accumulated 
# compound_c1_t_acc$COMPOUND <- "CLASS I"
# compound_c2_t_acc$COMPOUND <- "CLASS II"
# compound_m_t_VAR_acc$COMPOUND <- "VAR VECM - M"
# compound_m_t_BVAR_acc$COMPOUND <- "BVAR - M"
# compound_t_VAR_acc$COMPOUND <- "VAR/VECM - Q"
# compound_t_BVAR_acc$COMPOUND <- "BVAR - Q"

# compound_acc <- bind_rows(compound_c1_t_acc, 
#                            compound_c2_t_acc,
#                            compound_m_t_VAR_acc,
#                            compound_m_t_BVAR_acc,
#                            compound_t_VAR_acc,
#                            compound_t_BVAR_acc)

mean_diff <- compound_diff %>% 
  group_by(forecast_model, date) %>% 
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    lower  = mean(lower, na.rm = TRUE),
    upper  = mean(upper, na.rm = TRUE),
    .groups = "drop"
  ) 

mean_diff$COMPOUND <- 'AVERAGE MODEL'

# acc ====
# mean_acc <- compound_acc %>% 
#   group_by(forecast_model, date) %>% 
#   summarise(
#     mean  = mean(mean, na.rm = TRUE),
#     lower  = mean(lower, na.rm = TRUE),
#     upper  = mean(upper, na.rm = TRUE),
#     .groups = "drop"
#   ) 

# mean_acc$COMPOUND <- 'AVERAGE MODEL'

# last wrangling ====
compound_diff <- bind_rows(compound_diff, mean_diff)
# compound_acc <- bind_rows(compound_acc, mean_acc)

# saveRDS(compound_var, file = "data/forecast_compound.rds")

# Acumulado ====
# p_livre_obs <- readRDS("data/data_montly_l.rds") %>%
#   dplyr::select(date, p_livre) 

# saveRDS(p_livre_obs, file = "data/p_livre.rds")
# saveRDS(compound_acc, file = "data/forecast_compound_acc.rds")
saveRDS(compound_diff, file = "data/forecast_compound_diff.rds")
