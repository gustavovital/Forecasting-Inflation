rm(list = ls())
library(tidyverse)

# montly data ====
# forecast_m <- readRDS('data/forecast_m.rds')
COVID_forecast_m_t <- readRDS('data/COVID_forecast_m_t.rds')
# forecast_m_acc <- readRDS('data/forecast_m_acc.rds')
# forecast_m_acc_t <- readRDS('data/forecast_m_acc_t.rds')

# quarter data ====
COVID_forecast_t <- readRDS('data/COVID_forecast_df.rds')
# forecast_t_acc <- readRDS('data/forecast_t_acc.rds')

# stat data class I ====
# forecast_c1 <- readRDS('data/forecast_c1.rds')
COVID_forecast_c1_t <- readRDS('data/COVID_forecast_c1_t.rds')
# forecast_c1_acc <- readRDS('data/forecast_c1_acc.rds')
# forecast_c1_acc_t <- readRDS('data/forecast_c1_acc_t.rds')

# stat data class II ====
# forecast_c2 <- readRDS('data/forecast_c2.rds')
COVID_forecast_c2_t <- readRDS('data/COVID_forecast_c2_t.rds')
# forecast_c2_acc <- readRDS('data/forecast_c2_acc.rds')
# forecast_c2_acc_t <- readRDS('data/forecast_c2_acc_t.rds')

# WRANGLING DATA ====
# MONTLY ====
COVID_compound_m_t_VAR <- COVID_forecast_m_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_m_t_BVAR <- COVID_forecast_m_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# QUARTERLY DATA ====
COVID_compound_t_VAR <- COVID_forecast_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_t_BVAR <- COVID_forecast_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# STATISTICAL DATA ====
COVID_compound_c1_t <- COVID_forecast_c1_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(quarterly_mean, na.rm = TRUE),
    lower = median(quarterly_lower, na.rm = TRUE),
    upper = median(quarterly_upper, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_c2_t <- COVID_forecast_c2_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = median(mean, na.rm = TRUE),
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
COVID_compound_c1_t$COMPOUND <- "CLASS I"
COVID_compound_c2_t$COMPOUND <- "CLASS II"
COVID_compound_m_t_VAR$COMPOUND <- "VAR VECM - M"
COVID_compound_m_t_BVAR$COMPOUND <- "BVAR - M"
COVID_compound_t_VAR$COMPOUND <- "VAR/VECM - Q"
COVID_compound_t_BVAR$COMPOUND <- "BVAR - Q"

COVID_compound_diff <- bind_rows(COVID_compound_c1_t, 
                           COVID_compound_c2_t,
                           COVID_compound_m_t_VAR,
                           COVID_compound_m_t_BVAR,
                           COVID_compound_t_VAR,
                           COVID_compound_t_BVAR)
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

COVID_mean_diff <- COVID_compound_diff %>% 
  group_by(forecast_model, date) %>% 
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    lower  = mean(lower, na.rm = TRUE),
    upper  = mean(upper, na.rm = TRUE),
    .groups = "drop"
  ) 

COVID_mean_diff$COMPOUND <- 'AVERAGE MODEL'

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
COVID_compound_diff <- bind_rows(COVID_compound_diff, COVID_mean_diff)
# compound_acc <- bind_rows(compound_acc, mean_acc)

# saveRDS(compound_var, file = "data/forecast_compound.rds")

# Acumulado ====
# p_livre_obs <- readRDS("data/data_montly_l.rds") %>%
#   dplyr::select(date, p_livre) 

# saveRDS(p_livre_obs, file = "data/p_livre.rds")
# saveRDS(compound_acc, file = "data/forecast_compound_acc.rds")
saveRDS(COVID_compound_diff, file = "data/COVID_forecast_compound_diff.rds")
