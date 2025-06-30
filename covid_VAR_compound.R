rm(list = ls())
source('requirement.R')

# GET DATA ====
COVID_forecast_m_t <- readRDS('data/COVID_forecast_m_t.rds')
COVID_forecast_t <- readRDS('data/COVID_forecast_df.rds')
COVID_forecast_c1_t <- readRDS('data/COVID_forecast_c1_t.rds')
COVID_forecast_c2_t <- readRDS('data/COVID_forecast_c2_t.rds')

# WRANGLING DATA - MONTHLY ====
COVID_compound_m_t_VAR <- COVID_forecast_m_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower_95 = median(lower_95, na.rm = TRUE),
    upper_95 = median(upper_95, na.rm = TRUE),
    lower_80 = median(lower_80, na.rm = TRUE),
    upper_80 = median(upper_80, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_m_t_BVAR <- COVID_forecast_m_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower_95 = median(lower_95, na.rm = TRUE),
    upper_95 = median(upper_95, na.rm = TRUE),
    lower_80 = median(lower_80, na.rm = TRUE),
    upper_80 = median(upper_80, na.rm = TRUE),
    .groups = "drop"
  )

# QUARTERLY DATA ====
COVID_compound_t_VAR <- COVID_forecast_t %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower_95 = median(lower_95, na.rm = TRUE),
    upper_95 = median(upper_95, na.rm = TRUE),
    lower_80 = median(lower_80, na.rm = TRUE),
    upper_80 = median(upper_80, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_t_BVAR <- COVID_forecast_t %>%
  filter(!model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>% 
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower_95 = median(lower_95, na.rm = TRUE),
    upper_95 = median(upper_95, na.rm = TRUE),
    lower_80 = median(lower_80, na.rm = TRUE),
    upper_80 = median(upper_80, na.rm = TRUE),
    .groups = "drop"
  )

# STATISTICAL DATA ====
COVID_compound_c1_t <- COVID_forecast_c1_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(quarterly_mean, na.rm = TRUE),
    lower_95 = median(quarterly_lower_95, na.rm = TRUE),
    upper_95 = median(quarterly_upper_95, na.rm = TRUE),
    lower_80 = median(quarterly_lower_80, na.rm = TRUE),
    upper_80 = median(quarterly_upper_80, na.rm = TRUE),
    .groups = "drop"
  )

COVID_compound_c2_t <- COVID_forecast_c2_t %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean = median(quarterly_mean, na.rm = TRUE),
    lower_95 = median(quarterly_lower_95, na.rm = TRUE),
    upper_95 = median(quarterly_upper_95, na.rm = TRUE),
    lower_80 = median(quarterly_lower_80, na.rm = TRUE),
    upper_80 = median(quarterly_upper_80, na.rm = TRUE),
    .groups = "drop"
  )

# CREATE VAR COMPOUND ====
COVID_compound_c1_t$COMPOUND <- "CLASS I"
COVID_compound_c2_t$COMPOUND <- "CLASS II"
COVID_compound_m_t_VAR$COMPOUND <- "VAR/VECM - M"
COVID_compound_m_t_BVAR$COMPOUND <- "BVAR - M"
COVID_compound_t_VAR$COMPOUND <- "VAR/VECM - Q"
COVID_compound_t_BVAR$COMPOUND <- "BVAR - Q"

COVID_compound_diff <- bind_rows(
  COVID_compound_c1_t, 
  COVID_compound_c2_t,
  COVID_compound_m_t_VAR,
  COVID_compound_m_t_BVAR,
  COVID_compound_t_VAR,
  COVID_compound_t_BVAR
)

# AVERAGE MODEL ====
COVID_mean_diff <- COVID_compound_diff %>% 
  group_by(forecast_model, date) %>% 
  summarise(
    mean = mean(mean, na.rm = TRUE),
    lower_95 = mean(lower_95, na.rm = TRUE),
    upper_95 = mean(upper_95, na.rm = TRUE),
    lower_80 = mean(lower_80, na.rm = TRUE),
    upper_80 = mean(upper_80, na.rm = TRUE),
    .groups = "drop"
  ) 

COVID_mean_diff$COMPOUND <- 'AVERAGE MODEL'
COVID_compound_diff <- bind_rows(COVID_compound_diff, COVID_mean_diff)

saveRDS(COVID_compound_diff, file = "data/COVID_forecast_compound_diff.rds")
