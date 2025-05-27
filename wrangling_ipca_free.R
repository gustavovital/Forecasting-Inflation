# wrangling free IPCA and VAR compound
# rm(list = ls())

p_livre <- g_series(11428, 'p_livre') 

forecast_compound <- readRDS('data/forecast_compound_diff.rds')
COVID_forecast_compound <- readRDS('data/COVID_forecast_compound_diff.rds')

p_livre_t <- get_series(11428, start_date = "2023-01-01") %>%
  rename(date = date, p_livre = `11428`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  mutate(quarter = floor_date(date, unit = "quarter")) %>%
  group_by(quarter) %>%
  summarise(
    p_livre = sum(p_livre, na.rm = TRUE),
    .groups = "drop"
  )


p_livre_t <- p_livre_t %>% 
  mutate(forecast_model = 'Free Prices',
         date = quarter,
         mean = p_livre,
         lower = NA,
         upper = NA,
         COMPOUND = 'Free Prices') %>% 
  dplyr::select(forecast_model, date, mean, lower, upper, COMPOUND)

forecast_compound_ipca <- bind_rows(forecast_compound, p_livre_t)
COVID_forecast_compound_ipca <- bind_rows(COVID_forecast_compound, p_livre_t)

saveRDS(forecast_compound_ipca, 'data/forecast_compound_ipca.rds')
saveRDS(COVID_forecast_compound_ipca, 'data/COVID_forecast_compound_ipca.rds')
