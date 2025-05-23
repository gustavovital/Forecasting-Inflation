# get expectations for next month BCB

exp <- 
  get_top5s_monthly_market_expectations('IPCA', start_date = '2009-12-01') %>% 
  filter(typeCalc == 'C')

sum_date <- function(date) {
  return(date %m+% months(1)) 
}

exp <- exp %>%
  mutate(next_month = format(date %m+% months(1), "%m/%Y")) %>%
  filter(next_month == reference_date) %>% 
  group_by(mes_base = floor_date(date, "month")) %>% 
  slice_max(date) %>% 
  ungroup() %>% 
  dplyr::select(date, valor = mean) %>% 
  mutate(date = floor_date(date, 'month'))


