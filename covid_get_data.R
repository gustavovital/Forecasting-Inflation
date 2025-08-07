rm(list = ls())

data_statistic <- readRDS("data/data_statistic.rds")
data_montly_d <- readRDS("data/data_montly_d.rds")
data_montly_l <- readRDS("data/data_montly_l.rds")
data_quarter_d <- readRDS("data/data_quarter_d.rds")
data_quarter_l <- readRDS("data/data_quarter_l.rds")

# Apply to all relevant datasets
COVID_data_statistic <- data_statistic %>%
  mutate(D_COVID = ifelse(date >= as.Date("2020-03-01") & 
                            date <= as.Date("2020-12-30"), 1, 0))

COVID_data_montly_d <- data_montly_d %>%
  mutate(D_COVID = ifelse(date >= as.Date("2020-03-01") & 
                            date <= as.Date("2020-12-30"), 1, 0))

COVID_data_montly_l <- data_montly_l %>%
  mutate(D_COVID = ifelse(date >= as.Date("2020-03-01") & 
                            date <= as.Date("2020-12-30"), 1, 0))

COVID_data_quarter_d <- data_quarter_d %>%
  mutate(D_COVID = ifelse(date >= as.Date("2020-01-01") & 
                            date <= as.Date("2020-12-30"), 1, 0))

COVID_data_quarter_l <- data_quarter_l %>%
  mutate(D_COVID = ifelse(date >= as.Date("2020-01-01") & 
                            date <= as.Date("2020-12-30"), 1, 0))

saveRDS(COVID_data_statistic, 'data/COVID_data_statistic.rds')
saveRDS(COVID_data_montly_d, 'data/COVID_data_montly_d.rds')
saveRDS(COVID_data_montly_l, 'data/COVID_data_montly_l.rds')
saveRDS(COVID_data_quarter_d, 'data/COVID_data_quarter_d.rds')
saveRDS(COVID_data_quarter_l, 'data/COVID_data_quarter_l.rds')
