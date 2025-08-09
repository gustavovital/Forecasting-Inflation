library(dplyr)

# 1. List your files
files <- c(
  "data/COVID_data_statistic.rds",
  "data/COVID_data_montly_d.rds",
  "data/COVID_data_montly_l.rds",
  "data/COVID_data_quarter_d.rds",
  "data/COVID_data_quarter_l.rds"
)

# 2. For each file, read it, drop any row with an NA (except date), and collect its dates
clean_dates_list <- lapply(files, function(path) {
  df <- readRDS(path)
  # keep only rows where every column except date is non-NA
  df_clean <- df[ complete.cases( df %>% dplyr::select(-date) ), ]
  sort(unique(df_clean$date))
})

# 3. Find the common dates across all data‐frames
common_dates <- Reduce(intersect, clean_dates_list)

# 4. Report the common horizon
common_horizon <- tibble(
  start = as.Date(min(common_dates)),
  end   = as.Date(max(common_dates)),
  n_obs = length(common_dates)
)

print(common_horizon)
saveRDS(common_horizon, 'data/common_horizon.rds')
