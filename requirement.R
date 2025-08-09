# REQUIREMENT FOR THE PROJECT ====
# if (!requireNamespace("sidrar", quietly = TRUE)) install.packages("sidrar")
# if (!requireNamespace("ipeadatar", quietly = TRUE)) install.packages("ipeadatar")
# if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
# if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("dplyr")
# if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
# if (!requireNamespace("rbcb", quietly = TRUE)) install.packages("rbcb")
# if (!requireNamespace("urca", quietly = TRUE)) install.packages("urca")
# if (!requireNamespace("seasonal", quietly = TRUE)) install.packages("seasonal")
# if (!requireNamespace("tsibble", quietly = TRUE)) install.packages("tsibble")
# if (!requireNamespace("BMR", quietly = TRUE)) install.packages("BMR")

packages <- c(
  "sidrar", "seasonal", "urca", "ipeadatar", "quantmod", "tidyverse",
  "rbcb", "tsibble", "lubridate", "BMR", "vars", "forecast", "glue", 
  "patchwork", "RColorBrewer", "scales"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(sidrar)
library(seasonal)
library(urca)
library(ipeadatar)
library(quantmod)
library(tidyverse)
library(rbcb)
library(tsibble)
library(sidrar)
library(lubridate)
library(BMR)
library(vars)
library(forecast)
library(glue)
library(RColorBrewer)
library(patchwork)
library(scales)
library(fredr)
library(ggthemes)

# FUNCTIONS ====
source('get_u.R')

to_quarterly <- function(df, nome_var) {
  nome_var <- rlang::enquo(nome_var)
  
  df %>%
    mutate(quarter_date = as.Date(paste0(lubridate::year(date), "-", (lubridate::quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(!!rlang::as_name(nome_var) := mean(!!nome_var, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}

to_quarterly_last <- function(df, nome_var) {
  nome_var <- rlang::enquo(nome_var)
  
  df %>%
    mutate(quarter_date = as.Date(paste0(lubridate::year(date), "-", (lubridate::quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(!!rlang::as_name(nome_var) := dplyr::last(!!nome_var)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}

g_series <- function(codigo, nome) {
  get_series(codigo, start_date = "2009-12-01") %>%
    rename(date = date, !!nome := !!sym(as.character(codigo))) %>%
    mutate(date = floor_date(date, "month")) %>%
    arrange(date)
}

acum_series <- function(data) {
  var_name <- names(data)[2]
  data %>%
    mutate(
      !!sym(var_name) := cumsum(.data[[var_name]])
    ) %>%
    dplyr::select(date, !!sym(var_name))
}

acum_series_pct <- function(data) {
  var_name <- names(data)[2]
  data %>%
    mutate(
      !!sym(var_name) := (cumprod(1 + .data[[var_name]] / 100) - 1) * 100
    ) %>%
    dplyr::select(date, !!sym(var_name))
}

add_variable <- function(df, var_df, var_name) {
  var_df_renamed <- var_df %>%
    dplyr::select(date, !!sym(var_name)) %>%
    rename(!!var_name := !!sym(var_name))
  
  full_join(df, var_df_renamed, by = "date") %>%
    arrange(date) %>% 
    filter(date >= as.Date('2010-01-01'))
  
}

# statistical VAR ====
get_pcs <- function(df, vars, n = 2) {
  df_clean <- df %>% dplyr::select(all_of(vars)) %>% 
    drop_na()
  pcs <- prcomp(df_clean, scale. = TRUE)
  pcs$x[, 1:n] %>%
    as_tibble()
}

unit_root_table <- function(series, lags_adf = NULL, lags_kpss = NULL) {
  
  # Unit test impletentation in order to generalise unit root tests for 
  # time series. Course of econometrics II - FEP - UP
  # Authour: gustavovital
  # Date: 04/04/2025
  
  require(urca)
  
  series <- as.numeric(series)
  n <- length(series)
  if (is.null(lags_adf)) lags_adf <- trunc((n - 1)^(1/3))
  if (is.null(lags_kpss)) lags_kpss <- trunc((n - 1)^(1/4))
  
  results <- data.frame()
  
  # ADF Test (only tau)
  tau_names <- c(none = "tau1", drift = "tau2", trend = "tau3")
  
  for (type in c("none", "drift", "trend")) {
    test <- ur.df(series, type = type, lags = lags_adf)
    stat_name <- tau_names[[type]]
    
    stat <- test@teststat[, stat_name] 
    crit <- test@cval[stat_name, ]
    
    row <- data.frame(
      Test = "ADF",
      Model = type,
      Statistic = round(unname(stat), 3),
      CV_1pct = round(crit["1pct"], 3),
      CV_5pct = round(crit["5pct"], 3),
      CV_10pct = round(crit["10pct"], 3)
    )
    
    results <- rbind(results, row)
  }
  
  # PP Test (Only Z-tau)
  for (model in c("constant", "trend")) {
    test <- ur.pp(series, type = "Z-tau", model = model, lags = "long")
    row <- data.frame(
      Test = "PP",
      Model = model,
      Statistic = round(test@teststat[[1]], 3),
      CV_1pct = round(test@cval[1, "1pct"], 3),
      CV_5pct = round(test@cval[1, "5pct"], 3),
      CV_10pct = round(test@cval[1, "10pct"], 3)
    )
    results <- rbind(results, row)
  }
  
  # KPSS Test
  for (type in c("mu", "tau")) {
    test <- ur.kpss(series, type = type, use.lag = lags_kpss)
    crit_vals <- test@cval
    row <- data.frame(
      Test = "KPSS",
      Model = type,
      Statistic = round(test@teststat, 3),
      CV_1pct = round(unname(crit_vals[,"1pct"]), 3),
      CV_5pct = round(unname(crit_vals[,"5pct"]), 3),
      CV_10pct = round(unname(crit_vals[,"10pct"]), 3)
    )
    results <- rbind(results, row)
  }
  
  row.names(results) <- NULL
  return(results)
}
