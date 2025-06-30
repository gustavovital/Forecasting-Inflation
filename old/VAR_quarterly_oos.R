rm(list = ls())

library(tidyverse)
library(lubridate)
library(vars)
library(BMR)
library(forecast)
library(urca)
library(glue)

# Dados

data_quarter_d <- readRDS("data/data_quarter_d.rds") %>%
  filter(date >= as.Date("2012-01-01"))
data_quarter_l <- readRDS("data/data_quarter_l.rds") %>%
  filter(date >= as.Date("2012-01-01"))

# Modelos
var_model_vars <- list(
  VAR_I     = c("p_livre", "p_admin", "brlx", "r"),
  VAR_II    = c("p_livre", "p_admin", "brlx", "selic", "pi_sa", "m1"),
  VAR_III   = c("p_livre",'p_admin', "r", "embi", "pi_sa"),
  BVAR_I    = c("p_livre", "p_admin", "brlx", "selic"),
  BVAR_II   = c("p_livre", "p_admin", "brlx", "selic", "pi_sa", "m1"),
  BVAR_III  = c("p_livre", "p_admin", "brlx", "r"),
  VECM      = c("p_livre", "selic", "brlx", "pi_sa")  
)

var_model_lags <- list(
  VAR_I     = 2,
  VAR_II    = 1,
  VAR_III   = 1,
  BVAR_I    = 1,
  BVAR_II   = 2,
  BVAR_III  = 1,
  VECM      = 2
)

forecast_starts <- seq(as.Date("2023-01-01"), as.Date("2025-03-01"), by = "quarter")
forecast_df <- tibble()

for (i in seq_along(forecast_starts)) {
  forecast_model_id <- as.character(as.roman(i))
  start_date <- as.Date("2012-01-01")
  end_date <- forecast_starts[i] %m-% months(3)
  train_d <- data_quarter_d %>% filter(date >= start_date & date <= end_date)
  train_l <- data_quarter_l %>% filter(date >= start_date & date <= end_date)
  
  for (vars in seq_along(var_model_vars)) {
    model_name <- names(var_model_vars)[vars]
    message(glue("Estimating model {model_name}, window {forecast_model_id}"))
    
    if (model_name %in% c("VAR_I", "VAR_II", "VAR_III")) {
      data_var <- train_d[c("date", var_model_vars[[vars]])] %>% column_to_rownames("date")
      var_model <- VAR(data_var, p = var_model_lags[[vars]], type = "const")
      forecast_var <- predict(var_model, n.ahead = 4, ci = 0.95)
      
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = 'p_livre',
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
        mean      = forecast_var$fcst[['p_livre']][, "fcst"],
        lower     = forecast_var$fcst[['p_livre']][, "lower"],
        upper     = forecast_var$fcst[['p_livre']][, "upper"]
      )
      forecast_df <- bind_rows(forecast_df, fc_tmp)
      
    } else if (model_name %in% c("BVAR_I", "BVAR_II", "BVAR_III")) {
      data_var <- train_d[c("date", var_model_vars[[vars]])] %>% column_to_rownames("date")
      Y <- data.matrix(na.omit(data_var))
      vars_names <- colnames(Y)
      coef_prior <- sapply(vars_names, function(name) {
        as.numeric(arima(Y[, name], order = c(1, 0, 0))$coef[1])
      })
      
      bvar_model <- new(bvarm)
      bvar_model$build(Y, intercept = TRUE, lags = var_model_lags[[vars]])
      bvar_model$prior(coef_prior, 1, 1, 0.5, 0.5, 100, 1)
      bvar_model$gibbs(10000)
      
      fcst <- BMR:::forecast.Rcpp_bvarm(
        bvar_model, 
        shocks = FALSE, 
        var_names = vars_names, 
        n_ahead = 4
      )
      j <- which(vars_names == "p_livre")
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
        mean      = fcst$plot_vals[1:4, 2, j],
        lower     = fcst$plot_vals[1:4, 1, j],
        upper     = fcst$plot_vals[1:4, 3, j]
      )
      forecast_df <- bind_rows(forecast_df, fc_tmp)
      
    } else if (model_name == "VECM") {
      data_var <- train_l[c("date", var_model_vars[[vars]])] %>% column_to_rownames("date")
      jotest <- ca.jo(data_var, type = "trace", ecdet = "const", K = 2)
      vec2var_model <- vec2var(jotest, r = 2)
      forecast_vec <- predict(vec2var_model, n.ahead = 4, ci = 0.95)
      last_obs <- tail(train_l$p_livre, 1)
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
        mean      = diff(c(last_obs, forecast_vec$fcst[['p_livre']][, "fcst"])),
        lower     = diff(c(last_obs, forecast_vec$fcst[['p_livre']][, "lower"])),
        upper     = diff(c(last_obs, forecast_vec$fcst[['p_livre']][, "upper"]))
      )
      forecast_df <- bind_rows(forecast_df, fc_tmp)
    }
  }
}

forecast_t_acc <- forecast_df %>%
  group_by(forecast_model, model, date) %>%
  arrange(date) %>%
  slice(1:4) %>%  # 4 quarters ahead
  summarise(
    date = min(date),  # origin of forecast
    mean_acc  = (prod(1 + mean  / 100) - 1) * 100,
    lower_acc = (prod(1 + lower / 100) - 1) * 100,
    upper_acc = (prod(1 + upper / 100) - 1) * 100,
    .groups = "drop"
  ) 

# forecast_df <- readRDS("data/forecast_t.rds")
saveRDS(forecast_df, file = "data/forecast_t.rds")
saveRDS(forecast_t_acc, file = "data/forecast_t_acc.rds")
