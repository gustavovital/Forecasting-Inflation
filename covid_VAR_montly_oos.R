rm(list = ls())

source('requirement.R')

common_horizon <- read_rds('data/common_horizon.rds')
start_forecast <- common_horizon$end %m-% years(1)
end_forecast <- common_horizon$end %m+% months(3)

# GET DATA ----
COVID_data_montly_d <- readRDS("data/COVID_data_montly_d.rds")
COVID_data_montly_l <- readRDS("data/COVID_data_montly_l.rds")

COVID_data_montly_d <- COVID_data_montly_d %>% filter(date >= common_horizon$start & date <= common_horizon$end)
COVID_data_montly_l <- COVID_data_montly_l %>% filter(date >= common_horizon$start & date <= common_horizon$end)

# SETUP MODELS ----
var_model_vars <- list(
  VAR_I     = c("p_livre", "p_admin", "brlx", "r"),
  VAR_II    = c("p_livre", "p_admin", "brlx", "selic", "pi_sa", "m1"),
  VAR_III   = c("p_livre", "selic", "brlx", "pi"),
  BVAR_I    = c("p_livre", "p_admin", "brlx", "selic", "pi", "m1"),
  BVAR_II   = c("p_livre", "p_admin", "brlx", "selic", "pi_sa", "m1"),
  BVAR_III  = c("p_livre", "p_admin", "brlx", "r"),
  VECM      = c("p_livre", "selic", "brlx", "pi")  
)

var_model_lags <- list(
  VAR_I     = 2,
  VAR_II    = 6,
  VAR_III   = 1,
  BVAR_I    = 6,
  BVAR_II   = 6,
  BVAR_III  = 2,
  VECM      = 1
)

forecast_starts <- seq(as.Date(start_forecast), as.Date(end_forecast), by = "q")
forecast_df <- tibble()

# FORECAST ----
for (i in seq_along(forecast_starts)) {
  
  forecast_model_id <- as.character(as.roman(i))
  # start_date <- common_horizon$start
  start_date <- common_horizon$start
  end_date <- start_forecast
  train_d <- COVID_data_montly_d %>% filter(date >= start_date & date <= end_date)
  train_l <- COVID_data_montly_l %>% filter(date >= start_date & date <= end_date)
  
  for (vars in 1:length(var_model_vars)) {
    model_name <- names(var_model_vars)[vars]
    message(glue("Estimating model {model_name}, window {forecast_model_id}"))
    
    if (model_name %in% c("VAR_I", "VAR_II", "VAR_III")) {
      
      data_var <- train_d[c("date", var_model_vars[[vars]], "D_COVID")] %>% column_to_rownames("date")
      Y <- data_var[, var_model_vars[[vars]]]
      X <- data_var[, "D_COVID", drop = FALSE]
      var_model <- VAR(Y, p = var_model_lags[[vars]], type = "const", exogen = X)
      exog_fcst <- matrix(0, nrow = 12, ncol = 1)
      colnames(exog_fcst) <- "D_COVID"
      forecast_var <- stats::predict(var_model, n.ahead = 12, ci = 0.95, dumvar = exog_fcst)
      
      forecast_95 <- stats::predict(var_model, n.ahead = 12, ci = 0.95, dumvar = exog_fcst)
      forecast_80 <- stats::predict(var_model, n.ahead = 12, ci = 0.80, dumvar = exog_fcst)
      
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "month", length.out = 12),
        mean      = forecast_95$fcst[['p_livre']][, "fcst"],
        lower_95  = forecast_95$fcst[['p_livre']][, "lower"],
        upper_95  = forecast_95$fcst[['p_livre']][, "upper"],
        lower_80  = forecast_80$fcst[['p_livre']][, "lower"],
        upper_80  = forecast_80$fcst[['p_livre']][, "upper"]
      )
      
      forecast_df <- bind_rows(forecast_df, fc_tmp)
      
    } else if (model_name %in% c("BVAR_I", "BVAR_II", "BVAR_III")) {
      
      data_var <- train_d[c("date", var_model_vars[[vars]], "D_COVID")] %>% column_to_rownames("date")
      Y <- data.matrix(data_var[, var_model_vars[[vars]]])
      X_exog <- data.matrix(data_var[, "D_COVID", drop = FALSE])
      
      common_rows <- intersect(rownames(Y), rownames(X_exog))
      Y <- Y[common_rows, , drop = FALSE]
      X_exog <- X_exog[common_rows, , drop = FALSE]
      
      coef_prior <- sapply(colnames(Y), function(name) {
        as.numeric(arima(Y[, name], order = c(1, 0, 0), method = 'ML')$coef[1])
      })
      
      bvar_model <- new(bvarm)
      bvar_model$build(Y, X_exog, TRUE, var_model_lags[[vars]])
      bvar_model$prior(coef_prior, 1, 1, 0.5, 0.5, 100, 1)
      bvar_model$gibbs(10000)
      
      X_exog_future <- matrix(0, nrow = 12, ncol = 1)
      colnames(X_exog_future) <- "D_COVID"
      
      fcst <- BMR:::forecast.Rcpp_bvarm(
        bvar_model, 
        shocks = FALSE, 
        var_names = colnames(Y), 
        n_ahead = 12,
        x_future = X_exog_future
      )
      
      j <- which(colnames(Y) == "p_livre")
      
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "month", length.out = 12),
        mean      = fcst$forecast_mean[1:12, j],
        lower_95  = fcst$plot_vals[1:12, 1, j],  
        upper_95  = fcst$plot_vals[1:12, 3, j],  
        lower_80  = fcst$plot_vals[1:12, 1, j] + 0.2*(fcst$plot_vals[1:12, 3, j] - fcst$plot_vals[1:12, 1, j]),
        upper_80  = fcst$plot_vals[1:12, 3, j] - 0.2*(fcst$plot_vals[1:12, 3, j] - fcst$plot_vals[1:12, 1, j])
      )
              
      forecast_df <- bind_rows(forecast_df, fc_tmp)
      
    } else if (model_name == "VECM") {
      
      data_var <- train_l[c("date", var_model_vars[[vars]], "D_COVID")] %>%
        column_to_rownames("date")
      
      Y <- data_var[, var_model_vars[[vars]]]
      
      X_exog <- data_var[, "D_COVID", drop = FALSE]  
      X_exog <- as.matrix(X_exog)                   
      rownames(X_exog) <- rownames(Y)               
      
      
      stopifnot(identical(rownames(Y), rownames(X_exog)))
      
      # colnames(X_exog) <- "D_COVID"
      
      jotest <- ca.jo(Y, type = "trace", ecdet = "const", K = 2, dumvar = X_exog)
      
      
      vec2var_model <- vec2var(jotest, r = 2)
      
      
      X_future <- matrix(0, nrow = 12, ncol = 1)
      colnames(X_future) <- "D_COVID"
      
      forecast_vec <- stats::predict(vec2var_model, n.ahead = 12, ci = 0.95, dumvar = X_future)
      
      forecast_95 <- stats::predict(vec2var_model, n.ahead = 12, ci = 0.95, dumvar = X_future)
      forecast_80 <- stats::predict(vec2var_model, n.ahead = 12, ci = 0.80, dumvar = X_future)
      
      last_obs <- tail(train_l$p_livre, 1)
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "month", length.out = 12),
        mean      = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "fcst"])),
        lower_95  = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "lower"])),
        upper_95  = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "upper"])),
        lower_80  = diff(c(last_obs, forecast_80$fcst[['p_livre']][, "lower"])),
        upper_80  = diff(c(last_obs, forecast_80$fcst[['p_livre']][, "upper"]))
      )
    }
  }
}

COVID_forecast_m <- forecast_df

COVID_forecast_m_t <- COVID_forecast_m %>% 
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, model, date) %>%
  summarise(
    mean = (prod(1 + mean / 100) - 1) * 100,
    lower = (prod(1 + lower_95 / 100) - 1) * 100,
    upper = (prod(1 + upper_95 / 100) - 1) * 100,
    .groups = "drop"
  )

COVID_forecast_m_t <- COVID_forecast_m %>% 
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, model, date) %>%
  summarise(
    mean = (prod(1 + mean / 100) - 1) * 100,
    lower_95 = (prod(1 + lower_95 / 100) - 1) * 100,
    upper_95 = (prod(1 + upper_95 / 100) - 1) * 100,
    lower_80 = (prod(1 + lower_80 / 100) - 1) * 100,
    upper_80 = (prod(1 + upper_80 / 100) - 1) * 100,
    .groups = "drop"
  )

saveRDS(COVID_forecast_m, file = "data/COVID_forecast_m.rds")
saveRDS(COVID_forecast_m_t, file = "data/COVID_forecast_m_t.rds")
