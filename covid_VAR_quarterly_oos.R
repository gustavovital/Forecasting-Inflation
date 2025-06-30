rm(list = ls())

source('requirement.R')

# GET DATA ----
COVID_data_quarter_d <- readRDS("data/COVID_data_quarter_d.rds") %>%
  filter(date >= as.Date("2012-01-01"))
COVID_data_quarter_l <- readRDS("data/COVID_data_quarter_l.rds") %>%
  filter(date >= as.Date("2012-01-01"))

# SETUP MODELS ----
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

# FORECAST ----
for (i in seq_along(forecast_starts)) {
  forecast_model_id <- as.character(as.roman(i))
  start_date <- as.Date("2012-01-01")
  end_date <- forecast_starts[i] %m-% months(3)
  train_d <- COVID_data_quarter_d %>% filter(date >= start_date & date <= end_date)
  train_l <- COVID_data_quarter_l %>% filter(date >= start_date & date <= end_date)
  
  for (vars in seq_along(var_model_vars)) {
    model_name <- names(var_model_vars)[vars]
    message(glue("Estimating model {model_name}, window {forecast_model_id}"))
    
    if (model_name %in% c("VAR_I", "VAR_II", "VAR_III")) {
      data_var <- train_d[c("date", var_model_vars[[vars]], "D_COVID")] %>% column_to_rownames("date")
      Y <- data_var[, var_model_vars[[vars]]]
      X <- data_var[, "D_COVID", drop = FALSE]
      var_model <- VAR(Y, p = var_model_lags[[vars]], type = "const", exogen = X)
      
      exog_fcst <- matrix(0, nrow = 4, ncol = 1)
      colnames(exog_fcst) <- "D_COVID"
      
      
      forecast_95 <- predict(var_model, n.ahead = 4, ci = 0.95, dumvar = exog_fcst)
      forecast_80 <- predict(var_model, n.ahead = 4, ci = 0.80, dumvar = exog_fcst)
      
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = 'p_livre',
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
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
        as.numeric(arima(Y[, name], order = c(1, 0, 0))$coef[1])
      })
      
      bvar_model <- new(bvarm)
      bvar_model$build(Y, X_exog, TRUE, var_model_lags[[vars]])
      bvar_model$prior(coef_prior, 1, 1, 0.5, 0.5, 100, 1)
      bvar_model$gibbs(10000)
      
      X_exog_future <- matrix(0, nrow = 4, ncol = 1)
      colnames(X_exog_future) <- "D_COVID"
      
      fcst <- BMR:::forecast.Rcpp_bvarm(
        bvar_model, 
        shocks = FALSE, 
        var_names = colnames(Y), 
        n_ahead = 4,
        x_future = X_exog_future
      )
      
      j <- which(colnames(Y) == "p_livre")
      
      
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
        mean      = fcst$forecast_mean[1:4, j],
        lower_95  = fcst$plot_vals[1:4, 1, j],  
        upper_95  = fcst$plot_vals[1:4, 3, j],  
        lower_80  = fcst$plot_vals[1:4, 1, j] + 0.2*(fcst$plot_vals[1:4, 3, j] - fcst$plot_vals[1:4, 1, j]),
        upper_80  = fcst$plot_vals[1:4, 3, j] - 0.2*(fcst$plot_vals[1:4, 3, j] - fcst$plot_vals[1:4, 1, j])
      )
      forecast_df <- bind_rows(forecast_df, fc_tmp)
      
    } else if (model_name == "VECM") {
      data_var <- train_l[c("date", var_model_vars[[vars]], "D_COVID")] %>% column_to_rownames("date")
      Y <- data_var[, var_model_vars[[vars]]]
      X <- data_var[, "D_COVID", drop = FALSE]
      
      jotest <- ca.jo(Y, type = "trace", ecdet = "const", K = 2)
      vec2var_model <- vec2var(jotest, r = 2)
      
      
      forecast_95 <- predict(vec2var_model, n.ahead = 4, ci = 0.95, dumvar = matrix(0, nrow = 4, ncol = 1, dimnames = list(NULL, "D_COVID")))
      forecast_80 <- predict(vec2var_model, n.ahead = 4, ci = 0.80, dumvar = matrix(0, nrow = 4, ncol = 1, dimnames = list(NULL, "D_COVID")))
      
      last_obs <- tail(train_l$p_livre, 1)
      fc_tmp <- tibble(
        forecast_model = forecast_model_id,
        model     = model_name,
        component = "p_livre",
        date      = seq(forecast_starts[i], by = "quarter", length.out = 4),
        mean      = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "fcst"])),
        lower_95  = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "lower"])),
        upper_95  = diff(c(last_obs, forecast_95$fcst[['p_livre']][, "upper"])),
        lower_80  = diff(c(last_obs, forecast_80$fcst[['p_livre']][, "lower"])),
        upper_80  = diff(c(last_obs, forecast_80$fcst[['p_livre']][, "upper"]))
      )
      forecast_df <- bind_rows(forecast_df, fc_tmp)
    }
  }
}

COVID_forecast_df <- forecast_df

saveRDS(COVID_forecast_df, file = "data/COVID_forecast_df.rds")
