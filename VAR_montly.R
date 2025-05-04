# get DATA
data_montly_d <- readRDS("data/data_montly_d.rds")
data_montly_l <- readRDS("data/data_montly_l.rds")

# libraries ====
library(vars)
library(BMR)
library(tidyverse)

#...............................................................................
# Settings ----
#...............................................................................
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

#...............................................................................
# Estimating models ----
#...............................................................................
for(vars in 1:length(var_model_vars)){
  
  
  if(vars == 1 | vars == 2 | vars == 3){
    print(paste('Estimating VAR:', vars))
    data_var <- data_montly_d[c('date', unlist(var_model_vars[vars]))] %>%
      column_to_rownames("date")
    
    var_model <- VAR(data_var, p = unlist(var_model_lags[vars]), type = 'const')
    forecast_var <- predict(var_model, n.ahead = 12)
    
  } else {
    if(vars == 4 | vars == 5 | vars == 6){
      print(paste('Estimating VAR:', vars))
      data_var <- data_montly_d[c('date', unlist(var_model_vars[vars]))] %>%
        column_to_rownames("date")
      
      vars_names <- names(data_var)
      coef_prior <- sapply(vars_names[vars_names != "date"], function(name) {
        as.numeric(arima(data_montly_d[[name]], order = c(1,0,0))$coef[1])
      })
      
      Y <- data.matrix(na.omit(data_var))
      
      bvar_model <- new(bvarm)
      bvar_model$build(
        Y, intercept = TRUE, lags = unlist(var_model_lags[vars]))
      
      bvar_model$prior(coef_prior, 1, 1, 0.5, 0.5, 100, 1)
      bvar_model$gibbs(10000)
      
      forecast_bvar <- forecast(
        bvar_model, shocks = FALSE, 
        var_names = colnames(Y), n_ahead = 12, back_data = 10,
        plot=TRUE)
      
    } else{
      print(paste('Estimating VAR:', vars))
      data_var <- data_montly_l[c('date', unlist(var_model_vars[vars]))] %>%
        column_to_rownames("date")
      
      apply(
        data_var, 2, function(y) summary(
          ur.df(y, type = "drift", selectlags = "AIC")))
      
      jotest <- ca.jo(data_var, type = "trace", ecdet = "const", K = 2)
      summary(jotest)
      
      vecm_model <- cajorls(jotest, r = 2)
      vec2var_model <- vec2var(jotest, r = 2)
      
      forecast_vec <- predict(vec2var_model, n.ahead = 12)
      
      }
  }
}