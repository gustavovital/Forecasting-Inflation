# PREVISÕES HÍBRIDAS DE INFLAÇÃO COM VAR, BVAR E VECM
# Armazenando apenas p_livre e p_admin
rm(list = ls())
# Carregar dados e pacotes
data_montly_d <- readRDS("data/data_montly_d.rds")
data_montly_l <- readRDS("data/data_montly_l.rds")

library(vars)
library(BMR)
library(tidyverse)
library(forecast)
library(urca)
library(glue)

# Definição de modelos e defasagens
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

# Inicializar base de dados para armazenar previsões
forecast_df <- tibble()

# Estimar modelos e armazenar previsões de p_livre e p_admin
for (vars in 1:length(var_model_vars)) {
  
  model_name <- names(var_model_vars)[vars]
  message(glue("Estimating model: {model_name}"))
  
  if (model_name %in% c("VAR_I", "VAR_II", "VAR_III")) {
    
    data_var <- data_montly_d[c("date", var_model_vars[[vars]])] %>%
      column_to_rownames("date")
    
    var_model <- VAR(data_var, p = var_model_lags[[vars]], type = "const")
    forecast_var <- predict(var_model, n.ahead = 12, ci = 0.95)
    
    for (v in names(forecast_var$fcst)) {
      if (v %in% c("p_livre", "p_admin")) {
        df_tmp <- tibble(
          model     = model_name,
          component = v,
          horizon   = 1:12,
          mean      = forecast_var$fcst[[v]][, 1],
          lower     = forecast_var$fcst[[v]][, 2],
          upper     = forecast_var$fcst[[v]][, 3]
        )
        forecast_df <- bind_rows(forecast_df, df_tmp)
      }
    }
    
  } else if (model_name %in% c("BVAR_I", "BVAR_II", "BVAR_III")) {
    
    data_var <- data_montly_d[c("date", var_model_vars[[vars]])] %>%
      column_to_rownames("date")
    
    vars_names <- names(data_var)
    coef_prior <- sapply(vars_names, function(name) {
      as.numeric(arima(data_montly_d[[name]], order = c(1, 0, 0))$coef[1])
    })
    
    Y <- data.matrix(na.omit(data_var))
    
    bvar_model <- new(bvarm)
    bvar_model$build(Y, intercept = TRUE, lags = var_model_lags[[vars]])
    bvar_model$prior(coef_prior, 1, 1, 0.5, 0.5, 100, 1)
    bvar_model$gibbs(10000)
    
    fcst <- BMR:::forecast.Rcpp_bvarm(
      bvar_model, 
      shocks = FALSE, 
      var_names = colnames(Y), 
      n_ahead = 12
    )
    
    ci_fcst <- fcst$plot_vals
    
    for (j in 1:dim(ci_fcst)[3]) {
      var_name <- colnames(Y)[j]
      if (var_name %in% c("p_livre", "p_admin")) {
        df_tmp <- tibble(
          model     = model_name,
          component = var_name,
          horizon   = 1:12,
          mean      = ci_fcst[1:12, 2, j],
          lower     = ci_fcst[1:12, 1, j],
          upper     = ci_fcst[1:12, 3, j]
        )
        forecast_df <- bind_rows(forecast_df, df_tmp)
      }
    }
    
  } else if (model_name == "VECM")  {
    
    data_var <- data_montly_l[c("date", var_model_vars[[vars]])] %>%
      column_to_rownames("date")
    
    jotest <- ca.jo(data_var, type = "trace", ecdet = "const", K = 2)
    vec2var_model <- vec2var(jotest, r = 2)  # modelo em nível
    
    forecast_vec <- predict(vec2var_model, n.ahead = 12, ci = 0.95)
    
        df_tmp <- tibble(
          model     = model_name,
          component = 'p_livre',
          horizon   = 1:12,
          mean      = diff(c(tail(data_montly_l$p_livre,1), forecast_vec$fcst[['p_livre']][, "fcst"])),
          lower     = diff(c(tail(data_montly_l$p_livre,1), forecast_vec$fcst[['p_livre']][, "lower"])),
          upper     = diff(c(tail(data_montly_l$p_livre,1), forecast_vec$fcst[['p_livre']][, "upper"]))
        )
        forecast_df <- bind_rows(forecast_df, df_tmp)
      
    
  }
}


component_I <- forecast_df %>%
  filter(model %in% c('VAR_I', 'VAR_II', 'VAR_III', 'VECM')) %>%
  group_by(component, horizon) %>%
  summarise(
    model = paste("COMPONENT I"),
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  relocate(model)


component_II <- forecast_df %>%
  filter(model %in% c('BVAR_I', 'BVAR_II', 'BVAR_III')) %>%
  group_by(component, horizon) %>%
  summarise(
    model = paste("COMPONENT II"),
    mean  = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  relocate(model)

forecast_df <- bind_rows(forecast_df, component_I, component_II)

saveRDS(forecast_df, file = "data/forecast_montly.rds")
