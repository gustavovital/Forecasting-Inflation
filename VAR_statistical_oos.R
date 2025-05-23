rm(list = ls())

library(tidyverse)
library(tsibble)
library(forecast)
library(vars)
library(glue)
library(lubridate)

# Carregar dados
data_statistic <- readRDS("data/data_statistic.rds")
data_statistic <- data_statistic %>% filter(date >= as.Date("2012-01-01"))

# Rolling forecast windows
forecast_starts <- seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "quarter")
h <- 12  # horizonte de previsão

# Estratégias e defasagens
strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
lags_list <- 1:6

# Subgrupos (classe I)
subgrupos <- list(
  atividade = list(energia = c("e1", "e2", "e3"), producao = c("comercio", "uci", "u")),
  externo = list(precos_ext = c("ppi", "epi", "ipi"), quantum = c("quantum_x", "quantum_m")),
  financeiro = list(spread = c("spread_pf", "spread_pj", "spread_total"), juros = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12")),
  precos = list(ipcs = c("igpdi", "ipcbr", "ipcfipe"), inflacao = c("p_admin", "p_livre")),
  monetario = list(agregados = c("m1", "m2", "m3", "m4"), reservas = c("bm", "pmp", "depv")),
  choques = list(energia = c("crb", "gas", "oil"), combust = c("petrol", "ipa_ipc"))
)

# Grupos (classe II)
grupos <- list(
  atividade = c("comercio", "e1", "e2", "e3", "uci", "u"),
  externo = c("vix", "ppi", "epi", "ipi", "quantum_x", "quantum_m"),
  financeiro = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12", "spread_pf", "spread_pj", "spread_total"),
  precos = c("p_admin", "igpdi", "ipcbr", "ipcfipe", "p_livre"),
  monetario = c("m1", "m2", "m3", "m4", "bm", "pmp", "depv"),
  choques = c("crb", "gas", "ipa_ipc", "oil", "petrol")
)

# Função para extrair PCs
get_pcs <- function(df, vars) {
  X <- df[, vars] %>% na.omit()
  if (nrow(X) == 0) return(matrix(NA, nrow = nrow(df), ncol = 2))
  pr <- tryCatch(prcomp(scale(X)), error = function(e) NULL)
  if (is.null(pr)) return(matrix(NA, nrow = nrow(df), ncol = 2))
  pcs <- pr$x
  if (ncol(pcs) == 1) pcs <- cbind(pcs, rep(NA, nrow(pcs)))
  if (ncol(pcs) == 0) pcs <- matrix(NA, nrow = nrow(pcs), ncol = 2)
  if (ncol(pcs) > 2) pcs <- pcs[, 1:2]
  full_pcs <- matrix(NA, nrow = nrow(df), ncol = 2)
  rows <- as.integer(rownames(X))
  full_pcs[rows, ] <- pcs[, 1:2]
  return(full_pcs)
}

forecast_stat_class1 <- list()
forecast_stat_class2 <- list()

# CLASS I
forecast_count <- 1
for (start in forecast_starts) {
  end <- as.Date(start) %m-% months(1)
  df_train <- data_statistic %>% filter(date <= end)
  model_id <- 1
  forecast_model_id <- as.character(as.roman(forecast_count))
  
  for (s in strategies) {
    for (lag in lags_list) {
      for (g1 in subgrupos$atividade) {
        for (g2 in subgrupos$externo) {
          for (g3 in subgrupos$financeiro) {
            for (g4 in subgrupos$precos) {
              for (g5 in subgrupos$monetario) {
                for (g6 in subgrupos$choques) {
                  pcs_list <- list(
                    get_pcs(df_train, g1),
                    get_pcs(df_train, g2),
                    get_pcs(df_train, g3),
                    get_pcs(df_train, g4),
                    get_pcs(df_train, g5),
                    get_pcs(df_train, g6)
                  )
                  pcs_df <- suppressMessages(
                    bind_cols(pcs_list) %>%
                      set_names(paste0("PC", 1:ncol(.))) %>%
                      mutate(p_livre = df_train$p_livre) %>%
                      drop_na()
                  )
                  n_pcs <- ncol(pcs_df) - 1
                  X <- switch(s,
                              PC1 = pcs_df[, c(paste0("PC", seq(1, n_pcs, 2)), "p_livre")],
                              PC2 = pcs_df[, c(paste0("PC", seq(2, n_pcs, 2)), "p_livre")],
                              PC1_PC2 = pcs_df[, c(paste0("PC", 1:n_pcs), "p_livre")],
                              COMBO = {
                                pcs_combo <- pcs_df[, 1:n_pcs]
                                pcs_means <- map_dfc(seq(1, n_pcs, by = 2), ~ rowMeans(pcs_combo[, c(.x, .x + 1)], na.rm = TRUE))
                                set_names(pcs_means, paste0("PCmean_", seq_len(n_pcs / 2))) %>%
                                  mutate(p_livre = pcs_df$p_livre)
                              })
                  
                  model <- try(VAR(X, p = lag, type = "const"), silent = TRUE)
                  if (!inherits(model, "try-error")) {
                    fc <- try(predict(model, n.ahead = h, ci = 0.95), silent = TRUE)
                    if (!inherits(fc, "try-error")) {
                      fc_mat <- fc$fcst$p_livre
                      df_fc <- tibble(
                        forecast_model = forecast_model_id,
                        model_id = model_id,
                        strategy = s,
                        lags = lag,
                        date = seq(as.Date(start), by = "month", length.out = h),
                        mean = fc_mat[, 1],
                        lower = fc_mat[, 2],
                        upper = fc_mat[, 3],
                        class = "CLASS I"
                      )
                      forecast_stat_class1[[length(forecast_stat_class1) + 1]] <- df_fc
                      cat(glue("Model {model_id} ({forecast_model_id}) CLASS I estimated - Strategy: {s}, Lags: {lag}\n"))
                      model_id <- model_id + 1
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  forecast_count <- forecast_count + 1
}

forecast_stat_class1 <- bind_rows(forecast_stat_class1)
forecast_c1 <- forecast_stat_class1

#...............................................................................
# REDO CALCULATIONS WITH IPCA LIVRE
#...............................................................................
forecast_c1_acc <- forecast_c1 %>%
  group_by(forecast_model, date, strategy) %>%
  arrange(date) %>%
  slice(1:12) %>%
  summarise(
    date = min(date),  # This is the origin of the forecast
    mean = (prod(1 + mean / 100) - 1) * 100,
    lower = (prod(1 + lower / 100) - 1) * 100,
    upper = (prod(1 + upper / 100) - 1) * 100,
    .groups = "drop"
  )

forecast_c1_t <- forecast_c1 %>%
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, strategy, date) %>%
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )

forecast_c1_acc_t <- forecast_c1_acc %>%
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, strategy, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# forecast_c1 <- readRDS("data/forecast_c1.rds")
saveRDS(forecast_c1, file = "data/forecast_c1.rds") # all forecasts for differentiated series
saveRDS(forecast_c1_acc, file = "data/forecast_c1_acc.rds") # accumulated forecast for diff series
saveRDS(forecast_c1_t, file = "data/forecast_c1_t.rds") # diff forecast quarterly
saveRDS(forecast_c1_acc_t, file = "data/forecast_c1_acc_t.rds") # accumulated quarterly

# CLASS II (tbd)

grupo_combos <- combn(names(grupos), 3)

forecast_count <- 1
for (start in forecast_starts) {
  end <- as.Date(start) %m-% months(1)
  df_train <- data_statistic %>% filter(date <= end)
  model_id <- 1
  forecast_model_id <- as.character(as.roman(forecast_count))
  
  for (s in strategies) {
    for (lag in lags_list) {
      for (combo in grupo_combos) {
        pcs_list <- lapply(combo, function(gr) get_pcs(df_train, grupos[[gr]]))
        pcs_df <- suppressMessages(
          bind_cols(pcs_list) %>%
            set_names(paste0("PC", 1:ncol(.))) %>%
            mutate(p_livre = df_train$p_livre) %>%
            drop_na()
        )
        n_pcs <- ncol(pcs_df) - 1
        X <- switch(s,
                    PC1 = pcs_df[, c(paste0("PC", seq(1, n_pcs, 2)), "p_livre")],
                    PC2 = pcs_df[, c(paste0("PC", seq(2, n_pcs, 2)), "p_livre")],
                    PC1_PC2 = pcs_df[, c(paste0("PC", 1:n_pcs), "p_livre")],
                    COMBO = {
                      pcs_combo <- pcs_df[, 1:n_pcs]
                      pcs_means <- map_dfc(seq(1, n_pcs, by = 2), function(i) {
                        rowMeans(pcs_combo[, c(i, i + 1)], na.rm = TRUE)
                      }) %>%
                        set_names(paste0("PCmean_", seq_len(n_pcs / 2))) %>%
                        mutate(p_livre = pcs_df$p_livre)
                      pcs_means
                    })
        
        model <- try(VAR(X, p = lag, type = "const"), silent = TRUE)
        if (!inherits(model, "try-error")) {
          fc <- try(predict(model, n.ahead = 12, ci = 0.95), silent = TRUE)
          if (!inherits(fc, "try-error")) {
            fc_mat <- fc$fcst$p_livre
            df_fc <- tibble(
              forecast_model = forecast_model_id,
              model_id = model_id,
              strategy = s,
              lags = lag,
              date = seq(as.Date(start), by = "month", length.out = h),
              mean = fc_mat[, 1],
              lower = fc_mat[, 2],
              upper = fc_mat[, 3],
              class = "CLASS II"
            )
            forecast_stat_class2[[length(forecast_stat_class2) + 1]] <- df_fc
            cat(glue("Model {model_id} ({forecast_model_id}) CLASS II estimated - Strategy: {s}, Lags: {lag}\n"))
            model_id <- model_id + 1
          }
        }
      }
    }
  }
  forecast_count <- forecast_count + 1
}
#####
forecast_stat_class2 <- bind_rows(forecast_stat_class2)
forecast_c2 <- forecast_stat_class2

#...............................................................................
# REDO CALCULATIONS WITH IPCA LIVRE
#...............................................................................
# What to do? get he last value of p_livre and accumulate over it!! Only way 
# to do it. DO it after the wedding. MAYBE monday??? 

# forecast_c2_acc <- forecast_c2 %>%
# forecast_c2 %>%
#   group_by(forecast_model, date, strategy) %>%
#   arrange(date) %>%
#   slice(1:12) %>%
#   summarise(
#     date = min(date),  # This is the origin of the forecast
#     mean = (prod(1 + mean / 100) - 1) * 100,
#     lower = (prod(1 + lower / 100) - 1) * 100,
#     upper = (prod(1 + upper / 100) - 1) * 100,
#     .groups = "drop"
#   )

NEW_forecast_c2_median <- forecast_c2 %>%
  group_by(forecast_model, date) %>%
  arrange(date) %>%
  slice(1:12) %>%
  summarise(
    date = min(date),
    mean = median(mean),
    lower = median(lower),
    upper = median(upper),
    .groups = "drop"
  )


NEW_forecast_c2_t_acc <- NEW_forecast_c2_median %>%
  arrange(forecast_model, date) %>%
  group_by(forecast_model) %>%
  mutate(
    mean  = (cumprod(1 + mean  / 100) - 1) * 100,
    lower = (cumprod(1 + lower / 100) - 1) * 100,
    upper = (cumprod(1 + upper / 100) - 1) * 100
  ) %>%
  ungroup()
  
NEW_forecast_c2_t_acc %>%
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, date) %>%
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )



prod(1 + forecast_c2$mean / 100)*100
  
  
forecast_c2_t <- forecast_c2 %>%
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, strategy, date) %>%
  summarise(
    mean  = mean(mean, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = "drop"
  )

forecast_c2_acc_t <- forecast_c2_acc %>%
  mutate(date = floor_date(date, unit = "quarter")) %>%
  group_by(forecast_model, strategy, date) %>%
  summarise(
    mean = median(mean, na.rm = TRUE),
    lower = median(lower, na.rm = TRUE),
    upper = median(upper, na.rm = TRUE),
    .groups = "drop"
  )

# forecast_c2 <- readRDS("data/forecast_c2.rds")
saveRDS(forecast_c2, file = "data/forecast_c2.rds")
saveRDS(forecast_c2_acc, file = "data/forecast_c2_acc.rds")
saveRDS(forecast_c2_t, file = "data/forecast_c2_t.rds")
saveRDS(forecast_c2_acc_t, file = "data/forecast_c2_acc_t.rds")
