rm(list = ls()) 

library(tidyverse)
library(tsibble)
library(forecast)
library(vars)
library(glue)
library(lubridate)

# Carregar dados
COVID_data_statistic <- readRDS("data/COVID_data_statistic.rds")
COVID_data_statistic <- COVID_data_statistic %>% filter(date >= as.Date("2012-01-01"))

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
  df_train <- COVID_data_statistic %>% filter(date <= end)
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
                      mutate(p_livre = df_train$p_livre, D_COVID = df_train$D_COVID) %>%
                      drop_na()
                  )
                  n_pcs <- ncol(pcs_df) - 2
                  X <- switch(s,
                              PC1 = pcs_df[, c(paste0("PC", seq(1, n_pcs, 2)), "p_livre")],
                              PC2 = pcs_df[, c(paste0("PC", seq(2, n_pcs, 2)), "p_livre")],
                              PC1_PC2 = pcs_df[, c(paste0("PC", 1:n_pcs), "p_livre")],
                              COMBO = {
                                pcs_combo <- pcs_df[, 1:n_pcs]
                                pcs_means <- map_dfc(seq(1, n_pcs, by = 2), ~ rowMeans(pcs_combo[, c(.x, .x + 1)], na.rm = TRUE))
                                pcs_means <- set_names(pcs_means, paste0("PCmean_", seq_len(n_pcs / 2)))
                                y_target <- pcs_df$p_livre
                                bind_cols(pcs_means, tibble(p_livre = y_target))
                              })
                  
                  exog <- matrix(pcs_df$D_COVID, ncol = 1)
                  colnames(exog) <- "D_COVID"
                  rownames(exog) <- NULL
                  rownames(X) <- NULL
                  
                  model <- try(VAR(X, p = lag, type = "const", exogen = exog), silent = TRUE)
                  if (!inherits(model, "try-error")) {
                    fc <- try(predict(model, n.ahead = h, ci = 0.95, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
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

COVID_forecast_stat_class1 <- bind_rows(forecast_stat_class1)
COVID_forecast_c1 <- COVID_forecast_stat_class1

COVID_forecast_c1_t <- COVID_forecast_c1 %>%
  # 1. Convert percentages to decimals (if needed)
  mutate(
    mean = mean / 100,  # Only if your data is in raw % (e.g., 50% = 50, not 0.50)
    lower = lower / 100,
    upper = upper / 100
  ) %>%
  # 2. Extract year and quarter, then convert to R Date (first day of quarter)
  mutate(
    year = year(date),
    quarter = quarter(date),
    date = ymd(paste(year, 3 * quarter - 2, "01", sep = "-"))  # e.g., "2023-1-01" → "2023-01-01"
  ) %>%
  # 3. Group by all identifiers + quarter_date
  group_by(forecast_model, strategy, lags, class, date) %>%
  # 4. Calculate quarterly metrics
  summarize(
    quarterly_mean = prod(1 + mean, na.rm = TRUE) - 1,  # Chain-linked growth
    quarterly_lower = prod(1 + lower, na.rm = TRUE) - 1,
    quarterly_upper = prod(1 + upper, na.rm = TRUE) - 1,
    .groups = "drop"
  )



# COVID_forecast_c1 <- readRDS("data/COVID_forecast_c1.rds")
# forecast_c1_t <- readRDS("data/forecast_c1_t.rds")
saveRDS(COVID_forecast_c1, file = "data/COVID_forecast_c1.rds") # all forecasts for differentiated series
saveRDS(COVID_forecast_c1_t, file = "data/COVID_forecast_c1_t.rds") # diff forecast quarterly

# CLASS II

grupo_combos <- combn(names(grupos), 3)

forecast_count <- 1
for (start in forecast_starts) {
  end <- as.Date(start) %m-% months(1)
  df_train <- COVID_data_statistic %>% filter(date <= end)
  model_id <- 1
  forecast_model_id <- as.character(as.roman(forecast_count))
  
  for (s in strategies) {
    for (lag in lags_list) {
      for (combo in grupo_combos) { 
        pcs_list <- lapply(combo, function(gr) get_pcs(df_train, grupos[[gr]]))
        pcs_df <- suppressMessages(
          bind_cols(pcs_list) %>%
            set_names(paste0("PC", 1:ncol(.))) %>%
            mutate(p_livre = df_train$p_livre, D_COVID = df_train$D_COVID) %>%
            drop_na()
        )
        n_pcs <- ncol(pcs_df) - 2
        if (n_pcs < 2) next
        
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
        
        exog <- matrix(pcs_df$D_COVID, ncol = 1)
        colnames(exog) <- "D_COVID"
        rownames(X) <- NULL
        rownames(exog) <- NULL
        
        model <- try(VAR(X, p = lag, type = "const", exogen = exog), silent = TRUE)
        if (!inherits(model, "try-error")) {
          fc <- try(predict(model, n.ahead = h, ci = 0.95, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
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
COVID_forecast_stat_class2 <- bind_rows(forecast_stat_class2)
COVID_forecast_c2 <- COVID_forecast_stat_class2

COVID_forecast_c2_t <- COVID_forecast_c2 %>%
  # 1. Convert percentages to decimals (if needed)
  mutate(
    mean = mean / 100,  # Only if your data is in raw % (e.g., 50% = 50, not 0.50)
    lower = lower / 100,
    upper = upper / 100
  ) %>%
  # 2. Extract year and quarter, then convert to R Date (first day of quarter)
  mutate(
    year = year(date),
    quarter = quarter(date),
    date = ymd(paste(year, 3 * quarter - 2, "01", sep = "-"))  # e.g., "2023-1-01" → "2023-01-01"
  ) %>%
  # 3. Group by all identifiers + quarter_date
  group_by(forecast_model, strategy, lags, class, date) %>%
  # 4. Calculate quarterly metrics
  summarize(
    mean = prod(1 + mean, na.rm = TRUE) - 1,  
    quarterly_lower = prod(1 + lower, na.rm = TRUE) - 1,
    quarterly_upper = prod(1 + upper, na.rm = TRUE) - 1,
    .groups = "drop"
  )

# COVID_forecast_c2_t <- readRDS("data/COVID_forecast_c2_t.rds")
# saveRDS(COVID_forecast_c2, file = "data/COVID_forecast_c2.rds")
# saveRDS(forecast_c2_acc, file = "data/forecast_c2_acc.rds")
saveRDS(COVID_forecast_c2_t, file = "data/COVID_forecast_c2_t.rds")
# saveRDS(forecast_c2_acc_t, file = "data/forecast_c2_acc_t.rds")
