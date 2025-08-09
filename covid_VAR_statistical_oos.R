rm(list = ls()) 

source('requirement.R')

common_horizon <- read_rds('data/common_horizon.rds')
start_forecast <- common_horizon$end %m-% years(1)
end_forecast <- common_horizon$end %m+% months(3)

# GET DATA ----
COVID_data_statistic <- readRDS("data/COVID_data_statistic.rds")
COVID_data_statistic <- COVID_data_statistic %>% filter(date >= common_horizon$start & date <= common_horizon$end)

# SETUP MODELS ----
forecast_starts <- seq(as.Date(start_forecast), as.Date(end_forecast), by = "q")
h <- 12  

strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
lags_list <- 1:6

# CLASS I ----
subgrupos <- list(
  atividade = list(energia = c("e1", "e2", "e3"), producao = c("comercio", "uci", "u")),
  externo = list(precos_ext = c("ppi", "epi", "ipi"), quantum = c("quantum_x", "quantum_m")),
  financeiro = list(spread = c("spread_pf", "spread_pj", "spread_total"), juros = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12")),
  precos = list(ipcs = c("igpdi", "ipcbr", "ipcfipe"), inflacao = c("p_admin", "p_livre")),
  monetario = list(agregados = c("m1", "m2", "m3", "m4"), reservas = c("bm", "pmp", "depv")),
  choques = list(energia = c("crb", "gas", "oil"), combust = c("petrol", "ipa_ipc"))
)

# CLASS II
grupos <- list(
  atividade = c("comercio", "e1", "e2", "e3", "uci", "u"),
  externo = c("vix", "ppi", "epi", "ipi", "quantum_x", "quantum_m"),
  financeiro = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12", "spread_pf", "spread_pj", "spread_total"),
  precos = c("p_admin", "igpdi", "ipcbr", "ipcfipe", "p_livre"),
  monetario = c("m1", "m2", "m3", "m4", "bm", "pmp", "depv"),
  choques = c("crb", "gas", "ipa_ipc", "oil", "petrol")
)

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

# FORECAST - CLASS I ----
forecast_count <- 1
for (start in forecast_starts) {
  end <- start_forecast
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
                    
                    fc_95 <- try(stats::predict(model, n.ahead = h, ci = 0.95, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
                    fc_80 <- try(stats::predict(model, n.ahead = h, ci = 0.80, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
                    
                    if (!inherits(fc_95, "try-error") && !inherits(fc_80, "try-error")) {
                      fc_mat_95 <- fc_95$fcst$p_livre
                      fc_mat_80 <- fc_80$fcst$p_livre
                      
                      df_fc <- tibble(
                        forecast_model = forecast_model_id,
                        model_id = model_id,
                        strategy = s,
                        lags = lag,
                        date = seq(as.Date(start), by = "month", length.out = h),
                        mean = fc_mat_95[, 1],
                        lower_95 = fc_mat_95[, 2],
                        upper_95 = fc_mat_95[, 3],
                        lower_80 = fc_mat_80[, 2],
                        upper_80 = fc_mat_80[, 3],
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
  mutate(
    mean = mean / 100,
    lower_95 = lower_95 / 100,
    upper_95 = upper_95 / 100,
    lower_80 = lower_80 / 100,
    upper_80 = upper_80 / 100
  ) %>%
  mutate(
    year = year(date),
    quarter = quarter(date),
    date = ymd(paste(year, 3 * quarter - 2, "01", sep = "-"))
  ) %>%
  group_by(forecast_model, strategy, lags, class, date) %>%
  summarize(
    quarterly_mean = prod(1 + mean, na.rm = TRUE) - 1,
    quarterly_lower_95 = prod(1 + lower_95, na.rm = TRUE) - 1,
    quarterly_upper_95 = prod(1 + upper_95, na.rm = TRUE) - 1,
    quarterly_lower_80 = prod(1 + lower_80, na.rm = TRUE) - 1,
    quarterly_upper_80 = prod(1 + upper_80, na.rm = TRUE) - 1,
    .groups = "drop"
  )

saveRDS(COVID_forecast_c1, file = "data/COVID_forecast_c1.rds")
saveRDS(COVID_forecast_c1_t, file = "data/COVID_forecast_c1_t.rds")

grupo_combos <- combn(names(grupos), 3)
forecast_count <- 1

# FORECAST - CLASS II
for (start in forecast_starts) {
  end <- start_forecast
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
          
          fc_95 <- try(stats::predict(model, n.ahead = h, ci = 0.95, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
          fc_80 <- try(stats::predict(model, n.ahead = h, ci = 0.80, dumvar = matrix(0, nrow = h, ncol = 1, dimnames = list(NULL, "D_COVID"))), silent = TRUE)
          
          if (!inherits(fc_95, "try-error") && !inherits(fc_80, "try-error")) {
            fc_mat_95 <- fc_95$fcst$p_livre
            fc_mat_80 <- fc_80$fcst$p_livre
            
            df_fc <- tibble(
              forecast_model = forecast_model_id,
              model_id = model_id,
              strategy = s,
              lags = lag,
              date = seq(as.Date(start), by = "month", length.out = h),
              mean = fc_mat_95[, 1],
              lower_95 = fc_mat_95[, 2],
              upper_95 = fc_mat_95[, 3],
              lower_80 = fc_mat_80[, 2],
              upper_80 = fc_mat_80[, 3],
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
  mutate(
    mean = mean / 100,
    lower_95 = lower_95 / 100,
    upper_95 = upper_95 / 100,
    lower_80 = lower_80 / 100,
    upper_80 = upper_80 / 100
  ) %>%
  mutate(
    year = year(date),
    quarter = quarter(date),
    date = ymd(paste(year, 3 * quarter - 2, "01", sep = "-"))
  ) %>%
  group_by(forecast_model, strategy, lags, class, date) %>%
  summarize(
    quarterly_mean = prod(1 + mean, na.rm = TRUE) - 1,
    quarterly_lower_95 = prod(1 + lower_95, na.rm = TRUE) - 1,
    quarterly_upper_95 = prod(1 + upper_95, na.rm = TRUE) - 1,
    quarterly_lower_80 = prod(1 + lower_80, na.rm = TRUE) - 1,
    quarterly_upper_80 = prod(1 + upper_80, na.rm = TRUE) - 1,
    .groups = "drop"
  )

saveRDS(COVID_forecast_c2, file = "data/COVID_forecast_c2.rds")
saveRDS(COVID_forecast_c2_t, file = "data/COVID_forecast_c2_t.rds")