# ..............................................................................
# VAR ESTATÍSTICOS - PREVISAO DIRETA DURANTE O LOOP (NÃO ARMAZENAR MODELOS)
# ..............................................................................

rm(list = ls())

library(tidyverse)
library(tsibble)
library(forecast)
library(vars)
library(glue)

# Carregar dados tratados
data_statistic <- readRDS("data/data_statistic.rds")

# Função para extrair componentes principais
get_pcs <- function(df, vars) {
  X <- df[, vars] %>% na.omit()
  if (nrow(X) == 0) return(matrix(NA, nrow = nrow(df), ncol = 2))
  
  X_scaled <- scale(X)
  pr <- tryCatch(prcomp(X_scaled), error = function(e) NULL)
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

# Subgrupos da Classe I
subgrupos <- list(
  atividade = list(energia = c("e1", "e2", "e3"), producao = c("comercio", "uci", "u")),
  externo = list(precos_ext = c("ppi", "epi", "ipi"), quantum = c("quantum_x", "quantum_m")),
  financeiro = list(spread = c("spread_pf", "spread_pj", "spread_total"), juros = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12")),
  precos = list(ipcs = c("igpdi", "ipcbr", "ipcfipe"), inflacao = c("p_admin", "p_livre")),
  monetario = list(agregados = c("m1", "m2", "m3", "m4"), reservas = c("bm", "pmp", "depv")),
  choques = list(energia = c("crb", "gas", "oil"), combust = c("petrol", "ipa_ipc"))
)

# Grupos para Classe II
grupos <- list(
  atividade = c("comercio", "e1", "e2", "e3", "uci", "u"),
  externo = c("vix", "ppi", "epi", "ipi", "quantum_x", "quantum_m"),
  financeiro = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12", "spread_pf", "spread_pj", "spread_total"),
  precos = c("p_admin", "igpdi", "ipcbr", "ipcfipe", "p_livre"),
  monetario = c("m1", "m2", "m3", "m4", "bm", "pmp", "depv"),
  choques = c("crb", "gas", "ipa_ipc", "oil", "petrol")
)

strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
lags_list <- 1:6
h <- 12

forecast_class_I <- list()
forecast_class_II <- list()

# Classe I (1536 modelos)
model_id <- 1
for (s in strategies) {
  for (lag in lags_list) {
    for (g1 in subgrupos$atividade) {
      for (g2 in subgrupos$externo) {
        for (g3 in subgrupos$financeiro) {
          for (g4 in subgrupos$precos) {
            for (g5 in subgrupos$monetario) {
              for (g6 in subgrupos$choques) {
                
                pcs_list <- list(
                  get_pcs(data_statistic, g1),
                  get_pcs(data_statistic, g2),
                  get_pcs(data_statistic, g3),
                  get_pcs(data_statistic, g4),
                  get_pcs(data_statistic, g5),
                  get_pcs(data_statistic, g6)
                )
                
                pcs_df <- suppressMessages(
                  bind_cols(pcs_list) %>%
                    set_names(paste0("PC", 1:ncol(.))) %>%
                    mutate(p_livre = data_statistic$p_livre) %>%
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
                            }
                )
                
                model <- try(VAR(X, p = lag, type = "const"), silent = TRUE)
                if (!inherits(model, "try-error") && "p_livre" %in% colnames(X)) {
                  fc <- try(predict(model, n.ahead = h, ci = 0.95), silent = TRUE)
                  if (!inherits(fc, "try-error")) {
                    fc_mat <- fc$fcst$p_livre
                    forecast_class_I[[model_id]] <- tibble(
                      model_id = model_id,
                      strategy = s,
                      lags = lag,
                      horizon = 1:h,
                      mean = fc_mat[, 1],
                      lower = fc_mat[, 2],
                      upper = fc_mat[, 3]
                    )
                    cat(glue("Model {model_id}/1536 estimated\n"))
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

# Classe II (1440 modelos)
# Classe II - VAR Estatístico
model_id <- 1

# Estratégias e defasagens
# strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
# lags_list <- 1:6

# Subgrupos principais
# grupos <- list(
#   atividade   = c("comercio", "e1", "e2", "e3", "uci", "u"),
#   externo     = c("vix", "ppi", "epi", "ipi", "quantum_x", "quantum_m"),
#   financeiro  = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12", "spread_pf", "spread_pj", "spread_total"),
#   precos      = c("p_admin", "igpdi", "ipcbr", "ipcfipe", "p_livre"),
#   monetario   = c("m1", "m2", "m3", "m4", "bm", "pmp", "depv"),
#   choques     = c("crb", "gas", "ipa_ipc", "oil", "petrol")
# )

grupo_combos <- combn(names(grupos), 3)
# length(grupo_combos) * length(strategies) * length(lags_list)
# cat(glue("Estimating {total_models_2} models (CLASS II)...\n"))

for (s in strategies) {
  for (lag in lags_list) {
    for (combo in grupo_combos) {
      
      pcs_list <- lapply(combo, function(grupo) {
        get_pcs(data_statistic, grupos[[grupo]])
      })
      
      pcs_df <- suppressMessages(
        bind_cols(pcs_list) %>%
          set_names(paste0("PC", 1:ncol(.))) %>%
          mutate(p_livre = data_statistic$p_livre) %>%
          drop_na()
      )
      
      n_pcs <- ncol(pcs_df) - 1
      
      X <- switch(s,
                  "PC1"     = pcs_df[, c(paste0("PC", seq(1, n_pcs, 2)), "p_livre")],
                  "PC2"     = pcs_df[, c(paste0("PC", seq(2, n_pcs, 2)), "p_livre")],
                  "PC1_PC2" = pcs_df[, c(paste0("PC", 1:n_pcs), "p_livre")],
                  "COMBO"   = {
                    stopifnot(n_pcs %% 2 == 0)
                    pcs_combo <- pcs_df[, 1:n_pcs]
                    pcs_means <- map_dfc(seq(1, n_pcs, by = 2), function(i) {
                      rowMeans(pcs_combo[, c(i, i + 1)], na.rm = TRUE)
                    }) %>%
                      set_names(paste0("PCmean_", seq_len(n_pcs / 2))) %>%
                      mutate(p_livre = pcs_df$p_livre)
                    pcs_means
                  }
      )
      
      model <- try(VAR(X, p = lag, type = "const"), silent = TRUE)
      
      if (!inherits(model, "try-error")) {
        fc <- try(predict(model, n.ahead = 12, ci = 0.95), silent = TRUE)
        if (!inherits(fc, "try-error") && "p_livre" %in% names(fc$fcst)) {
          fc_tmp <- tibble(
            model_id = model_id,
            strategy = s,
            lags     = lag,
            groups   = paste(combo, collapse = "_"),
            horizon  = 1:12,
            mean     = fc$fcst$p_livre[, "fcst"],
            lower    = fc$fcst$p_livre[, "lower"],
            upper    = fc$fcst$p_livre[, "upper"]
          )
          forecast_class_II[[length(forecast_class_II) + 1]] <- fc_tmp
          cat(glue("Model {model_id}/1440 estimated\n"))
        }
      } else {
        cat(glue("Model {model_id}/1440 failed\n"))
      }
      
      model_id <- model_id + 1
    }
  }
}

# Salvar
# forecast_class_II <- bind_rows(forecast_class_II)
# saveRDS(forecast_class_II, file = "data/forecast_class_II.rds")