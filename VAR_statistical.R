rm(list = ls())

library(tidyverse)
library(tsibble)
library(forecast)
library(tidyverse)
library(vars)
library(glue)

# ..............................................................................
##### CLASS I MODELS VAR ########
# ..............................................................................

# Carregar os dados já tratados
data_statistic <- readRDS("data/data_statistic.rds")

# Função para extrair PCs
get_pcs <- function(df, vars) {
  X <- df[, vars] %>% na.omit()
  if (nrow(X) == 0) return(matrix(NA, nrow = nrow(df), ncol = 2))
  
  X_scaled <- scale(X)
  pr <- tryCatch(prcomp(X_scaled, center = TRUE, scale. = TRUE), error = function(e) NULL)
  
  if (is.null(pr)) return(matrix(NA, nrow = nrow(df), ncol = 2))
  
  pcs <- pr$x
  # Garantir que tenha pelo menos duas colunas
  if (ncol(pcs) == 1) pcs <- cbind(pcs, rep(NA, nrow(pcs)))
  if (ncol(pcs) == 0) pcs <- matrix(NA, nrow = nrow(pcs), ncol = 2)
  if (ncol(pcs) > 2) pcs <- pcs[, 1:2]
  
  # Expandir para o tamanho original com NAs onde necessário
  full_pcs <- matrix(NA, nrow = nrow(df), ncol = 2)
  rows <- as.integer(rownames(X))
  full_pcs[rows, ] <- pcs[, 1:2]
  
  return(full_pcs)
}

# 1. SUBGRUPOS por grupo =====================================

subgrupos <- list(
  atividade = list(
    energia = c("e1", "e2", "e3"),
    producao = c("comercio", "uci", "u")
  ),
  externo = list(
    precos_ext = c("ppi", "epi", "ipi"),
    quantum = c("quantum_x", "quantum_m")
  ),
  financeiro = list(
    spread = c("spread_pf", "spread_pj", "spread_total"),
    juros = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12")
  ),
  precos = list(
    ipcs = c("igpdi", "ipcbr", "ipcfipe"),
    inflacao = c("p_admin", "p_livre")
  ),
  monetario = list(
    agregados = c("m1", "m2", "m3", "m4"),
    reservas = c("bm", "pmp", "depv")
  ),
  choques = list(
    energia = c("crb", "gas", "oil"),
    combust = c("petrol", "ipa_ipc")
  )
)

# 2. Estratégias de combinação de PCs =========================
strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
lags_list <- 1:6
# 3. Preparar loop de estimação ===============================
model_list <- list()
model_id <- 1
error_list <- list()

total_models <- length(subgrupos$atividade) *
  length(subgrupos$externo) *
  length(subgrupos$financeiro) *
  length(subgrupos$precos) *
  length(subgrupos$monetario) *
  length(subgrupos$choques) *
  length(1:6) *
  length(strategies)

cat(glue("Estimating {total_models} models...\n"))

# Loop principal ==============================================
for (s in strategies) {
  for (lag in lags_list) {
    for (g1 in subgrupos$atividade) {
      for (g2 in subgrupos$externo) {
        for (g3 in subgrupos$financeiro) {
          for (g4 in subgrupos$precos) {
            for (g5 in subgrupos$monetario) {
              for (g6 in subgrupos$choques) {
                
                # Extrair os componentes principais
                pcs_list <- list(
                  get_pcs(data_statistic, g1),
                  get_pcs(data_statistic, g2),
                  get_pcs(data_statistic, g3),
                  get_pcs(data_statistic, g4),
                  get_pcs(data_statistic, g5),
                  get_pcs(data_statistic, g6)
                )
                
                pcs_df <- pcs_df <- suppressMessages(
                  bind_cols(pcs_list) %>%
                    set_names(paste0("PC", 1:ncol(.))) %>%
                    mutate(p_livre = data_statistic$p_livre) %>%
                    drop_na()
                )
                
                cat(glue::glue("NAs encontrados: {sum(is.na(pcs_df))} (modelo {model_id})\n"))
                # Estratégia de seleção de variáveis
                n_pcs <- ncol(pcs_df) - 1  # Exclui p_livre
                
                X <- switch(s,
                            "PC1"     = pcs_df[, c(paste0("PC", seq(1, n_pcs, 2)), "p_livre")],
                            "PC2"     = pcs_df[, c(paste0("PC", seq(2, n_pcs, 2)), "p_livre")],
                            "PC1_PC2" = pcs_df[, c(paste0("PC", 1:n_pcs), "p_livre")],
                            "COMBO"   = {
                              stopifnot(n_pcs %% 2 == 0)  # Garante que há pares completos
                              pcs_combo <- pcs_df[, 1:n_pcs]
                              pcs_means <- map_dfc(seq(1, n_pcs, by = 2), function(i) {
                                rowMeans(pcs_combo[, c(i, i + 1)], na.rm = TRUE)
                              }) %>%
                                set_names(paste0("PCmean_", seq_len(n_pcs / 2))) %>%
                                mutate(p_livre = pcs_df$p_livre)
                              pcs_means
                            }
                )
                
                # Estimar modelo VAR
                model <- try(VAR(X, p = lag, type = "const"), silent = TRUE)
                
                if (!inherits(model, "try-error")) {
                  model_list[[model_id]] <- list(
                    strategy = s,
                    lags = lag,
                    variables = colnames(X),
                    model = model
                  )
                  cat(glue::glue(
                    "\nModel {model_id}/{total_models} estimated\n",
                    "  Strategy: {s}\n",
                    "  Lags: {lag}\n",
                    "  NAs in data: {sum(is.na(pcs_df))}\n"
                  ))
                  model_id <- model_id + 1
                } else {
                  error_list[[paste0("model_", model_id)]] <- list(
                    strategy = s,
                    lag = lag,
                    error = model
                  )
                  cat(glue::glue(
                    "\nModel {model_id}/{total_models} FAILED\n",
                    "  Strategy: {s}\n",
                    "  Lags: {lag}\n",
                    "  Error: {conditionMessage(attr(model, 'condition'))}\n"
                  ))                }
              }
            }
          }
        }
      }
    }
  }
}

# ..............................................................................
##### CLASS II MODELS VAR ########
# ..............................................................................

# Carregar os dados já tratados
data_statistic <- readRDS("data/data_statistic.rds")

# Função para extrair PCs
get_pcs <- function(df, vars) {
  X <- df[, vars] %>% na.omit()
  if (nrow(X) == 0) return(matrix(NA, nrow = nrow(df), ncol = 2))
  
  X_scaled <- scale(X)
  pr <- tryCatch(prcomp(X_scaled, center = TRUE, scale. = TRUE), error = function(e) NULL)
  
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

# Listar os grupos maiores
grupos <- list(
  atividade   = c("comercio", "e1", "e2", "e3", "uci", "u"),
  externo     = c("vix", "ppi", "epi", "ipi", "quantum_x", "quantum_m"),
  financeiro  = c("selic", "igpm_3", "igpm_12", "ipca_3", "ipca_12", "spread_pf", "spread_pj", "spread_total"),
  precos      = c("p_admin", "igpdi", "ipcbr", "ipcfipe", "p_livre"),
  monetario   = c("m1", "m2", "m3", "m4", "bm", "pmp", "depv"),
  choques     = c("crb", "gas", "ipa_ipc", "oil", "petrol")
)

# Estratégias e defasagens
strategies <- c("PC1", "PC2", "PC1_PC2", "COMBO")
lags_list <- 1:6

# Preparação
model_list_2 <- list()
model_id_2 <- 1
error_list_2 <- list()

# Todas as combinações de 3 grupos distintos
grupo_combos <- combn(names(grupos), 3)

total_models_2 <- length(grupo_combos) * length(strategies) * length(lags_list)
cat(glue("Estimating {total_models_2} models (CLASS II)...\n"))

# Loop principal
for (s in strategies) {
  for (lag in lags_list) {
    for (combo in grupo_combos) {
      
      pcs_list <- lapply(combo, function(grupo) {
        get_pcs(data_statistic, grupos[[grupo]])
      })
      
      pcs_df <- bind_cols(pcs_list) %>%
        set_names(paste0("PC", 1:ncol(.))) %>%
        mutate(p_livre = data_statistic$p_livre) %>%
        drop_na()
      
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
        model_list_2[[model_id_2]] <- list(
          strategy = s,
          lags = lag,
          groups = combo,
          variables = colnames(X),
          model = model
        )
        cat(glue("Model {model_id_2}/1440 estimated: Strategy = {s}, Lags = {lag}, Groups = {paste(combo, collapse = ', ')}\n"))
        model_id_2 <- model_id_2 + 1
      } else {
        error_list_2[[paste0("model_", model_id_2)]] <- list(
          strategy = s,
          lags = lag,
          groups = combo,
          error = model
        )
        cat(glue("Model {model_id_2} failed: Strategy = {s}, Lags = {lag}, Groups = {paste(combo, collapse = ', ')}\n"))
      }
    }
  }
}


# ..............................................................................
# FORECAST CLASS I
# ..............................................................................
# Number of periods ahead
h <- 12

# Initialize forecast list
forecast_list <- list()

# Loop through models
for (i in seq_along(model_list)) {
  model <- model_list[[i]]$model
  lag   <- model_list[[i]]$lags
  strat <- model_list[[i]]$strategy
  
  # Try to forecast
  fc <- try(predict(model, n.ahead = h, ci = 0.95), silent = TRUE)
  
  if (!inherits(fc, "try-error")) {
    if ("p_livre" %in% names(fc$fcst)) {
      fc_mat <- fc$fcst$p_livre
      forecast_list[[i]] <- tibble(
        model_id  = i,
        strategy  = strat,
        lags      = lag,
        horizon   = 1:h,
        mean      = fc_mat[, 1],
        lower     = fc_mat[, 2],
        upper     = fc_mat[, 3]
      )
    }
  }
}

# Combine forecasts into a single dataframe
forecast_class1 <- bind_rows(forecast_list)

# ================================================
# FORECAST CLASS II
# ================================================

h <- 12  # Número de passos à frente
forecast_list_2 <- list()

for (i in seq_along(model_list_2)) {
  model <- model_list_2[[i]]$model
  strat <- model_list_2[[i]]$strategy
  lag   <- model_list_2[[i]]$lags
  grps  <- paste(model_list_2[[i]]$groups, collapse = "_")
  
  fc <- try(predict(model, n.ahead = h, ci = 0.95), silent = TRUE)
  
  if (!inherits(fc, "try-error")) {
    if ("p_livre" %in% names(fc$fcst)) {
      fc_mat <- fc$fcst$p_livre
      forecast_list_2[[i]] <- tibble(
        model_id  = i,
        strategy  = strat,
        lags      = lag,
        groups    = grps,
        horizon   = 1:h,
        mean      = fc_mat[, 1],
        lower     = fc_mat[, 2],
        upper     = fc_mat[, 3]
      )
    }
  }
}

forecast_class2 <- bind_rows(forecast_list_2)

forecast_class1 <- forecast_class1 %>% 
  dplyr::select(model_id, horizon, mean, lower, upper)

forecast_class2 <- forecast_class2 %>% 
  dplyr::select(model_id, horizon, mean, lower, upper)

# Create component ====
component_I <- forecast_class1 %>%
  group_by(horizon) %>%
  summarise(
    model_id     = "COMPONENT I",
    mean      = median(mean, na.rm = TRUE),
    lower     = median(lower, na.rm = TRUE),
    upper     = median(upper, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  relocate(model_id, horizon)

component_II <- forecast_class2 %>%
  group_by(horizon) %>%
  summarise(
    model_id     = "COMPONENT II",
    mean      = median(mean, na.rm = TRUE),
    lower     = median(lower, na.rm = TRUE),
    upper     = median(upper, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  relocate(model_id, horizon)

forecast_class1 <- forecast_class1 %>% dplyr::mutate(model_id = as.character(model_id))
forecast_class2 <- forecast_class2 %>% dplyr::mutate(model_id = as.character(model_id))

forecast_class1 <- bind_rows(forecast_class1, component_I)
forecast_class2 <- bind_rows(forecast_class2, component_II)

var_statistical <- bind_rows(forecast_class1, forecast_class2)

# Salvar se desejar
saveRDS(var_statistical, 'data/forecast_statistical.rds')
# saveRDS(forecast_class2, 'data/forecast_class_II.rds')
