# functions.R
source('get_u.R')

to_quarterly <- function(df, nome_var) {
  nome_var <- rlang::enquo(nome_var)
  
  df %>%
    mutate(quarter_date = as.Date(paste0(lubridate::year(date), "-", (lubridate::quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(!!rlang::as_name(nome_var) := mean(!!nome_var, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}

to_quarterly_last <- function(df, nome_var) {
  nome_var <- rlang::enquo(nome_var)
  
  df %>%
    mutate(quarter_date = as.Date(paste0(lubridate::year(date), "-", (lubridate::quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise(!!rlang::as_name(nome_var) := dplyr::last(!!nome_var)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}

g_series <- function(codigo, nome) {
  get_series(codigo, start_date = "2009-12-01") %>%
    rename(date = date, !!nome := !!sym(as.character(codigo))) %>%
    mutate(date = floor_date(date, "month")) %>%
    arrange(date) %>% 
    filter(date < as.Date('2025-01-01'))
}

acum_series <- function(data) {
  var_name <- names(data)[2]
  data %>%
    mutate(
      !!sym(var_name) := cumsum(.data[[var_name]])
    ) %>%
    dplyr::select(date, !!sym(var_name))
}

add_variable <- function(df, var_df, var_name) {
  var_df_renamed <- var_df %>%
    dplyr::select(date, !!sym(var_name)) %>%
    rename(!!var_name := !!sym(var_name))
  
  full_join(df, var_df_renamed, by = "date") %>%
    arrange(date) %>% 
    filter(date >= as.Date('2010-01-01') & date < as.Date('2025-01-01'))
    
}
