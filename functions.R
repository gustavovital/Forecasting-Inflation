# functions.R

to_quarterly <- function(df, nome_var) {
  df %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", (quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise({{ nome_var }} := mean(valor, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}

to_quarterly_last <- function(df, nome_var) {
  df %>%
    mutate(quarter_date = as.Date(paste0(year(date), "-", (quarter(date) - 1) * 3 + 1, "-01"))) %>%
    group_by(quarter_date) %>%
    summarise({{ nome_var }} := last(valor)) %>%
    ungroup() %>%
    rename(date = quarter_date)
}


