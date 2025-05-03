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

baixar_serie <- function(codigo, nome) {
  get_series(codigo, start_date = "2010-01-01") %>%
    rename(date = date, valor := !!sym(as.character(codigo))) %>%
    mutate(date = floor_date(date, "month")) %>%
    arrange(date) %>% 
    filter(date < as.Date('2025-01-01'))
}
