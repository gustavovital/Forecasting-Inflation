get_u <- function(){
  # Interpolate u to extend the series
  desemprego <- ipeadata("PNADC12_TDESOC12") %>%
    dplyr::select(date = date, valor = value) %>%
    mutate(date = as.Date(date)) 
  
  u <- ipeadata("PAN12_TD12") %>%
    dplyr::select(date = date, u = value) %>%
    mutate(date = as.Date(date)) 
  
  u_predict <- u$u[u$date < as.Date('2012-03-01')]
  
  df_comum <- inner_join(desemprego, u, by = "date")
  
  fit <- lm(valor ~ u, data = df_comum)
  
  new_data <- data.frame(u = u_predict)
  
  predicoes <- c(stats::predict(fit, newdata = new_data), desemprego$valor)
  
  u <- tibble(date = seq(as.Date('2002-03-01'), max(desemprego$date), by = 'm'),
              u = predicoes) %>% 
    filter(date >= as.Date("2009-12-01"))
  
  return(u)
}
