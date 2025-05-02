series_codes <- list(
  ipca_general = 433,         # IPCA (general inflation)
  free_prices = 11426,        # Preços Livres
  admin_prices = 11427,       # Preços Administrados
  exchange_rate = 1,          # BRL/USD (PTAX venda)
  selic_nominal = 1178,       # Selic Over - Monthly average
  m1 = 24363                  # Monetary Aggregate M1 - end of period
)

download_series <- function(code, series_name) {
  data <- rbcb::get_series(code, start_date = "2000-01-01") %>%
    rename(data = date, valor = code) %>%
    arrange(data)
  write.csv(data, paste0("../dados/", series_name, ".csv"), row.names = FALSE)
  cat(paste0("✅ Downloaded: ", series_name, "\\n"))
}

for (name in names(series_codes)) {
  download_series(series_codes[[name]], name)
}

#...............................................................................
##### Preços Livres ====
#...............................................................................
p_livre <- rbcb::get_series(11428, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `11428`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
##### Preços Administrados ====
#...............................................................................
p_admin <- rbcb::get_series(4449, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `4449`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
##### Expectations ====
#...............................................................................
source('get_expectations.R')
exp <- exp %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
##### Exchange Rate BRL/US$ ====
#...............................................................................
getSymbols("BRL=X", src = "yahoo", from = "2000-01-01")
cambio <- tibble(
  data = index(`BRL=X`),
  valor = as.numeric(Cl(`BRL=X`))
)

cambio <- cambio %>%
  mutate(mes = floor_date(data, "month")) %>%
  group_by(mes) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = mes) %>% 
  filter(date >= as.Date('2010-01-01')) %>% 
  filter(date < as.Date('2025-01-01'))

rm('BRL=X')

#...............................................................................
##### i (nominal interest rate) ====
#...............................................................................
selic <- ipeadatar::ipeadata("BM12_TJOVER12") %>%
  select(date, valor = value) %>%
  filter(date >= as.Date('2010-01-01')) %>% 
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
##### r (real interest rate) ====
#...............................................................................
r <- tibble(date = selic$date,
            value = selic$valor - exp$values)

#...............................................................................
##### Produção Industrial Brasil ====
#...............................................................................
pi <- rbcb::get_series(21859, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `21859`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
##### Moeda ====
#...............................................................................
m1 <- rbcb::get_series(27791, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `27791`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))


