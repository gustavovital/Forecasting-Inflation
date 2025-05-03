#...............................................................................
# COMPONENTES MENSAIS ====
#...............................................................................
#.......... Preços Livres ====
#...............................................................................
p_livre <- rbcb::get_series(11428, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `11428`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... Preços Administrados ====
#...............................................................................
p_admin <- rbcb::get_series(4449, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `4449`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... Expectations ====
#...............................................................................
source('get_expectations.R')
exp <- exp %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... Exchange Rate BRL/US$ ====
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
#.......... i (nominal interest rate) ====
#...............................................................................
selic <- ipeadatar::ipeadata("BM12_TJOVER12") %>%
  dplyr::select(date, valor = value) %>%
  filter(date >= as.Date('2010-01-01')) %>% 
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... r (real interest rate) ====
#...............................................................................
r <- tibble(date = selic$date,
            value = selic$valor - exp$values)

#...............................................................................
#.......... Produção Industrial Brasil ====
#...............................................................................
pi <- rbcb::get_series(21859, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `21859`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... Moeda ====
#...............................................................................
m1 <- rbcb::get_series(27791, start_date = "2010-01-01") %>%
  as_tibble() %>%
  rename(valor = `27791`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
# COMPONENTES TRIMESTRAIS ====
#...............................................................................
#.......... Preços Livres ====
#...............................................................................
p_livre_T <- to_quarterly(p_livre, valor)

#...............................................................................
#.......... Preços Administrados ====
#...............................................................................
p_admin_T <- to_quarterly(p_livre, valor)

#...............................................................................
#.......... Expectations ====
#...............................................................................
exp_T <- to_quarterly(exp, valor)

#...............................................................................
#.......... Exchange Rate BRL/US$ ====
#...............................................................................
cambio_T <- to_quarterly(cambio, valor)

#...............................................................................
#.......... i (nominal interest rate) ====
#...............................................................................
selic_T <- to_quarterly(selic, valor)

#...............................................................................
#.......... Produção Industrial Brasil ====
#...............................................................................
pi_T <- to_quarterly(pi, valor)

#...............................................................................
#.......... Moeda ====
#...............................................................................
m1_T <- to_quarterly_last(m1, valor)

#...............................................................................
#.......... EMBI (premio de risco) ====
#...............................................................................
# Treasury 3M (EUA) – FRED
getSymbols("DTB3", src = "FRED", from = "2010-01-01")
us3m <- tibble(
  date = index(DTB3),
  us3m = as.numeric(DTB3$DTB3)
)

risco <- full_join(selic, us3m, by = "date") %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(
    selic = mean(valor, na.rm = TRUE),
    us3m = mean(us3m, na.rm = TRUE)
  ) %>%
  mutate(
    risco = selic - us3m  # spread simples (% a.a.)
  ) %>%
  filter(!is.na(risco)) %>%
  mutate(valor = risco * 100) %>% 
  dplyr::select(date, valor)

risco_T <- to_quarterly(risco, valor)

#...............................................................................
# COMPONENTES ESTATISTICOS ====
#...............................................................................
#.......... ATIVIDADE ECONOMICA ====
#...............................................................................
#.................... Comercio Varegista ====
#...............................................................................
comercio <- sidrar::get_sidra(
  api = "/t/8880/n1/all/v/7169/p/all/c11046/56733/d/v7169%205"
) %>%
  dplyr::select(data = "Mês (Código)", valor = "Valor") %>%
  mutate(date = ym(data)) %>%
  dplyr::select(date, valor) %>%
  arrange(date) %>%
  filter(date >= as.Date("2010-01-01"))

#...............................................................................
#.................... Energia Eletrica ====
#...............................................................................
energia_total <- ipeadata("CONSUMOTOT") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01"))

energia_industrial <- ipeadata("CONSUMOIND") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01"))

energia_residencial <- ipeadata("CONSUMORES") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01"))

#...............................................................................
#.................... PIB ====
#...............................................................................
pib <- rbcb::get_series(29610, start_date = '2010-01-01')  %>%
  as_tibble() %>%
  rename(valor = `29610`) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.................... Capacidade instalada ====
#...............................................................................
uci <- ipeadata("CNI12_NUCAPD12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01"))

#...............................................................................
#.................... Desemprego ====
#...............................................................................
desemprego <- ipeadata("PNADC12_TDESOC12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2012-03-01"))

#...............................................................................
#.......... EXTERNO ====
#...............................................................................
#....................  VIX ====
#...............................................................................
getSymbols("^VIX", src = "yahoo", from = "2010-01-01")

vix <- tibble(
  date = index(VIX),
  valor = as.numeric(Cl(VIX))
) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = month) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#....................  PPI norte-americano ====
#...............................................................................
getSymbols("PPIACO", src = "FRED", from = "2010-01-01")

ppi <- tibble(
  date = index(PPIACO),
  valor = as.numeric(PPIACO$PPIACO)
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
getSymbols("IQ", src = "FRED", from = "2010-01-01")

epi <- tibble(
  date = index(IQ),
  valor = as.numeric(IQ$IQ)
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  ungroup()

#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
getSymbols("IR", src = "FRED", from = "2010-01-01")

ipi <- tibble(
  date = index(IR),
  valor = as.numeric(IR$IR)
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  ungroup()

#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
quantum_exp <- ipeadata("FUNCEX12_XQT12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01")) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
quantum_imp <- ipeadata("FUNCEX12_MDQT12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2010-01-01")) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... FINANCEIRO ====
#...............................................................................
#....................  SELIC Real ====
#...............................................................................
ipca_3m <- get_top5s_monthly_market_expectations("IPCA", start_date = "2010-01-01") %>%
  filter(typeCalc == "C", reference_date == format(date %m+% months(3), "%m/%Y")) %>%
  group_by(date) %>%
  summarise(ipca3m = mean(mean)) %>%
  mutate(mes = floor_date(date, "month")) %>%
  group_by(mes) %>%
  slice_tail(n = 1) %>%  # último valor do mês
  ungroup() %>%
  dplyr::select(date = mes, valor = ipca3m) %>% 
  filter(date < as.Date('2025-01-01'))
  

ipca_12m <- get_top5s_monthly_market_expectations("IPCA", start_date = "2010-01-01") %>%
  filter(typeCalc == "C", reference_date == format(date %m+% months(12), "%m/%Y")) %>%
  group_by(date) %>%
  summarise(ipca12m = mean(mean)) %>%
  mutate(mes = floor_date(date, "month")) %>%
  group_by(mes) %>%
  slice_tail(n = 1) %>%  # último valor do mês
  ungroup() %>%
  dplyr::select(date = mes, valor = ipca12m) %>% 
  filter(date < as.Date('2025-01-01'))

# IGP-M
igpm_obs <- rbcb::get_series(189, start_date = "2010-01-01")  # série IGP-M
igpm_obs <- igpm_obs %>%
  rename(date = date, igpm = `189`) %>%
  arrange(date) %>%
  mutate(date = floor_date(date, "month")) %>% 
  filter(date < as.Date('2025-01-01'))

# Criar deflatores ex post (shift -3 e -12 meses)
igpm_3m <- igpm_obs %>% mutate(date = date %m-% months(3))
igpm_12m <- igpm_obs %>% mutate(date = date %m-% months(12))                                

# criar series:
real_ipca_3m <- tibble(date = selic$date, valor = (selic$valor - ipca_3m$valor))
real_ipca_12m <- tibble(date = selic$date, valor = (selic$valor - ipca_12m$valor))
real_igpm_3m <- tibble(date = selic$date, valor = (selic$valor - igpm_3m$igpm))
real_igpm_12m <- tibble(date = selic$date, valor = (selic$valor - igpm_12m$igpm))

#...............................................................................
#....................  Spreads sobre a SELIC ====
#...............................................................................
spread_pf <- get_series(20745, start_date = "2010-01-01") %>%
  rename(date = date, spread_pf = `20745`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

spread_pj <- get_series(20746, start_date = "2010-01-01") %>%
  rename(date = date, spread_pj = `20746`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

spread_total <- get_series(20744, start_date = "2010-01-01") %>%
  rename(date = date, spread_total = `20744`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

taxa_bndes <- get_series(25322, start_date = "2010-01-01") %>%
  rename(date = date, bndes = `25322`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

spread_bndes <- taxa_bndes %>%
  left_join(selic, by = "date") %>%
  mutate(valor = bndes - valor) %>%
  dplyr::select(date, valor)

#...............................................................................
#.......... PREÇOS ====
#...............................................................................
#.................... IGP-DI ====
#...............................................................................
igpdi <- get_series(188, start_date = "2010-01-01") %>%
  rename(date = date, igpdi = `188`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.................... IPC-BR ====
#...............................................................................
ipcbr <- get_series(189, start_date = "2010-01-01") %>%
  rename(date = date, igpdi = `189`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.................... IPC-FIPE ====
#...............................................................................
ipcfipe <- get_series(190, start_date = "2010-01-01") %>%
  rename(date = date, igpdi = `190`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.......... MONETARIO  ====
#...............................................................................
#.................... M1, M2, M3, M4... ====
#...............................................................................
m1      <- baixar_serie(27841, "m1")
m2      <- baixar_serie(27842, "m2")
m3      <- baixar_serie(27813, "m3")
m4      <- baixar_serie(27815, "m4")
bm      <- baixar_serie(27807, "base_monetaria")
pmp     <- baixar_serie(27789, "papel_moeda_publico")
depv    <- baixar_serie(27790, "depositos_vista")

#...............................................................................
#.......... CHOQUES  ====
#...............................................................................
#.................... CBR ====
#...............................................................................
getSymbols("CRB", src = "yahoo", from = "2010-01-01")

crb <- tibble(
  date = index(CRB),
  valor = as.numeric(Cl(CRB))
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(crb = mean(valor, na.rm = TRUE)) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.................... GASOLINA ====
#...............................................................................
gasolina <- get_series(4458, start_date = "2010-01-01") %>%
  rename(date = date, gasolina = `4458`) %>%
  mutate(date = floor_date(date, "month")) %>% 
  filter(date < as.Date('2025-01-01'))

#...............................................................................
#.................... IPA-IPC ====
#...............................................................................
ipa    <- baixar_serie(7450, "ipa")
ipc    <- baixar_serie(191, "ipc")
ipa_ipc <- tibble(
  date = ipa$date,
  valor = (ipa$valor - ipc$valor)
)

#...............................................................................
#.................... OLEO ====
#...............................................................................
combustiveis <- baixar_serie(1483, 'oil')

#...............................................................................
#.................... PETROLEO ====
#...............................................................................
getSymbols("DCOILWTICO", src = "FRED", from = "2010-01-01")

petroleo <- tibble(
  date = index(DCOILWTICO),
  valor = as.numeric(DCOILWTICO$DCOILWTICO)
) %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(petroleo = mean(valor, na.rm = TRUE))