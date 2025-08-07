# GET DATA ====
data_montly_d <- tibble(date = seq.Date(as.Date("2010-01-01"), floor_date(Sys.Date(), "month"), by = "month"))
data_montly_l <- tibble(date = seq.Date(as.Date("2010-01-01"), floor_date(Sys.Date(), "month"), by = "month"))

data_quarter_d <- tibble(date = seq.Date(as.Date("2010-01-01"), floor_date(Sys.Date(), "month"), by = "quarter"))
data_quarter_l <- tibble(date = seq.Date(as.Date("2010-01-01"), floor_date(Sys.Date(), "month"), by = "quarter"))

data_statistic <- tibble(date = seq.Date(as.Date("2010-01-01"), floor_date(Sys.Date(), "month"), by = "month"))

# maybe it works.. ####
# Função para diferença com NAs para alinhar tamanho
safe_diff <- function(x, lag = 1) {
  c(rep(NA, lag), diff(x, lag = lag))
}

# Função para gerar coluna de diff e fazer left_join
join_diff <- function(base, serie_df, var_name, lag = 1) {
  diff_col <- serie_df %>%
    arrange(date) %>%
    mutate(!!paste0(var_name, "_diff") := safe_diff(.data[[var_name]], lag = lag)) %>%
    dplyr::select(date, !!paste0(var_name))
  base %>% left_join(diff_col, by = "date")
}

#...............................................................................
# COMPONENTES MENSAIS ====
#...............................................................................
#.......... Preços Livres ====
#...............................................................................
p_livre <- g_series(11428, 'p_livre') 
p_livre_acum <- acum_series_pct(p_livre)

data_montly_d <- data_montly_d %>%
  left_join(p_livre, by = 'date') 

data_montly_l <- data_montly_l %>%
  left_join(p_livre_acum, by = 'date') 

# data_montly_d <- add_variable(data_montly_d, p_livre, "p_livre")
# data_montly_l <- add_variable(data_montly_l, p_livre_acum, "p_livre")

#...............................................................................
#.......... Preços Administrados ====
#...............................................................................
p_admin <- g_series(4449, 'p_admin') 
# data_montly_d <- add_variable(data_montly_d, p_admin, "p_admin")

data_montly_d <- data_montly_d %>%
  left_join(p_admin, by = 'date') 

# p_admin_acum <- acum_series(p_admin)
# data_montly_l <- add_variable(data_montly_l, p_admin_acum, "p_admin")

#...............................................................................
#.......... Expectations ====
#...............................................................................
source('get_expectations.R')
exp <- exp

#...............................................................................
#.......... Exchange Rate BRL/US$ ====
#...............................................................................
# getSymbols("BRL=X", src = "yahoo", from = "2000-01-01", auto.assign = TRUE)

# cambio <- data.frame(
#   date = seq.Date(as.Date("2010-01-01"), as.Date("2024-12-01"), by = "month"),
#   valor = as.numeric(Cl(`BRL=X`))
# )

data_xts <- getSymbols("BRL=X", src = "yahoo", auto.assign = FALSE)

cambio <- zoo::fortify.zoo(data_xts)

cambio <- cambio %>%
  mutate(mes = floor_date(Index, "month")) %>%
  group_by(mes) %>%
  summarise(brlx = mean(`BRL=X.Close`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = mes) %>% 
  filter(date >= as.Date('2009-12-01'))

data_montly_l <- data_montly_l %>% 
  left_join(cambio, by='date')

# data_montly_l$brlx <- cambio$brlx[cambio$date >= as.Date('2010-01-01')]
# data_montly_d$brlx <- diff(cambio$brlx)
data_montly_d <- join_diff(data_montly_d, cambio, 'brlx')

#...............................................................................
#.......... i (nominal interest rate) ====
#...............................................................................
selic <- ipeadatar::ipeadata("BM12_TJOVER12") %>%
  dplyr::select(date, valor = value) %>%
  filter(date >= as.Date('2009-12-01')) %>% 
  arrange(date) 

# data_montly_d$selic <- diff(selic$valor[selic$date >= as.Date('2009-12-01')])

data_montly_d <- join_diff(data_montly_d, selic, 'valor') %>% 
  dplyr::rename(selic = valor)

# selic_a <- acum_series(selic)
# data_montly_l$selic <- selic$valor[selic$date >= as.Date('2010-01-01')]
data_montly_l <- data_montly_l %>% left_join(selic, by='date')
data_montly_l <- data_montly_l %>% dplyr::rename(selic = valor)
#...............................................................................
#.......... r (real interest rate) ====
#...............................................................................
r <- tibble(date = selic$date,
            value = selic$valor - exp$valor)

# data_montly_d$r <- r$value[r$date >= as.Date('2010-01-01')]

data_montly_d <- data_montly_d %>% left_join(r, by='date') %>% 
  dplyr::rename(r = value)

#...............................................................................
#.......... Produção Industrial Brasil ====
#...............................................................................
pi <- g_series(21859, 'pi')
pi_ts <- ts(pi$pi, start = c(year(min(pi$date)), month(min(pi$date))), frequency = 12)

#  X13
x13 <- seasonal::seas(pi_ts)
pi_sa <- seasonal::final(x13)

# safe_diff <- function(base, serie, lag = 1) {
#   n_base <- nrow(base)
#   n_diff <- length(diff(serie, lag = lag))
#   n_na <- n_base - n_diff
#   result <- c(rep(NA, n_na), diff(serie, lag = lag))
#   return(result)
# }

pi_sa <- pi %>%
  mutate(pi = as.numeric(pi_sa))

# data_montly_d$pi <- diff(pi$pi)
# data_montly_d$pi <- safe_diff(data_montly_d, pi$pi)

data_montly_d <- data_montly_d %>% join_diff(pi, 'pi')
# data_montly_d$pi_sa <- diff(pi_sa$pi)
data_montly_d <- data_montly_d %>% join_diff(pi_sa, 'pi') %>% dplyr::rename(pi = pi.x, pi_sa = pi.y)
# data_montly_l$pi <- pi$pi[pi$date >= as.Date('2010-01-01')]
data_montly_l <- data_montly_l %>% left_join(pi, by='date')
#...............................................................................
#.......... Moeda ====
#...............................................................................
m1 <- g_series(27841, 'm1')
# data_montly_d$m1 <- diff(m1$m1)

data_montly_d <- data_montly_d %>% join_diff(m1, 'm1')

saveRDS(data_montly_l, file = "data/data_montly_l.rds")
saveRDS(data_montly_d, file = "data/data_montly_d.rds")
#...............................................................................
# COMPONENTES TRIMESTRAIS ====
#...............................................................................
#.......... Preços Livres ====
#...............................................................................
p_livret <- p_livre %>%
  dplyr::filter(date >= as.Date('2010-01-01')) %>% 
  mutate(quarter = floor_date(date, unit = "quarter")) %>%
  group_by(quarter) %>%
  summarise(
    p_livre = sum(p_livre, na.rm = TRUE),
    .groups = "drop"
  )

p_livre_T <- tibble(date = p_livret$quarter,
                    p_livre = p_livret$p_livre)

p_livre_T_acum <- acum_series(p_livre_T)

data_quarter_d$p_livre <- p_livre_T$p_livre
data_quarter_l$p_livre <- p_livre_T_acum$p_livre
#...............................................................................
#.......... Preços Administrados ====
#...............................................................................
p_admint <- p_admin %>%
  dplyr::filter(date >= as.Date('2010-01-01')) %>% 
  mutate(quarter = floor_date(date, unit = "quarter")) %>%
  group_by(quarter) %>%
  summarise(
    p_admin = sum(p_admin, na.rm = TRUE),
    .groups = "drop"
  )

p_admin_T <- tibble(date = p_admint$quarter,
                    p_admin = p_admint$p_admin)

data_quarter_d$p_admin <- p_admin_T$p_admin

#...............................................................................
#.......... cambio ====
#...............................................................................
brlx_T <- to_quarterly(data_montly_d, brlx)
brlx_T_acum <- to_quarterly(data_montly_l, brlx)

data_quarter_d$brlx <- brlx_T$brlx
data_quarter_l$brlx <- brlx_T_acum$brlx

#...............................................................................
#.......... Juros Reais ====
#...............................................................................
r_T <- to_quarterly(data_montly_d, r)

data_quarter_d$r <- r_T$r
#...............................................................................
#.......... Juros Nominais ====
#...............................................................................
selic_T <- to_quarterly(data_montly_d, selic)

data_quarter_d$selic <- selic_T$selic
data_quarter_l$selic <- selic_T$selic
#...............................................................................
#.......... Produção Industrial Brasil ====
#...............................................................................
pi_T <- to_quarterly(data_montly_d, pi_sa)
data_quarter_d$pi_sa <- pi_T$pi_sa

data_quarter_l$pi_sa <- pi_T$pi_sa[pi_T$date > as.Date('2009-12-01')]

#...............................................................................
#.......... Moeda ====
#...............................................................................
m1_T <- to_quarterly_last(data_montly_d, m1)
data_quarter_d$m1 <- m1_T$m1
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
data_quarter_d$embi <- risco_T$valor

saveRDS(data_quarter_l, file = "data/data_quarter_l.rds")
saveRDS(data_quarter_d, file = "data/data_quarter_d.rds")
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
  filter(date >= as.Date("2009-12-01")) %>% 
  filter(date < as.Date("2025-01-01")) 

data_statistic$comercio <- diff(comercio$valor)
#...............................................................................
#.................... Energia Eletrica ====
#...............................................................................
energia_total <- g_series(1406,  'valor') 
energia_industrial <- g_series(1404, 'valor') 
energia_residencial <- g_series(1403, 'valor') 


data_statistic$e1 <- diff(energia_total$valor)
data_statistic$e2 <- diff(energia_industrial$valor)
data_statistic$e3 <- diff(energia_residencial$valor)


#...............................................................................
#.................... PIB ====
#...............................................................................
ibcbr <- g_series(29609, 'ibcbr') 
data_statistic$ibcbr <- diff(ibcbr$ibcbr)
#...............................................................................
#.................... Capacidade instalada ====
#...............................................................................
uci <- ipeadata("CNI12_NUCAPD12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2009-12-01")) %>% 
  filter(date < as.Date("2025-01-01")) 

data_statistic$uci <- diff(uci$valor)

#...............................................................................
#.................... Desemprego ====
#...............................................................................
u <- get_u()
data_statistic$u <- diff(u$u)

#...............................................................................
#.......... EXTERNO ====
#...............................................................................
#....................  VIX ====
#...............................................................................
vix <- getSymbols("^VIX", src = "yahoo", from = "2010-01-01", auto.assign = FALSE)
vix <- zoo::fortify.zoo(vix)

vix<-vix %>%
  mutate(month = floor_date(Index, "month")) %>%
  group_by(month) %>%
  summarise(valor = mean(`VIX.Close`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = month) %>% 
  filter(date < as.Date('2025-01-01')) %>% 
  filter(date >= as.Date("2009-12-01"))

data_statistic$vix <- (vix$valor)

#...............................................................................
#....................  PPI norte-americano ====
#...............................................................................
getSymbols("PPIACO", src = "FRED", from = "2009-12-01")

ppi <- zoo::fortify.zoo(PPIACO)

ppi <-ppi  %>%
  mutate(date = floor_date(Index, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(PPIACO, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$ppi <- diff(ppi$valor)
#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
getSymbols("IQ", src = "FRED", from = "2009-12-01")
epi <- zoo::fortify.zoo(IQ)

epi <- epi %>%
  mutate(date = floor_date(Index, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(IQ, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$epi <- diff(epi$valor)
#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
getSymbols("IR", src = "FRED", from = "2009-12-01")
ipi <- zoo::fortify.zoo(IR)

ipi <- ipi  %>%
  mutate(date = floor_date(Index, "month")) %>%
  group_by(date) %>%
  summarise(valor = mean(IR, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$ipi <- diff(ipi$valor)
#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
quantum_exp <- ipeadata("FUNCEX12_XQT12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2009-12-01")) %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$quantum_x <- diff(quantum_exp$valor)
#...............................................................................
#....................  índice de preços de exportação ====
#...............................................................................
quantum_imp <- ipeadata("FUNCEX12_MDQT12") %>%
  dplyr::select(date = date, valor = value) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2009-12-01")) %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$quantum_m <- diff(quantum_imp$valor)
#...............................................................................
#.......... FINANCEIRO ====
#...............................................................................
#....................  SELIC Real ====
#...............................................................................
selic <- ipeadatar::ipeadata("BM12_TJOVER12") %>%
  dplyr::select(date, valor = value) %>%
  filter(date >= as.Date('2009-12-01')) %>% 
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

# ipca_3m <- get_top5s_monthly_market_expectations("IPCA", start_date = "2009-12-01") %>%
#   filter(typeCalc == "C", reference_date == format(date %m+% months(3), "%m/%Y")) %>%
#   group_by(date) %>%
#   summarise(ipca3m = mean(mean)) %>%
#   mutate(mes = floor_date(date, "month")) %>%
#   group_by(mes) %>%
#   slice_tail(n = 1) %>%  # último valor do mês
#   ungroup() %>%
#   dplyr::select(date = mes, valor = ipca3m) %>% 
#   filter(date < as.Date('2025-01-01'))
  

# ipca_12m <- get_top5s_monthly_market_expectations("IPCA", start_date = "2009-12-01") %>%
#   filter(typeCalc == "C", reference_date == format(date %m+% months(12), "%m/%Y")) %>%
#   group_by(date) %>%
#   summarise(ipca12m = mean(mean)) %>%
#   mutate(mes = floor_date(date, "month")) %>%
#   group_by(mes) %>%
#   slice_tail(n = 1) %>%  # último valor do mês
#   ungroup() %>%
#   dplyr::select(date = mes, valor = ipca12m) %>% 
#   filter(date < as.Date('2025-01-01'))

# IGP-M
igpm_obs <- rbcb::get_series(189, start_date = "2009-12-01")  # série IGP-M
igpm_obs <- igpm_obs %>%
  rename(date = date, igpm = `189`) %>%
  arrange(date) %>%
  mutate(date = floor_date(date, "month")) %>% 
  filter(date < as.Date('2025-01-01'))

# Criar deflatores ex post (shift -3 e -12 meses)
# igpm_3m <- igpm_obs %>% mutate(date = date %m-% months(3))
# igpm_12m <- igpm_obs %>% mutate(date = date %m-% months(12))                                

ipca_obs <- rbcb::get_series(433, start_date = "2009-12-01") %>%
  rename(date = date, ipca = `433`) %>%
  arrange(date) %>%
  mutate(ipca_3  = dplyr::lag(ipca, 3),
         ipca_12 = dplyr::lag(ipca, 12)) %>%
  dplyr::select(date, ipca_3, ipca_12)

data_statistic <- data_statistic %>%
  left_join(ipca_obs, by = "date")

igpm_obs <- rbcb::get_series(189, start_date = "2009-12-01") %>%
  rename(date = date, igpm = `189`) %>%
  arrange(date) %>%
  mutate(igpm_3  = dplyr::lag(igpm, 3),
         igpm_12 = dplyr::lag(igpm, 12)) %>%
  dplyr::select(date, igpm_3, igpm_12)

# Merge com base principal
data_statistic <- data_statistic %>%
  left_join(igpm_obs, by = "date")

# criar series:
# real_igpm_3m <- left_join(selic, igpm_3m, by = "date") %>%
#   mutate(valor = valor - igpm) %>%
#   dplyr::select(date, valor) %>% 
#   filter(date >= as.Date('2009-12-01'))

# real_igpm_12m <- left_join(selic, igpm_12m, by = "date") %>%
#   mutate(valor = valor - igpm) %>%
#   dplyr::select(date, valor) %>% 
#   filter(date >= as.Date('2009-12-01'))

# real_ipca_3m <- tibble(date = selic$date, valor = (selic$valor - ipca_3m$valor))
# real_ipca_12m <- tibble(date = selic$date, valor = (selic$valor - ipca_12m$valor))


# length(diff(selic$valor))
data_statistic$selic <- diff(selic$valor)
# data_statistic$igpm_3 <- diff(real_igpm_3m$valor)
# data_statistic$igpm_12 <- diff(real_igpm_12m$valor)
# data_statistic$ipca_3 <- diff(real_ipca_3m$valor)
# data_statistic$ipca_12 <- diff(real_ipca_12m$valor)
#...............................................................................
#....................  Spreads sobre a SELIC ====
#...............................................................................
taxa_pf <- g_series(20737, 'pf') 
taxa_pj <- g_series(20738, 'pj') 
taxa_pt <- g_series(20736, 'pt') 

spread_pf <- left_join(taxa_pf, selic, by = "date") %>%
  mutate(valor = pf - valor) %>%
  dplyr::select(date, valor) %>% 
  filter(date >= as.Date('2011-12-01'))

spread_pj <- left_join(taxa_pj, selic, by = "date") %>%
  mutate(valor = pj - valor) %>%
  dplyr::select(date, valor) %>% 
  filter(date >= as.Date('2011-12-01'))

spread_total <- left_join(taxa_pt, selic, by = "date") %>%
  mutate(valor = pt - valor) %>%
  dplyr::select(date, valor) %>% 
  filter(date >= as.Date('2011-12-01'))

#...............................................................................
# necessidade de redução de obs.. nao tenho dados disponiveis ====
data_statistic <- data_statistic %>% 
  filter(date >= as.Date('2012-01-01'))
#...............................................................................

spread_pf <- diff(spread_pf$valor)
spread_pj <- diff(spread_pj$valor)
spread_total <- diff(spread_total$valor)

data_statistic$spread_pf <- spread_pf
data_statistic$spread_pj <- spread_pj
data_statistic$spread_total <- spread_total

#...............................................................................
#.......... PREÇOS ====
#...............................................................................
#.................... IGP-DI ====
#...............................................................................
igpdi <- get_series(188, start_date = "2012-01-01") %>%
  rename(date = date, igpdi = `188`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$igpdi <- igpdi$igpdi
#...............................................................................
#.................... IPC-BR ====
#...............................................................................
ipcbr <- get_series(189, start_date = "2012-01-01") %>%
  rename(date = date, igpdi = `189`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$ipcbr <- ipcbr$igpdi
#...............................................................................
#.................... IPC-FIPE ====
#...............................................................................
ipcfipe <- get_series(190, start_date = "2012-01-01") %>%
  rename(date = date, igpdi = `190`) %>%
  mutate(date = floor_date(date, "month")) %>%
  arrange(date) %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$ipcfipe <- ipcfipe$igpdi
#...............................................................................
#.................... Livres e Adm ====
#...............................................................................
data_statistic$p_livre <- data_montly_d$p_livre[data_montly_d$date >= as.Date('2012-01-01')]
data_statistic$p_admin <- data_montly_d$p_admin[data_montly_d$date >= as.Date('2012-01-01')]
#...............................................................................
#.......... MONETARIO  ====
#...............................................................................
#.................... M1, M2, M3, M4... ====
#...............................................................................
m1      <- g_series(27841, "m1") %>% filter(date >= '2011-12-01')
m2      <- g_series(27842, "m2")%>% filter(date >= '2011-12-01')
m3      <- g_series(27813, "m3")%>% filter(date >= '2011-12-01')
m4      <- g_series(27815, "m4")%>% filter(date >= '2011-12-01')
bm      <- g_series(27807, "base_monetaria")%>% filter(date >= '2011-12-01')
pmp     <- g_series(27789, "papel_moeda_publico")%>% filter(date >= '2011-12-01')
depv    <- g_series(27790, "depositos_vista")%>% filter(date >= '2011-12-01')

data_statistic$m1 <- diff(m1$m1)
data_statistic$m2 <- diff(m2$m2)
data_statistic$m3 <- diff(m3$m3)
data_statistic$m4 <- diff(m4$m4)
data_statistic$bm <- diff(bm$base_monetaria)
data_statistic$pmp <- diff(pmp$papel_moeda_publico)
data_statistic$depv <- diff(depv$depositos_vista)
#...............................................................................
#.......... CHOQUES  ====
#...............................................................................
#.................... CBR ====
#...............................................................................
dbc <- getSymbols("DBC", src = "yahoo", from = "2011-12-01", auto.assign = FALSE)

dbc <- zoo::fortify.zoo(dbc)

crb <- dbc %>% 
  mutate(date = floor_date(Index, "month")) %>%
  group_by(date) %>%
  summarise(crb = mean(`DBC.Close`, na.rm = TRUE)) %>%
  filter(date < as.Date('2025-01-01'))

data_statistic$crb <- diff(crb$crb)
#...............................................................................
#.................... GASOLINA ====
#...............................................................................
gasolina <- g_series(1393, 'gas') %>% filter(date >= '2011-12-01')

data_statistic$gas <- diff(gasolina$gas)
#...............................................................................
#.................... IPA-IPC ====
#...............................................................................
ipa    <- g_series(7450, "ipa")
ipc    <- g_series(191, "ipc")
ipa_ipc <- tibble(
  date = ipa$date,
  valor = (ipa$ipa - ipc$ipc)
) %>% filter(date >= '2012-01-01')

data_statistic$ipa_ipc <- ipa_ipc$valor
#...............................................................................
#.................... OLEO ====
#...............................................................................
combustiveis <- g_series(1483, 'oil') %>% filter(date >= '2011-12-01')
oil <- diff(combustiveis$oil)

data_statistic$oil <- oil
#...............................................................................
#.................... PETROLEO ====
#...............................................................................
petroleo <- getSymbols("DCOILWTICO", src = "FRED", from = "2010-01-01", auto.assign = FALSE)

petroleo <- zoo::fortify.zoo(petroleo)

petroleo <- petroleo %>%
  mutate(date = floor_date(Index, "month")) %>%
  group_by(date) %>%
  summarise(petroleo = mean(DCOILWTICO, na.rm = TRUE)) %>% 
  filter(date >= '2011-12-01') %>% 
  filter(date < as.Date('2025-01-01'))

data_statistic$petrol <- diff(petroleo$petroleo)

saveRDS(data_statistic, file = "data/data_statistic.rds")

rm(list = ls())
