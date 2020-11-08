pkgs <- c("tidyverse", "lubridate", "readxl", "highcharter", "knitr", "tidyquant", "timetk", "tibbletime", "quantmod", "PerformanceAnalytics", "scales", "matrixStats")

sapply(pkgs, function(pkg) if(!require(pkg, character.only = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org/"))

sapply(pkgs, library, character.only = TRUE)

jse <- read_xlsx("prices.xlsx", guess_max = Inf)


## ----returns------------------------------------------------------------------
jse <- tk_xts(jse)
jse_returns <- Return.calculate(jse)


## ----mcap---------------------------------------------------------------------
mcap <- read_excel("mnth_mcap.xlsx")
mcap <- mcap[, colnames(jse_returns)]

returns <- apply.monthly(jse_returns, Return.cumulative)[-1,]

mcap <- mcap %>% 
  mutate(across(where(is.logical), as.numeric)) #%>% 
#  mutate(Date = index(returns))


## ----mcap_rank, message=FALSE-------------------------------------------------
mcap <- mcap %>%
  rowwise() %>%
  mutate(my_ranks = list(rank(desc(c_across(where(is.numeric))), 
                              na.last = "keep", ties.method = "first"))) %>%
  unnest_wider(c(my_ranks)) %>%
  rename_at(vars(starts_with("...")), ~ str_replace(., fixed("..."), "rank_x"))%>%
  select(contains("rank")) %>% 
  `colnames<-`(colnames(returns))


mcap <- mcap %>% 
  mutate(across(where(is.numeric), function(x) ifelse(x <= 100, 1, NA)))

## ----exclusions---------------------------------------------------------------
returns <- returns * as.matrix(mcap)


## ----ret----------------------------------------------------------------------
ret <- returns
ret[is.na(ret)] <- 0


## ----mom----------------------------------------------------------------------
mom <- ret %>%
  tk_tbl() %>% 
  select(-index) %>% 
  mutate(month = rep(1:41, each=6)) %>%
  select(month, everything()) %>%
  arrange(month) %>%
  group_by(month) %>%
  mutate(across(everything(), function(x) cumprod(1+x))) %>%
  mutate(across(everything(),dplyr::lag))

na.ret <- cbind(1, returns)

mom[is.na(na.ret)] <- NA

## ----winner-------------------------------------------------------------------
quants <- mom %>%
  group_by(month) %>%
  filter(row_number()==6) %>%
  gather(firm, value, -month) %>% 
  group_by(month) %>% 
  mutate(port = ntile(desc(value), 6)) %>%
  group_by(firm) %>%
  mutate(port = dplyr::lag(port)) %>% 
  ungroup() %>%
  arrange(month) %>% 
  pivot_wider(month, firm, values_from  = port) %>%
  group_by(month)

mom <- left_join(quants, mom, "month") %>% 
  select("month", contains(".x")) %>%
  `colnames<-`(gsub(".x", "", colnames(.)))

winner <- mom

winner[winner != 1] <- NA


## ----momentum-----------------------------------------------------------------
winner <- winner[, -1] * as.matrix(ret)
mom_ret <- rowMeans(winner, na.rm = T)


## ----vol----------------------------------------------------------------------
vol <- rollapply(data = returns, width = 12, FUN = sd, fill = NA)
volAnn <- vol[endpoints(vol, on = "years")] * sqrt(12)

yrs <- year(index(volAnn))[-1]
yrs <- yrs[-length(yrs)]

for (yr in yrs) {
  mon <- as.character(yr)
  temp <- coredata(volAnn[mon])
  vol[mon, ] <- rbind(temp, temp[rep(1, 11), ])
}

vol["2020", ] <- coredata(volAnn["2020"])


## ----rank---------------------------------------------------------------------
vol100 <- t(apply(-vol, 1, rank, na.last = "keep", ties.method = "first"))


## ----lowvol-------------------------------------------------------------------
low_vol <- ifelse(vol100 > 80, 1, NA)

vol_ret <- rowMeans(low_vol * as.matrix(ret), na.rm = T)


## ----monthlyReturns-----------------------------------------------------------
port_ret <- data.frame(Date = index(ret)[-c(1:11)], Momentum = mom_ret[-c(1:11)], `Low Volatility` = vol_ret[-c(1:11)], check.names = FALSE) %>% 
  tk_xts()


## ----cumulative, message=FALSE, warning=FALSE---------------------------------
cum_mom <- cumprod(1+mom_ret[-c(1:11)])
cum_vol <- cumprod(1+vol_ret[-c(1:11)])
cum_ret <- data.frame(Date = index(ret)[-c(1:11)], Momentum = cum_mom, `Low Volatility` = cum_vol, check.names = FALSE) %>% tk_xts()


## ----graph--------------------------------------------------------------------


## ----drawdowns----------------------------------------------------------------
port_ret <- data.frame(Date = index(ret)[-c(1:11)], Momentum = mom_ret[-c(1:11)], `Low Volatility` = vol_ret[-c(1:11)]) %>% 
  tk_xts()

dd <- Drawdowns(port_ret)


## ----volatility---------------------------------------------------------------
port_ret <- data.frame(Date = index(ret)[-c(1:11)], Momentum = mom_ret[-c(1:11)], `Low Volatility` = vol_ret[-c(1:11)], check.names = FALSE) %>% 
  tk_xts()

port_vol <- rollapply(port_ret, 12, sd) * sqrt(12)

