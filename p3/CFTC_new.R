# CFTC
startQuandl()

CFTC <- list('JPY'=Quandl('CFTC/JY_F_L_ALL', type='xts'), 'GBP'=Quandl('CFTC/BP_F_L_ALL', type='xts'), 'EUR'=Quandl('CFTC/EC_F_L_ALL', type='xts'), 'CAD'=Quandl('CFTC/CD_F_L_ALL', type='xts'), 'NZD'=Quandl('CFTC/NE_F_L_ALL', type='xts'), 'AUD'=Quandl('CFTC/AD_F_L_ALL', type='xts'), 'CHF'=Quandl('CFTC/SF_F_L_ALL', type='xts'))
CFTC['JPY']
JPY_L <- Quandl('CFTC/JY_F_L_ALL', type='xts') # LEGACY
JPY_L <- (JPY_L$`Noncommercial Long`-JPY_L$`Noncommercial Short`)/JPY_L$`Open Interest`
GBP_L <- Quandl('CFTC/BP_F_L_ALL', type='xts')
GBP_L <- (GBP_L$`Noncommercial Long`-GBP_L$`Noncommercial Short`)/GBP_L$`Open Interest`
EUR_L <- Quandl('CFTC/EC_F_L_ALL', type='xts')
EUR_L <- (EUR_L$`Noncommercial Long`-EUR_L$`Noncommercial Short`)/EUR_L$`Open Interest`
CAD_L <- Quandl('CFTC/CD_F_L_ALL', type='xts')
CAD_L <- (CAD_L$`Noncommercial Long`-CAD_L$`Noncommercial Short`)/CAD_L$`Open Interest`
NZD_L <- Quandl('CFTC/NE_F_L_ALL', type='xts')
NZD_L <- (NZD_L$`Noncommercial Long`-NZD_L$`Noncommercial Short`)/NZD_L$`Open Interest`
AUD_L <- Quandl('CFTC/AD_F_L_ALL', type='xts')
AUD_L <- (AUD_L$`Noncommercial Long`-AUD_L$`Noncommercial Short`)/AUD_L$`Open Interest`
CHF_L <- Quandl('CFTC/SF_F_L_ALL', type='xts')
CHF_L <- (CHF_L$`Noncommercial Long`-CHF_L$`Noncommercial Short`)/CHF_L$`Open Interest`

head(AUD_L)

asd <- fivequantile(fx[['AUDUSD']], 2)
asd <- fivequantile(fx_weekly[['AUDUSD']], 2)

asd <- fivequantile(fx[['EURUSD']], 2)
asd <- fivequantile(fx[['USDJPY']], 2)
asd <- fivequantile(fx[['NZDUSD']], 2) # GAPS
asd <- fivequantile(fx[['USDCAD']], 2)
asd <- fivequantile(fx[['USDCHF']], 2)

fivequantile
check <- abs(diff(AUD_L))
# plot.zoo(AUD_L$`Open Interest`)
check <- abs(diff(log(AUD_L$`Open Interest`)))
check <- abs(diff(log(AUD_L$`Open Interest`)))

check <- abs(diff(JPY_L))
check <- abs(diff(NZD_L)) # GAPS
check <- abs(diff(CAD_L))
check <- abs(diff(GBP_L))
check <- abs(diff(CHF_L))

all <- cbind(AUD_L,CAD_L,CHF_L,EUR_L,JPY_L,NZD_L)
View(all['2000/'])

test <- sort(as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[1]])+x)))))

sum(check[index(check) %in% (index(asd[[1]])+7)])
sum(check[index(check) %in% (index(asd[[2]])+7)])
sum(check[index(check) %in% (index(asd[[3]])+7)])
sum(check[index(check) %in% (index(asd[[4]])+7)])
sum(check[index(check) %in% (index(asd[[5]])+7)])

sum(check[index(check) %in% as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[1]])+x)))) ])
sum(check[index(check) %in% as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[2]])+x)))) ])
sum(check[index(check) %in% as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[3]])+x)))) ])
sum(check[index(check) %in% as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[4]])+x)))) ])
sum(check[index(check) %in% as.Date(unlist(lapply(c(7:11), function(x) as.character(index(asd[[5]])+x)))) ])
