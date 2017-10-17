require(stargazer)
require(quantmod)
### 2017
FD1b <- readRDS('~/Dropbox/newdiss/G10-indices.rds') # from 2004 # aus FuturesCarryd_newInd.xlsx
FD1c <- readRDS('~/Dropbox/newdiss/G10_Fut_Cont_Index.rds') # long-term
all.equal(FD1b, FD1c['2004-03-31/2014'], check.attributes=FALSE) # same only the ordering is different

### SUMMA DAILY
summa <- diff(log(FD1b))
summa <- cbind(summa[2:nrow(summa),],
               readRDS('~/Dropbox/newdiss/files/longLEG.rds'),
               readRDS('~/Dropbox/newdiss/files/shortLEG.rds'),
               'HML'=readRDS('~/Dropbox/newdiss/files/total-Reb.rds'))
# which.na.xts(summa[,'NOK'])
head(summa) # from 2004-04-01 / 2014
plot(cumsum(summa[,'longLeg']))
plot(summa$CHF)
plot(FD1b$CHF)
plot(FD1b$JPY)

stockm <- cbind(getSymbols('^GSPC', from='1990-03-31', to='2014-12-31', auto.assign = F)[,4],
                getSymbols('VTI', from='1990-03-31', to='2014-12-31', auto.assign = F)[,6],
                getSymbols('^VIX', from='1990-03-31', to='2014-12-31', auto.assign = F)[,4],
                readRDS('~/Dropbox/newdiss/files/Oct_JPMVXYG7.rds'))
stockm <- diff(log(stockm)) # [2:nrow(stockm),] # remove first NA

# stockm3 <- cbind(summa[,1],stockm2)[,2:4] # nrow(stockm3[which(is.na(stockm3[,2])),]) # 98 === DUMMY DAMIT SELBER INDEX
# nrow(stockm2)

daily <- cbind(summa, stockm)['2004-04-01/2014-12-31']
monthly <- apply.monthly(daily, colSums, na.rm = T)
# stockm4=na.omit(diff(log(apply.monthly(stockm, last))))
tail(daily, 20)

### DAILY ----
multi.fun <- function(x) {
  c(MEAN = mean(x, na.rm=T), SD = sd(x, na.rm=T), 
    SKEW = skewness(x, na.rm=T), KURT = kurtosis(x, na.rm=T),
    MAX = max(x, na.rm=T), MIN = min(x, na.rm=T))
}

asd = sapply(daily[,c(1:ncol(summa))]*100, multi.fun)
asd = rbind(asd, 
            'SP'=apply(daily[,c(1:ncol(summa))], 2, function(x) cor(x, daily[,'GSPC.Close'], use='complete.obs')),
            'TOT'=apply(daily[,c(1:ncol(summa))], 2, function(x) cor(x, daily[,'VTI.Adjusted'], use='complete.obs')),
            'VIX'=apply(daily[,c(1:ncol(summa))], 2, function(x) cor(x, daily[,'VIX.Close'], use='complete.obs')))

round(asd[c(1:2),], 3)
stargazer(asd)

DBCFHX <- cbind(diff(log(readRDS('~/Dropbox/newdiss/files/Oct_DBCFHX.rds'))), stockm)['1993-03-15/2014']
multi.fun(DBCFHX[,1]*100)
cor(DBCFHX[,'DBCFHX'], DBCFHX[,'GSPC.Close'], use='complete.obs')
cor(DBCFHX[,'DBCFHX'], DBCFHX[,'VIX.Close'], use='complete.obs')

# MONTHLY ----

asd = sapply(monthly[,c(1:ncol(summa))]*100, multi.fun)
asd = rbind(asd, 
            'SP'=apply(monthly[,c(1:ncol(summa))], 2, function(x) cor(x, monthly[,'GSPC.Close'], use='complete.obs')),
            'TOT'=apply(monthly[,c(1:ncol(summa))], 2, function(x) cor(x, monthly[,'VTI.Adjusted'], use='complete.obs')),
            'VIX'=apply(monthly[,c(1:ncol(summa))], 2, function(x) cor(x, monthly[,'VIX.Close'], use='complete.obs')))

round(asd[c(1:2),], 3)
stargazer(asd)

### OTHER TRY ----
require(tseries)
# jarque.bera.test(summa[,6])
# SharpeRatio.annualized(summa, Rf = 0) # uses geometric returns
# http://quant.stackexchange.com/questions/3607/should-i-use-an-arithmetic-or-a-geometric-calculation-for-the-sharpe-ratio
require(PerformanceAnalytics)
table.Stats(summa)
chart.RiskReturnScatter(summa, Rf = 0)

