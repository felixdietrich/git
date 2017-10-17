require(stargazer)
require(quantmod)
### 2017
FD1b <- readRDS('~/Dropbox/newdiss/G10-indices.rds')
FD1c <- readRDS('~/Dropbox/newdiss/G10_Fut_Cont_Index.rds')
all.equal(FD1b, FD1c['2004-03-31/2014'], check.attributes=FALSE) # same only the ordering is different

JPM <- readRDS('~/Dropbox/newdiss/files/Oct_JPMVXYG7.rds')['2004-03-31/2014-12-31']
stockm <- cbind(getSymbols('^GSPC', from='2004-03-31', to='2014-12-31', auto.assign = F)[,4],
             getSymbols('VTI', from='2004-03-31', to='2014-12-31', auto.assign = F)[,6],
             getSymbols('^VIX', from='2004-03-31', to='2014-12-31', auto.assign = F)[,4],
             JPM)
stockm2 <- diff(log(stockm))[2:nrow(stockm),] # remove first NA

stockm3 <- cbind(summa[,1],stockm2)[,2:4] # nrow(stockm3[which(is.na(stockm3[,2])),]) # 98 === DUMMY DAMIT SELBER INDEX
nrow(stockm2)

stockm4=na.omit(diff(log(apply.monthly(stockm, last))))

FD1b <- readRDS('~/Dropbox/newdiss/G10-indices.rds') # aus FuturesCarryd_newInd.xlsx

### SUMMA DAILY
head(FD1b)
summa=diff(log(FD1b))
summa=cbind(summa[2:nrow(summa),],
            readRDS('~/Dropbox/newdiss/files/longLEG.rds'),
            readRDS('~/Dropbox/newdiss/files/shortLEG.rds'),
            'HML'=readRDS('~/Dropbox/newdiss/files/total-Reb.rds'))
# which.na.xts(summa[,'NOK'])
head(summa)
plot(cumsum(summa[,'longLeg']))

### DAILY
multi.fun <- function(x) {
  c(MEAN = mean(x, na.rm=T), SD = sd(x, na.rm=T), 
    SKEW = skewness(x, na.rm=T), KURT = kurtosis(x, na.rm=T),
    MAX = max(x, na.rm=T), MIN = min(x, na.rm=T))
}

asd = sapply(summa, multi.fun)
round(asd[c(1,2),], 3)

dd
dd=rbind(
  MEAN = round(apply(summa*100,2,mean,na.rm = T),3),
  SD = round(apply(summa*100,2,sd,na.rm = T),3),
  SKEW = round(apply(summa*100,2,skewness,na.rm = T),2),
  KURT = round(apply(summa*100,2,kurtosis,na.rm = T),2),
  MAX = round(apply(summa*100,2,max,na.rm = T),2),
  MIN = round(apply(summa*100,2,min,na.rm = T),2),
  SP=round(apply(summa,2,function(x) cor(stockm3[,1],x, use='complete.obs')),2),
  TOT=round(apply(summa,2,function(x) cor(stockm3[,2],x, use='complete.obs')),2),
  VIX=round(apply(summa,2,function(x) cor(stockm3[,3],x, use='complete.obs')),2)
)
stargazer(dd, digits=NA)

### MONTHLY
summa=apply.monthly(summa, colSums)
mm=rbind(
  MEAN = round(apply(summa,2,mean,na.rm = T) * 100 * 12,3),
  SD = round(apply(summa,2,sd,na.rm = T) * 100 * sqrt(12),3),
  SR = round((apply(summa,2,mean,na.rm = T) * 12)/(apply(summa,2,sd,na.rm = T) * sqrt(12)),3),
  SKEW = round(apply(summa,2,skewness,na.rm = T),2),
  KURT = round(apply(summa,2,kurtosis,na.rm = T),2),
  MAX = round(apply(summa,2,max,na.rm = T) * 100,2),
  MIN = round(apply(summa,2,min,na.rm = T) * 100,2),
  SP=round(apply(summa,2,function(x) cor(stockm4[,1],x, use='complete.obs')),2),
  TOT=round(apply(summa,2,function(x) cor(stockm4[,2],x, use='complete.obs')),2),
  VIX=round(apply(summa,2,function(x) cor(stockm4[,3],x, use='complete.obs')),2)
)
mm
stargazer(mm, digits=NA)
summa

plot(summa$CHF)
plot(FD1b$CHF)
plot(FD1b$JPY)


require(tseries)
# jarque.bera.test(summa[,6])
# SharpeRatio.annualized(summa, Rf = 0) # uses geometric returns
# http://quant.stackexchange.com/questions/3607/should-i-use-an-arithmetic-or-a-geometric-calculation-for-the-sharpe-ratio
require(PerformanceAnalytics)
table.Stats(summa)
chart.RiskReturnScatter(summa, Rf = 0)

