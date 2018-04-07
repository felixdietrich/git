asd <- fixed_delta(fx[[4]][,c(2,1)], num=0.25)
asd
plot.zoo(cumsum(asd$total))
x = asd[,'total.adj']
pop(x)

source('p3/p3_functions.R')
# fx from server.R
fx <- readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uxe9zevohx8wayr/fx_bloom2.rds"))) # 2000-01-04/2016
fx_weekly <- lapply(fx, function(x) x[weekdays(index(x))=='Monday'])
fx_weekly_2005 <- lapply(fx_weekly, function(x) x['2005-03-09/'])
fx_2005 <- lapply(fx, function(x) x['2005-03-09/'])

### I USE SAME QUANTILES ???

fx <- create_data(source='datastream',order='spot_first',period='2000/2016')

head(fx[[1]])
fx['AUDUSD']
lapply(fx, head)
asd <- fivequantile(fx[['AUDUSD']], 2)
asd <- fivequantile(fx_2005[['AUDUSD']], 2)
asd <- fivequantile(fx_weekly[['AUDUSD']], 2)
asd <- fivequantile(fx_weekly_2005[['AUDUSD']], 2)

lovtus = fivequantile(generate_new(fx[['AUDUSD']]), 2)
lovtus

### FINAL
require(boot)
xxx <- generate_new(fx[['EURUSD']], day='Monday') # NEW END 2016 # NZDUSD
xxx <- generate_new(fx[['AUDUSD']], day='Monday') # NEW END 2016 # NZDUSD
xxx <- generate_new(fx[['USDCAD']], day='Friday') # NEW END 2016 # NZDUSD

xxx <- generate_new(fx[['USDCHF']], day='Monday') # only for USDCHF is the rate lowest in quantile 1

res <- generate_new2(fx[['AUDUSD']], day='Tuesday', l=5); res
res <- generate_new2(fx[['AUDUSD']], day='Friday', l=100); res
res <- generate_new2(fx[['NZDUSD']], day='Friday', l=5); res
res <- generate_new2(fx[['USDJPY']], day='Friday', l=5); res
res <- generate_new2(fx[['EURUSD']], day='Monday', l=5); res
res <- generate_new2(datax[['EURUSD']], day='Friday', l=5); res
# res <- generate_new2(datax[['USDCAD']], day='Friday', l=5); res
which.na.xts(dataq[[1]])

res <- generate_new2(cbind(data[[4]][,2],data[[4]][,1]), day='Monday', l=5); res

nrow(datax[['EURUSD']])
nrow(fx[['EURUSD']])
head(fx[['EURUSD']])
head(cbind(data[[4]][,2],data[[4]][,1]))
res <- generate_new2(fx[['USDCAD']], day='Friday', l=5); res
plot.zoo(cumsum(as.numeric(dataq[[5]][,'diff'])))
datacheck
# 2018
xxx <- generate_new(fx_2005[['EURUSD']], day='Friday')
res <- generate_new2(fx_2005[['EURUSD']], day='Friday')

weekdays(index(dataq[[3]]))

lovtus = fivequantile(xxx, 2)
# lapply(lovtus, nrow)
lovtus2 = lapply(lovtus, function(x) c(colMeans(x,na.rm=T),nrow(x))) ### DIE ERGEBNISSE KOENNEN HIER LEICHT UNTERSCHIEDLICH ZU FOMC_CHECK sein weil erst colMeans dann verhaeltnis
lovtus3 = do.call(rbind, lovtus2)
lovtus3 <- cbind(lovtus3[,c(1,2,8,9,5,6,7)],lovtus3[,'Real_a']/lovtus3[,'Vol']) 
lovtus3 <- cbind(lovtus3, lovtus3[,'w_abs_ahead']/lovtus3[,'exp']) 
colnames(lovtus3)=c("Spot","Vol","RV","RV_ahead","Abs","Abs_ahead","Exp","RV %","Abs %")
lovtus3
round(cbind(lovtus3,res), 4)
### FINAL
# stargazer???


# all.equal(asd, qsort[['AUDUSD']])
require(robustbase)
pdf('testasdasd.pdf', height = 21, width = 28)
pdf('against_1.pdf', height = 6, width = 6); 

par(mfrow=c(3,4))
pdf('against_1.pdf', height = 6, width = 6); plot_against(generate_new(fx[['AUDUSD']]), type='Volatility', title='AUD/USD'); dev.off()
pdf('against_2.pdf', height = 6, width = 6); plot_against(generate_new(fx[['AUDUSD']]), type='Move', title='AUD/USD'); dev.off()
pdf('against_3.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDCAD']]), type='Volatility', title='USD/CAD'); dev.off()
pdf('against_4.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDCAD']]), type='Move', title='USD/CAD'); dev.off()
pdf('against_5.pdf', height = 6, width = 6); plot_against(generate_new(fx[['EURUSD']]), type='Volatility', title='EUR/USD'); dev.off()
pdf('against_6.pdf', height = 6, width = 6); plot_against(generate_new(fx[['EURUSD']]), type='Move', title='EUR/USD'); dev.off()
pdf('against_7.pdf', height = 6, width = 6); plot_against(generate_new(fx[['NZDUSD']]), type='Volatility', title='NZD/USD'); dev.off()
pdf('against_8.pdf', height = 6, width = 6); plot_against(generate_new(fx[['NZDUSD']]), type='Move', title='NZD/USD'); dev.off()
pdf('against_9.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDJPY']]), type='Volatility', title='USD/JPY'); dev.off()
pdf('against_10.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDJPY']]), type='Move', title='USD/JPY'); dev.off()
pdf('against_11.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDCHF']]), type='Volatility', title='USD/CHF'); dev.off()
pdf('against_12.pdf', height = 6, width = 6); plot_against(generate_new(fx[['USDCHF']]), type='Move', title='USD/CHF'); dev.off()
dev.off()
fx[['EURUSD']]

asd <- fivequantile(fx_weekly[['EURUSD']], 2)
asd <- fivequantile(fx_weekly_2005[['EURUSD']], 2)

asd <- fivequantile(fx_weekly[['USDJPY']], 2)
asd <- fivequantile(fx_weekly[['USDCHF']], 2)
asd <- fivequantile(fx_weekly[['USDCAD']], 2)
names(fx_weekly)
gsub('\\.','','hi..tler')
analyse_hf('AUD.USD','US')
analyse_hf_daily('AUD.USD','US')
analyse_hf = function(x,y='FULL')
{
  data <- readRDS(paste0('~/Dropbox/data/currencies/',x,'.9999.rds'))
  asd <- fivequantile(fx_weekly[[gsub('\\.','',x)]], 2)
  print(unlist(lapply(asd, function(x) nrow(x['2005-03-10/']))))
  data1 <- midpoint(data)
  data2 <- bidask(data)
  data1 <- diff(log(data1))
  data1 <- na.omit(data1)
  if(y=='US') data1 <- data1['T09:30/T16:00']
  if(y=='US') data2 <- data2['T09:30/T16:00']
  pdf(paste0(x,'.pdf'))
  par(mfrow=c(3,2))
  Acf(data1, lag.max = 10, ylim=c(-0.045,0.045), main='Total Period')
  for (i in c(1:5)) {
    Acf(data1[as.character(week_this(asd[[i]]['2005-03-10/']))], lag.max = 10, ylim=c(-0.045,0.045), main=paste('Quantile',i)) }
  dev.off()
  print( sapply(c(1:5), function(x) ar(data1[as.character(week_this(asd[[x]]['2005-03-10/']))], aic=FALSE, order.max=1, method=c('ols'))$ar) )
  print( sapply(c(1:5), function(x) median(data2[as.character(week_this(asd[[x]]['2005-03-10/']))], na.rm=T)*100) )
}
analyse_hf_daily = function(x,y='FULL',hampel=NULL)
{
  data <- readRDS(paste0('~/Dropbox/data/currencies/',x,'.9999.rds'))
  data1 <- midpoint(data)
  if(!is.null(hampel)) data1 <- readRDS(paste0('~/Dropbox/data/currencies/',x,'.9999.hampel.rds'))$y
  # data <- readRDS(paste0('~/Dropbox/data/currencies/',x,'.9999.rds'))['/2014'] # for CHF
  asd <- fivequantile(fx[[gsub('\\.','',x)]], 2)
  print(unlist(lapply(asd, function(x) nrow(x['2005-03-10/']))))
  data2 <- bidask(data)
  data1 <- diff(log(data1))
  data1 <- na.omit(data1)
  if(y=='US') data1 <- data1['T09:30/T16:00']
  if(y=='US') data2 <- data2['T09:30/T16:00']
  print( round(sapply(c(1:5), function(x) ar(data1[as.character(index(asd[[x]]['2005-03-10/']))], aic=FALSE, order.max=1, method=c('ols'))$ar),5) )
  print( round(sapply(c(1:5), function(x) median(data2[as.character(index(asd[[x]]['2005-03-10/']))], na.rm=T)*100),5) )
}

AUDUSD <- readRDS('~/Dropbox/data/currencies/AUD.USD.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/AUD.USD.9999.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/AUD.USD.9999.hampel.rds')$y

AUDUSD <- readRDS('~/Dropbox/data/currencies/USD.CAD.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/USD.CAD.9999.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/USD.CAD.9999.hampel.rds')$y

AUDUSD <- readRDS('~/Dropbox/data/currencies/EUR.USD.9999.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/USD.JPY.9999.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/USD.CHF.9999.rds')
AUDUSD <- readRDS('~/Dropbox/data/currencies/NZD.USD.rds')
head(AUDUSD)
weekdays(index(first(AUDUSD)))
asd[[1]]
AUDUSD <- midpoint(AUDUSD)
AUDUSD <- bidask(AUDUSD)
AUDUSD <- AUDUSD[.indexmin(AUDUSD) %in% c(seq(0,55,5))]
# AUDUSD <- AUDUSD[.indexmin(AUDUSD) %in% c(seq(0,50,10))]
AUDUSD <- AUDUSD[.indexmin(AUDUSD) %in% c(0,30)]
# AUDUSD <- AUDUSD[.indexhour(AUDUSD) %in% seq(0,23) & .indexmin(AUDUSD) %in% c(0)]

AUDUSD <- diff(log(AUDUSD))
AUDUSD <- na.omit(AUDUSD)
head(AUDUSD)
AUDUSD <- AUDUSD['T09:30/T16:00']
head(AUDUSD['2005-03-10'])
index(asd[[1]]['2005-03-09/'])


acf(AUDUSD)
require(forecast)

# 
acf_custom(AUDUSD[as.character(week_this(asd[[1]]['2005-03-10/']))])

AUDUSD[as.character(week_this(asd[[1]]['2005-03-10/']))]
unique(sort(as.character(week_this(asd[[1]]['2005-03-10/']))))
anyDuplicated(index(test))

sapply(c(1:5), function(x) ar(AUDUSD[as.character(week_this(asd[[x]]['2005-03-10/']))], aic=FALSE, order.max=1, method=c('ols'))$ar)
sapply(c(1:5), function(x) ar(AUDUSD[as.character(index(asd[[x]]['2005-03-10/']))], aic=FALSE, order.max=1, method=c('ols'))$ar) # clearer picture if daily sorted?

### DO LIQUIDITY!
sapply(c(1:5), function(x) median(AUDUSD[as.character(week_this(asd[[x]]['2005-03-10/']))])*100)
# sapply(c(1:5), function(x) mean(AUDUSD[as.character(week_this(asd[[x]]['2005-03-10/']))])*100)
sapply(c(1:5), function(x) median(AUDUSD[as.character(index(asd[[x]]['2005-03-10/']))])*100)

### LIQUIDITY EXAMPLES
plot.zoo


ar(na.omit(diff(log(AUDUSD['2014-09-18']))), aic=FALSE, order.max=1, method=c('ols'))$ar
dev.off()
plot.zoo(AUDUSD['2014-09-18'])
# weekdays(as.Date('2014-09-18'))

lapply(c('AUD.USD','EUR.USD','USD.CAD','USD.JPY','USD.CHF','NZD.USD'), function(z) { xample(z,'2014-12-24'); xample(z,'2014-09-18') })

xample('AUD.USD','2014-09-18')
xample('AUD.USD','2014-12-24')
xample('NZD.USD','2014-09-18')
xample('NZD.USD','2014-12-24')

getwd()
x='AUD.USD'
dates='2014-12-24'

xample = function(x,dates) {
  AUDUSD <- midpoint(readRDS(paste0('~/Dropbox/data/currencies/',x,'.9999.rds')))
  # name <- paste0(x,'-',dates,'.pdf') # print(name)
  pdf(paste0(gsub('\\.','-',x),'-',dates,'.pdf'))
  plot.zoo(AUDUSD[dates], xlab='', ylab='', main=paste0(substr(x,1,3),substr(x,5,7),' ',dates))
  mtext(paste('AR(1):',round(ar(na.omit(diff(log(AUDUSD[dates]))), aic=FALSE, order.max=1, method=c('ols'))$ar, 4)))
  dev.off()
}

### NEW CHECKS ----
res <- generate_new2(fx[['EURUSD']], day='Friday')
res
a = ar(dataq[[1]][,3], aic=FALSE, order.max = 1)
a
a = ar(dataq[[1]][,3], aic=FALSE, order.max = 5)
a
a$partialacf
car::durbinWatsonTest(as.numeric(dataq[[1]][,3]))
lmtest::dwtest#(as.numeric(dataq[[1]][,3]))
# The Durbin Watson test reports a test statistic, with a value from 0 to 4, where:
# 2 is no autocorrelation.
# 0 to <2 is positive autocorrelation (common in time series data).
# >2 to 4 is negative autocorrelation (less common in time series data).
dataq[[1]]
pracma::hurstexp(dataq[[1]][,'diff'])
pracma::hurstexp(dataq[[2]][,'diff'])
pracma::hurstexp(dataq[[3]][,'diff'])
pracma::hurstexp(dataq[[4]][,'diff'])
invisible(pracma::hurstexp(dataq[[5]][,'diff'])$Hs)
aa$Hs
lag=5
Box.test(dataq[[1]][,'diff'], lag=lag)$p.value
Box.test(dataq[[2]][,'diff'], lag=lag)$p.value
Box.test(dataq[[3]][,'diff'], lag=lag)$p.value
Box.test(dataq[[4]][,'diff'], lag=lag)$p.value
Box.test(dataq[[5]][,'diff'], lag=lag)$p.value
# Augmented Dickey–Fuller test ==>> stationary?
tseries::adf.test(dataq[[1]][,'diff'])
tseries::adf.test(dataq[[2]][,'diff'])
tseries::adf.test(dataq[[3]][,'diff'])
tseries::adf.test(dataq[[4]][,'diff'])
tseries::adf.test(dataq[[5]][,'diff'])$statistic

ou_chan(cumsum(dataq[[4]][,'diff']), return='t')
ou_chan(cumsum(dataq[[5]][,'diff']), return='t')

### THIS IS NOT REALLY THE SAME THING. could be a directional issue???
pracma::hurstexp
require(boot)
lynx.2 <- mean(tsboot(dataq[[1]][,'diff'], rssimple, R = 1000, l = 100, sim = "fixed")$t)

lynx.2 <- tsboot(dataq[[1]][,'diff'], rssimple, R = 1000, l = 5, sim = "fixed")
asd <- split(dataq[[1]], 'weeks')
mean(unlist(lapply(asd, function(x) rssimple(x$diff))))

lynx.2
mean(lynx.2$t)-lynx.2$t0

x <- rnorm(1000)  # no unit-root
x <- cumsum(rnorm(1000))

tseries::adf.test(x)
felix_trend(cumsum(dataq[[1]][,'diff']))
felix_trend(cumsum(dataq[[2]][,'diff']))
felix_trend(cumsum(dataq[[3]][,'diff']))
felix_trend(cumsum(dataq[[4]][,'diff']))
felix_trend(cumsum(dataq[[5]][,'diff']))

felix_trend(xts(as.numeric(dataq[[5]][,'diff']), as.Date(1:nrow(dataq[[5]][,'diff']))))
dynlm

summary(lm(USDCAD ~ lag(USDCAD,k=-3)))

a = split(cumsum(dataq[[1]][,'diff']),'weeks')[[20]]
b = cumsum(dataq[[1]][,'diff']['2003-01-06/2003-01-10'])
b = cumsum(dataq[[1]][,'diff'])[250:254]
b
ou_chan(a)
ou_chan(b)
temp2
temp
asd = tsboot(cumsum(dataq[[1]][,'diff']), ou_chan, R = 1000, l = 5, n.sim = 5, sim = "fixed") # n.sim = The length of the simulated time series.
str(asd)
asd$seed
asd$
ou_chan(cumsum(dataq[[1]][,'diff']))

asd$n.sim
mean(tsboot(cumsum(newxts(dataq[[1]][,'diff'])), ou_chan, R = 1000, l = 5, sim = "fixed")$t)
plot.zoo(cumsum(dataq[[1]][,'diff']))
# First, I want to highly recommend the maximum entropy bootstrap (meboot). I abandoned the block bootstrap in favor of meboot, and I've been very pleased with the results. 
# https://stats.stackexchange.com/questions/13675/alternative-to-block-bootstrap-for-multivariate-time-series

asd <- fbootstrap(cumsum(dataq[[1]][,'diff']), size=5, ou_chan)
mean(asd[[1]])
mean(tsboot(cumsum(dataq[[1]][,'diff']), ou_chan, R = 880, l = 5, n.sim=5, sim = "fixed")$t)
