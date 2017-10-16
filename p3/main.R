source('functions.R')
source('functions_plot.R')

volatilities <- readRDS('p3/volatilities.rds')
butterflies <- readRDS('p3/butterflies.rds')
newspot <- readRDS('SpotRates_Bloomberg.rds')
# volatilities <- lapply(volatilities, function(x) x[,2])
butterflies <- lapply(butterflies, function(x) x[,2]) # 1W starts at 2005-03-08 
# wherestart(butterflies[[6]])
# butterflies[[1]]
EURUSD <- cbind(volatilities[['EURUSD']],butterflies[['EURUSD']])
EUR <- readRDS('~/Dropbox/Currencies_1997.rds')[,'EUR']
head(volatilities[['EURUSD']][,2])
head(EURUSD)
### 
x <- volatilities[['EURUSD']][,2]+butterflies[['EURUSD']]
plot.zoo(x)
xx <- cbind(x, volatilities[['EURUSD']][,2])
lines(as.zoo(volatilities[['EURUSD']][,2]), col='red')
x
data <- na.locf(cbind(EURUSD[,2],EUR)['2000/2010'])
data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'AUD'])['2000/2010']),
              na.locf(cbind(volatilities[['NZDUSD']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'NZD'])['2000/2010']),
              na.locf(cbind(volatilities[['USDCAD']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'CAD'])['2000/2010']),
              na.locf(cbind(volatilities[['EURUSD']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'EUR'])['2000/2010']),
              na.locf(cbind(volatilities[['USDCHF']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'CHF'])['2000/2010']),
              na.locf(cbind(volatilities[['USDJPY']][,2],readRDS('~/Dropbox/Currencies_1997.rds')[,'JPY'])['2000/2010']))
data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],newspot[,'AUDUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['NZDUSD']][,2],newspot[,'NZDUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2],newspot[,'USDCAD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2],newspot[,'EURUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2],newspot[,'USDCHF'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2],newspot[,'USDJPY'])['2005-03-08/2016']))
true <- list(na.locf(cbind(volatilities[['AUDUSD']][,2]+butterflies[['AUDUSD']],newspot[,'AUDUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['NZDUSD']][,2]+butterflies[['NZDUSD']],newspot[,'NZDUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2]+butterflies[['USDCAD']],newspot[,'USDCAD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2]+butterflies[['EURUSD']],newspot[,'EURUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2]+butterflies[['USDCHF']],newspot[,'USDCHF'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2]+butterflies[['USDJPY']],newspot[,'USDJPY'])['2005-03-08/2016']))

lapply(true, function(x) wherestart(x, 'na'))
wherestart(true[[1]], mode='na')
tail(data)
lapply(data, head)
str(volatilities)
# AUDUSD NZDUSD USDCAD EURUSD USDCHF USDJPY

x <- lapply(true, function(y) fixed_delta(y, 'delta', 0.25))
x <- lapply(data, function(y) fixed_delta(y, 'delta', 0.25))
x <- lapply(data, function(y) fixed_delta(y, 'fixed', 0.015))

names(x) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
tail(x[[6]])

### strangle returns given volatility -----
fritz <- lapply(x, function(y) {
data <- y
data$t <- lag(data$total, k=-1)
data$t.adj <- lag(data$total.adj, k=-1)
data <- data[,c('Vol','t','t.adj')]
# data <- data[,c('Vol','total','total.adj')]
fivequantile(data, 1)
# tenquantile(data, 1)
})
names(fritz) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
lapply(fritz[['AUDUSD']], function(x) sum(x[,'t'], na.rm=T))
lapply(fritz[['NZDUSD']], function(x) sum(x[,'t'], na.rm=T))
lapply(fritz[['USDCAD']], function(x) sum(x[,'t.adj'], na.rm=T))
lapply(fritz[['EURUSD']], function(x) sum(x[,'t'], na.rm=T))
lapply(fritz[['USDCHF']], function(x) sum(x[,'t.adj'], na.rm=T))
lapply(fritz[['USDJPY']], function(x) sum(x[,'t.adj'], na.rm=T))

lapply(fritz[['AUDUSD']], function(x) sum(x[,'total'], na.rm=T))
lapply(fritz[['NZDUSD']], function(x) sum(x[,'total'], na.rm=T))
lapply(fritz[['USDCAD']], function(x) sum(x[,'total.adj'], na.rm=T))
lapply(fritz[['EURUSD']], function(x) sum(x[,'total'], na.rm=T))
lapply(fritz[['USDCHF']], function(x) sum(x[,'total.adj'], na.rm=T))
lapply(fritz[['USDJPY']], function(x) sum(x[,'total.adj'], na.rm=T))

### end

# xx <- do.call(cbind, lapply(x, function(y) y[,'total'])) # FUER YEN IST ES FALSCH
xx <- cbind(x[['AUDUSD']][,'total'],x[['NZDUSD']][,'total'],x[['USDCAD']][,'total.adj'],x[['EURUSD']][,'total'],x[['USDCHF']][,'total.adj'],x[['USDJPY']][,'total.adj'])

colnames(xx) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
xxx <- cumsum(xx)
tail(xxx) # -0.004280888 0.04624821 0.3631171 0.2655923 0.2410946 0.4432563

plot.zoo(xxx, plot.type = 'single', col=c(1:6))
legend('topleft', c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY'), lty=1, col=c(1:6))
xxxx <- xts(rowSums(xxx), index(xxx))
plot.zoo(xxxx)

rets <- xts(rowSums(xx), index(xx))
require(PerformanceAnalytics)
SharpeRatio.annualized(rets)
charts.PerformanceSummary(rets)
Return.annualized(rets)
StdDev.annualized(rets)

### given volatility
JPM <- readRDS('~/Dropbox/newdiss/files/JPM-newcombined.rds')
f <- na.omit(cbind(JPM,rets))
g <- fivequantile(f, 1)
g <- tenquantile(f, 1)
lapply(g, function(x) sum(x[,2], na.rm=T))

### fancy plot
head(rets)
ss <- cbind(x[['AUDUSD']][,'Vol'],x[['NZDUSD']][,'Vol'],x[['USDCAD']][,'Vol'],x[['EURUSD']][,'Vol'],x[['USDCHF']][,'Vol'],x[['USDJPY']][,'Vol'])
ss <- xts(rowMeans(ss, na.rm=T), index(ss))
colnames(ss) <- 'VolAverage'
plot.zoo(cumsum(rets))
usr <- par('usr')
ss$rank <- ntile(ss, 100)
ss$rank <- ntile(ss$VolAverage, 5)
one.dimensions <- colorRampPalette( c( rgb(0,1,0,0.25) , rgb(1,0,0,0.25) ), alpha=TRUE )( 5 )

gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=one.dimensions[ss$rank], border=NA)
gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=adjustcolor(ss$rank+1, 0.25), border=NA)
gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=adjustcolor(c('darkgreen','green','white','red','darkred'), 0.25)[ss$rank], border=NA)

head(x[['AUDUSD']])

### NEW JUMPS

