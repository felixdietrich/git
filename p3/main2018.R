# load("/Users/fd/Dropbox/newdiss/git/2018.RData")

ar.ols(dataq[[1]][,3]*100, aic = F, order.max = 1)$ar

xxx <- readRDS('~/Downloads/Diss2/Diss Excel Files OLD/newfromJan_daily.rds')[['Spot']]
xxx[,'AUD']
xxx['2016']
true <- list(na.locf(cbind(volatilities[['AUDUSD']][,2]+butterflies[['AUDUSD']],xxx[,'AUD'])['2005-03-08/2016']), # ist NICHT wegen HF sondern butterfly start
             na.locf(cbind(volatilities[['NZDUSD']][,2]+butterflies[['NZDUSD']],xxx[,'NZD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2]+butterflies[['USDCAD']],xxx[,'CAD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2]+butterflies[['EURUSD']],xxx[,'EUR'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2]+butterflies[['USDCHF']],xxx[,'CHF'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2]+butterflies[['USDJPY']],xxx[,'JPY'])['2005-03-08/2016']))
data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],xxx[,'AUD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['NZDUSD']][,2],xxx[,'NZD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2],xxx[,'CAD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2],xxx[,'EUR'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2],xxx[,'CHF'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2],xxx[,'JPY'])['2005-03-08/2016']))
data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],xxx[,'AUD'])['2000/2016']),
             na.locf(cbind(volatilities[['NZDUSD']][,2],xxx[,'NZD'])['2000/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2],xxx[,'CAD'])['2000/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2],xxx[,'EUR'])['2000/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2],xxx[,'CHF'])['2000/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2],xxx[,'JPY'])['2000/2016']))
datax <- list(na.locf(cbind(xxx[,'AUD'],volatilities[['AUDUSD']][,2])['2000/2016']),
             na.locf(cbind(xxx[,'NZD'],volatilities[['NZDUSD']][,2])['2000/2016']),
             na.locf(cbind(xxx[,'CAD'],volatilities[['USDCAD']][,2])['2000/2016']),
             na.locf(cbind(xxx[,'EUR'],volatilities[['EURUSD']][,2])['2000/2016']),
             na.locf(cbind(xxx[,'CHF'],volatilities[['USDCHF']][,2])['2000/2016']),
             na.locf(cbind(xxx[,'JPY'],volatilities[['USDJPY']][,2])['2000/2016']))
names(datax) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
true[[1]]
timedefinition <- 5/252
timedefinition <- 5/260
timedefinition
x <- lapply(true, function(y) fixed_delta(y, 'delta', 0.25, version='OLD')) # c('Vol','Spot')
x <- lapply(data, function(y) fixed_delta(y, 'delta', 0.25, version='OLD')) # c('Vol','Spot')
x <- lapply(data, function(y) fixed_delta(y, 'delta', 0.10, version='OLD'))
x <- lapply(data, function(y) fixed_delta(y, 'fixed', 0.015, version='NEW'))
x <- lapply(data, function(y) straddle(y, version='OLD')) # c('Vol','Spot')
x <- lapply(data, function(y) straddle(y, version='OLD', version2='5')) # THATS THE SAME (inkl. CONVERTED) as the new script!!

names(x) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')

head(data[[1]])
head(datax[[1]])
plot.zoo(datax[[6]][,1])
head(x[[6]], 10)

plot.zoo(cumsum(x[[1]][,'total.adj'])) # MAN MUSS ADJUSTED SONST YEN ZU KRASS ABSOLUT
plot.zoo(cumsum(x[[2]][,'total.adj']))
plot.zoo(cumsum(x[[3]][,'total.adj']))
plot.zoo(cumsum(x[[4]][,'total.adj']))
plot.zoo(cumsum(x[[5]][,'total.adj']))
plot.zoo(cumsum(x[[6]][,'total.adj']))

source('CustomFunc.R')
total <- x[[1]][,'total.adj']+x[[2]][,'total.adj']+x[[3]][,'total.adj']+x[[4]][,'total.adj']+x[[5]][,'total.adj']+x[[6]][,'total.adj']
plot.zoo(cumsum(total))
sr_freq(total)
xx <- cbind(x[['AUDUSD']][,'total'],x[['NZDUSD']][,'total'],x[['USDCAD']][,'total'],x[['EURUSD']][,'total'],x[['USDCHF']][,'total'],x[['USDJPY']][,'total'])
xx <- cbind(x[['AUDUSD']][,'total.adj'],x[['NZDUSD']][,'total.adj'],x[['USDCAD']][,'total.adj'],x[['EURUSD']][,'total.adj'],x[['USDCHF']][,'total.adj'],x[['USDJPY']][,'total.adj'])
xx <- cbind(jojo[['AUDUSD']][,'totl']/1000,jojo[['NZDUSD']][,'totl']/1000,jojo[['USDCAD']][,'totl']/1000,jojo[['EURUSD']][,'totl']/1000,jojo[['USDCHF']][,'totl']/1000,jojo[['USDJPY']][,'totl']/1000)
xx <- cbind(jojo[['AUDUSD']][,'l']/1000,jojo[['NZDUSD']][,'l']/1000,jojo[['USDCAD']][,'l']/1000,jojo[['EURUSD']][,'l']/1000,jojo[['USDCHF']][,'l']/1000,jojo[['USDJPY']][,'l']/1000)
colnames(xx) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
xx$all <- rowSums(xx)
asd <- sapply(xx, function(x) round(multi.sapply(x, mean_freq, sd_freq, SharpeRatio.annualized.arith, skewness, kurtosis, min, max, count_negative, nrow, sum), digits=4))
asd
stargazer::stargazer(asd, digits=4)

sr_freq(xts(rowSums(xx), index(xx)))
xxf <- xts(cumsum(rowSums(xx)), index(xx))
plot.zoo(xxf)

plot.zoo(cumsum(xx), plot.type = 'single')

xxx <- cumsum(xx)
tail(xxx)

pdf('ShortStraddle_1_5_252_delta.pdf') # pdf('ShortStraddle_1_5_252_newskript.pdf') # pdf('ShortStraddle_1_5_252.pdf')
pdf('ShortStraddle_1_1_52_b.pdf')
plot.zoo(xxx, plot.type = 'single', col=c(1:6), xlab='', ylab='', main='Cumulative Returns'); mtext('Short Straddle (delta-hedged)')
legend('topleft', c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY'), lty=1, col=c(1:6))
dev.off()

head(datax[['AUDUSD']])
test <- compute_strangle(datax[['AUDUSD']]) # c('Spot','Vol',
asd <- apply.weekly(test, colSums)[,c('tot','reth')]

head(test, 20)
head(asd)
sum(test['2000-01-10/2000-01-14'][,'tot'])

head(x[['AUDUSD']])
head(xx)
sr_freq(x[['AUDUSD']][,'total'])
sr_freq(asd[,'tot'])
head(fx[['AUDUSD']])

