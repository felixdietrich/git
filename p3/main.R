source('functions.R')
source('functions_plot.R')
source('p3/p3_functions.R')
source('~/Dropbox/newdiss/git/p3/BSfromDelta.R')

volatilities <- readRDS('~/Dropbox/newdiss/git/p3/volatilities.rds')
butterflies <- readRDS('p3/butterflies.rds')
head(butterflies[[1]])
# butterflies_2000 <- lapply(butterflies, function(x) x['2000/'])
butterflies <- readRDS('p3/butterflies_2000.rds')
head(butterflies[[1]]['2004/'])
head(butterflies[[1]])
newspot <- readRDS('~/Dropbox/newdiss/git/SpotRates_Bloomberg.rds')
head(newspot)

# volatilities <- lapply(volatilities, function(x) x[,2])
butterflies <- lapply(butterflies, function(x) x[,2]) # 1W starts at 2005-03-08 
butterflies <- lapply(butterflies, function(x) x[,6]) # 10 delta butterflies!
# wherestart(butterflies[[6]])
# butterflies[[1]]
EURUSD <- cbind(volatilities[['EURUSD']],butterflies[['EURUSD']])
EUR <- readRDS('~/Dropbox/Currencies_1997.rds')[,'EUR']
head(volatilities[['EURUSD']][,2])
head(butterflies[['EURUSD']])

head(EURUSD)
### 
hampel <- mod_hampel(butterflies[['EURUSD']][,c(2,6)], 30)
hampel[[2]]
plot.zoo(hampel[[1]], plot.type = 'single', col=c(1,2))
plot.zoo(butterflies[['EURUSD']][,c(2,6)], plot.type = 'single', col=c(1,2))
plot.zoo(hampel[[1]]['2015'], plot.type = 'single', col=c(1,2))
plot.zoo(butterflies[['EURUSD']]['2016'][,c(2,6)], plot.type = 'single', col=c(1,2))
plot.zoo(butterflies[['EURUSD']]['2016-10'][,c(2,6)], plot.type = 'single', col=c(1,2))
butterflies[['EURUSD']]['2016-10'][,c(2,6)]
pdf('But_6.pdf')
hampel <- mod_hampel(butterflies[['EURUSD']][,c(2,6)], 30)
plot.zoo(hampel[[1]]['2005/2016'], plot.type = 'single', col=c(1,2), xlab='', ylab='', main='EUR/USD')
# plot.zoo(butterflies[['NZDUSD']]['2005/2016'][,c(2,6)], plot.type = 'single', col=c(1,2), xlab='', ylab='', main='NZD/USD')
legend('topleft',c('25 Delta Butterfly','10 Delta Butterfly'),col=c(1,2),lty=c(1,1))
dev.off()

x <- volatilities[['EURUSD']][,2]+butterflies[['EURUSD']][,2]
lapply(volatilities, function(x) first(na.omit(x[,2])))
lapply(butterflies, function(x) first(na.omit(x)))

plot.zoo(x)
xx <- cbind(x, volatilities[['EURUSD']][,2])
lines(as.zoo(volatilities[['EURUSD']][,2]), col='red')

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
data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],newspot[,'AUDUSD'])['2000/2016']),
             na.locf(cbind(volatilities[['NZDUSD']][,2],newspot[,'NZDUSD'])['2000/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2],newspot[,'USDCAD'])['2000/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2],newspot[,'EURUSD'])['2000/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2],newspot[,'USDCHF'])['2000/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2],newspot[,'USDJPY'])['2000/2016']))
true <- list(na.locf(cbind(volatilities[['AUDUSD']][,2]+butterflies[['AUDUSD']],newspot[,'AUDUSD'])['2005-03-08/2016']), # ist NICHT wegen HF sondern butterfly start
             na.locf(cbind(volatilities[['NZDUSD']][,2]+butterflies[['NZDUSD']],newspot[,'NZDUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCAD']][,2]+butterflies[['USDCAD']],newspot[,'USDCAD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['EURUSD']][,2]+butterflies[['EURUSD']],newspot[,'EURUSD'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDCHF']][,2]+butterflies[['USDCHF']],newspot[,'USDCHF'])['2005-03-08/2016']),
             na.locf(cbind(volatilities[['USDJPY']][,2]+butterflies[['USDJPY']],newspot[,'USDJPY'])['2005-03-08/2016']))
true <- list(na.locf(cbind(volatilities[['AUDUSD']][,2]+butterflies[['AUDUSD']],newspot[,'AUDUSD'])),
             na.locf(cbind(volatilities[['NZDUSD']][,2]+butterflies[['NZDUSD']],newspot[,'NZDUSD'])),
             na.locf(cbind(volatilities[['USDCAD']][,2]+butterflies[['USDCAD']],newspot[,'USDCAD'])),
             na.locf(cbind(volatilities[['EURUSD']][,2]+butterflies[['EURUSD']],newspot[,'EURUSD'])),
             na.locf(cbind(volatilities[['USDCHF']][,2]+butterflies[['USDCHF']],newspot[,'USDCHF'])),
             na.locf(cbind(volatilities[['USDJPY']][,2]+butterflies[['USDJPY']],newspot[,'USDJPY'])))


lapply(true, function(x) wherestart(x, 'na'))
wherestart(true[[1]], mode='na')
tail(data)
lapply(data, head)
str(volatilities)
# AUDUSD NZDUSD USDCAD EURUSD USDCHF USDJPY

timedefinition = 5/252
x <- lapply(true, function(y) fixed_delta(y, 'delta', 0.25, version='OLD'))
x <- lapply(data, function(y) fixed_delta(y, 'delta', 0.25))
x <- lapply(data, function(y) fixed_delta(y, 'fixed', 0.015))

names(x) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
tail(x[[6]])

head(x[[1]])
### strangle returns given volatility -----
fritz <- lapply(x, function(y) {
data <- y
data$t <- lag(data$total, k=-1) # MUSS NOCH GELAGGED WERDEN siehe p3 functions !!!
data$t.adj <- lag(data$total.adj, k=-1)
data <- data[,c('Vol','t','t.adj')]
# data <- data[,c('Vol','total','total.adj')]
fivequantile(data, 1)
# tenquantile(data, 1)
})

lapply(fritz[['AUDUSD']], function(x) pop(x[,'t.adj']))
lapply(fritz[['EURUSD']], function(x) pop(x[,'t.adj']))
lapply(fritz[['USDJPY']], function(x) pop(x[,'t.adj']))
lapply(fritz[['NZDUSD']], function(x) pop(x[,'t.adj']))

### DO returns given volatility
head(fritz[[1]])
names(fritz) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
stargazer::stargazer(rbind(unlist(lapply(fritz[['AUDUSD']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4))), # t # mit alten daten
                           unlist(lapply(fritz[['NZDUSD']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4))), # t
                           unlist(lapply(fritz[['USDCAD']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4))),
                           unlist(lapply(fritz[['EURUSD']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4))), # t
                           unlist(lapply(fritz[['USDCHF']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4))),
                           unlist(lapply(fritz[['USDJPY']], function(x) round(sum(x[,'t.adj'], na.rm=T), 4)))))

stargazer::stargazer(rbind(unlist(lapply(fritz[['AUDUSD']], function(x) round(sr_freq(x[,'t.adj']), 4))),
                           unlist(lapply(fritz[['NZDUSD']], function(x) round(sr_freq(x[,'t.adj']), 4))),
                           unlist(lapply(fritz[['USDCAD']], function(x) round(sr_freq(x[,'t.adj']), 4))),
                           unlist(lapply(fritz[['EURUSD']], function(x) round(sr_freq(x[,'t.adj']), 4))),
                           unlist(lapply(fritz[['USDCHF']], function(x) round(sr_freq(x[,'t.adj']), 4))),
                           unlist(lapply(fritz[['USDJPY']], function(x) round(sr_freq(x[,'t.adj']), 4)))))

sr_freq(x[['AUDUSD']][,'total.adj'])
sr_freq(x[['USDJPY']][,'total.adj'])
sr_freq(fritz[['AUDUSD']][[5]][,'t'])


### end

# xx <- do.call(cbind, lapply(x, function(y) y[,'total'])) # FUER YEN IST ES FALSCH
xx <- cbind(x[['AUDUSD']][,'total'],x[['NZDUSD']][,'total'],x[['USDCAD']][,'total.adj'],x[['EURUSD']][,'total'],x[['USDCHF']][,'total.adj'],x[['USDJPY']][,'total.adj'])

colnames(xx) <- c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY')
xxx <- cumsum(xx)
tail(xxx) # -0.004280888 0.04624821 0.3631171 0.2655923 0.2410946 0.4432563

pdf('10Delta_new.pdf') # pdf('10Delta.pdf')
plot.zoo(xxx, plot.type = 'single', col=c(1:6), xlab='', ylab='', main='Cumulative Returns'); mtext('Short 10 Delta Strangle')
legend('topleft', c('AUDUSD','NZDUSD','USDCAD','EURUSD','USDCHF','USDJPY'), lty=1, col=c(1:6))
dev.off()

xxxx <- xts(rowSums(xxx), index(xxx))
plot.zoo(xxxx)

xx$all <- rowSums(xx)
asd <- sapply(xx, function(x) round(multi.sapply(x, mean_freq, sd_freq, SharpeRatio.annualized.arith, skewness, kurtosis, min, max, count_negative, nrow, sum), digits=4))
asd
stargazer::stargazer(asd, digits=4)


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
ss$rank <- dplyr::ntile(ss, 100)
ss$rank <- dplyr::ntile(ss$VolAverage, 5)
one.dimensions <- colorRampPalette( c( rgb(0,1,0,0.25) , rgb(1,0,0,0.25) ), alpha=TRUE )( 5 )

plotrix::gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=one.dimensions[ss$rank], border=NA)
plotrix::gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=adjustcolor(ss$rank+1, 0.25), border=NA)
plotrix::gradient.rect(first(index(ss)), usr[3]+0.01, last(index(ss)), usr[4]-0.01, col=adjustcolor(c('darkgreen','green','white','red','darkred'), 0.25)[ss$rank], border=NA)

head(x[['AUDUSD']])

### NEW JUMPS

