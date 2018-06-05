FXILL <- read.csv('~/Downloads/Diss2/FXIlliquidity.csv', sep=';', stringsAsFactors = FALSE, header=FALSE)
FXILL <- xts(FXILL[,2], as.yearmon(FXILL[,1], format='%b-%y'))
# saveRDS(FXILL, 'FXILL.rds')

bid <- read.csv('~/Downloads/Diss2/Paper1/ddata_bid.csv', sep=';', stringsAsFactors = FALSE)
ask <- read.csv('~/Downloads/Diss2/Paper1/ddata_ask.csv', sep=';', stringsAsFactors = FALSE)

bid <- xts(bid[,2:ncol(bid)], as.Date(bid[,1], format='%m/%d/%y'))
ask <- xts(ask[,2:ncol(ask)], as.Date(ask[,1], format='%m/%d/%y'))
colnames(bid); colnames(ask)
ba <- (ask-bid)/((ask+bid)/2)
bam <- apply.monthly(ba, mean)
bam$mean <- rowMeans(bam, na.rm=T)

# ‘‘Consumption’’ stands for real growth in non-durables and services consumption expenditures, 
# BARROSO: For this we use the monthly growth rate of Real Personal Consumption Expenditures donwloaded from the Federal Reserve of St. Louis.
# PCE
# DPCERAM1M225NBEA

# ‘‘Employment’’ denotes U.S. total nonfarm employment growth, 
require(quantmod)
getSymbols('PAYEMS', src='FRED')
getSymbols('PCE', src='FRED')
PCE <- lag(PCE, k=(-1))
index(PCE) = as.yearmon(index(PCE))

# ‘‘ISM’’ denotes the ISM manufactur-ing index, 
USSLIND
# kein ISM

# ‘‘IP’’ denotes growth in real industrial produc-tion, 
INDPRO

# ‘‘CPI’’ denotes the inflation rate, 
CPIAUCSL
# ‘‘M2’’ is the growth in real money balances, 
getSymbols('M2', src='FRED')

# ‘‘Disp inc’’ is growth in real disposable personal income, 
getSymbols('DSPIC96', src='FRED')

# ‘‘TED’’ denotes the TED spread (the difference between 3-month interbank rate, Libor and 3-month T-bill rate), 
getSymbols('TB3MS', src='FRED')
getSymbols('USD3MTD156N', src='FRED')
getSymbols('TEDRATE', src='FRED')
# plot(TEDRATE); plot(USD3MTD156N-TB3MS)

# ‘‘Term’’ denotes the term spread (20-year maturity minus 3-month T-bill rate), 
# HMLFX is the return to the carry trade long-short portfolio


### SUNSPOTS
setwd('~/Dropbox/MS/')
volatilities <- readRDS('~/Dropbox/MS/Volatilities_ExclFullDev.rds')
eq1 <- readRDS('Excess_ExclFullDev.rds')
eq1 <- eq1['1976-03/']
summary(lm(eq1$Excl ~ sqrt(volatilities$Excl*12)))
head(volatilities)

fx_variance <- readRDS('fx_variance.rds')
fx_variance_ch <- diff(fx_variance) 
index(fx_variance) <- as.yearmon(index(fx_variance))
index(fx_variance_ch) <- as.yearmon(index(fx_variance_ch))
fx_variance <- fx_variance['1976-03/']
fx_variance_ch <- fx_variance_ch['1976-03/']

spot_num <- readRDS('~/R/spot_num.rds')
spot_ch <- diff(spot_num)[,1]['1976-03/2014']
spot_num <- spot_num[,1]['1976-03/2014']
# mean(spot_num)
spot_num <- spot_num-mean(spot_num)
asd=summary(lm(eq1$Excl ~ spot_num))
asd$coefficients[2,3]

# The conventional estimation approach with overlapping data is to use the Newey-West estimation procedure.

lm.beta = function (MOD) # https://stackoverflow.com/questions/24305271/extracting-standardized-coefficients-from-lm-in-r
{
  b <- summary(MOD)$coef[-1, 1]
  sx <- sd(MOD$model[-1])
  sy <- sd(MOD$model[1])
  beta <- b * sx/sy
  return(beta)
}


allfreq = function(x,y,z='nonoverlap',what='rsq') {
  # https://stackoverflow.com/questions/47123863/lm-beta-produces-different-results-with-standardized-coefficients-calculated-by
  # temp.summ <- summary(temp.lm)
  # temp.summ$coefficients <- unclass(coeftest(temp.lm, vcov. = NeweyWest)) # Newey!
  # temp.summ
  res = list()
  if(z=='nonoverlap') {
    for (i in c(1:36)) {
      data_x <- period.apply(x, endpoints(x, "months", k=i), sum)
      data_y <- period.apply(y, endpoints(y, "months", k=i), sum)
      if(what=='rsq') res[[i]] <- summary(lm(data_x ~ data_y))$r.squared
      if(what=='coef') res[[i]] <- summary(lm(data_x ~ data_y))$coefficients[2,1]
      if(what=='t') res[[i]] <- summary(lm(data_x ~ data_y))$coefficients[2,3]
      print(paste(i,res[[i]]))
      } }
  if(z=='rolling') {
    for (i in c(1:36)) {
      data_x <- rollapply(x, i, mean)
      # data_y <- rollapply(y, i, sum) # oder mean?
      data_y <- rollapply(y, i, mean)
      # if(what=='rsq') res[[i]] <- summary(lm(data_x ~ data_y))$r.squared 
      # if(what=='coef') res[[i]] <- summary(lm(data_x ~ data_y))$coefficients[2,1]
      # if(what=='t') res[[i]] <- summary(lm(data_x ~ data_y))$coefficients[2,3]
      if(what=='rsq') res[[i]] <- summary(lm(scale(data_x) ~ scale(data_y)))$r.squared 
      if(what=='coef') res[[i]] <- summary(lm(scale(data_x) ~ scale(data_y)))$coefficients[2,1]
      # if(what=='t') res[[i]] <- abs(summary(lm(scale(data_x) ~ scale(data_y)))$coefficients[2,3])
      if(what=='t') res[[i]] <- abs(summary(lm(data_x ~ data_y))$coefficients[2,3])
      print(paste(i,res[[i]]))
      } }
  names(res) <- seq(1:36)
  return(unlist(res))
}
allfreq(eq1$Excl,spot_num) # "1 0.00383816223631704"

luk=allfreq(eq1$Excl,spot_num,'rolling')
luk=allfreq(eq1$Excl,spot_ch,'rolling')
luk=allfreq(eq1$Excl,fx_variance_ch,'rolling')

luk=allfreq(eq1$Excl,spot_num,'rolling',what='coef')
luk=allfreq(eq1$Excl,sqrt(fx_variance*12),'rolling')
luk=allfreq(eq1$Excl,sqrt(fx_variance*12),'rolling',what='coef')
luk=allfreq(eq1$Excl,spot_num,'nonoverlap',what='coef')
luk=allfreq(eq1$Excl,spot_num,'rolling',what='t')
plot(luk)

index(PAYEMS) <- as.yearmon(index(PAYEMS)) ### PAYEMS
temp = na.omit(cbind(eq1$Excl,diff(PAYEMS)))
luk=allfreq(temp[,1],temp[,2],'rolling')

temp2 = apply.monthly(na.omit(TEDRATE), last) ### TEDRATE
index(temp2) <- as.yearmon(index(temp2))
TEDm <- temp2
temp2 = na.omit(cbind(eq1$Excl,temp2,diff(temp2)))
luk=allfreq(temp2[,1],temp2[,2],'rolling')
plot.zoo(temp2, plot.type='single')
plot(luk)
allfreq(eq1$Excl,volatilities$Excl,'rolling')

results
results <- list('sunspots'=allfreq(reg$Excl,reg$Sun,'rolling'),
                'fxvariance'=allfreq(reg$Excl,reg$Var,'rolling'),
                'liquidity'=allfreq(reg$Excl,reg$Ill,'rolling'),
                'tedspread'=allfreq(reg$Excl,reg$Ted,'rolling'),
                'payrolls'=allfreq(reg$Excl,reg$Pay,'rolling'))

results <- list('sunspots'=allfreq(reg$Excl,reg$Sun,'rolling','coef'),
                'fxvariance'=allfreq(reg$Excl,reg$Var,'rolling','coef'),
                'liquidity'=allfreq(reg$Excl,reg$Ill,'rolling','coef'),
                'tedspread'=allfreq(reg$Excl,reg$Ted,'rolling','coef'),
                'payrolls'=allfreq(reg$Excl,reg$Pay,'rolling','coef'))

results <- list('sunspots'=allfreq(reg$Excl,reg$Sun,'rolling','t'),
                'fxvariance'=allfreq(reg$Excl,reg$Var,'rolling','t'),
                'liquidity'=allfreq(reg$Excl,reg$Ill,'rolling','t'),
                'tedspread'=allfreq(reg$Excl,reg$Ted,'rolling','t'),
                'payrolls'=allfreq(reg$Excl,reg$Pay,'rolling','t'))

# results <- list('sunspots'=allfreq(eq1$Excl,spot_ch,'rolling'),
#                 'fxvariance'=allfreq(eq1$Excl,fx_variance_ch,'rolling'),
#                 'payrolls'=allfreq(temp[,1],temp[,2],'rolling'),
#                 'tedspread'=allfreq(temp2[,1],temp2[,3],'rolling'))
# results <- list('sunspots'=allfreq(eq1$Excl,spot_ch,'nonoverlap'), # NEW NEW
#                 'fxvariance'=allfreq(eq1$Excl,fx_variance_ch,'nonoverlap'),
#                 'payrolls'=allfreq(temp[,1],temp[,2],'nonoverlap'),
#                 'tedspread'=allfreq(temp2[,1],temp2[,3],'nonoverlap'))
# results <- list('sunspots'=allfreq(eq1$Excl,spot_ch,'rolling','coef'),
#                 'fxvariance'=allfreq(eq1$Excl,fx_variance_ch,'rolling','coef'),
#                 'payrolls'=allfreq(temp[,1],temp[,2],'rolling','coef'),
#                 'tedspread'=allfreq(temp2[,1],temp2[,3],'rolling','coef'))

results2 = do.call(cbind, results)
head(results2)
save('Macro_new4')
plot.zoo(results2, plot.type = 'single', col=seq(1:5), main='Macro Risks', ylab='', xlab='')
plot.zoo(results2[,c(1:4)], plot.type = 'single', col=seq(1:4), main='Macro Risks', ylab='', xlab=''); mtext('R-squared')

mtext('R-squared')
mtext('Standardized Coefficients')
mtext('t-stat Coefficients')
#  ylim=c(0,0.05)
legend('topleft',c(expression(paste(Delta,' sunspots')),
                   expression(paste(Delta,' fxvariance')),
                   expression(paste(Delta,' liquidity')),
                   expression(paste(Delta,' tedspread')),
                   expression(paste(Delta,' payrolls'))),col=seq(1:5), lty=1)
legend('bottomleft',c(expression(paste(Delta,' sunspots')),
                   expression(paste(Delta,' fxvariance')),
                   expression(paste(Delta,' liquidity')),
                   expression(paste(Delta,' tedspread')),
                   expression(paste(Delta,' payrolls'))),col=seq(1:5), lty=1)
legend('topleft',names(results)[1:4],col=seq(1:4), lty=1)
dev.off()


barplot(apply.yearly(eq1$Excl, mean))
barplot(apply.yearly(spot_num, sum))


asd=cbind(eq1$Dev,spot_num)
makequantilebarroso(asd)

plot.zoo(spot_num)
head(asdx)
save('sunspotsrisk', width = 9)
pdf('sunspotsrisk_2018.pdf', width = 9)
asd=cbind(eq1$Excl,spot_num)
asdx=apply.yearly(asd, colSums)
barplot(asdx[,1], names.arg = year(index(asdx)), col='grey', main='Returns vs. Sunspots risk', las=2)
add_legend("bottom", c('Yearly momentum returns','Sunspots'), col=c('darkgrey','red'), lty = 1, cex=0.8, lwd=2, bty='n')
# barplot(asdx[,1], names.arg = year(index(asdx)), col='grey', border='white')
par(new = T)
plot.zoo(asdx[,2], col='red', ylab='', xlab='', xaxt='n', yaxt='n', bty='n')
axis(side = 4)
dev.off()

summary(lm(eq1$Excl['1991-01/'] ~ FXILL['/2014']))
summary(lm(eq1$Dev['1991-01/'] ~ FXILL['/2014']))
barplot(apply.yearly(eq1$Excl['1991-01/'], mean))
plot.zoo(FXILL)
plot.zoo(eq1$Excl['1991-01/'])

bam[,'INDORU..EO.']
bam[,'THABAH..EO.']
bam[,'MALADL..EO.']

bidaskmonthly = bam$mean['1983-11/2014']
index(bidaskmonthly) <- as.yearmon(index(bidaskmonthly))
summary(lm(eq1$Dev['1983-11/'] ~ bidaskmonthly))
summary(lm(eq1$Excl['1983-11/'] ~ bidaskmonthly))
plot.zoo(bidaskmonthly, xlab='', ylab='')
plot.zoo(bidaskmonthly['1996/2001'], xlab='', ylab='')
par(new = T)
barplot(apply.yearly(eq1$Excl['1997/2005'], mean))
barplot(eq1$Excl['1983-11/'])



save('bidaskvsreturns', width=9)
pdf('bidaskvsreturns_2018.pdf', width=9)
# plot.zoo(bidaskmonthly, xlab='', ylab='', bty='n', yaxt='n', main='Returns vs. FX Bid/Ask %')
# par(new = T)
asdx = apply.yearly(eq1$Excl['1983-11/'], sum)
barplot(asdx, names.arg=year(index(asdx)), col='grey', las=2, main='Returns vs. FX Bid/Ask %') # xaxt='n') # , yaxt='n') # /100
add_legend("bottom", c('Yearly momentum returns','FX Bid/Ask %'), col=c('darkgrey','red'), lty = 1, cex=0.8, lwd=2, bty='n')
mtext('Datastream 43 Currencies')
par(new = T)
plot.zoo(bidaskmonthly, xlab='', ylab='', xaxt='n', yaxt='n', col='red', bty='n')
axis(side = 4)
dev.off()

# save('bidaskvsreturns2', width=9)
pdf('illvsreturns_2018.pdf', width=9)
# plot.zoo(FXILL, xlab='', ylab='', bty='n', yaxt='n', main='Returns vs. FX Illiquidity Factor')
# mtext('Factor fro')
# par(new = T)
asdx = apply.yearly(eq1$Excl['1991-01/'], sum)
barplot(asdx, col='darkgrey', las=2, names.arg=year(index(asdx)), main='Returns vs. FX Illiquidity Factor') # , yaxt='n')
add_legend("bottom", c('Yearly momentum returns','FX Illiquidity'), col=c('grey','red'), lty = 1, cex=0.8, lwd=2, bty='n')
par(new = T)
plot.zoo(FXILL, xlab='', ylab='', xaxt='n', yaxt='n', col='red', bty='n')
# plot.zoo(TEDm['1991-01/2014'], xlab='', ylab='', xaxt='n', yaxt='n', col='red', bty='n')
axis(side = 4)
dev.off()

plot.zoo(FXILL['2008'])
plot.zoo(eq1$Excl['2008'])

# C. Comovement across Asset Classes // ASNESS
# Global FUNDING liquidity shocks negatively impact value returns and positively affect momentum returns, 
# but global MARKET liquidity shocks do not seem to have much impact, consistent with the U.S. liquidity measures.
nrow(monthly$Value['1976-03/2014'])
nrow(eq1$Excl['1976-03/2014'])
cor(monthly$Value['1976-03/2014'],eq1$Excl['1976-03/2014'])
cor(monthly$Value['1976-03/2014'],eq1$Dev['1976-03/2014'])

tail(monthly$Barroso['2014'])
tail(monthly)
plot.zoo(eq1$Excl)

### ###
head(temp)
x <- eq1$Excl['1976-03/2014']

head(TEDm)
reg <- cbind(eq1$Excl,eq1$Dev,sqrt(fx_variance)*12,FXILL,diff(PAYEMS),TEDm,spot_num)['1991/2014']
reg <- cbind(eq1$Excl,eq1$Dev,sqrt(fx_variance)*12,FXILL,diff(PAYEMS),TEDm,spot_num)['1986/2014']
reg <- cbind(eq1$Excl,eq1$Dev,sqrt(fx_variance)*12,FXILL,diff(PAYEMS),TEDm,spot_num,FF$Mkt.RF,FF$SMB,FF$HML)


reg <- cbind(eq1$Excl,eq1$Dev,sqrt(fx_variance)*12,FXILL,log(PAYEMS),TEDm,log(PCE),spot_num,FF$Mkt.RF,FF$SMB,FF$HML)['1976-03/2014']
# reg <- cbind(eq1$Excl,eq1$Dev,sqrt(fx_variance)*12,FXILL,diff(PAYEMS),TEDm,spot_num,FF$Mkt.RF,FF$SMB,FF$HML)
colnames(reg) <- c('Excl','Dev','Var','Ill','Pay','Ted','PCE','Sun','Mkt','SMB','HML')
reg <- cbind(eq1$Excl,eq1$Dev,diff(reg)[,c(3:ncol(reg))])['1976-03/2014']
reg <- cbind(eq1$Excl,eq1$Dev,diff(reg)[,c(3:ncol(reg))])['1991/2014']
reg <- cbind(eq1$Excl,eq1$Dev,diff(reg)[,c(3:ncol(reg))])['1985/2014']

head(reg)
head(TEDm)
head(reg['1990/'], 20)
# plot(diff(log(PCE))); plot(ROC(PCE)) # same
plot(diff(log(PAYEMS)))

### NEW
results <- list('sunspots'=allfreq(eq1$Excl,spot_num,'nonoverlap'),
                'fxvariance'=allfreq(eq1$Excl,fx_variance,'nonoverlap'),
                'tedspread'=allfreq(na.omit(cbind(eq1$Excl,TEDm))[,1],na.omit(cbind(eq1$Excl,TEDm))[,2],'nonoverlap'))
results <- list('sunspots'=allfreq(reg$Excl,reg$Sun,'nonoverlap'),
                'fxvariance'=allfreq(reg$Excl,reg$Var,'nonoverlap'),
                'tedspread'=allfreq(reg$Excl,reg$Ted,'nonoverlap'))
### NEW
plot(diff(PCE))
plot(diff(PAYEMS))

require(stargazer)
stargazer(cbind(
  t(cbind('Var'=round(summary(lm(Excl ~ Var, data = reg))$r.squared*100, 3),
      'Ill'=round(summary(lm(Excl ~ Ill, data = reg))$r.squared*100, 3),
      'Pay'=round(summary(lm(Excl ~ Pay, data = reg))$r.squared*100, 3),
      'Ted'=round(summary(lm(Excl ~ Ted, data = reg))$r.squared*100, 3),
      'PCE'=round(summary(lm(Excl ~ PCE, data = reg))$r.squared*100, 3),
      'Sun'=round(summary(lm(Excl ~ Sun, data = reg))$r.squared*100, 3),
      'Mkt'=round(summary(lm(Excl ~ Mkt, data = reg))$r.squared*100, 3),
      'SMB'=round(summary(lm(Excl ~ SMB, data = reg))$r.squared*100, 3),
      'HML'=round(summary(lm(Excl ~ HML, data = reg))$r.squared*100, 3),
      'FF'=round(summary(lm(Excl ~ Mkt + SMB + HML, data = reg))$r.squared*100, 3))),
  t(cbind('Var'=round(summary(lm(Dev ~ Var, data = reg))$r.squared*100, 3),
      'Ill'=round(summary(lm(Dev ~ Ill, data = reg))$r.squared*100, 3),
      'Pay'=round(summary(lm(Dev ~ Pay, data = reg))$r.squared*100, 3),
      'Ted'=round(summary(lm(Dev ~ Ted, data = reg))$r.squared*100, 3),
      'PCE'=round(summary(lm(Dev ~ PCE, data = reg))$r.squared*100, 3),
      'Sun'=round(summary(lm(Dev ~ Sun, data = reg))$r.squared*100, 3),
      'Mkt'=round(summary(lm(Dev ~ Mkt, data = reg))$r.squared*100, 3),
      'SMB'=round(summary(lm(Dev ~ SMB, data = reg))$r.squared*100, 3),
      'HML'=round(summary(lm(Dev ~ HML, data = reg))$r.squared*100, 3),
      'FF'=round(summary(lm(Dev ~ Mkt + SMB + HML, data = reg))$r.squared*100, 3)))))

summary(lm(Excl ~ Var, data = reg))
summary(lm(Excl ~ Ill, data = reg))
summary(lm(Dev ~ Ted, data = reg))


FF <- readRDS('~/R/programs/MomentumHasItsMoments_programs/F-F_Research_Data_Factors.rds')
head(FF)

checkx <- cbind(eq1$Excl, TEDm)['1986/2014']
### CHECK TED
for (i in c(1:36)) {
  data_x <- rollapply(checkx[,1], i, mean)
  data_y <- rollapply(checkx[,2], i, mean)
  # print(summary(lm(scale(data_x) ~ scale(data_y)))$r.squared)
  print(summary(lm(scale(data_x) ~ scale(data_y))))
}

###

head(df.spot2)
test <- diff(log(df.spot2))
sort(apply(test, 2, function(x) sd(na.omit(x))))
plot(df.spot2$INR); plot(df.spot2$KWD)
sort(apply(asd[[1]]$ind.returns$rx, 2, function(x) sd(na.omit(x))))
ncol(fx_variance)
nocol = sort(colMeans(fx_variance, na.rm=T)) # 2018
fx_variance <- readRDS('fx_variance_ncol48.rds')
nocol = names(nocol[(43-14):43])
nocol
# ATS,KRW,AUD,CHF,NZD,RUB,CZK,UAH,HUF,BRL,PLN,ISK,ZAR,MYR,IDR # SO WAR ES PRE 2018
# nocol=c('ATS','KRW','AUD','CHF','NZD','RUB','CZK','UAH','HUF','BRL','PLN','ISK','ZAR','MYR','IDR')
# ALL fxvar(inkl. 48) SEK,ESP,PTE,KRW,CHF,AUD,NZD,RUB,CZK,HUF,PLN,BRL,ZAR,UAH,MYR,ISK,IDR 
# ALL fxvar(inkl. 43) SEK,ESP,PTE,KRW,CHF,AUD,NZD,RUB,CZK,HUF,PLN,BRL,ZAR,UAH,ISK  # 15

testy <- readRDS('~/Dropbox/MS/dailyrx.rds')
testy <- testy^2
testy <- apply.monthly(testy, colSums)
sort(apply(testy, 2, function(x) mean(na.omit(x))))
apply(testy, 2, function(x) mean(na.omit(x)))

testx <- rbind(apply(testy, 2, function(x) mean(na.omit(x))),
               apply(asd[[1]]$ind.returns$rx, 2, function(x) mean(na.omit(x)))[c(4:51)])

testx <- rbind(apply(asd[[1]]$ind.returns$rx, 2, function(x) sd(na.omit(x))),
               apply(asd[[1]]$ind.returns$rx, 2, function(x) mean(na.omit(x))))
testx <- testx[,c(4:ncol(testx))]

testx <- testx[,!colnames(testx) %in% c('MYR','IDR','EGP','KWD','SAR','HKD')]
testx <- testx[,colnames(testx) %in% colnames(in.portfolio)]

save('scatterscat4')
# save('scatterscat2')
pdf('Scatter_2018_a2.pdf')
# plot(testx[1,],testx[2,], xlab='Volatility', ylab='Profit Contribution', main='Risk/Return Trade-off') # ylab='Excess Return'
plot(testx[1,],testx[2,], xlab='Volatility', ylab='Excess Return', main='Risk/Return Trade-off') # ylab='Excess Return'
mtext('Long Only')
abline(lm(testx[2,]~testx[1,]), col="red")
# abline(robustbase::ltsReg(testx[2,]~testx[1,]), col="red")
# plot(testx[1,],testx[2,], xlab='var', ylab='mean', main='Risk/Return Trade-off')
# abline(lm(testx[2,]~testx[1,]), col="red")
text(testx[1,], testx[2,], labels=colnames(testx), cex=0.7, pos=1, col='red')
dev.off()


### 2018
# find pegged
df.spot <- readRDS('~/Dropbox/MS/df.spot.rds')
plot.zoo(df.spot$ISK)
plot.zoo(df.spot$EGP)
abs(diff(log(df.spot)))<0.0001
# a = readRDS('~/Dropbox/MS/portfolio_excl.rds')
# a$ind.returns$sp
in.portfolio <- readRDS('~/Dropbox/MS/portfolio_excl.rds')$in.portfolio
in.portfolio <- xts(in.portfolio[,4:ncol(in.portfolio)], as.yearmon(paste(in.portfolio$year,'-',in.portfolio$month,sep='')))
in.portfolio <- lag(in.portfolio, k=1) # lag due to code
rx <- readRDS('~/Dropbox/MS/portfolio_excl.rds')$ind.returns$rx
# rx <- readRDS('~/Dropbox/MS/portfolio_excl.rds')$ind.returns$sp ### ATTENTION
rx <- xts(rx[,4:ncol(rx)], as.yearmon(paste(rx$year,'-',rx$month,sep='')))
all.equal(colnames(rx),colnames(in.portfolio))
ip <- in.portfolio
head(ip)
ncol(ip)
ip[ip %in% c(0)] <- NA
# ip[ip %in% c(-1,1)] <- 0
# test2 <- Reduce('+', list(ip,rx)) # WRONG HERE
test2 <- ip*rx
rxmom <- colMeans(test2, na.rm=T)
rxmom <- colSums(test2, na.rm=T)
rxmom <- apply(rx, 2, function(x) mean(na.omit(x)))
sdmom = apply(rx, 2, function(x) sd(na.omit(x))) # DAS IST FALSCH
sdmom = colMeans(fx_variance, na.rm=T) # TRY NEW
sdmom = sqrt(colMeans(fx_variance, na.rm=T)*12) # TRY NEW
all.equal(names(sdmom),names(rxmom))
testx = rbind(sdmom,rxmom)

###


asd <- readRDS('~/Dropbox/MS/returns_full_raw.rds')
col <- colnames(asd[[1]]$ind.returns$rx)[c(4:ncol(asd[[1]]$ind.returns$rx))]
col
nocol=c('ATS','KRW','AUD','CHF','NZD','RUB','CZK','UAH','HUF','BRL','PLN','ISK','ZAR','MYR','IDR') # ZAR=13
res <- list()
for (i in c(14:1)) { 
  print(col[!col %in% nocol[i:15]])
  res[i] <- mean(apply(asd[[1]]$ind.returns$rx, 2, function(x) sd(na.omit(x)))[col[!col %in% nocol[i:15]]]) }
### NEW 2018
res <- list()
for (i in c(15:1)) { 
  if(i==15) luktus <- currencies_excl
  if(i!=15) luktus <- currencies_excl[!currencies_excl %in% nocol[(i+1):15]]
  print(length(luktus)); print(luktus)
  res[i] <- mean( sort(colMeans(fx_variance, na.rm=T)[luktus]) ) 
  }
res

### 
resf <- cbind(unlist(res),unlist(res2)/100,unlist(res3))
save('riskreturntrade')
plot(resf[,1], ylim=c(min(resf[,c(1:2)]),max(resf[,c(1:2)])), xlab='# more currencies', ylab='', type='o', main='Risk/Return Trade-off')
mtext('Portfolio Statistics')
points(resf[,2], col='red')
par(new = T)
plot(resf[,3], xlab='', ylab='', xaxt='n', yaxt='n', col='blue')
axis(side = 4)
dev.off()
mean(apply(asd[[1]]$ind.returns$rx, 2, function(x) sd(na.omit(x)))[col[!col %in% nocol]])
mean(apply(asd[[1]]$ind.returns$rx, 2, function(x) sd(na.omit(x)))[col])

reff <- cbind(sqrt(resf[,1]*12)*100,
              resf[,2]*sqrt(12)*100,
              resf[,3]*12)
pdf('riskreturntrade_new2.pdf')
plot(reff[,1], ylim=c(min(reff[,c(1:2)]),max(reff[,c(1:2)])), xlab='# more (idiosyncratic) volatile currencies', ylab='', 
     type='p', main='Risk/Return Trade-off')
mtext('Portfolio Statistics')
points(reff[,2], col='red')
par(new = T)
# barplot(reff[,3], xlab='', ylab='') #xaxt='n', yaxt='n', col='blue')
plot(reff[,3], type='l', xlab='', ylab='', xaxt='n', yaxt='n', col='blue')
axis(side = 4)
legend('topleft',c('Idiosyncratic Volatility (lhs)','Momentum Volatility (lhs)','Momentum Return (rhs)'),pch=c(1,1,NA),lty=c(NA,NA,1),col=c('black','red','blue'))
dev.off()

reff[,3]/reff[,2]

### ###
# xcheck <- read.csv('~/Downloads/Diss/rawdata/10_Portfolios_Prior_12_2.txt') # wrong; needs mix of size and mom
ycheck <- read.table('~/Downloads/Diss/new/F-F_Momentum_Factor.TXT', stringsAsFactors = FALSE)
head(ycheck)
ycheck <- xts(ycheck, as.yearmon(rownames(ycheck), '%Y%m'))

check <- na.omit(cbind(eq1$Excl, ycheck))
plotzoo(cumsum(check/100), plot.type = 'single')
cor(check[,1], check[,2])

check <- na.omit(cbind(eq1$Dev, ycheck))
cor(check[,1], check[,2])
