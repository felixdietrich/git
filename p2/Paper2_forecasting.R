# https://github.com/KevinKotze/tsm/blob/master/R/cw.R # - habe ich das schon gemacht?

### CHECK SEPT 2017
test <- pred_os
head(test)

apply(pred_is, 2, max)*100
apply(pred_is, 2, min)*100
0.1*-0.038

test <- pred_is
msfe = function(x) {  (x - test$ahead) ^ 2 }
check <- cbind(msfe(test$random),msfe(test$th))
check <- cbind(msfe(test$random),msfe(test$ar))
check <- cbind(msfe(test$mean),msfe(test$th))
colMeans(check)

check$diff <- check[,1]-check[,2]
pdf('~/Dropbox/newdiss/pics/Performance_IS.pdf', height = 7)
plot.zoo(cumsum(check$diff), xlab='', ylab='Cumulative SSE Difference')
mtext('IS-fitted Model')
dev.off()

# OOS R2
head(test)
check <- cbind(msfe(test$mean),msfe(test$risk))
check <- cbind(msfe(test$mean),msfe(test$riskch))
check <- cbind(msfe(test$mean),msfe(test$th))
check <- cbind(msfe(test$mean),msfe(test$ar))
check <- cbind(msfe(test$mean),msfe(test$random))

(1-colSums(check)[2]/colSums(check)[1])*100 # 0.006868235

### CHECK AUG 2017
head(trading)
head(pred_os)
check <- trading[,c('carry','ahead','riskch',"riskchpercdummy_l",'riskchpercdummy_u')]
check$pred <- pred_is$th
check$random <- pred_is$random
check$mean <- pred_is$mean

check$pred <- pred_os$th
check$random <- pred_os$random
check$mean <- pred_os$mean
check <- check['2004-12-30/']
check$c1 <- check$riskch*check$riskchpercdummy_l
check$c2 <- check$riskch*check$riskchpercdummy_u
check$intercept <- models_IS$th$coefficients[1]
check$coef1 <- models_IS$th$coefficients[2]
check$coef2 <- models_IS$th$coefficients[3]
head(check)
check$intercept <- models_OS$th$coefficients[1]
check$coef1 <- models_OS$th$coefficients[2]
check$coef2 <- models_OS$th$coefficients[3]
check$pred2 <- check$intercept+check$c1*check$coef1+check$c2*check$coef2
all.equal(check$pred,check$pred2, check.attributes=FALSE)

check_n <- check[check$pred<0]
check_n <- check[check$ahead<0]
check_n <- check[check$pred>0]
check_n <- check[check$ahead>0]
accuracy(as.numeric(check_n[,'ahead']), as.numeric(check_n[,'pred']))
accuracy(as.numeric(check_n[,'ahead']), as.numeric(check_n[,'random']))
accuracy(as.numeric(check_n[,'ahead']), as.numeric(check_n[,'mean']))
options(scipen = 999)
min(pred_is$th[pred_is$th<0])
min(pred_is$ar[pred_is$ar<0])

### GENERAL CHECKING ----
require(forecast)
mean(sqrt((test[,'ahead'] - test[,'th']) ^ 2)); mean(abs((test[,'ahead'] - test[,'th']))) # 2 MAE definitions
accuracy(as.numeric(test[,'ahead']), as.numeric(test[,'th']))

### OUT-OF-SAMPLE CHECK ----
# https://stats.stackexchange.com/questions/92498/forecasting-time-series-regression-in-r-using-lm
# But for one step ahead forecast in this case, no new data is required. 
xx1=trading['/2010']
xx2=trading['2011/']
rwtest=lm(ahead ~ carry, data=xx1) 
require(tseries)
rwtest2=ar.ols(as.timeSeries.xts(xx1$carry), order.max = 1) 
xts(fitted(rwtest2), as.Date(attributes(ff)$positions))[1:5] # hier haben forecasts correctes date
predict.lm(rwtest)[1:5]
fitted(rwtest)[1:5]
OOStest=cbind(as.xts(predict.lm(rwtest, xx2), dateFormat="Date"),rwtest$coefficients[1]+xx2$carry*rwtest$coefficients[2])
all.equal(OOStest[,1],OOStest[,2], check.attributes=FALSE)

### ROLLING FORECAST ERRORS ----
ferrors <- (test[,'ahead'] - test[,'th']) ^ 2
testfor = function(x) { sqrt(mean(x)) } 
plot(rollapply(ferrors, 250, testfor))
head(rollapply(ferrors, 1, testfor)); head(sqrt(ferrors))

ferrors <- cbind(test[,'ahead'],test[,'th'])
testfor2 = function(x) { 
  a=as.numeric(x[,1]); b=as.numeric(x[,2])
  accuracy(a, b)[,'RMSE'] } 
plot(rollapply(ferrors, 250, testfor2, by.column = FALSE))

### Rolling model (!!!) not the same as ROLLING EVALUATION ----
rsqpred = function(x,y) { 
  model=lm(y, data=x)
  res1=summary(model)['r.squared']$r.squared
  names(res1)='rsquared'
  res2=summary(model)$coefficients[,3]
  names(res2)=rep('significance',length(res2))
  res3=summary(model)$coefficients[,1]
  names(res3)=rep('estimate',length(res3))
  res4=last(predict.lm(model, x)) ### ROLLING PREDICTION (OUT OF SAMPLE, weil last (?))
  names(res4)='prediction'
  # res5=last(model$model$ahead) # access original series from model
  res=c(res1,res2,res3,res4)
  return(res)
}

check <- rsqpred(trading['2000'],'ahead ~ carry')
check[4]+check[5]*last(trading['2000']$carry) # same as column 'prediction'

adsd1=rollapply(trading, 250, rsqpred, y='ahead ~ riskch:riskchpercdummy_l + riskch:riskchpercdummy_u', by.column = F)
adsd2=rollapply(trading, 250, rsqpred, y='ahead ~ carry', by.column = F)
# adsd3=rollapply(trading, 250, rsqpred, y='ahead ~ 0', by.column = F) # how to get Rsquared for random walk?

### NEW SEPT
adsd1=rollapply(trading, 250, rsqpred, y='ahead ~ riskch:riskchpercdummy_l + riskch:riskchpercdummy_u', by.column = F)
test <- na.omit(cbind(pred_is$mean,adsd1$prediction,pred_is$ahead))
msfe = function(x) {  (x - test$ahead) ^ 2 }
check <- cbind(msfe(test$mean),msfe(test$prediction))
colMeans(check)
check$diff <- check[,1]-check[,2]
check$cum <- cumsum(check$diff)
check$block <- index(check) %in% c(as.Date(c('1998-10-12'))+c(-5:5), # sharp appreciation of the yen against the US dollar between 6 and 8 October 1998
                                   as.Date(c('2007-08-15'))+c(-5:5), # Quant Meltdown
                                   as.Date(c('2008-03-14'))+c(-5:5), # Bear Stearns
                                   as.Date(c('2008-10-21'))+c(-5:5), # Lehman
                                   as.Date(c('2011-03-17'))+c(-5:5), # Fukushima
                                   as.Date(c('2011-08-08'))+c(-5:5)) # US downgrade

check <- as.zoo(check)
pdf('~/Dropbox/newdiss/pics/RollingPerformance_123.pdf', height = 6)
plot(check$cum, xlab='', ylab='Cumulative SSE Difference')
mtext('1Y Rolling Model')
xblocks(check$block==1, col=rgb(1,0,0))
dev.off()
trading['2011-03-10/2011-03']
test['2011-03-10/2011-03']
trading['1998-10']
test['1998-10']
### END NEW SEPT

# plot all results of rolling model
# plotf = function(x) { plot(as.zoo(x), ylab='', xlab='', main=attributes(x)$dimnames[[2]])
#   abline(h = 0, col='red', lty='dotted')
#   abline(h = mean(x, na.rm=T), col='blue', lty='solid') }
# par(mfrow = c(3,3))
# lapply(adsd1, plotf)
# significance on a rolling basis (is less?)
# plot(as.zoo(adsd1[,3]))
# abline(h=mean(adsd1[,3], na.rm=T), col='red')

rsqtest=as.zoo(na.omit(cbind(adsd1$rsquared,adsd2$rsquared)))
pdf(file = "~/Dropbox/newdiss/pics/RollRsquared.pdf", width = 5, height = 5, bg='white') 
plot(rsqtest[,1], ylim=c(min(rsqtest),max(rsqtest)), type='n', ylab='', xlab='', main='R-Squared')
mtext('Rolling 1-Year Model', cex=0.8)
lines(rsqtest[,1], col='red')
lines(rsqtest[,2], col='blue')
legend('topleft', c('Threshold model','AR(1) model'), lwd=2, col=c('red','blue'), lty=c(1,1), cex=0.8) 
dev.off()

### GENERAL CHECK ----
require(forecast)
# sqrt(mean((trading[,'ahead'] - trading[,8]) ^ 2 , na.rm = T )) # RMSE
# mean( abs(trading[,'ahead'] - trading[,8]) , na.rm = T )
forecast_test <- cbind(0,trading$ahead)
accuracy(0, as.numeric(trading$ahead)) # RMSE 0.004466786 ????? muss falsch sein
accuracy(as.numeric(forecast_test[,1]), as.numeric(forecast_test[,2]))

accuracy(as.numeric(test$th), as.numeric(test$ahead)) # RMSE 0.00630702
accuracy(as.numeric(test$ar), as.numeric(test$ahead)) # RMSE 0.006342733

accuracy(as.numeric(adsd1$prediction), as.numeric(trading$ahead)) # RMSE 0.006178313 nice (aber u.a. weil start different see below)
accuracy(as.numeric(adsd1$prediction), as.numeric(trading$carry)) # RMSE 0.006061574 
plot(as.zoo((trading[,'ahead'] - adsd1$prediction) ^ 2)) 
plot(as.zoo((trading[,'ahead'] - 0) ^ 2)) # start different

testnew=na.omit(cbind(trading[,'ahead'],adsd1$prediction,0,0.0001884791)) ### MAKE SAMPLES COMPARABLE
accuracy(as.numeric(testnew[,2]), as.numeric(testnew[,1]))
accuracy(as.numeric(testnew[,3]), as.numeric(testnew[,1]))
accuracy(as.numeric(testnew[,4]), as.numeric(testnew[,1]))

accuracy(as.numeric(adsd2$prediction), as.numeric(trading$ahead)) # RMSE 0.006241352 
0.006241352/0.006178313
accuracy(as.numeric(adsd1$prediction), as.numeric(trading$ahead))
# it is important to recall that RMSE has the same unit as the dependent variable (DV).
### END GENERAL CHECK

### DIEBOLD MARIANO FORECAST EQUALITY ----
eval <- test # all forecast predictions
dm.test(as.numeric(eval$ahead)-as.numeric(eval$th), as.numeric(eval$ahead)-as.numeric(eval$random), alternative = 'less') #checked: egal wierum
# dm.test(as.numeric(eval$ahead)-as.numeric(eval$th), as.numeric(eval$ahead)-as.numeric(eval$X0))[[1]] # DM statistic
# dm.test(as.numeric(eval$ahead)-as.numeric(eval$th), as.numeric(eval$ahead)-as.numeric(eval$X0))[[4]] # p.value
dm.test(as.numeric(eval$ahead)-as.numeric(eval$ar), as.numeric(eval$ahead)-as.numeric(eval$random), alternative = 'less') #forecast package
dm.test(as.numeric(eval$ahead)-as.numeric(eval$th), as.numeric(eval$ahead)-as.numeric(eval$ar), alternative = 'less') #forecast package
# The null hypothesis is that the two methods have the same forecast accuracy. 
# For alternative="less", the alternative hypothesis is that method 2 is less accurate than method 1.
# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1.
dm.test(as.numeric(eval$ahead)-as.numeric(eval$ar), as.numeric(eval$ahead)-as.numeric(eval$th), alternative = 'greater')
dm.test(as.numeric(eval$ahead)-as.numeric(eval$ar), as.numeric(eval$ahead)-as.numeric(eval$th), alternative = 'two.sided')


### FELIX AUG 2017
# clark-west?
dm.test
e1 <- as.numeric(eval$ahead)-as.numeric(eval$ar) # that you hope to beat (forecast errors e t)
e2 <- as.numeric(eval$ahead)-as.numeric(eval$th) # your estimated model (forecast errors epsilon t)
# when the models behind e and epsulon are nested (say, e is generated by a special case of the model that generates epsilon),
h = 1
d <- c(abs(e1))^2 - c(abs(e2))^2
d <- 2*(e1)*(e1-e2) # 3.702916 Statistics
# d <- 2*(abs(e1))*(abs(e1)-abs(e2)) # 3.533815 

d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", plot = FALSE)$acf[, , 1]
d.var <- sum(c(d.cov[1], 2 * d.cov[-1]))/length(d)
dv <- d.var

STATISTIC <- mean(d, na.rm = TRUE)/sqrt(dv)
STATISTIC
n <- length(d)
k <- ((n + 1 - 2 * h + (h/n) * (h - 1))/n)^(1/2)
STATISTIC <- STATISTIC * k
pt(STATISTIC, df = n - 1)
### FELIX AUG 2017


### ROLLING MSE but this is IN-SAMPLE. of any use??
head(trading)
sqrt(mean((trading[,'ahead'] - threspred) ^ 2 , na.rm = T )) # 0.006392296 for old data / 0.00630702 for new data
sqrt(mean((trading[,'ahead']) ^ 2 , na.rm = T )) # 0.006436797 for old data / 0.0063457 for new data
sqrt(mean((eval[,'ahead'] - eval$ar) ^ 2 , na.rm = T )) # 0.006392296 for old data / 0.00630702 for new data
head(threspred)
plot(as.zoo(rollapply((trading[,'ahead'] - threspred) ^ 2, 252, mean)), ylab='', xlab='')


### MeanForecastErrors ----
eval <- test['2007']
pdf(file = "~/Dropbox/newdiss/pics/MeanForecastErrors.pdf", width = 5, height = 5, bg='white') 
plot(as.zoo(abs(eval[,'ahead'] - eval$th)), ylab='', xlab='', main='Mean Absolute Forecast Errors') # , type='n'
lines(as.zoo(abs(eval[,'ahead'] - eval$ar)), col='blue', lty='dotted')
lines(as.zoo(abs(eval[,'ahead'])), col='red', lty='dotted') 
legend('topleft', c('Threshold model','AR(1) model','Random walk'), lwd=2, col=c('black','blue','red'), lty=c(3,3,3), cex=0.8) 
dev.off()
# lines(as.zoo(abs(eval[,'ahead'] - eval$th)), col='black') 

# plot(rollapply((threspred - trading[,'ahead']) ^ 2, 252, mean))
# plot(rollapply((trading[,'ahead'] - arpred) ^ 2, 252, mean))
# plot(as.zoo((trading[,'ahead'] - arpred) ^ 2), ylab='', xlab='') # too noisy

### SUPERIOR PREDICTABILITY ----
### based on in-sample fit
plot(rollapply((trading[,'ahead']) ^ 2, 252, mean))
# SUPERIOR? man MUSS rolling nehmen denn sonst Error in plot.window(...) : need finite 'ylim' values
sup1=rollapply((trading[,'ahead'] - threspred)^2, 252, mean)
# sup1=rollapply((eval[,'ahead'] - eval$ar)^2, 252, mean)
sup2=rollapply((trading[,'ahead'])^2, 252, mean)
ratio=sup1/sup2
mean(sup1, na.rm=T)/mean(sup2, na.rm=T)
pdf(file = "~/Dropbox/newdiss/pics/RMSERatios.pdf", width = 5, height = 5, bg='white') 
plot(as.zoo(ratio), main='Ratio of RMSE', ylab='', xlab='')
mtext('Threshold model vs. Random walk', cex=0.8)
abline(h=mean(ratio, na.rm=T), col='blue')
legend('bottomleft', paste('mean',round(mean(ratio, na.rm=T), digits=4)), lwd=2, col='blue',cex=0.8)
abline(h=1, col='red', lty='dotted')
dev.off()
0.00630702/0.0063457 # ratio of THRESHOLD vs. RANDOM WALK
0.006342733/0.0063457 # ratio of AR vs. RANDOM WALK

### based on rolling fit
# sup1=rollapply((trading[,'ahead'] - adsd1$prediction)^2, 252, mean)
sup1=rollapply((testnew[,'ahead'] - testnew[,'prediction'])^2, 252, mean)
sup2=rollapply((testnew[,'ahead'])^2, 252, mean)
ratio=sup1/sup2
mean(sup1, na.rm=T)/mean(sup2, na.rm=T)
pdf(file = "~/Dropbox/newdiss/pics/RMSERatiosOOS.pdf", width = 5, height = 5, bg='white') 
plot(as.zoo(ratio), main='Ratio of RMSE', ylab='', xlab='')
mtext('Threshold model (Rolling 1-Year Model) vs. Random walk', cex=0.8)
abline(h=mean(ratio, na.rm=T), col='blue')
legend('bottomleft', paste('mean',round(mean(ratio, na.rm=T), digits=4)), lwd=2, col='blue',cex=0.8)
abline(h=1, col='red', lty='dotted')
dev.off()
