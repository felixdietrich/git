### 2017
require(xts)
require(lmtest) # coeftest
require(sandwich); require(lm.beta) # lm.beta
require(TTR) # runPercentRank {TTR}
require(car) # durbinWatsonTest
require(tseries) # adf.test
require(stargazer)
# require(TTR); require(xlsx); require(reshape2) # require(dynlm)

options(scipen = 999)
# setwd("~/Downloads/Diss/")

xquantile = function(x,y,z) { if(z=='low') return( x[which(x<quantile(x, probs=c(1-y, y), na.rm=T)[1]),] )
  if(z=='high') return( x[which(x>quantile(x, probs=c(1-y, y), na.rm=T)[2]),] ) }
rquantile = function(x,y,z,n) { if(z=='low') return( x[runPercentRank(x, n=n)<=(1-y)] )
  if(z=='high') return( x[runPercentRank(x, n=n)>=y] ) }

perc <- function(x,n) { runPercentRank(x, n=n) }

rollit = function(x) { cbind(rollapply(x, 5, sum), 
                             rollapply(x, 10, sum), 
                             rollapply(x, 22, sum), 
                             rollapply(x, 44, sum)) }

rollitlag = function(x, what = sum) { cbind(lag(rollapply(x, 5, what), k=-5), 
                                            lag(rollapply(x, 10, what),k=-10), 
                                            lag(rollapply(x, 22, what), k=-22), 
                                            lag(rollapply(x, 44, what), k=-44)) }

makedataset = function(var1,var2,option='') 
{ 
  dat <- na.locf( cbind(get(var1),get(var2)) ) # na.omit(data) ist difference zu vorher
  colnames(dat) <- c('carry','risk') 
  data <- cbind(dat, 
                diff(log(dat$carry)), rollit(diff(log(dat$carry))),
                lag(diff(log(dat$carry)),k=-1), rollitlag(diff(log(dat$carry))), 
                diff(log(dat$risk)), rollit(diff(log(dat$risk))) )
  if(option=='withreturns') {
    data <- cbind(cumsum(dat$carry),dat$risk, 
                  dat$carry, rollit(dat$carry),
                  lag(dat$carry,k=-1), rollitlag(dat$carry), 
                  diff(log(dat$risk)), rollit(diff(log(dat$risk))) )
  }
  colnames(data)=c('carry','risk', paste(rep(c('carry','carry','risk'), each=5),c(1,5,10,22,44),rep(c('','l',''), each=5),sep='') )
  return(data)
}

regressions = function(data,type='contemp') {
  z <- as.data.frame(matrix(NA, nrow=25, ncol=5))
  rownames(z) <- as.numeric(paste0(c(rep(1,5),rep(5,5),rep(10,5),rep(22,5),rep(44,5)),c(1,5,10,22,44)))
  zz <- 0
  for (i in c(1,5,10,22,44)) { 
    for (j in c(1,5,10,22,44)) { 
      zz <- zz + 1 
      if(type=='contemp') temp.lm <- lm(as.formula(paste0('carry',i,' ~ ','risk',j)), data=data)
      if(type=='predictive') temp.lm <- lm(as.formula(paste0('carry',i,'l ~ ','risk',j)), data=data)
      temp.summ <- summary(temp.lm)
      temp.summ$coefficients <- unclass(coeftest(temp.lm, vcov. = NeweyWest)) # Newey!
      z[zz,] <- cbind( round(temp.summ$coefficients[1], digits=4), 
                       round(lm.beta(temp.lm)$standardized.coefficients[2], digits=4),
                       round(temp.summ$r.squared*100, digits=2), 
                       round(durbinWatsonTest(temp.summ$residuals), digits=2),
                       round(as.numeric(adf.test(temp.summ$residuals)$statistic), digits=2) ) } 
      colnames(z) <- c('intercept','std coef','r2','dwt','adf')
  }
  return(z)
}

shade_it <- function(data,ind,n,z,mode='long',main=NULL,transparent=NULL) {
  data <- as.zoo(data); ind <- as.zoo(ind)
  z1=1-z; z2=z
  # long <- ifelse(perc(data, n=n)<=z1, 1, 0)
  long <- runPercentRank(ind, n=n)<=z1
  short <- runPercentRank(ind, n=n)>=z2
  col1 <- 'green'; col2 <- 'red'
  if(!is.null(transparent)) col1 <- adjustcolor("green", alpha.f = 0.2); col2 <- adjustcolor("red", alpha.f = 0.2)
  plot(data, ylab='', xlab='', main=main)
  if(mode=='long' | mode=='both') xblocks(long, col = col1) # IF OR
  if(mode=='short' | mode=='both') xblocks(short, col = col2) # IF OR
}

afterplot1 <- function(data,ind,n,z,mode='long') { ### HABE NEUERES SKRIPT/damit nicht arbeiten!
  ind <- as.zoo(ind)
  perc <- function(x,n) { runPercentRank(x, n=n) }
  z1=1-z; z2=z
  
  long <<- ind[ perc(ind, n=n)<=z1 ] # <<- !!!
  short <<- ind[ perc(ind, n=n)>=z2 ] # <<- !!!
  
  longx <- lapply(index(long), function(x) { temp <- data[paste(x,'/',sep='')]; return(temp) } ) # data[paste(x,'/',sep='')]
  shortx <- lapply(index(short), function(x) { temp <- data[paste(x,'/',sep='')]; return(temp) } )
  
  longx <- lapply(longx, function(x) x[1:min(20,nrow(x)),])
  shortx <- lapply(shortx, function(x) x[1:min(20,nrow(x)),])
  
  # weil es erst DANACH (x) die diff berechnet, ist das hier nur forward looking (zeigt nicht t0)
  longx <- lapply(longx, function(x) c(as.numeric( cumsum(na.omit( diff(log(x)) )) ), rep(0,20))[1:19] ) # rep(NA,20)
  shortx <- lapply(shortx, function(x) c(as.numeric( cumsum(na.omit( diff(log(x)) )) ), rep(0,20))[1:19] )
  
  if(mode=='long') return(longx)
  if(mode=='short') return(shortx)
}

afterplot2 <- function(data,q1,q2) {
  temp <- cbind(rowMeans(data, na.rm=T),
                apply(data, 1, function(x) quantile(x,probs=q1,na.rm=T)),
                apply(data, 1, function(x) quantile(x,probs=q2,na.rm=T)))
  plot(temp[,1], ylim=c(min(temp),max(temp)), ylab='', xlab='', type='n')
  polygon(c(seq(1,nrow(temp),1),rev(seq(1,nrow(temp),1))), c(temp[,2],rev(temp[,3])), col = "grey90", border = FALSE)
  lines(temp[,1], lwd=2)
}