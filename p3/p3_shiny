### STRANGLES ----
fixed_delta = function(data, type='delta', num, version='OLD') {
  colnames(data) <- c('Vol','Spot')
  # data$Real <- sqrt( rollapply(diff(log(data$Spot)), 5, function(x) sum(x^2))*52 )*100
  data <- data[weekdays(index(data))=='Friday'] # switch to weekly frequency (after realized vol is calculated)
  
  # data$Real.l <- lag(data$Real, k=-1)
  # data$abs=abs(diff(data[,'Spot'])) # NICHT LOG WEIL PREMIUM IST AUCH NICHT IN LOG
  # data$abs.l=lag(data$abs, k=-1)
  if(type=='delta') {
    data$down <- round(apply.daily(data, put_fromdelta, num), 4)
    data$up <- round(apply.daily(data, call_fromdelta, num), 4)
  }
  if(type=='fixed') {
    data$down <- round(data$Spot*(1-num), 4)
    data$up <- round(data$Spot*(1+num), 4)
  }

  data$put <- apply.daily(data, put_calc) # calculates option value; needs Vol, Spot, down
  data$call <- apply.daily(data, call_calc) # calculates option value; needs Vol, Spot, up
  data$premia.t <- data$put+data$call # t=true (time when received)
  data$premia <- lag(data$put+data$call, k=1) # (time when realized)
  
  if(version=='NEW') {
    data$premia.t <- (data$put+data$call)/data$Spot # NEW
    data$premia <- lag(data$premia.t, k=1) # NEW
  }
  ### CHECK HERE PREMIA
  data$down.l <- lag(data$down, k=1)
  data$up.l <- lag(data$up, k=1)
  data$lossdown <- ifelse(data$Spot<data$down.l, data$Spot-data$down.l, 0)
  data$lossup <- ifelse(data$Spot>data$up.l, data$up.l-data$Spot, 0)
  data$losses <- data$lossdown+data$lossup
  
  data$total <- data$premia + data$losses
  data$total.adj <- data$total/data$Spot
  if(version=='NEW') {
    data$total <- data$premia + data$losses/data$Spot
    data$total.adj <- data$total
  }
  # data$total.l=lag(data$total, k=-1)
  # data$total.s=data$total.l*(data$Vol/mean(data$Vol, na.rm=T)) # scaled?
  data <- data[2:nrow(data),]
  # data$cumsum <- cumsum(data$total.adj)
  return(data)
}


### QUANTILE AND AUTOCORRELATIONS ----
fivequantile = function(x,col) {
  xxx <- quantile(x[,col], probs=c(0.2,0.4,0.6,0.8)) # same as probs=c(0,0.2,0.4,0.6,0.8,1))
  list(x[x[,col]<=xxx[1]],
       x[x[,col]>xxx[1] & x[,col]<=xxx[2]],
       x[x[,col]>xxx[2] & x[,col]<=xxx[3]],
       x[x[,col]>xxx[3] & x[,col]<=xxx[4]],
       x[x[,col]>xxx[4]])
}
tenquantile = function(x,col) {
  xxx <- quantile(x[,col], probs=seq(0.1,0.9,0.1))
  list(x[x[,col]<=xxx[1]],
       x[x[,col]>xxx[1] & x[,col]<=xxx[2]],
       x[x[,col]>xxx[2] & x[,col]<=xxx[3]],
       x[x[,col]>xxx[3] & x[,col]<=xxx[4]],
       x[x[,col]>xxx[4] & x[,col]<=xxx[5]],
       x[x[,col]>xxx[5] & x[,col]<=xxx[6]],
       x[x[,col]>xxx[6] & x[,col]<=xxx[7]],
       x[x[,col]>xxx[7] & x[,col]<=xxx[8]],
       x[x[,col]>xxx[8] & x[,col]<=xxx[9]],
       x[x[,col]>xxx[9]])
}

week_this_after_friday = function(series) { # needs Friday
  sort(as.Date(unlist(lapply(c(3:7), function(x) as.character(index(series)+x)))))
}
week_this = function(series) { # needs Monday
  as.Date(unlist(lapply(c(0:4), function(x) as.character(index(series)+x))))
}
week_following = function(series) { # needs Monday
  as.Date(unlist(lapply(c(7:11), function(x) as.character(index(series)+x))))
}
range_vol = function(series) {
  abs(as.numeric(log(last(series[,1])))-as.numeric(log(first(series[,1]))))/(as.numeric(log(max(series[,1])))-as.numeric(log(min(series[,1]))))
}
range_vol2 = function(series) {
  cbind(abs(as.numeric(log(last(series[,1])))-as.numeric(log(first(series[,1])))),(as.numeric(log(max(series[,1])))-as.numeric(log(min(series[,1])))))
}

generate = function(x) {
  data <- x
  # data=fx[[1]]
  colnames(data) <- c('Spot','Vol') ### HERE ANDERSRUM ALS OBEN!!!
  data2 <- data[weekdays(index(data))=='Friday'] # TO.WEEKLY
  data2$w_diff <- diff(log(data2$Spot))
  data2$w_diff_ahead <- lag(data2$w_diff, k=-1)
  data2$w_abs <- abs( diff(log(data2$Spot)) )
  data2$w_abs_ahead <- abs( lag(data2$w_diff, k=-1) )
  data2$exp=data2$Vol/100*sqrt(5/365) ### WRONG !!!
  data$diff <- diff(log(data$Spot))
  data$diff_ahead <- lag(data$diff, k=-1)
  data$Real <- sqrt( rollapply(data$diff, 5, function(x) sum(x^2))*52 )*100
  data2$Real <- data$Real[weekdays(index(data))=='Friday']
  data2$Real_a <- lag(data2$Real, k=-1)
  return(data2)
}

generate_new = function(x,day='Monday') { 
  data <- x
  # data <- fx[[4]]
  colnames(data)=c('Spot','Vol')
  data2 <- data[weekdays(index(data))==day] # TO.WEEKLY # =='Monday'
  # xx <- fivequantile(data2, 1)
  # dates = lapply(xx, function(x) sort(c(index(x),index(x)+1,index(x)+2,index(x)+3,index(x)+4)))
  # dates = lapply(xx, function(x) sort(c(index(x)+1,index(x)+2,index(x)+3,index(x)+4,index(x)+7)))
  # data[dates[[2]]]
  # anyDuplicated(unlist(dates))
  data2$w_diff <- diff(log(data2$Spot))
  data2$w_diff_ahead <- lag(data2$w_diff, k=-1)
  data2$w_abs <- abs( diff(log(data2$Spot)) )
  data2$w_abs_ahead <- abs( lag(data2$w_diff, k=-1) )
  # data2$exp=data2$Vol/100*sqrt(5/365) ### ?!? 1/52 ???
  data2$exp=data2$Vol/100*sqrt(1/52) ### ?!? 1/52 = 5/260
  
  data$diff <- diff(log(data$Spot))
  data$diff_ahead <- lag(data$diff, k=-1)
  data$Real <- sqrt( rollapply(data$diff, 5, function(x) sum(x^2))*52 )*100
  
  data2$Real <- data$Real[weekdays(index(data))==day]
  data2$Real_a <- lag(data2$Real, k=-1)
  
  return(data2)
}

generate_new2 = function(x,day='Monday',l=100,method='ahead') { # l not needed
  data <- x
  # data <- fx[['EURUSD']]
  colnames(data)=c('Spot','Vol')
  data$diff <- diff(log(data$Spot))
  data$diffw <- rollapply(data$diff, 5, sum)
  # day = 'Monday'
  data2 <- data[weekdays(index(data))==day] # TO.WEEKLY
  # data2$w_diff <- diff(log(data2$Spot))
  # data <- cbind(data$diff,data2$w_diff,rollapply(data$diff, 5, sum))
  # head(data, 30)
  
  xx <- fivequantile(data2, 2)
  # dates = lapply(xx, function(x) sort(c(index(x),index(x)+1,index(x)+2,index(x)+3,index(x)+4)))
  # dates = lapply(xx, function(x) sort(c(index(x)+1,index(x)+2,index(x)+3,index(x)+4,index(x)+7)))
  
  dates = lapply(xx, function(x) sort(c(index(x)+1,index(x)+2,index(x)+3,index(x)+4,index(x)+5,index(x)+6,index(x)+7)))
  # dates = lapply(xx, function(x) sort(c(index(x),index(x)-1,index(x)-2,index(x)-3,index(x)-4,index(x)-5,index(x)-6)))
  dates = lapply(dates, function(x) x[!weekdays(x) %in% c('Saturday','Sunday')])
 
  dataq <<- list(data[dates[[1]]],data[dates[[2]]],data[dates[[3]]],data[dates[[4]]],data[dates[[5]]]) # HERE USE DAILY DATA AGAIN
  # dataq <<- list(na.omit(data[dates[[1]]]),na.omit(data[dates[[2]]]),na.omit(data[dates[[3]]]),na.omit(data[dates[[4]]]),na.omit(data[dates[[5]]])) # if backwards sorted
  
  # dataq[[1]]
  # lapply(dataq, function(x) sum(weekdays(index(x))=='Monday'))
  # lapply(dates, length)
  # anyDuplicated(unlist(dates))
  # sum(unlist(lapply(dataq, nrow))); nrow(data)
  # weekdays(index(head(data)))
  VR <- lapply(dataq, function(x) { 
    (5*var(x[,'diff']))/var(x[,'diffw'][weekdays(index(x)) %in% day])
  })
  BOX <- lapply(dataq, function(x) { 
    # c(Box.test(x[,'diff'], lag=1)$p.value, Box.test(x[,'diff'], type=c('Ljung-Box'), lag=1)$p.value)
    Box.test(x[,'diff'], lag=1)$p.value
  })
  AR <- lapply(dataq, function(x) { 
    # c(ar(x[,'diff'], aic=FALSE, order.max=1, method=c('ols'))$ar, 
    c(summary(lm(x[,'diff'] ~ lag(x[,'diff'], k=-1)))$coefficients[2,'Estimate'],
      # summary(lm(x[,'diff'] ~ lag(x[,'diff'], k=-1)+0))$coefficients[1,1],
      summary(lm(x[,'diff'] ~ lag(x[,'diff'], k=-1)))$coefficients[2,'t value']
      #summary(lm(x[,'diff'] ~ lag(x[,'diff'], k=-1)))$r.squared)
    )
  })
  # HE <- lapply(dataq, function(x) { 
  #   rssimple(as.numeric(x[,'diff']))
  # })
  # HE2 <- lapply(dataq, function(x) { 
  #   mean(tsboot(x[,'diff'], rssimple, R = 1000, l = l, n.sim = l, sim = "fixed")$t)
  # })
  # HE3 <- lapply(dataq, function(x) {
  #   # pracma::hurstexp(as.numeric(x[,'diff']))$Hs
  #   asd <- split(x, 'weeks')
  #   mean(unlist(lapply(asd, function(yy) rssimple(yy[,'diff']))))
  # })
  # OU3 <- lapply(dataq, function(x) { 
  #   asd <- split(x, 'weeks')
  #   temp <<- unlist(lapply(asd, function(yy) ou_chan(cumsum(yy[,'diff']))))
  #   mean(temp)
  # })
  # OU2 <- lapply(dataq, function(x) { 
  #   # mean(tsboot(x[,'diff'], ou_chan, R = 1000, l = l, sim = "fixed")$t)
  #   temp2 <<- tsboot(cumsum(x[,'diff']), ou_chan, R = 1000, l = l, n.sim = l, sim = "fixed")$t
  #   mean(temp2)
  # })
  # do_adf = function(x) { 
  #   k = trunc((length(x)-1)^(1/3))
  #   # print(k) # k=4
  #   tseries::adf.test(x)$statistic }
  # ADF <- lapply(dataq, function(x) { 
  #   do_adf(cumsum(x[,'diff']))
  # })
  # ADF2 <- lapply(dataq, function(x) { 
  #   mean(tsboot(cumsum(x[,'diff']), do_adf, R = 1000, l = 100, n.sim = 100, sim = "fixed")$t) # not l
  # })
  # # ADF3 <- lapply(dataq, function(x) {  # KANN MAN AUF 5 DAY FRIST NICHT NUTZEN ODER
  # #   asd <- split(x, 'weeks')
  # #   mean(unlist(lapply(asd, function(yy) do_adf(yy[,'diff']))))
  # # })
  # FD2 <- lapply(dataq, function(x) { 
  #   mean(tsboot(cumsum(x[,'diff']), felix_trend, R = 1000, l = l, n.sim = l, sim = "fixed")$t)
  # })
  # FD3 <- lapply(dataq, function(x) { 
  #   asd <- split(x, 'weeks')
  #   mean(unlist(lapply(asd, function(yy) felix_trend(cumsum(yy[,'diff'])))))
  # })
  res <- as.data.frame(do.call(rbind, AR))
  res$BOXp <- unlist(BOX)
  res$VR <- unlist(VR)
  # res$HE <- unlist(HE)
  # res$HE2 <- unlist(HE2)
  # res$HE3 <- unlist(HE3)
  # res$OU2 <- unlist(OU2)
  # res$OU3 <- unlist(OU3)
  # res$ADF <- unlist(ADF)
  # res$ADF2 <- unlist(ADF2)
  # # res$ADF3 <- unlist(ADF3)
  # res$FD2 <- unlist(FD2)
  # res$FD3 <- unlist(FD3)
  
  # colnames(res) <- c('AR','AR-t','BOX-p','VR','HE','HE2','HE3','OU2','OU3','ADF','ADF2','FD2','FD3')
  colnames(res) <- c('AR','AR-t','BOX-p','VR')
  
  # check <- data[as.Date(sort(unlist(dates)))]
  # (5*var(check[,'diff']))/var(check[,'diffw'][weekdays(index(check)) %in% 'Monday'])
  # hist(dataq[[1]][,2])
  
  return(res)
}


plot_against = function(xxx,type='Volatility',title='') {
x <- as.numeric(xxx$Vol)
if(type=='Move') {
y <- as.numeric(xxx$w_abs_ahead)
plot(x,y, col=rgb(0,0,255,50,maxColorValue=255), pch=16, xlab='Implied Volatility', ylab='Realized Move (ahead)', main=paste(title,'Move'))
}
if(type=='Volatility') {
y <- as.numeric(xxx$Real_a)
plot(x,y, col=rgb(0,0,255,50,maxColorValue=255), pch=16, xlab='Implied Volatility', ylab='Realized Volatility (ahead)', main=paste(title,'RV'))
}
abline(lm(y~x), col="red")
abline(ltsReg(y~x), col="green") # Least Trimmed Squares Robust (High Breakdown) Regression  })
legend('topleft',c('Linear','Least Trimmed Squares Robust'),lty=c(1,1),col=c('red','green'),cex=0.8)
}

### 
# data <- data[[1]]
straddle = function(data, version='OLD', version2='5') {
  colnames(data) <- c('Vol','Spot')
  data <- data[weekdays(index(data))=='Friday'] # switch to weekly frequency (after realized vol is calculated)

  if(version2=='1') {
  data$put <- apply.daily(data, straddle_calc) # calculates option value; needs Vol, Spot -- and 1/52
  data$call <- apply.daily(data, straddle_calc) # calculates option value; needs Vol, Spot -- and 1/52
  }
  if(version2=='5') {
  data$put <- apply.daily(data, straddle_calc2) # 5/252 anstatt 1/52
  data$call <- apply.daily(data, straddle_calc2) 
  }
  
  data$premia.t <- data$put+data$call # t=true (time when received)
  data$premia <- lag(data$put+data$call, k=1) # (time when realized)
  
  if(version=='NEW') {
    data$premia.t <- (data$put+data$call)/data$Spot # NEW
    data$premia <- lag(data$premia.t, k=1) # NEW
  }
  ### CHECK HERE PREMIA
  data$down.l <- lag(data$Spot, k=1)
  data$up.l <- lag(data$Spot, k=1)
  data$lossdown <- ifelse(data$Spot<data$down.l, data$Spot-data$down.l, 0)
  data$lossup <- ifelse(data$Spot>data$up.l, data$up.l-data$Spot, 0)
  data$losses <- data$lossdown+data$lossup
  
  data$total <- data$premia + data$losses ### AT TIME T+1
  data$total.adj <- data$total/data$Spot
  if(version=='NEW') {
    data$total <- data$premia + data$losses/data$Spot
    data$total.adj <- data$total
  }

  data <- data[2:nrow(data),]
  return(data)
}
