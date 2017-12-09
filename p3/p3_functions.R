# source('p3/BSfromDelta.R')

### STRANGLES ----
fixed_delta = function(data, type='delta', num) {
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
  
  data$down.l <- lag(data$down, k=1)
  data$up.l <- lag(data$up, k=1)
  data$lossdown <- ifelse(data$Spot<data$down.l, data$Spot-data$down.l, 0)
  data$lossup <- ifelse(data$Spot>data$up.l, data$up.l-data$Spot, 0)
  data$losses <- data$lossdown+data$lossup
  
  data$total <- data$premia + data$losses
  data$total.adj <- data$total/data$Spot
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

week_this = function(series) { # needs Monday
  as.Date(unlist(lapply(c(0:4), function(x) as.character(index(series)+x))))
}
week_following = function(series) { # needs Monday
  as.Date(unlist(lapply(c(7:11), function(x) as.character(index(series)+x))))
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
  data2$exp=data2$Vol/100*sqrt(5/365)
  data$diff <- diff(log(data$Spot))
  data$diff_ahead <- lag(data$diff, k=-1)
  data$Real <- sqrt( rollapply(data$diff, 5, function(x) sum(x^2))*52 )*100
  data2$Real <- data$Real[weekdays(index(data))=='Friday']
  data2$Real_a <- lag(data2$Real, k=-1)
  return(data2)
}

generate_new = function(x) { 
  data <- x
  # data <- fx[[4]]
  colnames(data)=c('Spot','Vol')
  data2 <- data[weekdays(index(data))=='Monday'] # TO.WEEKLY
  # xx <- fivequantile(data2, 1)
  # dates = lapply(xx, function(x) sort(c(index(x),index(x)+1,index(x)+2,index(x)+3,index(x)+4)))
  # dates = lapply(xx, function(x) sort(c(index(x)+1,index(x)+2,index(x)+3,index(x)+4,index(x)+7)))
  # data[dates[[2]]]
  # anyDuplicated(unlist(dates))
  data2$w_diff <- diff(log(data2$Spot))
  data2$w_diff_ahead <- lag(data2$w_diff, k=-1)
  data2$w_abs <- abs( diff(log(data2$Spot)) )
  data2$w_abs_ahead <- abs( lag(data2$w_diff, k=-1) )
  data2$exp=data2$Vol/100*sqrt(5/365)
  
  data$diff <- diff(log(data$Spot))
  data$diff_ahead <- lag(data$diff, k=-1)
  data$Real <- sqrt( rollapply(data$diff, 5, function(x) sum(x^2))*52 )*100
  
  data2$Real <- data$Real[weekdays(index(data))=='Monday']
  data2$Real_a <- lag(data2$Real, k=-1)
  head(data, 15)
  return(data2)
}

generate_new2 = function(x) {
  data <- x
  # data <- fx[[4]]
  colnames(data)=c('Spot','Vol')
  data$diff <- diff(log(data$Spot))
  data$diffw <- rollapply(data$diff, 5, sum)
  
  data2 <- data[weekdays(index(data))=='Monday'] # TO.WEEKLY
  # data2$w_diff <- diff(log(data2$Spot))
  # data <- cbind(data$diff,data2$w_diff,rollapply(data$diff, 5, sum))
  # head(data, 30)
  
  xx <- fivequantile(data2, 2)
  # dates = lapply(xx, function(x) sort(c(index(x),index(x)+1,index(x)+2,index(x)+3,index(x)+4)))
  dates = lapply(xx, function(x) sort(c(index(x)+1,index(x)+2,index(x)+3,index(x)+4,index(x)+7)))
  dataq <- list(data[dates[[1]]],data[dates[[2]]],data[dates[[3]]],data[dates[[4]]],data[dates[[5]]])
  # anyDuplicated(unlist(dates))
  # sum(unlist(lapply(dataq, nrow))); nrow(data)
  # weekdays(index(head(data)))
  VR <- lapply(dataq, function(x) { 
    (5*var(x[,'diff']))/var(x[,'diffw'][weekdays(index(x)) %in% 'Monday'])
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
  res <- as.data.frame(do.call(rbind, AR))
  res$BOXp <- unlist(BOX)
  res$VR <- unlist(VR)
  colnames(res) <- c('AR','AR-t','BOX-p','VR')
  
  # check <- data[as.Date(sort(unlist(dates)))]
  # (5*var(check[,'diff']))/var(check[,'diffw'][weekdays(index(check)) %in% 'Monday'])
  # hist(dataq[[1]][,2])
  
  return(res)
}

plot_against_realized = function(xxx) {
x <- as.numeric(xxx$Vol)
y <- as.numeric(xxx$Real_a)
plot(x,y, col=rgb(0,0,255,50,maxColorValue=255), pch=16, xlab='Implied Volatility', ylab='Realized Volatility (ahead)')
abline(lm(y~x), col="red")
abline(ltsReg(y~x), col="green") # Least Trimmed Squares Robust (High Breakdown) Regression  })
legend('topleft',c('Linear','Least Trimmed Squares Robust'),lty=c(1,1),col=c('red','green'),cex=0.8)
}
