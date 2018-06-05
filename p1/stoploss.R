setwd('~/Dropbox/MS/')
require(xts)
require(PerformanceAnalytics)
source('~/R/CustomFunc.R')

# dailyrx=readRDS('~/Dropbox/MS/dailyrx.rds')
# dailyrx2=do.call(rbind, lapply(split(dailyrx, 'months'), cumsum)) # CUMSUM
# 
# long2b=xts(rowMeans(long*dailyrx2, na.rm=T), index(dailyrx2)) # ROWMEANS
# short2b=xts(rowMeans(short*dailyrx2, na.rm=T), index(dailyrx2)) # ROWMEANS
# rx3b=apply.monthly(long2b, last)+apply.monthly(short2b, last) # dailyrx2 ist ja cumsum // NEU +
# ret=readRDS('~/Dropbox/MS/portfolio_full.rds')$portfolio
# all.equal(rx3, ret$excess, check.attributes=FALSE); head(rx3); head(ret$excess)
# all.equal(rx3, rx3b) # TRUE, rx3

populate = function(x) { # OHNE CUMSUM apply to dailyrx2
  if(unique(is.na(x))=='TRUE') return(x)
  naseries=cbind(x,NA)[,2]; colnames(naseries)=colnames(x)
  where=which(x<(stoplossdummy))[1]
  if(!is.na(where)) return(na.locf(cbind(x[1:where],naseries)[,1]))
  # if(is.na(where)) return(x) # NORMAL SETTING
  if(is.na(where)) return(naseries) # COMPARE SETTING
  print('fritz') # passiert nicht
}
populate2 = function(zz) {
  do.call(cbind, lapply(as.list(zz), populate))
}
populatef = function(x) { # hier EXCEEDANCES, muss MONATLICH ANGEWENDET WERDEN (!)
  # IM PAPER CONDITIONAL MONTHLY RETURNS grafik! was passiert wenn stop loss getriggered
  # naseries=cbind(x,NA)[,2]
  where=which(x<(stoplossdummy))[1]
  naseries=cbind(x,NA)[,2]; colnames(naseries)=colnames(x)
  # print(nrow(x))
  # print(where)
  # if(!is.na(where)) return(na.locf(cbind(x[where:nrow(x)],naseries)[,1]))
  if(!is.na(where)) { # if(min(x[where:nrow(x)])<(-1)) print(x[where:nrow(x)]) # (-1) die exceedance?
    return(x[where:nrow(x)]) }
  # if(is.na(where)) return(NULL)
  if(is.na(where)) return(naseries) # new june
}
populate2f = function(x) {
  do.call(cbind, lapply(as.list(x), populatef)) # column into lists
  # lapply(as.list(x), populatef)
}

stoploss = function(dailyip,show='strategy') { # stoplossdummy,
  # stoplossdummy<<-stoplossdummy
  # dailyip=readRDS('~/Dropbox/MS/dailyip.rds') 
  long=ifelse(dailyip==1,1,NA)
  short=ifelse(dailyip==-1,-1,NA)
  dailyrx=readRDS('~/Dropbox/MS/dailyrx.rds') 
  dailyrx2=do.call(rbind, lapply(split(dailyrx, 'months'), cumsum)) # CUMSUM
  
  long_split <- split(long*dailyrx2, 'months')
  short_split <- split(short*dailyrx2, 'months')
  
  # split into month, and then into columns in populate2
  stoploss_l <- do.call(rbind, lapply(long_split, populate2))
  stoploss_s <- do.call(rbind, lapply(short_split, populate2))
  
  #long3=xts(rowMeans(stoploss_l, na.rm=T), index(stoploss_l))
  #short3=xts(rowMeans(stoploss_s, na.rm=T), index(stoploss_s))
  #rx4a=apply.monthly(long3, last)+apply.monthly(short3, last)
  rx4b <- xts(rowMeans(apply.monthly(stoploss_l, last), na.rm=T)+
           rowMeans(apply.monthly(stoploss_s, last), na.rm=T), 
           index(apply.monthly(stoploss_l, last)))
  
  if(show=='strategy') { 
    return(rx4b)
  #all.equal(rx4a,rx4b) # TRUE
  }
  
  if(show=='detailslong') {
    # asd=lapply(long_split, function(x) lapply(as.list(x), populatef))
    return(lapply(long_split, populate2f))
  }
  if(show=='detailsshort') {
    # lapply(short_split, function(x) lapply(as.list(x), populatef))
    return(lapply(short_split, populate2f))
  }
}

yy=-0.01
stoplossx = function(yy) { 
  
  populate = function(x) { 
    if(unique(is.na(x))=='TRUE') return(x)
    naseries=cbind(x,NA)[,2]
    where=which(x<(yy))[1]
    if(!is.na(where)) return(na.locf(cbind(x[1:where],naseries)[,1]))
    if(is.na(where)) return(x)
  }
  populate2 = function(zz) {
    do.call(cbind, lapply(as.list(zz), populate))
  }
  
  # dailyip=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/kfxg1vi1s6hv6cg/dailyip.rds")))
  dailyip <- readRDS('~/Dropbox/MS/dailyip.rds') 
  long <- xts(ifelse(coredata(dailyip)==1,1,NA), index(dailyip))
  short <- xts(ifelse(coredata(dailyip)==-1,-1,NA), index(dailyip))
  # dailyrx=readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/of47enf79ugkq8c/dailyrx.rds")))
  dailyrx <- readRDS('~/Dropbox/MS/dailyrx.rds') 
  dailyrx2 <- do.call(rbind, lapply(split(dailyrx, 'months'), cumsum)) 
  
  long_split <- split(long*dailyrx2, 'months')
  short_split <- split(short*dailyrx2, 'months')
  
  # split into month, and then into columns in populate2
  stoploss_l <- do.call(rbind, lapply(long_split, populate2))
  stoploss_s <- do.call(rbind, lapply(short_split, populate2))
  
  rx4b <- xts(rowMeans(apply.monthly(stoploss_l, last), na.rm=T)+
              rowMeans(apply.monthly(stoploss_s, last), na.rm=T), 
              index(apply.monthly(stoploss_l, last)))
  gc()
  return(rx4b)
}


# stoplossjune = function(stoplossdummy,zzz,show='strategy') { # stoplossdummy,
### EXPLAIN DAILYFD
stoplossjune = function(zzz,show='strategy') {  
  # stoplossdummy = stoplossdummy
  
  populate = function(x) { # OHNE CUMSUM apply to dailyrx2
    if(unique(is.na(x))=='TRUE') return(x)
    naseries=cbind(x,NA)[,2]
    where=which(x<(stoplossdummy))[1]
    if(!is.na(where)) return(na.locf(cbind(x[1:where],naseries)[,1]))
    if(is.na(where)) return(x)
    print('fritz') # passiert nicht
  }
  populate2 = function(zz) {
    do.call(cbind, lapply(as.list(zz), populate))
  }
  
  dailyfd <- readRDS('~/Dropbox/MS/dailyfd.rds') # ncol 48
  
  makeinportfoliodaily = function(x) {
    xx=cbind(dailyfd[,x],in.portfolio[,x]) 
    xx=na.locf(xx) # so wird vom anfang monat bis ende monat aufgezogen (dank yearmon), oder?
    colnames(xx)=rep(x, 2)
    return(xx[,2])
  }
  in.portfolio <- readRDS(paste('portfolio_',zzz,'.rds',sep=''))$in.portfolio
  in.portfolio <- xts(in.portfolio[,4:ncol(in.portfolio)], as.yearmon(paste(in.portfolio$year,'-',in.portfolio$month,sep='')))
  in.portfolio <- lag(in.portfolio, k=1)
  index(in.portfolio) <- as.Date(unlist(lapply(split(dailyfd, 'months'), function(x) index(first(x)))))
  
  dailyip <- do.call(cbind, lapply(colnames(in.portfolio), makeinportfoliodaily)) # ncol 43 bei excl
  # ncol(dailyip)
  # print(colnames(dailyip))
  
  # stoplossdummy<<-stoplossdummy
  # dailyip2=readRDS('~/Dropbox/MS/dailyip.rds') 
  # all.equal(colnames(dailyip),colnames(dailyip2)); all.equal(nrow(dailyip),nrow(dailyip2))
  # all.equal(dailyip,dailyip2, check.attributes=FALSE)
  # eq=cbind(dailyip$MYR,dailyip2$MYR)
  # all.equal(eq[,1], eq[,2]) # HIER GIBT ES FEHLER wegen lag
  
  long <- xts(ifelse(coredata(dailyip)==1,1,NA), index(dailyip))
  short <- xts(ifelse(coredata(dailyip)==-1,-1,NA), index(dailyip))
  
  dailyrx <- readRDS('~/Dropbox/MS/dailyrx.rds') 
  dailyrx2 <- do.call(rbind, lapply(split(dailyrx, 'months'), cumsum)) 
  dailyrx2 <- dailyrx2[,colnames(dailyip)]
  print(all.equal(colnames(dailyrx2),colnames(dailyip)))
  
  long_split <- split(long*dailyrx2, 'months')
  short_split <- split(short*dailyrx2, 'months')
  
  # split into month, and then into columns in populate2
  stoploss_l <- do.call(rbind, lapply(long_split, populate2))
  stoploss_s <- do.call(rbind, lapply(short_split, populate2))
  
  lonG <- rowMeans(apply.monthly(stoploss_l, last), na.rm=T)
  shorT <- rowMeans(apply.monthly(stoploss_s, last), na.rm=T)
  indeX <- index(apply.monthly(stoploss_l, last))
  # rx4b <- xts(rowMeans(apply.monthly(stoploss_l, last), na.rm=T)+
  #               rowMeans(apply.monthly(stoploss_s, last), na.rm=T), 
  #             index(apply.monthly(stoploss_l, last)))
  rx4b <- xts(lonG+shorT, indeX)
  
  if(show=='strategy') { return(rx4b) }
  
  if(show=='detailslong') { return(stoploss_l) }
  if(show=='detailsshort') { return(stoploss_s) }
}

# n <- 1000
# x <- cumsum(sample(c(-1, 1), n, TRUE))
# plot(x)
