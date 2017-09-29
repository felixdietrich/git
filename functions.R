# loadFunctions('fritz')
# source('~/Dropbox/newdiss/git/functions.R')
print(c('Data','Holidays','Outliers','IB','OutliersOLD'))
loadFunctions <- function(x)
{
  ### Data ----
  if(sum(x %in% 'Data')>=1) {
    
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    duplicatedindex <<- function(x,y='show') { if(y=='show') print(x[ duplicated( index(x) ),  ]) 
                                               if(y=='remove') x[ ! duplicated( index(x) ),  ]   }
    
    findmissingvalues <<- function(z,j)
    {
      subset(apply.daily(z, nrow), apply.daily(z, nrow)!=j)
    }
    
    findgaps <<- function(x,y='intraday') { 
      if(y=='intraday') { 
        gapsat=index(x[ which( diff(index(x))>1 ) +2 ]) # nochmal checken, old code
        # gapdates=as.Date(gapsat, tz=indexTZ(x))
        gapdates=as.Date(gapsat)
        print(gapdates)
        where=gapdates[which(diff(gapdates)<1)] # wo gibt es gaps die nicht weekend sind
        return(where) }
      if(y=='weekendcurrencies') { 
        temp=as.list(which( diff(index(mid))>16 ))
        # temp=as.character(as.Date(index(x[ which( diff(index(x))>16 ) ])))
        # print(lapply(temp, function(y) tail(x[y])))
        print(lapply(temp, function(y) x[(y-3):(y+3)]))
        return(index(x[ which( diff(index(x))>16 ) ])) }
      if(y=='highfreq') { return(index(x[ which( diff(index(x))>1 ) ])) }
      
      # weekly:
      # dates_have=dates_have[4:length(dates_have)] # remove the first non-consistent
      # dates_have
      # dates_want=dates_have[1]+seq(from=0,to=length(dates_have),by=1)*7
      # lapply(g, na.omit)
      # then rbind
      # then duplicates remove
    }
    
    dataone <<- function(dat=NULL,where=NULL,what=NULL,as=NULL) # dat muss list sein
    {
      if(!is.null(where)) setwd(where)
      if(is.null(dat)) dat <- lapply(what, readRDS)
      # print(head(dat))
      dat <- lapply(dat, na.omit)
      dat <- do.call(rbind, dat)
      f1 = duplicated( index(dat) ) 
      f2 = duplicated( index(dat), fromLast = 'TRUE' ) 
      test1=dat[f1]; test2=dat[f2]; print(paste('duplicates equal',all.equal(test1,test2)))
      mergedat <- dat[ ! duplicated( index(dat) ),  ] 
      if(!is.null(as)) saveRDS(mergedat, as)
      return(mergedat)
    }
    # dataone('~/',c('MMPamin.rds','MMPbmin.rds','MMPcmin.rds','MMPdmin.rds'),'MMPaminnew.rds')
    
    # twowayplot = function(x,y) 
    # { 
    #   tempdata=cbind(x,y)
    #   name1=gsub('.Open','',colnames(x)[1]); name2=gsub('.Open','',colnames(y)[1])
    #   plot(as.zoo(tempdata[,1]), las=1, xlab="", ylab='', main=paste(name1,'vs.',name2)) #mtext('AUD/JPY')
    #   par(new=TRUE)      
    #   plot(as.zoo(tempdata[,2]), col=3, bty='n', xaxt="n", yaxt="n", xlab="", ylab="")
    #   axis(side = 4)
    #   legend('topleft', c(name1,name2), lty=c('solid','solid'), lwd=2, col=c('black','green'), cex=0.8) # inset=c(-0.4,0)
    # }
    
    normalize <<- function(x) { 
      if(ncol(x)==1) return(x*as.numeric(100/x[1]))
      if(ncol(x)>1) { 
        temp <- apply(x, 2, function(x) x/as.numeric(first(x)))
        ifelse(class(x)[1]=='xts',return(xts(temp, index(x))),return(temp)) }
      }
    
    indexmin = function(x,y,z) { x[.indexhour(x) %in% y & .indexmin(x) %in% z] } # gibt dasselbe
    
    which.max.xts = function(data) {
      ncol=seq(1:ncol(data))
      # lapply(ncol, function(x) data[which.max(data[,x]),x])
      lapply(ncol, function(x) data[data[,x] %in% sort(coredata(data[,x]), decreasing=TRUE)[1:5]])
    }
    which.min.xts = function(data) {
      ncol=seq(1:ncol(data))
      # lapply(ncol, function(x) data[which.min(data[,x]),x])
      lapply(ncol, function(x) data[data[,x] %in% sort(coredata(data[,x]))[1:5]])
    }
    which.na.xts = function(data) {
      # ncol=seq(1:ncol(data))
      # lapply(ncol, function(x) data[which.min(data[,x]),x])
      data[is.na(rowSums(data))]
    }
    which.na.xts2 = function(data) {
      do.call(rbind, lapply(c(1:ncol(data)), function(x) data[is.na(data[,x])]))
    }
    
    snap <<- function(yyy) # mit getClose function zusammenlegen! wo hatte ich das benutzt?
    {
      tempX=yyy['T16:59/T17:00:01'] # at 16:59
      tempY=cbind(xts(tempX[,4], order.by=as.Date(index(tempX), 'EST5EDT')),
                  xts(tempX[,8], order.by=as.Date(index(tempX), 'EST5EDT')))
      tempZ=tempY[,2]-tempY[,1]
      return(tempZ)
    }
    snap2 <<- function(yyy)
    {
      tempX=yyy['T16:59/T17:00:01'] # at 16:59
      tempY=cbind(xts(tempX[,4], order.by=as.Date(index(tempX), 'EST5EDT')),
                  xts(tempX[,8], order.by=as.Date(index(tempX), 'EST5EDT')))
      tempZ=(tempY[,2]-tempY[,1])/((tempY[,2]+tempY[,1])/2)
      return(tempZ)
    }
    
    getOHLC <<- function(z,close='Real') { # ,what=NULL
      data <- z
      indexTZ(data) <- 'EST5EDT'
      data <- (data[,c(1:4)]+data[,c(5:8)])/2
      
      if(close=='Real') data <- z['T09:30/T16:00']
      if(close=='1MinBefore') data <- z['T09:31/T15:59']
      # if(close=='Full') # nothing
      tz <- indexTZ(data)
      teSSt <<- data
      data <- to.daily(data, drop.time = FALSE) ### drop.time = FALSE
      index(data) <- as.Date(index(data), tz=tz)
      return(data)
    }
    
    getClose <<- function(z,what=NULL,plot=NULL) {      
      indexTZ(z) <- 'EST5EDT'
      lala <- gsub(".Open", "", colnames(z)[1])
      z$Mid <- (z[,8]+z[,4])/2
      close1 <- z$Mid['T16:00/T16:00:01']
      close2 <- z$Mid['T15:59/T15:59:01']
      index(close1) <- as.Date(index(close1), tz='EST5EDT') # nicht notwendig, aber...
      index(close2) <- as.Date(index(close2))
      from <- index(first(close1))
      to <- index(last(close1))
      close <- cbind(close1,close2)
      colnames(close) <- c('RealClose','1MinBefore')
      if(!is.null(plot)) {
        require(quantmod)
        xx <- getSymbols(plot, auto.assign=F, from=from, to=to, src='google')
        close <- cbind(close1,close2,xx[,4])
        colnames(close) <- c('RealClose','1MinBefore','Quantmod') }
      # print(head(close))
      if(!is.null(plot)) {
        if(what=='RealClose') plot(close[,'RealClose'], main=plot)
        if(what=='1MinBefore') plot(close[,'1MinBefore'], main=plot)
        try(lines(xx[,6], col='red'), silent=TRUE)
        try(lines(xx[,4], col='green'), silent=TRUE)
      }
      if(!is.null(what)) return(close)
      colnames(close1) <- lala
      return(close1)
    }
    
    getSundays <<- function(year) {      
      dates <- seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")), by = "day")
      dates[weekdays(dates) == "Sunday"]      
    }
    
    removeSundays <<- function(z) {      
      z <- z[!weekdays(as.Date(index(z), tz=indexTZ(z))) %in% 'Sunday']
      return(z)
    }
    
    plothourly <<- function(z,sunday='yes',outliers=NULL,save='no') # braucht als z bid-ask argument 
    {
      Sys.setenv(TZ=indexTZ(z))
      print(Sys.timezone())
      asd <- seq(0:23)
      if(sunday=='no') z <- z[!weekdays(as.Date(index(z), tz='EST5EDT')) %in% 'Sunday']
      if(!is.null(outliers)) z <- z[!z %in% sort(as.numeric(z), decreasing=T)[1:outliers]]
      test <- lapply(asd, function(x) z[.indexhour(z) %in% x])
      tobox <- do.call(cbind, lapply(test, function(x) as.numeric(x)))
      boxplot(tobox, col='magenta', border='lightgrey', main=substr(colnames(z),1,6)) # names=names, 
      if(save=='yes') {
        pdf(paste(substr(colnames(z),1,6),'plot_hourly.pdf',sep=''), width=14, height = 7)
        boxplot(tobox, col='magenta', border='lightgrey', main=substr(colnames(z),1,6)) # names=names, 
        mtext(paste(as.Date(index(first(z)), tz='EST5EDT'),'-',as.Date(index(last(z)), tz='EST5EDT')))
        dev.off() }
    }
    
    checkcorrecttime <<- function(z,critical=NULL,year='2016',sunday='yes',save='no',outliers=NULL) # braucht als z bid-ask argument 
    {
      plotdata = function(x) { plot(x, main=index(first(x))); Sys.sleep(5) }
      ### OLD WAY OF SUBSETTING
      # subset <- paste('T',format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5), '%H:%M'),'/T',
      #                     format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5)+10, '%H:%M:%S'),sep='')
      subset <- paste('T',format(as.POSIXct('2000-01-01 0:00', tz='')+60*seq(5,1435,by=5), '%H:%M'),'/T',
                      format(as.POSIXct('2000-01-01 0:00', tz='')+60*seq(5,1435,by=5)+10, '%H:%M:%S'),sep='')
      subset <- subset[!subset %in% c("T17:00/T17:00:10","T17:05/T17:05:10","T17:10/T17:10:10","T17:15/T17:15:10")]
      # make hourly indicator from 08:05 to 17:25
      
      ### NEW WAY
      Sys.setenv(TZ=indexTZ(z))
      print(Sys.timezone())
      # asd <- apply(cbind(rep(0:23, each=12),seq(0,55,by=5)), 1, as.list) # sonst wird 0:00 wird purem datum
      asd <- apply(cbind(rep(0:23, each=12),seq(1,56,by=5)), 1, as.list)
      names <- apply(cbind(rep(0:23, each=12),seq(1,56,by=5)), 1, function(x) paste(x[1],':',sprintf("%02d", x[2]),sep=''))
      # plot TZ critical dates
      if(!is.null(critical)) {
        if(year=='2016') datex <- readRDS('~/Dropbox/data/critical_timezones_dates2016.rds')
        if(year=='2014') datex <- readRDS('~/Dropbox/data/critical_timezones_dates2014.rds')
        data <- z[as.character(datex)]; data2 <- split(data, 'days'); lapply(data2, plotdata) }
      # doublecheck z[1725,] as.Date(index(z), tz='EST5EDT')[1725] # das braucht man OBWOHL Sys.timezone korrekt ist
      
      if(sunday=='no') z <- z[!weekdays(as.Date(index(z), tz='EST5EDT')) %in% 'Sunday']
      if(!is.null(outliers)) z <- z[!z %in% sort(as.numeric(z), decreasing=T)[1:outliers]]
      
      if(is.null(critical)) {
        # test <- lapply(subset, function(x) z[x])
        test <- lapply(asd, function(x) z[.indexhour(z) %in% x[[1]] & .indexmin(z) %in% x[[2]]])
        names(test) <- names
        print(test['17:01']); print(test['17:06']); print(test['17:11'])
        test['17:01'] <- 0; test['17:06'] <- 0; test['17:11'] <- 0
        tobox <- do.call(cbind, lapply(test, function(x) as.numeric(x)))
        # names <- unlist(lapply(test, function(x) unique(format(index(x), '%H:%M')))) # OLD
        boxplot(tobox, col='magenta', border='lightgrey', names=names, main=substr(colnames(z),1,6)) 
        if(save=='yes') {
          pdf(paste(substr(colnames(z),1,6),'plotx.pdf',sep=''), width=14, height = 7)
          boxplot(tobox, col='magenta', border='lightgrey', names=names, main=substr(colnames(z),1,6)) 
          mtext(paste(as.Date(index(first(z)), tz='EST5EDT'),'-',as.Date(index(last(z)), tz='EST5EDT')))
          dev.off()
        }
        # return(tobox)
      }
    }
    
  }
  
  ### Holidays ----
  if(sum(x %in% 'Holidays')>=1) {
    
    holremove <<- function(x,daysafter='yes')  # updated!
    { 
      require(timeDate) # as.Date(index(head(AUDJPY['2006-03-13'])), tz='') as.Date(index(head(AUDJPY['2006-03-13'])), tz='EST5EDT') das problem tritt aber nicht auf wenn US hours 930-1600 sind
      x=x[!as.Date(index(x),tz='EST5EDT') %in% c(as.Date(holiday(2005:2015, Holiday = listHolidays('US'))),
                                                 as.Date(holiday(2005:2015, "ChristmasEve")),as.Date(DENewYearsEve(2005:2015)))]
      ### wie bei HF dataset
      if(daysafter=='yes') {
        unwanted=c(as.Date("2005-01-02"),as.Date("2006-01-02"),as.Date("2007-01-02"),as.Date("2008-01-02"),as.Date("2009-01-02"),as.Date("2010-01-02"),as.Date("2011-01-02"),as.Date("2012-01-02"),as.Date("2013-01-02"),as.Date("2014-01-02"),as.Date("2015-01-02"),as.Date("2016-01-02"),
                   as.Date("2005-12-26"),as.Date("2006-12-26"),as.Date("2007-12-26"),as.Date("2008-12-26"),as.Date("2009-12-26"),as.Date("2010-12-26"),as.Date("2011-12-26"),as.Date("2012-12-26"),as.Date("2013-12-26"),as.Date("2014-12-26"),as.Date("2015-12-26"),as.Date("2016-12-26"))
        x=x[!as.Date(index(x),tz='EST5EDT') %in% unwanted] }
      return(x)
    }
    showholidays <<- function(from=1990,to=2020) 
    { 
      require(timeDate)
      dates1 = c(as.character(holiday(from:to, Holiday = listHolidays('US'))), 
                 as.character(holiday(from:to, "ChristmasEve")), 
                 as.character(DENewYearsEve(from:to)))
      sort(dates1)
    }
    isholremoved <<- function(x,from=1990,to=2020,daysafter='yes') # daysafter makes clear: which holidays: only holidays or also 2.1. and 26.12.
    { 
      require(timeDate)
      dates1 = c(as.character(holiday(from:to, Holiday = listHolidays('US'))), 
                 as.character(holiday(from:to, "ChristmasEve")), 
                 as.character(DENewYearsEve(from:to)))
      output=c(); for (i in from:to){ output[i]=as.character(paste(i,'-01-02',sep='')) }; dates2=na.omit(output)
      output=c(); for (i in from:to){ output[i]=as.character(paste(i,'-12-26',sep='')) }; dates3=na.omit(output)
      unwanted = as.Date(c(dates1,dates2,dates3))
      if(daysafter=='no') unwanted = as.Date(c(dates1))
      if(daysafter=='check') unwanted = as.Date(c(dates2,dates3))
      if(daysafter=='yes') unwanted = as.Date(c(dates1,dates2,dates3))
      x[as.Date(index(x),tz='EST5EDT') %in% unwanted] 
    }
    
  }
  
  ### Outliers ----
  if(sum(x %in% 'Outliers')>=1) {
    
    checkoutliers_ba <<- function(z,j=0.999,print='FALSE') # z=variables, j=which quantile, print or not
    {
      print(quantile(z[,8]-z[,4], probs=c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999), na.rm=T)) #na.rm new
      uni=unique(z[,8]-z[,4][which(z[,8]-z[,4]>quantile(z[,8]-z[,4], probs=c(j), na.rm=T))])
      print(sort(uni, decreasing = T))
      if(print=='TRUE') print(z[(z[,8]-z[,4]) %in% c(uni[4:length(uni)])])
      return(unique(format(index(z[which(z[,8]-z[,4]>quantile(z[,8]-z[,4], probs=c(j), na.rm=T))]), '%Y-%m-%d'))) # invisible(uni)
    }
    
    removeoutliers_ba_quantile <<- function(z,j,print='FALSE') # new variable inserted 2017 / changed aug 2017
    {
      q=quantile(z[,8]-z[,4], probs = j, na.rm=T)
      print(sort(unique(z[,8]-z[,4][which(z[,8]-z[,4]>q)]), decreasing = T)); print(q)
      if(print=='TRUE') print(z[which(z[,8]-z[,4]>q)])
      if(print=='TRUE') print(z[which(z[,8]-z[,4]<0)])
      z=z[-which(z[,8]-z[,4]>=q)]
      z=z[-which(z[,8]-z[,4]<=0)]
      return(z)
    }
    
    printoutliers <<- function(z,j,k=0) # wofuer ist k? 
    {
      if(ncol(z)!=1) { q=quantile(z[,8]-z[,4], probs = j, na.rm=T)
      print(sort(unique(z[,8]-z[,4][which(z[,8]-z[,4]>q)]), decreasing = T)); print(q)
      if(k==1) print(z[which(z[,8]-z[,4]>q)])
      if(k==1) print(z[which(z[,8]-z[,4]<0)])
      z=z[-which(z[,8]-z[,4]>q)]
      z=z[-which(z[,8]-z[,4]<0)]
      return(z) }
      if(ncol(z)==1) { print(quantile(na.omit(z), probs=c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999,0.99999)))
        print(quantile(na.omit(z), probs=(1-c(0.95,0.96,0.97,0.98,0.99,0.995,0.999,0.9995,0.9999,0.99999)))) }
    }
    
    makewithoutoutliers <<- function(z,j) # new variable inserted 2017
    {
      x = readRDS(paste('~/Dropbox/data/',z,'amin.rds',sep = '')) # assign?
      x <- x[!duplicated(index(x)),]
      x = x['1980/']
      indexTZ(x) = 'EST5EDT'
      x = x['T09:30/T16:00']
      remove = c(0,which(is.na(x[,4])),which(is.na(x[,8])),which(x[,8]-x[,4] < 0),
                 which(x[,8]-x[,4] >= quantile(x[,8]-x[,4], probs = j, na.rm = T))) # wozu c0 nochmal? 
      # rbind index muesste auch gehen
      # outliers1=DBVmin[,4][which(DBVmin[,4]>1)]; nrow(outliers1) ### somit kriegt man day after thanksgiving (friday)
      # outliers2=DBVmin[,4][which(DBVmin[,4]<0)] ### !
      # outliers=rbind(outliers1,outliers2) #outliers
      # DBVmin=DBVmin[-DBVmin[index(outliers), which.i=TRUE]]
      y = x[remove]
      x = x[-remove]
      print(length(remove)) # print how many
      saveRDS(y, paste('~/Dropbox/',z,'mindeleted.rds',sep = ''))
      assign(z, x, envir = .GlobalEnv)
      saveRDS(x, paste('~/Dropbox/',z,'minclean.rds',sep = ''))
    }
    
    outliers2017 <<- function(x,probs,return='outliers') {
      outl <- na.omit(abs(diff(x)))
      na <- cbind(x,NA)[,2] # series with NA's and same dates
      quant <- quantile(outl, probs=probs)
      outliers <- outl[which(outl>quant)]
      data <- x[index(x) %in% index(outliers)]
      removed <- x[!index(x) %in% index(outliers)]
      na2 <- na[!index(na) %in% index(removed)] # print(na2)
      cleaned <- na.locf(rbind(x[!index(x) %in% index(outliers)],na2))
      if(return=='outliers') return(outliers)
      if(return=='data') return(data)
      if(return=='cleaned') return(cleaned)
      if(return=='removed') return(removed)
    }
  }
  
  ### IB ----
  if(sum(x %in% 'IB')>=1) {
    
    checkcorrecttime <<- function(z,y='n') #  hier fuer BA
    {
      # testx=as.numeric(apply.daily(HYG2, function(x) index(first(x))))
      # as.POSIXct(testx, origin = "1970-01-01", tz='EST5EDT')
      # ss=HYG['2008']
      # ss1=ss['T09:55/T09:55:30']
      # ss2=ss['T09:05/T09:05:30']
      # boxplot(as.numeric(ss1$ba), horizontal = TRUE, border='green', col='green', main='09:05 vs. 09:55')
      # boxplot(as.numeric(ss2$ba), horizontal = TRUE, add = TRUE, border='red', col='red')
      subset <- paste('T',format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5), '%H:%M'),'/T',
                      format(as.POSIXct('2000-01-01 8:00', tz='')+60*seq(5,565,by=5)+10, '%H:%M:%S'),sep='')
      print(subset)
      
      z$BA=z[,8]-z[,4]
      # one=z$BA['T09:50/T09:50:30']; two=z$BA['T10:50/T10:50:30']
      # barplot(mean(one, na.rm=T),mean(two, na.rm=T), xlab='', ylab='')
      # print(median(one, na.rm=T)) print(median(two, na.rm=T)) # besser vllt fuer jede stunde und dann barplot?
      if(y=='noplot') { }
    }
    
    makecorrecttime2 <<- function(z)
    {
      tryCatch(makecorrecttime(z), error=function(e) print('ERROR'))
    }
    makecorrecttime <<- function(z,data=NULL,plot=NULL,save=NULL) # ACHTUNG ASSIGNED AUCH
    {
      require(quantmod)
      if(is.null(data)) x = readRDS(paste('~/Dropbox/data/universe/',z,'amin.rds',sep = '')) # assign?
      if(!is.null(data)) x <- data
      x <- x[!duplicated(index(x)),]
      x = x['1980/']
      indexTZ(x) = 'EST5EDT'
      x = x['T09:30/T16:00']
      colnames(x) = c(paste(z,'.bid.Open',sep = ''),paste(z,'.bid.High',sep = ''),
                      paste(z,'.bid.Low',sep = ''),paste(z,'.bid.Close',sep = ''),
                      paste(z,'.ask.Open',sep = ''),paste(z,'.ask.High',sep = ''),
                      paste(z,'.ask.Low',sep = ''),paste(z,'.ask.Close',sep = ''))
      assign(z, x, envir = .GlobalEnv)
      if(!is.null(save)) saveRDS(x, paste('~/data/',z,'mincorrecttime.rds',sep = ''))
      if(!is.null(plot)) { 
       if(plot=='save') pdf(file = paste('~/Dropbox/',z,'.pdf',sep = ''), width = 5, height = 5, bg='white')
        xx=getSymbols(z, auto.assign=F, from='2004-01-01', src='google')
        #lim1=as.numeric(min(xx[,4],xx[,6],apply.daily(x[,4], median, na.rm=T)))
        #lim2=as.numeric(max(xx[,4],xx[,6],apply.daily(x[,4], median, na.rm=T)))
        #plot(apply.daily(x[,4], median, na.rm=T), ylim=c(lim1,lim2), main=z)
        xxx <- apply.daily(x[,4], median, na.rm=T)
        index(xxx) <- as.Date(index(xxx), tz='EST5EDT')
        # toplot <<- as.zoo(xxx)
        toplot <<- as.zoo(na.omit(cbind(xxx,xx[,4])))
        try(toplot <<- as.zoo(na.omit(cbind(xxx,xx[,4],xx[,6]))))
        plot.zoo(toplot[,1], xlab='', ylab='', main=z)
        try(lines(toplot[,3], col='red'), silent=TRUE)
        try(lines(toplot[,2], col='green'), silent=TRUE)
        # try(lines(xx[,6], col='red'), silent=TRUE)
        # try(lines(xx[,4], col='green'), silent=TRUE)
       if(plot=='save') { print(paste(z,'plot saved')); dev.off() }
      }
    }
    
    makeustime <<- function(z,data=NULL)
    {
      if(is.null(data)) x = readRDS(paste('~/Dropbox/data/',z,'amin.rds',sep = '')) 
      if(!is.null(data)) x = data
      x <- x[!duplicated(index(x)),]
      x = x['1980/']
      indexTZ(x) = 'EST5EDT'
      colnames(x) = c(paste(z,'.bid.Open',sep = ''),paste(z,'.bid.High',sep = ''),
                      paste(z,'.bid.Low',sep = ''),paste(z,'.bid.Close',sep = ''),
                      paste(z,'.ask.Open',sep = ''),paste(z,'.ask.High',sep = ''),
                      paste(z,'.ask.Low',sep = ''),paste(z,'.ask.Close',sep = ''))
      assign(z, x, envir = .GlobalEnv)
      saveRDS(x, paste('~/data/',z,'minustime.rds',sep = ''))
    }
    
    readin <<- function(z)
    {
      x = readRDS(paste(a,z,b,sep = '')) # assign?
      assign(z, x, envir = .GlobalEnv)
    }
    # a='~/Dropbox/data/intlETF/minute/'
    # b='mincorrecttime.rds'
    # daten=c('EWA','EWC','EWD','EWG','EWH','EWI','EWJ','EWK','EWL','EWM','EWN','EWO','EWP','EWQ','EWS','EWT','EWU','EWW','EWY','EWZ','EZU','SPY')
    # sapply(daten, readin)
  }
  
  ### OutliersOLD ----
  if(sum(x %in% 'OutliersOLD')>=1) {
    removeoutliers <<- function(x) # nrow circa 369000 per year
    {
      xxx=0
      bidask=x[,8]-x[,4]
      for (i in ((nrow(x)-369000):1))
      { 
        if (is.na(bidask)) # (is.na(x[i,4]) || is.na(x[i,8])) ### FEHLER??? hier muss noch ,i
        { print('na') }
        else { if (as.numeric(bidask[i,]) >= quantile(bidask[i:(i+369000),], probs = 0.99, na.rm = T)) 
        { print(i); xxx = rbind(xxx,i) } } }
      xxx2=which(bidask<0)
      xxx3=rbind(xxx,xxx2)
      x=x[-as.numeric(xxx3),]
    }
    
    ### DIESE FOR A RELATIVELY CLEAN SERIES LIKE VIXmin (oder pre-cleaned DBV?)
    # der VIX hat eine standard abweichung 
    removeoutliers_ba <<- function(x) # nrow circa 369000 per year ## hier multiple columns durch ,
    { 
      require(chemometrics) # wo ist der unterschied zwischen BA und RA?
      xxx = 0; xx1 = sd_trim(x, trim = 0.1); xx2 = sd(x) 
    print(xx1,xx2)
    for (i in ((nrow(x)-50):1))
    { if (x[i,] >= (median(x[i:(i+50),])+5*xx1)) # bisher immer 5
    { print(x[i,]); xxx = rbind(xxx,i) } }
    x[xxx,] }
    
    removeoutliers_ra <<- function(x,y,z,setting) { # y = window, z = st.dev. away
      require(chemometrics); xxx = 0
      x_orig<-x
      if (setting == 'grob') { # hier koennte man auch noch ein split.xts wie bei vladi einbauen
        xx1 = sd_trim(x, trim = 0.1); xx2 = sd(x); print(xx1); print(xx2)
        med=median(x); med1=med + z * xx2; med2=med - z * xx2; print(c(med,med1,med2)) # changed to xx2 weil xx1 multiple columns returned
        # x[which(x>med1 | x<med2),]
        luk1=x[rowSums(x>med1)!=0,] # NICE WAY TO FIND OUTLIER IN ANY COLUMN
        luk2=x[rowSums(x<med2)!=0,] # NICE WAY TO FIND OUTLIER IN ANY COLUMN
        res=rbind(luk1,luk2); print(res); return(res)
      }
      else {
        # for (i in ((nrow(x) - y):1)) ### UNIVARIATE
        # { 
        #   xx1 = sd(x[i:(i + y)], na.rm=T); print(i); print(xx1) # wahlweise ROLLING STDEV
        #   if (x[i] >= (median(x[i:(i + y)], na.rm=T) + z * xx1) | x[i] <= (median(x[i:(i + y)], na.rm=T) - z * xx1))
        #     # falls ein value gefunden wurde, setze ihn dann auf 0, damit nicht verzerrt
        #   { print(x[i]); x[i]<-NA # ; Sys.sleep(10); print(x[(i-5):(i+5)]) funktioniert
        #     xxx = rbind(xxx,i) } 
        # } 
        x = xts(rowMeans(x), order.by = index(x))
        xx1 = sd_trim(x, trim = 0.1); xx2 = sd(x); print(xx1); print(xx2)
        for (i in ((nrow(x) - y):1))
        { 
          # xx1 = sd(x[i:(i + y)], na.rm=T); print(i); print(xx1) 
          if (x[i] >= (median(x[i:(i + y)], na.rm=T) + z * xx1) | x[i] <= (median(x[i:(i + y)], na.rm=T) - z * xx1))
            # falls ein value gefunden wurde, setze ihn dann auf 0, damit nicht verzerrt
          { print(x[i]); x[i]<-NA # ; Sys.sleep(10); print(x[(i-5):(i+5)]) funktioniert
          xxx = rbind(xxx,i) } 
        } 
        # x_orig[xxx] 
        return(xxx)
      }
    }
    
    removeoutliers_mid <<- function(x) # nrow circa 369000 per year
    { xxx=0; xx=sd_trim(x, trim=0.1); print(xx)
    for (i in 6:nrow(x)) 
      #{ if (as.numeric(x[i,]) >= as.numeric(x[i-1,])+as.numeric(xx)/2) { print(x[i,]); xxx = rbind(xxx,i) } } 
    { if (as.numeric(x[i,]) >= min(as.numeric(x[i:(i-5),]))+as.numeric(xx)) { print(x[i,]); xxx = rbind(xxx,i) } } 
    x[xxx,] 
    }
    # removeoutliersnew_c = function(x)
    # { xxx=0
    #   for (i in ((nrow(x)-1000):1))
    #   { if (x[i,] >= (median(x[i:(i+1000),])+5*sd_trim(x[i:(i+1000),], trim=0.1)))
    #   { print(x[xxx,]); xxx = rbind(xxx,i) } }
    #   x[xxx,] } ### ODER MAN MACHT wenn next value 10*vorher ist

  }
}
