### Plot ----

doubleplot <- function(x,y=NULL,main='',omit=FALSE,align=TRUE,
                       col1='black',col2='red',cex.main=NULL,legend='false',xaxt='t',polygon=FALSE) { # y=NULL new
  if (is.null(y)) xx=as.zoo(x)
  if (class(index(x))!=class(index(y))) {
    if (class(index(x))=='yearmon') index(y)=as.yearmon(index(y))
    if (class(index(y))=='yearmon') index(x)=as.yearmon(index(x)) }
  if (omit==TRUE) xx=as.zoo(na.omit(cbind(x,y)))
  if (omit==FALSE) xx=as.zoo(cbind(na.omit(x),na.omit(y))) # xx=as.zoo(cbind(x,y))
  if (align==FALSE) {
    if (polygon==FALSE) { 
      plot(xx[,2], col=col2, lty=2, xlab='', yaxt='n', ylab='', xaxt=xaxt); axis(side = 4) }
    if (polygon==TRUE) { 
      plot(xx[,2], col=col2, type='n', xlab='', yaxt='n', ylab='', xaxt=xaxt)
      polygon(c(index(xx), rev(index(xx))),c(rep(min(xx[,2], na.rm=T),nrow(xx)),rev(xx[,2])),col=col2,border=NA) ### nrow(asd) -> nrow(xx)
      axis(side = 4) }
    par(new = T)
    plot(xx[,1], col=col1, main=main, cex.main=cex.main, xlab='', ylab='', xaxt=xaxt)
    if(legend!='false') legend(legend,colnames(xx),lty=1,col=c(col1,col2), cex=0.8) }
  if (align==TRUE) {
    plot(xx[,1], col=col1, main=main, xlab='', ylab='', ylim=c(min(xx, na.rm=T),max(xx, na.rm=T)))
    if(legend!='false') legend(legend,colnames(xx),lty=1,col=c(col1,col2), cex=0.8) # c('DBCFHX','USDJPY25R1M')
    lines(xx[,2], col=col2) }
}

facet_plot <- function(x,y=NULL,option='base',main=NULL,timex='time',valuex='value') { # 
  if(!is.null(y)) x <- cbind(x,y)
  if(option=='top') {
    print('option: top')
    # require(ggplot2)
    # require(reshape2)
    # localenv <- environment()
    print( ggplot( melt(data.frame(time=index(x), x), id.vars="time"), aes(time, value)) + # environment = localenv
             geom_line() + labs(x = NULL, y = NULL, title = main) +
             facet_wrap( ~ variable, scale = "free_y", ncol=1) + theme(plot.title = element_text(hjust = 0.5)) )
    # print( ggplot( asd, aes_string(timex, valuex)) + # environment = localenv
    #   geom_line() + labs(x = NULL, y = NULL, title = main) +
    #   facet_wrap( ~ variable, scale = "free_y", ncol=1) + theme(plot.title = element_text(hjust = 0.5)) )
  }
  if(option=='right') {
    print('option: right')
    # require(ggplot2)
    # require(reshape2)
    print( ggplot( melt(data.frame(time=time(x), x), id.vars="time"), aes(time, value)) + 
             geom_line() + labs(x = NULL, y = NULL, title = main) +
             facet_grid(variable ~ ., scale = "free_y") + theme(plot.title = element_text(hjust = 0.5)) )
  }
  if(option=='base') {
    print('option: base')
    require(timeSeries)
    plot(as.timeSeries(x), format = "%B %Y", xlab='', main=main)
  }
}

facetplot <- function(df,colnames=NULL) { ### OLD VERSION
  # https://learnr.wordpress.com/2009/05/18/ggplot2-three-variable-time-series-panel-chart/
  require(ggplot2)
  require(reshape2)
  if(!is.null(colnames)) colnames(df) <- colnames
  df_melt = melt(data.frame(date=index(df), coredata(df)), id.vars = 'date')
  ggplot(df_melt, aes(x = date, y = value)) + 
    geom_line() + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + 
    facet_wrap(~ variable, scales = 'free_y', ncol = 1)
}

smoothplot <- function(x, main=NULL, mtext=NULL, colors=seq(1:10), lty=NULL, lwd=NULL,
                      legend='FALSE', colnames='FALSE', cex.legend=1) {
  xx<-na.omit(x)
  # IF NCOL 1 plot(zoo(smooth.spline(as.timeSeries(xx))$y, order.by = index(xx)), ylab='', xlab='', main=y)
  xxx=do.call(cbind, lapply(c(1:ncol(xx)), 
                            function(x) zoo(smooth.spline(as.timeSeries(xx[,x]))$y, order.by = index(xx))))
  if(colnames=='FALSE') colnames(xxx)=seq(1:ncol(xx))
  if(colnames!='FALSE') colnames(xxx)=colnames
  plot(xxx, plot.type='single', ylab='', xlab='', lty=lty, lwd=lwd, main=main, col=colors)
  mtext(mtext)
  if(legend!='FALSE') legend(legend, colnames(xxx), col=colors, lty=1, cex=cex.legend) }

multiplot <- function(x,smooth=NULL,main='',col=c(1:10),cex.main=NULL,legend='false') { 
  if(!is.null(smooth)) { 
    require(timeSeries)
    xx<-na.omit(x)
    x=do.call(cbind, lapply(c(1:ncol(xx)), function(x) zoo(smooth.spline(as.timeSeries(xx[,x]))$y, order.by = index(xx))))
  }
  for (i in c(1:ncol(x))) {
    if(i==1) plot.zoo(x[,i], xlab='', ylab='', col=col[i], main=main)
    if(i>1) plot.zoo(x[,i], xlab='', ylab='', col=col[i], xaxt='n', yaxt='n', main=NULL)
    par(new = T) } 
}

add_legend <- function(...,bty='n',marleft=0,marbottom=0) { # http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
  # opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(marbottom, marleft, 0, 0), new=TRUE) # The default fig setting is (0, 1, 0, 1) and uses the entire device space.
  on.exit(par(opar)) # bottom, left, top, and right
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...,bty=bty) } # horiz=TRUE

fplot <- function(x,col='rainbow',legend='totalbottom',main=NULL,mtext=NULL,save=NULL,lty=NULL,lwd=NULL) { # oder multiplot?
  color <- col
  if(col=='rainbow') color <- palette(rainbow(6)) # palette1 <- distinctColorPalette(ncol(x)) 
  cei1=ceiling(ncol(x)/6); cei2=ceiling(ncol(x)/cei1); 
  cei3=c(rep(1, 6),rep(2, 6),rep(3, 6),rep(4, 6))[1:ncol(x)] # das hat irgendwie mit bar plots zu tun die ich mal gemacht habe
  if(is.null(lty)) lty=cei3
  if(!is.null(save)) pdf(file = paste(save,'.pdf',sep=''), bg='white') # width = 9, height = 7, 
  plot(as.zoo(x), plot.type='single', xlab='', ylab='', main=main, 
       lty = lty, lwd = lwd, col = color)
  mtext(mtext)
  # legend('bottom',colnames(x),lty=1,col=c(1:ncol(x)),cex=0.7,horiz=TRUE)
  if(legend=='totalbottom') add_legend("bottom", colnames(x), col=color, lty = cei3, cex=0.8, lwd=2, ncol=cei2, bty='n') # horiz=TRUE, 
  if(legend!='totalbottom') legend(legend, colnames(x), col=color, lty = cei3, cex=0.8, lwd=1)
  if(!is.null(save)) dev.off() 
}

plot_wo_weekend <- function(x,xblocks=TRUE,main=NULL,mtext=NULL) { 
  library(stringr) # also check formatC sprintf
  z0 <- zoo(coredata(x)) 
  plot(z0, type='n', xaxt = "n", xlab='', ylab='', main=main) 
  mtext(mtext)
  # testx <- x['T9:30/T16:00']
  UShours <- c(paste0(str_pad(9, 2, pad = "0"),c(30:59)),
               paste0(rep(10:15, each=60),str_pad(seq(0,59,by=1), 2, pad = "0")),
               '1600')
  if(xblocks==TRUE) xblocks(zoo(format(index(test), '%H%M') %in% UShours, index(z0)), col='lightgray')
  lines(z0)
  axis(1, time(z0), lab = format(time(x), "%d\n%Hh"), cex.axis = .7) 
}