asd <- readRDS('~/Dropbox/MS/portfolio_excl.rds')
eq2 <- xts(asd$portfolio$excess, as.yearmon(paste(asd$in.portfolio$year,'-',asd$in.portfolio$month,sep='')))
###
in.portfolio <- xts(asd$in.portfolio[,4:ncol(asd$in.portfolio)], as.yearmon(paste(asd$in.portfolio$year,'-',asd$in.portfolio$month,sep='')))
in.portfolio <- lag(in.portfolio, k=1)

longf <- shortf <- in.portfolio
longf[longf %in% c(-1,0)] <- NA
shortf[shortf %in% c(1,0)] <- NA
cbind(eq2,xts(100*(rowMeans(longf*rx, na.rm=T)+rowMeans(shortf*rx, na.rm=T)), index(rx))) # SAME

in.portfolio <- readRDS('~/Dropbox/MS/portfolio_full.rds')$in.portfolio
in.portfolio <- xts(in.portfolio[,4:ncol(in.portfolio)], as.yearmon(paste(in.portfolio$year,'-',in.portfolio$month,sep='')))
in.portfolio <- lag(in.portfolio, k=1) # lag due to code
rx <- readRDS('~/Dropbox/MS/portfolio_full.rds')$ind.returns$rx
rx <- xts(rx[,4:ncol(rx)], as.yearmon(paste(rx$year,'-',rx$month,sep='')))
all.equal(colnames(rx),colnames(in.portfolio))
ip <- in.portfolio
head(ip)
ip[ip %in% c(0)] <- NA
# ip[ip %in% c(-1,1)] <- 0
# test2 <- Reduce('+', list(ip,rx)) # WRONG HERE
test2 <- ip*rx
test2[is.na(test2)] <- 0
test2
trading = test2[,sort(colnames(test2))]
# trading = test2
pdf(file = 'tradingx4.pdf', width = 12, height = 16, bg='white') # tradingx3.pdf
par(mfrow=c(8,6))
for (i in seq(1:48)) { 
  # mar – following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
  # mgp – sets the axis label locations relative to the edge of the inner plot window. 
  #       The first value represents the location the labels (i.e. xlab and ylab in plot), 
  #       the second the tick-mark labels, and third the tick marks.
  # tcl The length of tick marks as a fraction of the height of a line of text.
  # par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
  par(mgp=c(2.2,0.45,0), mar=rep(2,4))
  # plot.zoo(trading[,i], xlab='', ylab='', main=colnames(trading[,i])) 
  plot.zoo(cumsum(trading[,i]), xlab='', ylab='', main=colnames(trading[,i]))
}
dev.off()

### TEST 2018
ncol(dailyip)
