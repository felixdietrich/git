create_data = function(source='datastream',order='vol_first',period='2000/2016',but_incl=FALSE) {

xxx <- readRDS('~/Downloads/Diss2/Diss Excel Files OLD/newfromJan_daily.rds')[['Spot']]
newspot <- readRDS('~/Dropbox/newdiss/git/SpotRates_Bloomberg.rds')
volatilities <- readRDS('~/Dropbox/newdiss/git/p3/volatilities.rds')
butterflies <- readRDS('p3/butterflies_2000.rds')
# names(butterflies)

# data <- list('AUDUSD'=volatilities[['AUDUSD']][,2],'NZDUSD'=volatilities[['NZDUSD']][,2],
#              'USDCAD'=volatilities[['USDCAD']][,2],'EURUSD'=volatilities[['EURUSD']][,2],
#              'USDCHF'=volatilities[['USDCHF']][,2],'USDJPY'=volatilities[['USDJPY']][,2]) # ALL.EQUAL
data <- lapply(volatilities, function(x) x[,2])
if(but_incl==TRUE) {
data <- lapply(names(data), function(x) data[[x]]+butterflies[[x]][,2]) }
# mapply(butterflies)
# head(butterflies[[1]])
# head(volatilities[[1]])
# test <- butterflies[[1]][,2]+volatilities[[1]][,2]
# wherestart(test)
# butterflies[[1]]['2004-07-12']
# volatilities[[1]]['2004-07-12']

spot <- as.list(xxx[,c('AUD','NZD','CAD','EUR','CHF','JPY')]) # NAMES ARE IN CORRECT ORDER
if(source=='bloomberg') {
spot <- as.list(newspot) # NAMES ARE IN CORRECT ORDER
}
names(spot) <- names(data)

final <- lapply(names(data), function(x) cbind(data[[x]],spot[[x]])) 

if(order=='spot_first') {
final <- lapply(names(data), function(x) cbind(spot[[x]],data[[x]])) 
}

names(final) <- names(data)
final <- lapply(final, function(x) na.locf(x))
final <- lapply(final, function(x) x[period])
return(final)
}
# lapply(data, function(x) x[,c(2,1)])

# data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],xxx[,'AUD'])['2005-03-08/2016']), # ist NICHT wegen HF sondern butterfly start
#              na.locf(cbind(volatilities[['NZDUSD']][,2],xxx[,'NZD'])['2005-03-08/2016']),
#              na.locf(cbind(volatilities[['USDCAD']][,2],xxx[,'CAD'])['2005-03-08/2016']),
#              na.locf(cbind(volatilities[['EURUSD']][,2],xxx[,'EUR'])['2005-03-08/2016']),
#              na.locf(cbind(volatilities[['USDCHF']][,2],xxx[,'CHF'])['2005-03-08/2016']),
#              na.locf(cbind(volatilities[['USDJPY']][,2],xxx[,'JPY'])['2005-03-08/2016']))
# data <- list(na.locf(cbind(volatilities[['AUDUSD']][,2],newspot[,'AUDUSD'])['2000/2016']),
#              na.locf(cbind(volatilities[['NZDUSD']][,2],newspot[,'NZDUSD'])['2000/2016']),
#              na.locf(cbind(volatilities[['USDCAD']][,2],newspot[,'USDCAD'])['2000/2016']),
#              na.locf(cbind(volatilities[['EURUSD']][,2],newspot[,'EURUSD'])['2000/2016']),
#              na.locf(cbind(volatilities[['USDCHF']][,2],newspot[,'USDCHF'])['2000/2016']),
#              na.locf(cbind(volatilities[['USDJPY']][,2],newspot[,'USDJPY'])['2000/2016']))
# 
# 
