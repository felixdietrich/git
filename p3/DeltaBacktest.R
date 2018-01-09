fx <- readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uxe9zevohx8wayr/fx_bloom2.rds")))
fx_weekly <- lapply(fx, function(x) x[weekdays(index(x))=='Friday'])
# fx[[1]][weekdays(index(x))=='Friday'])

compute_strangle = function(zzz) {
# check <- cbind(fx[[1]],day_subset(fx[[1]][,1], 'Friday')) # hier noch nicht lag benutzen, weil sonst weekly
check <- cbind(zzz,day_subset(zzz[,1], 'Friday')) # hier noch nicht lag benutzen, weil sonst weekly
check <- na.locf(check)
colnames(check) <- c('Spot','Vol','Strike')
# check$Strike_lag <- lag(check$Strike, k=1) # braucht man vermutlich gar nicht weil dort kein delta hedge mehr gemacht wird
day_ss <- function(ss) { switch(ss, 'Friday' = 5, 'Monday' = 4, 'Tuesday' = 3, 'Wednesday' = 2, 'Thursday' = 1 ) }
check$dayz <- sapply(weekdays(index(check)), day_ss)

# check = na.omit(check)
check$s <- day_subset(apply.daily(check, function(x) 2*GBSOption(TypeFlag = "c", S = as.numeric(x[,'Spot']), X = as.numeric(x[,'Strike']), Time = as.numeric(x[,'dayz']/252), r = 0, b = 0, sigma = as.numeric(x[,'Vol'])/100)@price*1000), 'Friday')
# check$s2 <- apply.daily(check, function(x) GBSOption(TypeFlag = "p", S = as.numeric(x[,'Spot']), X = as.numeric(x[,3]), Time = as.numeric(x[,'dayz']/252), r = 0, b = 0, sigma = as.numeric(x[,'Vol'])/100)@price*1000)
check$sc <- day_subset(apply.daily(check, function(x) GBSOption(TypeFlag = "c", S = as.numeric(x[,'Spot']), X = as.numeric(x[,'Strike']), Time = as.numeric(x[,'dayz']/252), r = 0, b = 0, sigma = as.numeric(x[,'Vol'])/100)@price*1000), 'Friday', type='remove')
check$sp <- day_subset(apply.daily(check, function(x) GBSOption(TypeFlag = "p", S = as.numeric(x[,'Spot']), X = as.numeric(x[,'Strike']), Time = as.numeric(x[,'dayz']/252), r = 0, b = 0, sigma = as.numeric(x[,'Vol'])/100)@price*1000), 'Friday', type='remove')
check$st <- rowSums(check[,c('s','sc','sp')], na.rm=T)
check$final <- day_subset(abs(check$Spot-lag(check$Strike,k=1))*1000, zz='Friday')
check$tot <- ifelse(weekdays(index(check))!='Friday',diff(check$st),check$final-lag(check$st,k=1))*-1 # for short
head(check, 26)
# check$st2 <- apply.daily(check, function(x) GBSOption(TypeFlag = "c", S = as.numeric(x[,1]), X = as.numeric(x[,3]), Time = as.numeric(x[,4]/252), r = 0, b = 0, sigma = as.numeric(x[,2])/100)@price*1000)

check$dc <- apply.daily(check, function(x) GBSGreeks(Selection = "delta", TypeFlag = "c", S = as.numeric(x[,'Spot']), X = as.numeric(x[,'Strike']), Time = as.numeric(x[,'dayz'])/252, r = 0, b = 0, sigma = as.numeric(x[,2])/100))
check$dp <- apply.daily(check, function(x) GBSGreeks(Selection = "delta", TypeFlag = "p", S = as.numeric(x[,'Spot']), X = as.numeric(x[,'Strike']), Time = as.numeric(x[,'dayz'])/252, r = 0, b = 0, sigma = as.numeric(x[,2])/100))
check$delta <- round((check$dc + check$dp),2) # nicht *-1 weil man ja REINKAUFT (straddle waere -1)
check$deltalag <- lag(check$delta, k=1)
check <- check[,c('Spot','Vol','Strike','dayz','s','sc','sp','st','final','tot','delta','deltalag')]
check$diff <- diff(check$Spot)
check$reth <- check$deltalag*check$diff*1000
return(check)
}

names(fx)
eur <- compute_strangle(fx[['AUDUSD']])
1.0295-1.0122
sum(eur['2000-01-10/2000-01-14'][,'tot'])
17.3-12.785322
head(eur, 30)

asd <- cbind(apply.weekly(eur, function(x) last(x[,'Vol'])),apply.weekly(eur, colSums)[,c('tot','reth')])
asd <- asd[2:nrow(asd)]
asd$totl <- lag(asd$tot, k=-1)
asd$rethl <- lag(asd$reth, k=-1)
asd$l <- asd$totl+asd$rethl
xxx <- fivequantile(asd, 1)
lapply(xxx, function(x) sr_freq(x[,'l']))
lapply(xxx, function(x) sr_freq(x[,'totl']))

head(asd[,'tot'])
plot.zoo(cumsum(asd[,'tot']))
