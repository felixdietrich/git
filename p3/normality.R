source('p3/p3_functions.R')
source('CustomFunc.R')
fx <- readRDS(gzcon(url("https://dl.dropboxusercontent.com/s/uxe9zevohx8wayr/fx_bloom2.rds"))) # 2000-01-04/2016
fx <- lapply(fx, function(x) { x[,1] <- diff(log(x[,1])); return(x) }) ### LOG
fx_weekly <- lapply(fx, function(x) x[weekdays(index(x))=='Monday']) # ist egal dass hier schon returns gemacht wurden

cur = 'EURUSD'
# AUDUSD <- fx[['AUDUSD']]
AUDUSD <- fx[[cur]]
AUDUSD
# head(AUDUSD)
# asd <- fivequantile(fx_weekly[['AUDUSD']], 2)
asd <- fivequantile(fx_weekly[[cur]], 2)

# lapply(asd, function(x) weekdays(index(x)))
# asd[[1]]
# week_this(asd[[1]]['2005-03-10/'])
norm <- sapply(c(1:5), function(x) AUDUSD[as.character(week_this(asd[[x]]))]) # returns also list?
rang = c(floor_dec(min(fx[[cur]][,1], na.rm=T),2), ceiling_dec(max(fx[[cur]][,1], na.rm=T),2))

# anyDuplicated(c(index(norm[[1]]),index(norm[[2]]),index(norm[[3]]),index(norm[[4]]),index(norm[[5]])))
# head(norm[[1]])
asd <- sapply(norm, function(x) round(multi.sapply(x[,1], mean_freq, sd_freq, SharpeRatio.annualized.arith, skewness, kurtosis, min, max, count_negative, nrow, sum), digits=4))
stargazer::stargazer(t(asd), digits = 3)
# head(norm[[1]])

### FREQUENCY BASED
for (i in c(1:5)) {
  pdf(paste0('ddens',cur,i,'.pdf'))
  std <- sqrt((mean(norm[[i]][,2])/100)^2/365)
  g <- na.omit(norm[[i]][,1])
  h <- hist(g, breaks = 50, density = 30, col = "lightgray", xlim=rang, xlab = "Return", main = paste('Quantile',i)) 
  abline(v=mean(norm[[i]][,2]/100*sqrt(1/365), na.rm=T), col='red', lwd=0.5)
  abline(v=mean(norm[[i]][,2]/100*-1*sqrt(1/365), na.rm=T), col='red', lwd=0.5)
  # abline(v=mean(asd[[i]][,2]/100, na.rm=T)*sqrt(1/52), col='orange', lwd=0.5)
  # abline(v=mean(asd[[i]][,2]/100, na.rm=T)*-1*sqrt(1/52), col='orange', lwd=0.5)
  mtext(cur)
  # xfit <- seq(min(g), max(g), length = 40)
  xfit <- seq(rang[1], rang[2], length = 100) # length = 40
  yfit <- dnorm(xfit, mean = 0, sd = std)  # mean = mean(g), sd = sd(g)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(g) 
  lines(xfit, yfit, col = "black", lwd = 2)
  dev.off()
}

g<-norm[[1]][,1]
h <- hist(g, breaks = 50, density = 30, col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 
lines(xfit, yfit, col = "black", lwd = 2)


hist(norm[[1]][,1], breaks=50)
hist(norm[[2]][,1], breaks=50)
hist(norm[[3]][,1], breaks=50)
hist(norm[[4]][,1], breaks=50)
hist(norm[[5]][,1], breaks=50)
mean(norm[[1]][,2])/100*1/365
plot(density(rnorm(100)))
norm[[1]]
index(norm[[2]])

# https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
options(scipen = 999)
m<-0 #mean(norm[[1]][,1])
# std<-mean(norm[[1]][,2])/100*1/365 #sqrt(var(g))
# HIER SOLLTE MAN LAGGEN

std<-sqrt((mean(norm[[1]][,2])/100)^2/365)
hist(norm[[1]][,1], density=20, breaks=50, prob=TRUE, xlab="x-variable", main="normal curve over histogram") # ylim=c(0, 2)
curve(dnorm(x, mean=0, sd=std), col="darkblue", lwd=2, add=TRUE, yaxt="n")
# plot(dnorm(x, mean=m, sd=std))
# dnorm(400, mean=0.01, sd=8)

dev.off()

hist(norm[[1]][,1], density=30, breaks=200, prob=TRUE, xlab="x-variable", main="normal curve over histogram") # ylim=c(0, 2)
lines(density(rnorm(x, mean=0, sd=0.08)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
curve(dnorm(x, mean=0, sd=std, log=FALSE), col="darkblue", lwd=2, add=TRUE, yaxt="n")


