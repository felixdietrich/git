### generate models ----
models_IS=list('risk'=lm(ahead ~ risk, data=trading), 'riskch'=lm(ahead ~ riskch, data=trading),
               'th'=lm(ahead ~ riskch:riskchpercdummy_l + riskch:riskchpercdummy_u, data=trading),
               'ar'=lm(ahead ~ carry, data=trading), 'arth'=lm(ahead ~ carry:ardummy_l + carry:ardummy_u, data=trading))
OOS=nrow(trading)/2 # 2609
models_OS=list('risk'=lm(ahead ~ risk, data=trading, subset=0:OOS), 'riskch'=lm(ahead ~ riskch, data=trading, subset=0:OOS),
               'th'=lm(ahead ~ riskch:riskchpercdummy_l + riskch:riskchpercdummy_u, data=trading, subset=0:OOS),
               'ar'=lm(ahead ~ carry, data=trading, subset=0:OOS), 'arth'=lm(ahead ~ carry:ardummy_l + carry:ardummy_u, data=trading, subset=0:OOS))
# arth also significant but direction? 

### print results ----
modelresult = function(x,whatcoef=0) { temp.summ <- summary(x)
if(whatcoef==1) temp.summ$coefficients <- unclass(coeftest(x, vcov. = vcovHC))
if(whatcoef==2) temp.summ$coefficients <- unclass(coeftest(x, vcov. = NeweyWest))
round(rbind(lm.beta(x)$standardized.coefficients[1], lm.beta(x)$standardized.coefficients[2], ifelse(length(lm.beta(x)$standardized.coefficients)==3,lm.beta(x)$standardized.coefficients[3],NA),
            temp.summ$coefficients[1,1], temp.summ$coefficients[2,1], ifelse(length(temp.summ$coefficients)==12,temp.summ$coefficients[3,1],NA),
            temp.summ$coefficients[1,4], temp.summ$coefficients[2,4], ifelse(length(temp.summ$coefficients)==12,temp.summ$coefficients[3,4],NA),
            round(temp.summ$adj.r.squared*100, digits=2), nobs(x)),digits=4) } # r.squared*100

res_IS=do.call(cbind, lapply(models_IS,modelresult,whatcoef=2)) 
colnames(res_IS)=names(models_IS); rownames(res_IS)=c('stco1','stco2','stco3','co1','co2','co3','p1','p2','p3','R%','obs')
res_IS

res_OS=do.call(cbind, lapply(models_OS,modelresult,whatcoef=2)) 
colnames(res_OS)=names(models_OS); rownames(res_OS)=c('stco1','stco2','stco3','co1','co2','co3','p1','p2','p3','R%','obs')
res_OS

require(lmtest)
lapply(models_OS,summary)
lapply(models_IS,summary)
invisible(lapply(models_IS,model_eval,printsummary=FALSE))
invisible(lapply(models_OS,model_eval,printsummary=FALSE))

# double-check
test <- summary(lm(formula = ahead ~ riskch:riskchpercdummy_l + riskch:riskchpercdummy_u, data = trading)); test
all.equal(test,summary(models_IS['th']$th))
test <- lm(formula = ahead ~ riskch:riskchpercdummy_l + carry + riskch:riskchpercdummy_u, data = trading)
coeftest(test, vcov. = NeweyWest)

### PREDICTION ----
# prediction hat dasselbe date wie die variables ist aber 1-day ahead, daher mit ahead vergleichen
# see here: head(as.xts(predict.lm(models_IS[['th']], trading), dateFormat="Date")); head(trading) # same start date

genprediction_is = function(x) { as.xts(predict.lm(x, trading), dateFormat="Date") }
genprediction_os = function(x) { as.xts(predict.lm(x, trading[OOS:nrow(trading),]), dateFormat="Date") } ### THIS IS OOS data

# let all models create a forecast, make the starting periods comparable (na.omit) and compare to one day ahead (next periods carry return)
temp_mean <- mean(trading$ahead,na.rm=T)
temp_mean <- mean(trading$ahead[1:OOS,],na.rm=T)

pred_is <- na.omit(cbind(do.call(cbind, lapply(models_IS,genprediction_is)),random=0,mean=temp_mean,trading$ahead)) 
pred_is <- cbind(do.call(cbind, lapply(models_IS,genprediction_is)),random=0,mean=temp_mean,trading$ahead)

pred_os <- na.omit(cbind(do.call(cbind, lapply(models_OS,genprediction_os)),random=0,mean=temp_mean,trading$ahead)) ### OOS

# cbind(pred_is$th, trading$riskchpercdummy_u)['2008']
# saveRDS(test$th, 'thresholdpredictions.rds') # saved from DBCFHX

msfe = function(x) {  sqrt(mean( (x - test$ahead) ^ 2, na.rm = T)) }
mae = function(x) {   mean( abs(x - test$ahead),  na.rm = T) }
dir = function(x) {   mean( ifelse(test$ahead*x >= 0, 1, 0), na.rm = T) }
# dir = function(x) {   mean( ifelse(test$ahead*x > 0, 1, 0), na.rm = T) }
count1 = function(x) { nrow(x[which(x<0)]) } # how many times a negative forecast?
count2 = function(x) { nrow(x[which(x>0)]) } # how many times a positive forecast?
count3 = function(x) { nrow(x[which(x==0)]) }

test <- pred_is
test <- pred_os
head(test)
nrow(test)
# nrow(test$ahead[test$ahead>=0])/nrow(test) # 0.5735914
# nrow(test$ahead[test$ahead>0])/nrow(test) # 0.5295132
# test <- test[!test$ahead==0] # test if zeros make a difference -> no, similar results
# nrow(test[which(test$th<0)])
# mean( ifelse(test$ahead*test$th >= 0, 1, 0), na.rm = T)
# mean( ifelse(test$th < 0 & test$ahead < 0, 1, 0), na.rm = T)
# mean( ifelse(test$th > 0 & test$ahead > 0, 1, 0), na.rm = T)
# test[test$th < 0 & test$ahead < 0]
do.call(cbind, lapply(test, mae))
asd <- rbind(do.call(cbind, lapply(test, msfe)), do.call(cbind, lapply(test, mae)), do.call(cbind, lapply(test, dir)), 
             do.call(cbind, lapply(test, count1)), do.call(cbind, lapply(test, count2)), do.call(cbind, lapply(test, count3)))[,c(1:7)]
rownames(asd)=c('msfe','mae','dir','count1','count2','count3')
asd[1:2,] <- asd[1:2,]*1000
options(scipen = 999)
stargazer::stargazer(asd[,c('risk','riskch','th','ar','random','mean')], digits=4)
round(asd, digits=6)
sapply(test, min)*100
length(which(test$th==Mode(test$th))) # 4162
length(which(test$th!=Mode(test$th))) # 1056
length(which(test$th>Mode(test$th))) # 533
length(which(test$th<Mode(test$th))) # 523
533+4162
colSums(asd[c(4,5),])
colSums(asd[c(4:6),])
4162+1056

test <- pred_os
asd <- rbind(do.call(cbind, lapply(test, msfe)), do.call(cbind, lapply(test, mae)), do.call(cbind, lapply(test, dir)), 
             do.call(cbind, lapply(test, count1)), do.call(cbind, lapply(test, count2)), do.call(cbind, lapply(test, count3)))[,c(1:7)]
rownames(asd)=c('msfe','mae','dir','count1','count2','count3')
round(asd, digits=6)