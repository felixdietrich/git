require(fOptions)
if(!exists('timedefinition')) timedefinition <- 1/52
# https://quant.stackexchange.com/questions/29507/calculate-strike-from-black-scholes-delta
BSStrikeFromDelta <- function(S0, T, r, sigma, delta, right)
{
  strike <- ifelse(right=="C", 
                   S0 * exp(-qnorm(delta * exp(r*T) ) * sigma * sqrt(T) + ((sigma^2)/2) * T),
                   S0 * exp(qnorm(abs(delta)* exp(r*T) ) * sigma * sqrt(T) + ((sigma^2)/2) * T))
  return(strike)
}

# double check the function
# spot <- 1.15
# time <- 1/52
# sigma <- 20/100
# GBSGreeks(Selection = "delta", TypeFlag = "c", S = spot, X = BSStrikeFromDelta(spot, time, 0.0, sigma, 0.25, 'C'), Time = time, r = 0, b = 0, sigma = sigma)
# GBSGreeks(Selection = "delta", TypeFlag = "p", S = spot, X = BSStrikeFromDelta(spot, time, 0.0, sigma, -0.25, 'P'), Time = time, r = 0, b = 0, sigma = sigma)

put_fromdelta = function(x,y) { BSStrikeFromDelta(as.numeric(x[,'Spot']), timedefinition, 0.0, as.numeric(x[,'Vol'])/100, -(y), 'P') }
call_fromdelta = function(x,y) { BSStrikeFromDelta(as.numeric(x[,'Spot']), timedefinition, 0.0, as.numeric(x[,'Vol'])/100, y, 'C') }
# WHAT ABOUT IMPLIED VOLS and BUTTERFLIES HERE
# asd <- fx[[4]][,c(2,1)]
# colnames(asd) = c('Vol','Spot')
# put_fromdelta(asd[1,], 0.25)
# put_fromdelta(data.frame('Vol'=13.50,'Spot'=1.0296), 0.25)

# test if interest rates affect
# GBSOption(TypeFlag = "p", S = 1.10, X = 1.09, Time = 1/52, r = 0.05, b = 0.03, sigma = 9/100)@price 

put_calc = function(x) { 
  spot <- as.numeric(x[,'Spot']); strike <- as.numeric(x[,'down']); vol <- as.numeric(x[,'Vol'])
  GBSOption(TypeFlag = "p", S = spot, X = strike, Time = timedefinition, r = 0.00, b = 0.00, sigma = vol/100)@price 
}
call_calc = function(x) { 
  spot <- as.numeric(x[,'Spot']); strike <- as.numeric(x[,'up']); vol <- as.numeric(x[,'Vol'])
  GBSOption(TypeFlag = "c", S = spot, X = strike, Time = timedefinition, r = 0.00, b = 0.00, sigma = vol/100)@price 
}

straddle_calc = function(x) { 
  spot <- as.numeric(x[,'Spot']); strike <- as.numeric(x[,'Spot']); vol <- as.numeric(x[,'Vol'])
  GBSOption(TypeFlag = "c", S = spot, X = strike, Time = timedefinition, r = 0.00, b = 0.00, sigma = vol/100)@price 
  # as.numeric(x[,'dayz']/252
}
straddle_calc2 = function(x) { 
  spot <- as.numeric(x[,'Spot']); strike <- as.numeric(x[,'Spot']); vol <- as.numeric(x[,'Vol'])
  GBSOption(TypeFlag = "c", S = spot, X = strike, Time = 5/252, r = 0.00, b = 0.00, sigma = vol/100)@price # SLIGHTLY HIGHER than 1/52
}

