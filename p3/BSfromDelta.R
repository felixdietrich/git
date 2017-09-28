# https://quant.stackexchange.com/questions/29507/calculate-strike-from-black-scholes-delta
BSStrikeFromDelta <- function(S0, T, r, sigma, delta, right)
{
  strike <- ifelse(right=="C", 
                   S0 * exp(-qnorm(delta * exp(r*T) ) * sigma * sqrt(T) + ((sigma^2)/2) * T),
                   S0 * exp(qnorm(abs(delta)* exp(r*T) ) * sigma * sqrt(T) + ((sigma^2)/2) * T))
  return(strike)
}

spot <- 1.15
time <- 1/52
sigma <- 20/100

BSStrikeFromDelta(spot, time, 0.0, sigma, 0.25, 'C')

require(fOptions)
GBSGreeks(Selection = "delta", TypeFlag = "c", S = spot, X = BSStrikeFromDelta(spot, time, 0.0, sigma, 0.25, 'C'), Time = time, r = 0, b = 0, sigma = sigma)
GBSGreeks(Selection = "delta", TypeFlag = "p", S = spot, X = BSStrikeFromDelta(spot, time, 0.0, sigma, -0.25, 'P'), Time = time, r = 0, b = 0, sigma = sigma)

getwd()
source('test.R')
