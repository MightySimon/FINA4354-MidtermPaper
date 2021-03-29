#An illustration of the deep hedging algorithm
#I only show the steps in one certain realization of the pricing functions

#Assume the GBM model: all equity prices follows the formula
#S_t = S_0 * e^((miu - 0.5 * sigma^2)*t + sigma * Wt)
#where Wt is the Wiener process, W=sqrt(t)*Z
#We assume that in this simulation, Z = 1

#setting:
#We hedge an AAPL European Call Option with its underlying stock, i.e. AAPL stock

#1. obtain data
library("quantmod")
getSymbols("AAPL", from = "2020/01/01", to = "2020/12/31", return.class = "data.frame")
head(AAPL)
n <- nrow(AAPL)
AAPL.rlist <- diff(log(AAPL[,4]))
AAPL.r <- mean(AAPL.rlist)
AAPL.vol <- sd(AAPL.rlist) / sqrt(1/252)

#2. establish the model
#---Assumptions---
T <- 1  #total time frame = 1
K <- 120    #strike price
trading.days <- c(seq(from = 30, to = 252, by = 30), 252)   #assume hedging portfolio is adjustable every 30 days
n.trading.days <- length(trading.days)

s <- rep(0, times = n.trading.days)
s.EurCall <- rep(0, times = n.trading.days)  #find the prices at each trading node

Z <- 1  #given value
for(i in 1:n.trading.days){
    t = trading.days[i]/252
    W <- sqrt(t) * Z
    s[i] <- AAPL[n,4]*exp((AAPL.r-(1/2)*AAPL.vol^2)*T + AAPL.vol*W)
    s.EurCall[i] <- if (s[i] > K) s[i]-K  else 0
}

#market frictions:
c <- 0.1    #assume $0.1 transaction cost per stock traded, both long and short
            #in this case, cost = c*abs(d(k)-d(k-1))
bar <- 1    #assume in each point, at most 1 stock can be traded, either long or short
            #in this case, constraint: abs(d(k)-d(k-1)) <= bar
            #i.e. -bar + d(k-1) < d(k) < bar + d(k-1)
            #when k = 1, then constraint: abs(d(k)) <= bar
            #i.e. -bar < d(k) < bar

#setting the hedging portfolio's positions
d <- rep(0, times = n.trading.days)

#the P function to be optimized:
#P = -S.EurCallOpt + <d,S> - cost
P <- function(stk, stk.call, dk.early, dk){
    cost <- d * abs(dk.early - dk)
    hedge.portfolio.val <- dk * stk
    return(abs(hedge.portfolio.val - stk - cost))
}
library("GenSA")
for(i in 1:n.trading.days){
    #find the constrained optimal solution
    dk.minusOne <- if (i == 1) 0 else d[i-1]
    result <- GenSA(fn = P, lower = -bar + dk.minusOne, upper = bar + dk.minusOne, stk = s[i], stk.call = s.EurCall[i], dk.early = dk.minusOne)
    d[i] <- result$par
}



