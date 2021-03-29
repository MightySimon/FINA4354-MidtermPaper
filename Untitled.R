#Illustration Code

#setting:
#We hedge an S&P500 European Call Option (Z) with 5 of its most outstanding underlyings (S = (S1, ..., S5))
#Apple Inc. (AAPL), Microsoft Corp. (MSFT), Amazon.com Inc. (AMZN), Facebook Inc A (FB), Alphabet Inc C (GOOGL)

#1. obtain data
library("quantmod")
for (ticker in c("SPY", "AAPL", "MSFT", "AMZN", "FB", "GOOGL")) {
    getSymbols(ticker, from = "2020/01/01", to = "2020/12/31", return.class = "data.frame")
}
#they have the same number of rows, 252
n <- nrow(SPY)

Z.rlist <- diff(log(SPY[,4]))
Z.r <- mean(Z.rlist)
Z.vol <- sd(Z.rlist) / sqrt(1/252)

S.r <- rep(0, 5)
S.vol <- rep(0, 5)

AAPL.rlist <- diff(log(AAPL[,4]))
S.r[1] <- mean(AAPL.rlist)
S.vol[1] <- sd(AAPL.rlist) / sqrt(1/252)
MSFT.rlist <- diff(log(MSFT[,4]))
S.r[2] <- mean(MSFT.rlist)
S.vol[2] <- sd(MSFT.rlist) / sqrt(1/252)
AMZN.rlist <- diff(log(AMZN[,4]))
S.r[3] <- mean(AMZN.rlist)
S.vol[3] <- sd(AAPL.rlist) / sqrt(1/252)
FB.rlist <- diff(log(FB[,4]))
S.r[4] <- mean(FB.rlist)
S.vol[4] <- sd(FB.rlist) / sqrt(1/252)
GOOGL.rlist <- diff(log(GOOGL[,4]))
S.r[5] <- mean(GOOGL.rlist)
S.vol[5] <- sd(GOOGL.rlist) / sqrt(1/252)

#2. establish the model
#---Assumptions---
#for Z: European call option
T <- 1  #total time frame = 1
K <- 300    #strike price
trading.days <- c(seq(from = 30, to = 252, by = 30), 252)   #assume hedging portfolio is adjustable every 30 days
n.trading.days <- length(trading.days)

#3. find the prices at each trading node
Z.EurCall <- rep(0, times = n.trading.days)
S <- matrix(data = 0, nrow = n.trading.days, ncol = 5)  

for(i in 1:n.trading.days){
    t = trading.days[i]/252
    W <- sqrt(t) * 1 # 1 is a certain draw from std norm distribution
    
    #Simulate European option of Z
    Z.price <- SPY[n,4]*exp((Z.r-(1/2)*Z.vol^2)*t + Z.vol*W)
    Z.EurCall[i] <- if (Z.price > K) Z.price - K  else 0
    
    #Simulate assets S = (S1, ..., S5)
    S[i,1] <- AAPL[n,4]*exp((S.r[1]-(1/2)*(S.vol[1])^2)*t + S.vol[1]*W)
    S[i,2] <- MSFT[n,4]*exp((S.r[2]-(1/2)*(S.vol[2])^2)*t + S.vol[2]*W)
    S[i,3] <- AMZN[n,4]*exp((S.r[3]-(1/2)*(S.vol[3])^2)*t + S.vol[3]*W)
    S[i,4] <- FB[n,4]*exp((S.r[4]-(1/2)*(S.vol[4])^2)*t + S.vol[4]*W)
    S[i,5] <- GOOGL[n,4]*exp((S.r[5]-(1/2)*(S.vol[5])^2)*t + S.vol[5]*W)

}

Z.EurCall
S   #shows the simulation results

#4. assumptions - market frictions:
c <- 0.1    #assume $0.1 transaction cost per stock traded, both long and short
bar <- 50    #assume in each point, at most 1 stock can be traded, either long or short
            #when k = 1, then constraint: abs(d(k)) <= bar

#5. setting the hedging portfolio's positions, i.e. the solution d(elta)
d <- matrix(data = 0, nrow = n.trading.days, ncol = 5)  

#6. Total cost: a global variable accumulating all costs in transaction
Cost <- 0

#7. the P function to be optimized:
#P = -S.EurCallOpt + <d,S> - cost
P <- function(Z.EurCall.price, S.price, dk.early, dk)
    # Measures P at time T: Z.EurCall.price as portfolio Z's value at T, 
    # S.price as a vector, 
    # dk.early and dk are the position vectors d(k-1) and d(k)
{
    cost.single <- c * sum(abs(dk.early - dk))
    hedge.port.val <- sum(S.price * dk)
    return(-(hedge.port.val - Z.EurCall.price - Cost - cost.single))
}

#8. optimization process
library("GenSA")
for(i in 1:n.trading.days){
    #find the constrained optimal solution
    dk.minusOne <- if (i == 1) rep(0, 5) else d[i-1,]
    result <- GenSA(fn = P, lower = -bar + dk.minusOne, upper = bar + dk.minusOne, 
                    Z.EurCall.price = Z.EurCall[i], S.price = S[i,], dk.early = dk.minusOne)
    d[i,] <- result$par
    Cost <- Cost + c*sum(abs(d[i] - dk.minusOne))
}

#9. display results
d
