# FINA4354-MidtermPaper
A simplified deep hedging model for illustration

This is a very simple illustration of the deep hedging concept.
It is only an extraction of parts of the main idea, and the actual process can be many times more complicated.
In this code, I have made several assumptions for the purpose of simplication.
- The optimization is only based on one outcome in the distribution of all possible outcomes of stochastic process Z.
- The measure to determine the optimal P is set to be f(P) = -P, i.e. the minimal amount of cash used to buy the portfolio represented by P/
- Assume that process P and S = (S0, ..., Sn) follows the GBM model, i.e.
#S_t = S_0 * e^((miu - 0.5 * sigma^2)*t + sigma * Wt)
Where Wt = sqrt(t) * Z is a Wiener process.
- Assume that in this outcome, Z = 1

The setting is that:
- The portfolio to be hedged (Z) is a European call option on S&P 500 index.
- The hedging asset(s) is 5 of its most outstanding underlyings: AAPL, MSFT, AMZN, FB, GOOGL.

The result seems to make no sense: d becomes the maximum shares longable in each time points.
- This is our model is very defferent from the reality, and the "optimization" should be reached generally.
- The measure f(P) should not be P, since in this case, longing each stock as much as possible could maximize P or minimize f(P).
- Note that it is the *illustration* of this model that counts, but not the *utility*. 
I am sorry that this code is actually practically useless, but Neural network is really impossible to be realized within several lines of codes.

Thanks for your reading.

Best,
Simon
