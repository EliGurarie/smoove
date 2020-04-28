require(smoove)
# Simulate a single change point

set.seed(101)
ucvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=1, method="exact")
ucvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=5, tau=10, v0 = ucvm1$V[100], method="exact")
T <- c(ucvm1$T, ucvm1$T[100] + ucvm2$T)
Z <- c(ucvm1$Z, ucvm1$Z[100] + ucvm2$Z)
plot_track(Z)

# testing the true changepoint
testCP(Z, T, 100, 1, 200)
# both AIC and BIC are very much lower for the changepoint model

# testing a changepoint in the middle of the first 100 points 
testCP(Z, T, 50, 1, 100)

