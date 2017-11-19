# Simulate a single change point process (see example in vignette)

ucvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=1, method="exact")
ucvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=10, v0 = ucvm1$V[100], method="exact")

T <- c(ucvm1$T, ucvm1$T[100] + ucvm2$T)
Z <- c(ucvm1$Z, ucvm1$Z[100] + ucvm2$Z)
plot.track(Z)

findSingleBreakPoint(Z,T, method = "sweep")
findSingleBreakPoint(Z,T, method = "optimize")
