if(interactive()){
require(smoove)

#----------------------------------------------------------
# Example 1: VAF method (high resolution, regular sampling)
#----------------------------------------------------------

nu <- 10
tau <- 3
ucvm1 <- simulateUCVM(nu=nu, tau = tau, dt = 0.1, T.max = 1000)	
plot(ucvm1$Z, asp=1, cex=0.5, pch=19, col=rgb(0,0,0,.1))
estimateUCVM(Z = ucvm1$Z, T = ucvm1$T, CI=TRUE, method="vaf", diagnose=TRUE)

# coarser sampling 
ucvm2 <- simulateUCVM(nu=nu, tau = tau, dt = 1, T.max = 1000, method = "exact")	
plot(ucvm2$Z, asp=1, cex=0.5, pch=19, col=rgb(0,0,0,.3), type="o")
with(ucvm2, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vaf"))

# possible slightly improved speed estimate by splining.
with(ucvm2, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vaf", spline=TRUE))

#----------------------------------------------------------
# Example 2: CRW method (low resolution, regular sampling)
#----------------------------------------------------------

tau <- 1
nu <- 8
ucvm3 <- simulateUCVM(T=1:1000, nu = nu, tau = tau, method = "exact")
plot(ucvm3$Z, asp=1, type="o", cex=0.5, pch=19)
with(ucvm3, estimateUCVM(Z = Z, T = T, CI=TRUE, method="crw", diagnose=TRUE))

# Example 2b: CRW method, poor diagnostics (highly auto-correlated step-lengths)
tau <- 20
ucvm4 <- simulateUCVM(T=1:1000, nu=nu, tau=tau, method="exact")
plot(ucvm4$Z, asp=1, type="o", cex=0.5, pch=19)
with(ucvm4, estimateUCVM(Z = Z, T = T, CI=TRUE, method="crw", diagnose=TRUE))

#--------------------------------------------------------------------------------
# Example 3: One-step likelihood method (higher resolution, irregular sampling)
#--------------------------------------------------------------------------------

nu <- 10
tau <- 3

# Irregular timing

T <- cumsum(rexp(1000,1/.1))
ucvm5 <- simulateUCVM(T=T, nu=nu, tau=tau, method="exact")
plot(ucvm5$Z, asp=1, type="o", cex=0.5, pch=19)
with(ucvm5, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vLike"))

# low resolution example

T <- cumsum(rexp(1000,1))
ucvm6 <- simulateUCVM(T=T, nu=nu, tau=tau, method="exact")
plot(ucvm6$Z, asp=1, type="o", cex=0.5, pch=19)
with(ucvm6, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vLike"))

# improved with splining
with(ucvm6, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vLike", spline=TRUE))

#--------------------------------------
# Example 4: Full-likelihood method 
#--------------------------------------

nu <- 10; tau <- 3; v0 <- 10
T <- cumsum(rexp(100,1))
ucvm7 <- simulateUCVM(T=T, nu=nu, tau=tau, method = "exact") 
plot(ucvm7$Z, asp=1, type="o", cex=0.5, pch=19)
with(ucvm7, estimateUCVM(Z = Z, T = T, CI=TRUE, method="zLike"))

#-------------------------------------------------------
# Example 5: `crawl' method (from Johnson et al. 2008) 
#-------------------------------------------------------

# 5a. same track as above
with(ucvm7, estimateUCVM(Z = Z, T = T, method="crawl"))

# 5b. much longer track
T <- cumsum(rexp(1000,1))
ucvm8 <- simulateUCVM(T = T, nu = nu, tau = tau, method="exact") 
plot(ucvm8$Z, asp=1, type="o", cex=0.5, pch=19, col=rgb(0,0,0,.2))
with(ucvm8, estimateUCVM(Z = Z, T = T, method="crawl"))
}