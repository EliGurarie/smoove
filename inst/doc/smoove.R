## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ----loading_cvm,warning=FALSE,message=FALSE,cache=FALSE-----------------
require(smoove)
require(magrittr)
require(plyr)
require(scales)

## ------------------------------------------------------------------------
mean(Mod(ucvm2$V))
with(ucvm2, mean(Mod(V)) + c(-2,2)*sd(Mod(V)) / sqrt(length(V)))

## ------------------------------------------------------------------------
ucvm.vaf <- with(ucvm1, getEVAF(Z = Z, T = T, lagmax = 30)) 
acvm.vaf <- with(acvm, getEVAF(Z = Z, T = T, lagmax = 30))  
rcvm.vaf <- with(rcvm, getEVAF(Z = Z, T = T, lagmax = 30))  
racvm.vaf <- with(racvm, getEVAF(Z = Z, T = T, lagmax = 30))

## ------------------------------------------------------------------------
head(ucvm.vaf)

## ----cache=FALSE---------------------------------------------------------
ucvm1$parameters[1:2]

## ----cache=TRUE----------------------------------------------------------
ucvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 0, Tmax = 100, dt = .1)
acvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 2, Tmax = 100, dt = .1)
rcvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 0, Tmax = 100, dt = .1)
racvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 2, Tmax = 100, dt = .1)

## ------------------------------------------------------------------------
with(ucvm, estimateRACVM(Z=Z, T=T, model="UCVM", compare.models=FALSE))

## ----fig.height = 4, echo=-1, cache=FALSE, fig.width = 6-----------------
par(bty="l")
findSingleBreakPoint(Z,T, method = "sweep")

## ------------------------------------------------------------------------
simPhaselist[[1]]

## ------------------------------------------------------------------------
summarizePhases(simPhaselist)

## ----echo = FALSE, eval= FALSE-------------------------------------------
#  require(doParallel)
#  cl <- makeCluster(detectCores())
#  registerDoParallel(cl)

## ----eval=FALSE, include=FALSE-------------------------------------------
#  #save(k.sweep, file = "./vignettes/k.sweep.robj")

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width=90)

## ----width=100--------------------------------------------------------------------------
k.phases <- estimatePhases(k.CPtable, verbose=FALSE)
summarizePhases(k.phases)

