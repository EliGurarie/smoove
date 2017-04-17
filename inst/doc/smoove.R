## ----loading_cvm,warning=FALSE,message=FALSE-----------------------------
require(smoove)

## ----ucvm1, cache=TRUE---------------------------------------------------
nu <- 2
tau <- 5
dt <- .1 
ucvm1 <- simulateUCVM(nu=nu, tau=tau, T.max = 1000, dt = dt)

## ------------------------------------------------------------------------
str(ucvm1)

## ----FirstPlot, echo=-1, fig.height=3, cache=TRUE------------------------
par(mfrow=c(1,3), bty="l", mar = c(4,4,1,1))
plot(ucvm1$Z, asp=1, type="l", main = "plotting Z")
plot(ucvm1$XY, asp=1, type="l", main = "plotting XY")
plot.track(ucvm1$XY, col=rgb(0,0,0,.01), main = "using plot.track")

## ------------------------------------------------------------------------
mean(Mod(ucvm1$V))

## ------------------------------------------------------------------------
sqrt(mean(Mod(ucvm1$V)^2))

## ----ucvm.exact, echo=-1, fig.height=4, cache=TRUE-----------------------
par(mar = c(0,3,0,0), oma = c(4,2,2,2), mgp = c(2,0.5,0), tck=-0.01, xpd=NA, bty="l", las=1)
T <- cumsum(rexp(100))
ucvm2 <- simulateUCVM.exact(T = T, nu = 2, tau = 2)
with(ucvm2, scan.track(time = T, z = Z))

## ------------------------------------------------------------------------
summary(diff(ucvm2$T))

## ------------------------------------------------------------------------
mean(Mod(ucvm2$V))
with(ucvm2, mean(Mod(V)) + c(-2,2)*sd(Mod(V)) / sqrt(length(V)))

## ----simulateRACVM, cache=TRUE-------------------------------------------
rcvm <- simulateRACVM(tau = 6, eta = 2, omega = 1, mu = 0, Tmax = 1000, dt = .1)
acvm <- simulateRACVM(tau = 6, eta = 2, omega = 0, mu = 2, Tmax = 1000, dt = .1)
racvm <- simulateRACVM(tau = 6, eta = 2, omega = 1, mu = 1, Tmax = 1000, dt = .1)

## ----plotRACVM, cache=TRUE, echo=-1, fig.height=4, bty="l"---------------
layout(rbind(c(1,2,2),c(1,3,3))); par(bty="l", mar = c(3,3,3,1), mgp = c(2,0.5,0), tck=0.01)
plot.track(rcvm$Z[rcvm$T < 100],  main = "RCVM")
plot.track(acvm$Z[acvm$T < 100], main = "ACVM")
plot.track(racvm$Z[racvm$T < 100], main = "RACVM")

## ------------------------------------------------------------------------
ucvm.vaf <- with(ucvm1, getEVAF(Z = Z, T = T, lagmax = 30)) 
acvm.vaf <- with(acvm, getEVAF(Z = Z, T = T, lagmax = 30))  
rcvm.vaf <- with(rcvm, getEVAF(Z = Z, T = T, lagmax = 30))  
racvm.vaf <- with(racvm, getEVAF(Z = Z, T = T, lagmax = 30))

## ------------------------------------------------------------------------
head(ucvm.vaf)

## ----EVAFplots, echo = -1, cache=TRUE------------------------------------
layout(cbind(1:4*2-1, 1:4*2, 1:4*2)); par(mar=c(3,2,2,0), oma = c(4,2,2,2), bty="l", mgp=c(1.5,0,0), tck=0.01, xpd=NA, las=1)
plot.track(ucvm1$Z[1:1e3], main = "UCVM"); plot(ucvm.vaf, type="l", lwd=1.5)
plot.track(acvm$Z[1:1e3], main = "ACVM"); plot(acvm.vaf, type="l", lwd=1.5)
plot.track(rcvm$Z[1:1e3], main = "RCVM"); plot(rcvm.vaf, type="l", lwd=1.5)
plot.track(racvm$Z[1:1e3], main = "RACVM"); plot(racvm.vaf, type="l", lwd=1.5)

## ----VAF1, fig.height=4, echo=-1, cache=TRUE-----------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=2)
estimateUCVM(z = ucvm1$Z, t = ucvm1$T, method = "vaf", diagnose = TRUE, CI=TRUE)

## ----cache=FALSE---------------------------------------------------------
ucvm1$parameters[1:2]

## ----VAF.lores, cache=TRUE-----------------------------------------------
ucvm.lores <- simulateUCVM(nu=10, tau = 4, dt = 1, T.max = 1000, method = "exact")	
with(ucvm.lores, estimateUCVM(z = Z, t = T, CI=TRUE, method="vaf"))

## ----VAF.lores.splined, cache=TRUE---------------------------------------
with(ucvm.lores, estimateUCVM(z = Z, t = T, CI=TRUE, method="vaf", spline = TRUE))

## ----CRW, echo=-1, fig.height=5, out.width = "4in", fig.align = "center"----
  par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1)
  tau <- 2; nu <- 8
  ucvm3 <- simulateUCVM(T=1:1000, nu = nu, tau = tau, method = "exact")
  plot(ucvm3$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateUCVM(z = ucvm3$Z, t = ucvm3$T, CI=TRUE, method="crw", diagnose=TRUE)

## ----vLike, cache=TRUE---------------------------------------------------
estimateUCVM(z = ucvm1$Z, t = ucvm1$T, method = "vLike", CI=TRUE)

## ----vLike.irregular, cache=TRUE-----------------------------------------
estimateUCVM(z = ucvm2$Z, t = ucvm2$T, method = "vLike", CI=TRUE)

## ----zLike.irregular, cache=TRUE-----------------------------------------
estimateUCVM(z = ucvm2$Z, t = ucvm2$T, method = "zLike", CI=TRUE)

## ----crawl1, cache=TRUE--------------------------------------------------
with(ucvm2, estimateUCVM(z = Z, t = T, method = "crawl"))

## ----crawl2, cache=TRUE--------------------------------------------------
with(ucvm1, estimateUCVM(z = Z, t = T, method = "crawl"))$nutau

## ----cache=TRUE----------------------------------------------------------
ucvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 0, Tmax = 100, dt = .1)
acvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 2, Tmax = 100, dt = .1)
rcvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 0, Tmax = 100, dt = .1)
racvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 2, Tmax = 100, dt = .1)

## ----ucvm, cache=TRUE, echo=FALSE----------------------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot.track(ucvm$Z)

## ----ucvm.fit, cache=TRUE, size = "small"--------------------------------
with(ucvm, estimateRACVM(Z=Z, T=T, compare.models=TRUE))

## ------------------------------------------------------------------------
with(ucvm, estimateRACVM(Z=Z, T=T, model="UCVM", compare.models=FALSE))

## ----acvm, cache=TRUE, echo=FALSE----------------------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot.track(acvm$Z)

## ----acvm.fit, cache=TRUE, size = "small"--------------------------------
with(acvm, estimateRACVM(Z=Z, T=T, model = "ACVM", 
                         compare.models=TRUE))

## ----rcvm, cache=TRUE, echo=-1-------------------------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1, col=rgb(0,0,0,.5)); plot.track(rcvm$Z)

## ----rcvm.fit, cache=TRUE, size = "small"--------------------------------
with(rcvm, estimateRACVM(Z=Z, T=T, model = "RCVM", 
                         compare.models=TRUE))

## ----racvm, cache=TRUE, echo=-1------------------------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot.track(racvm$Z)

## ----racvm.fit, cache=TRUE, size = "footnotesize"------------------------
with(racvm, estimateRACVM(Z=Z, T=T, model = "RACVM", 
                          compare.models=TRUE))

## ----KestrelFlight, fig.height=8, fig.width = 6, echo=-1, out.width="3in", fig.align="center"----
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1, bty="l")
data(Kestrel); plot.track(Kestrel[,c("X","Y")], cex=0.3)

## ----K1, echo=-1, fig.height=4, cache=TRUE-------------------------------
  par(mar = c(0,2,0,0), oma = c(4,4,2,2), bty="l")
  K1 <- Kestrel[3360:3450,]
  with(K1, scan.track(x=X, y=Y))

## ----K1.fit, cache=TRUE--------------------------------------------------
(fit1 <- with(K1, estimateRACVM(XY = cbind(X,Y), T = timestamp, spline=TRUE, 
                                model = "RACVM", time.units = "sec")))

## ----K1.sim, echo=-1, fig.height=4, cache=TRUE---------------------------
par(mar = c(0,2,0,0), oma = c(4,4,2,2), bty="l")
p.fit1 <- with(fit1$results, list(eta=eta[1], tau = tau[1], 
                                  mu = mu.x[1] + 1i*mu.y[1], 
                                  omega = omega[1], v0 = diff(K1$Z)[1]))
K1.sim <- with(p.fit1, simulateRACVM(eta=eta, tau=tau, mu = mu, omega=omega, 
                                     Tmax = nrow(K1), v0 = v0, dt = 1))
with(K1.sim, scan.track(z=Z))

## ----TwoPhaseCVM, fig.height = 4, echo=-1, out.width = "6in", cache=TRUE, fig.align="center"----
par(bty="l")
ucvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=1, method="exact")
ucvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=10, v0 = ucvm1$V[100], method="exact")
T <- c(ucvm1$T, ucvm1$T[100] + ucvm2$T)
Z <- c(ucvm1$Z, ucvm1$Z[100] + ucvm2$Z)
plot.track(Z)

## ----fig.height = 4, echo=-1, cache=FALSE--------------------------------
par(bty="l")
findSingleBreakPoint(Z,T, method = "sweep")

## ------------------------------------------------------------------------
max(ucvm1$T)

## ----simSweepGenerate, echo=-1, out.width = "4in", fig.height = 4, fig.width = 4, cache=FALSE, fig.align="center"----
par(bty="l"); set.seed(101)
ucvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=1, method="exact")
ucvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=5, tau=5, v0 = ucvm1$V[100], method="exact")
ucvm3 <- simulateUCVM(T=cumsum(rexp(100)), nu=5, tau=1, v0 = ucvm2$V[100], method="exact")

T <- c(ucvm1$T, ucvm1$T[100] + ucvm2$T, ucvm1$T[100] + ucvm2$T[100] + ucvm3$T)
Z <- c(ucvm1$Z, ucvm1$Z[100] + ucvm2$Z, ucvm1$Z[100] + ucvm2$Z[100] + ucvm3$Z)
plot.track(Z)

## ----simSweep1, cache=TRUE-----------------------------------------------
simSweep <- sweepRACVM(Z=Z, T=T, windowsize = 100, windowstep = 10, model = "UCVM", progress=FALSE)

## ----eval=FALSE, include=FALSE-------------------------------------------
#  save(simSweep, file = "simSweep.robj")

## ----echo=-1, fig.height=3-----------------------------------------------
par(bty="l")
plotWindowSweep(simSweep)

## ------------------------------------------------------------------------
CP.all <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 0)
CP.all

## ------------------------------------------------------------------------
CP.clustered <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 4)
CP.clustered

## ----simGetCPtable-------------------------------------------------------
getCPtable(CPs = CP.clustered, Z = Z, T = T, modelset = "UCVM", tidy = FALSE, iterate = FALSE)

## ----simGetCPtable2, cache=TRUE------------------------------------------
getCPtable(CPs = CP.clustered, Z = Z, T = T, modelset = "UCVM", tidy = "extremes", iterate = TRUE)

## ----exampleCPtables, cache=TRUE-----------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 1) %>% 
  getCPtable(Z = Z, T = T, modelset = "UCVM")
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(Z = Z, T = T, modelset = "UCVM")
simSweep %>% findCandidateChangePoints(clusterwidth = 10) %>% 
  getCPtable(Z = Z, T = T, modelset = "UCVM")

## ----simSweepAllModels, cache=TRUE---------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% getCPtable(Z = Z, T = T, modelset ="all")

## ------------------------------------------------------------------------
simCP.table <- simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(Z = Z, T = T, modelset = "UCVM")
simPhases <- getPhases(simCP.table, Z = Z, T = T)

## ----echo=-(1:2), message=FALSE------------------------------------------
  layout(c(1,1,1,1,1,1,2,2,3,3)) 
  par(mar=c(4,0,1,0), oma = c(5,5,1,2), xpd=NA, las = 1, cex.lab = 1.5, tck = 0.01, mgp = c(1.5,0.25,0), bty="n")
  
  require(gplots)
  cols <- rich.colors(length(simPhases))
  T.cuts <- c(T[1], simCP.table$CP, T[length(T)])
  Z.cols <- cols[cut(T, T.cuts, include.lowest = TRUE)]
  
  phaseTable <- summarizePhases(simPhases)
  
  plot(Z, asp=1, type="l", xpd=FALSE)
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  legend("topright", legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
         fill=cols, ncol=1, bty="n", title = "Phase: model")

  par(mar=c(1,0,1,0), xpd=NA)
  plotPhaseParameter("tau", simPhases, ylab="time units", xaxt="n", xlab="", col=cols, log="y")
  plotPhaseParameter("eta", simPhases,  ylab="distance / time", xlab="time", col=cols)

## ----subsetKestrel, echo=-1, out.height = "4in", fig.height=4------------
par(bty="l", mar = c(0,5,0,0), xpd=NA, oma= c(4,0,4,2))
data(Kestrel)
k <- Kestrel[3730:4150,]
head(k)
with(k, scan.track(x=X, y=Y))

## ----sweepKestrel, cache=TRUE, message=FALSE-----------------------------
k$T <- as.numeric(k$timestamp - min(k$timestamp))
k.sweep <- with(k, sweepRACVM(XY=cbind(X,Y), T=T, windowsize = 50, windowstep = 5, model = "RACVM", time.units = "sec", progress=FALSE))

## ----eval=FALSE, include=FALSE-------------------------------------------
#  save(k.sweep, file = "./vignettes/k.sweep.robj")

## ----plotKestrelSweep, echo=-1, fig.height=3, cache=TRUE-----------------
par(bty="l")
plotWindowSweep(k.sweep)

## ----findKestrelCPs, cache=TRUE------------------------------------------
k.CPs <- findCandidateChangePoints(windowsweep = k.sweep, clusterwidth = 2)

## ----filterKestrelCPs, cache=TRUE----------------------------------------
XY <- cbind(k$X, k$Y)
k.CPtable <- getCPtable(CPs = k.CPs, XY = XY, T = k$T, 
           modelset = "all", tidy = c("extremes"), iterate = FALSE, spline=TRUE)

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width=90)

## ----size = "footnotesize", width=100---------------------------------------------------
Z <- XY[,1] + 1i*XY[,2]
k.phases <- getPhases(k.CPtable, T=k$T, Z=Z, verbose=FALSE)
summarizePhases(k.phases)

## ----kestrelMegaPlot, echo=-(1:2), eval=TRUE, cache=TRUE--------------------------------
  layout(c(1,1,1,1,2,3,4,5,6)) 
  par(mar=c(4,0,1,0), oma = c(5,5,1,2), xpd=NA, las = 1, cex.lab = 1.25, tck = 0.01, mgp = c(1.5,0.25,0), bty="n")
  
  cols <- rich.colors(length(k.phases))
  T.cuts <- c(k$T[1], k.CPtable$CP, k$T[length(k$T)])
  Z.cols <- cols[cut(k$T, T.cuts, include.lowest = TRUE)]
  
  phaseTable <- summarizePhases(k.phases)
  
  plot(Z, asp=1, type="l", xpd=FALSE, xlab="X (m)")
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  legend("topleft", legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
         fill=cols, ncol=2, bty="n", title = "Phase: model")

  par(mar=c(1,0,1,0), xpd=NA)
  plotPhaseParameter("tau", k.phases, ylab="sec", xaxt="n", xlab="", col=cols, log="y")
  plotPhaseParameter("eta", k.phases,  ylab="m / sec", xlab="time", col=cols)
  plotPhaseParameter("omega", k.phases,  ylab="rad / sec", xlab="time", col=cols)
  plotPhaseParameter("mu.x", k.phases,  ylab="m / sec", xlab="time", col=cols)
  plotPhaseParameter("mu.y", k.phases,  ylab="m / sec", xlab="time", col=cols)

