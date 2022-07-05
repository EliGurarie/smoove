## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ----loading_cvm,warning=FALSE,message=FALSE,cache=FALSE----------------------
require(smoove)
require(magrittr)
require(plyr)
require(scales)

## ----ucvm1, cache=TRUE--------------------------------------------------------
nu <- 2
tau <- 5
dt <- .1 
ucvm1 <- simulateUCVM(nu=nu, tau=tau, T.max = 1000, dt = dt)

## -----------------------------------------------------------------------------
str(ucvm1)

## ----FirstPlot, echo=-1, fig.height=3, cache=TRUE, fig.width = 6--------------
par(mfrow=c(1,3), bty="l", mar = c(4,4,1,1))
plot(ucvm1$Z, asp=1, type="l", main = "plotting Z")
plot(ucvm1$XY, asp=1, type="l", main = "plotting XY")
plot_track(ucvm1$XY, col=rgb(0,0,0,.01), main = "using plot_track")

## -----------------------------------------------------------------------------
mean(Mod(ucvm1$V))

## -----------------------------------------------------------------------------
sqrt(mean(Mod(ucvm1$V)^2))

## ----ucvm.exact, echo=-1, fig.height=4, cache=TRUE, fig.width = 6, echo = -1----
set.seed(2)
par(mar = c(0,3,0,0), oma = c(4,2,2,2), mgp = c(2,0.5,0), tck=-0.01, xpd=NA, bty="l", las=1)
T <- cumsum(rexp(100))
ucvm2 <- simulateUCVM.exact(T = T, nu = 2, tau = 2)
with(ucvm2, scan_track(time = T, z = Z))

## -----------------------------------------------------------------------------
summary(diff(ucvm2$T))

## -----------------------------------------------------------------------------
mean(Mod(ucvm2$V))
with(ucvm2, mean(Mod(V)) + c(-2,2)*sd(Mod(V)) / sqrt(length(V)))

## ----simulateRACVM, cache=TRUE------------------------------------------------
rcvm <- simulateRACVM(tau = 6, eta = 2, omega = 1, mu = 0, Tmax = 1000, dt = .1)
acvm <- simulateRACVM(tau = 6, eta = 2, omega = 0, mu = 2, Tmax = 1000, dt = .1)
racvm <- simulateRACVM(tau = 6, eta = 2, omega = 1, mu = 1, Tmax = 1000, dt = .1)

## ----plotRACVM, cache=TRUE, echo=-1, fig.height=4, bty="l", fig.width = 6-----
layout(rbind(c(1,2,2),c(1,3,3))); par(bty="l", mar = c(3,3,3,1), mgp = c(2,0.5,0), tck=0.01)
plot_track(rcvm$Z[rcvm$T < 100],  main = "RCVM")
plot_track(acvm$Z[acvm$T < 100], main = "ACVM")
plot_track(racvm$Z[racvm$T < 100], main = "RACVM")

## -----------------------------------------------------------------------------
ucvm.vaf <- with(ucvm1, getEVAF(Z = Z, T = T, lagmax = 30)) 
acvm.vaf <- with(acvm, getEVAF(Z = Z, T = T, lagmax = 30))  
rcvm.vaf <- with(rcvm, getEVAF(Z = Z, T = T, lagmax = 30))  
racvm.vaf <- with(racvm, getEVAF(Z = Z, T = T, lagmax = 30))

## -----------------------------------------------------------------------------
head(ucvm.vaf)

## ----EVAFplots, echo = -1, cache=TRUE, fig.width=6, fig.height = 6------------
layout(cbind(1:4*2-1, 1:4*2, 1:4*2)); par(mar=c(3,2,2,0), oma = c(4,2,2,2), bty="l", mgp=c(1.5,0,0), tck=0.01, xpd=NA, las=1)
plot_track(ucvm1$Z[1:1e3], main = "UCVM"); plot(ucvm.vaf, type="l", lwd=1.5)
plot_track(acvm$Z[1:1e3], main = "ACVM"); plot(acvm.vaf, type="l", lwd=1.5)
plot_track(rcvm$Z[1:1e3], main = "RCVM"); plot(rcvm.vaf, type="l", lwd=1.5)
plot_track(racvm$Z[1:1e3], main = "RACVM"); plot(racvm.vaf, type="l", lwd=1.5)

## ----VAF1, fig.height=4, echo=-1, cache=TRUE, fig.width = 6-------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=2)
estimateUCVM(Z = ucvm1$Z, T = ucvm1$T, method = "vaf", diagnose = TRUE, CI=TRUE)

## ----cache=FALSE--------------------------------------------------------------
ucvm1$parameters[1:2]

## ----VAF.lores, cache=TRUE----------------------------------------------------
ucvm.lores <- simulateUCVM(nu=10, tau = 4, dt = 1, T.max = 1000, method = "exact")	
with(ucvm.lores, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vaf"))

## ----VAF.lores.splined, cache=TRUE--------------------------------------------
with(ucvm.lores, estimateUCVM(Z = Z, T = T, CI=TRUE, method="vaf", spline = TRUE))

## ----CRW, echo=-1, fig.height=5, out.width = "4in", fig.align = "center"------
  par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1)
  tau <- 2; nu <- 8
  ucvm3 <- simulateUCVM(T=1:1000, nu = nu, tau = tau, method = "exact")
  plot(ucvm3$Z, asp=1, type="o", cex=0.5, pch=19)
  estimateUCVM(Z = ucvm3$Z, T = ucvm3$T, CI=TRUE, method="crw", diagnose=TRUE)

## ----vLike, cache=TRUE--------------------------------------------------------
estimateUCVM(Z = ucvm1$Z, T = ucvm1$T, method = "vLike", CI=TRUE)

## ----vLike.irregular, cache=TRUE----------------------------------------------
estimateUCVM(Z = ucvm2$Z, T = ucvm2$T, method = "vLike", CI=TRUE)

## ----zLike.irregular, cache=TRUE----------------------------------------------
estimateUCVM(Z = ucvm2$Z, T = ucvm2$T, method = "zLike", CI=TRUE)

## ----crawl1, cache=TRUE-------------------------------------------------------
with(ucvm2, estimateUCVM(Z = Z, T = T, method = "crawl"))

## ----crawl2, cache=TRUE-------------------------------------------------------
with(ucvm1, estimateUCVM(Z = Z, T = T, method = "crawl"))

## ----cache=TRUE---------------------------------------------------------------
ucvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 0, Tmax = 100, dt = .1)
acvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 2, Tmax = 100, dt = .1)
rcvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 0, Tmax = 100, dt = .1)
racvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 2, Tmax = 100, dt = .1)

## ----ucvm, cache=TRUE, echo=FALSE, out.width = "4in"--------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot_track(ucvm$Z)

## ----ucvm.fit, cache=TRUE, size = "small"-------------------------------------
with(ucvm, estimateRACVM(Z=Z, T=T, compare.models=TRUE))

## -----------------------------------------------------------------------------
with(ucvm, estimateRACVM(Z=Z, T=T, model="UCVM", compare.models=FALSE))

## ----acvm, cache=TRUE, echo=FALSE, fig.height = 3, fig.width = 6--------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot_track(acvm$Z)

## ----acvm.fit, cache=TRUE, size = "small"-------------------------------------
with(acvm, estimateRACVM(Z=Z, T=T, model = "ACVM", compare.models=TRUE))

## ----rcvm, cache=TRUE, echo=-1, out.width = "4in"-----------------------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1, col=rgb(0,0,0,.5)); plot_track(rcvm$Z)

## ----rcvm.fit, cache=TRUE, size = "small"-------------------------------------
with(rcvm, estimateRACVM(Z=Z, T=T, model = "RCVM", compare.models=TRUE))

## ----racvm, cache=TRUE, echo=-1, fig.height = 3, fig.width = 6----------------
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1); plot_track(racvm$Z)

## ----racvm.fit, cache=TRUE, size = "footnotesize"-----------------------------
with(racvm, estimateRACVM(Z=Z, T=T, model = "RACVM", compare.models=TRUE))

## ----KestrelFlight, fig.height=8, fig.width = 6, echo=-1, out.width="3in", fig.align="center"----
par(bty="l", mgp = c(1.5,0.25,0), tck=0.01, las=1, bty="l")
data(Kestrel); plot_track(Kestrel[,c("X","Y")], cex=0.3)

## ----K1, echo=-1, fig.height=4, cache=TRUE, fig.width = 8---------------------
  par(mar = c(0,2,0,0), oma = c(4,4,2,2), bty="l")
  K1 <- Kestrel[3360:3450,]
  with(K1, scan_track(x=X, y=Y))

## ----K1.fit, cache=TRUE-------------------------------------------------------
(fit1 <- with(K1, estimateRACVM(XY = cbind(X,Y), T = timestamp, spline=TRUE, 
                                model = "RACVM", time.units = "sec")))

## ----K1.sim, echo=-1, fig.height=4, cache=TRUE, fig.width = 8-----------------
par(mar = c(0,2,0,0), oma = c(4,4,2,2), bty="l")
p.fit1 <- with(fit1$results, list(eta=eta[1], tau = tau[1], 
                                  mu = mu.x[1] + 1i*mu.y[1], 
                                  omega = omega[1], v0 = diff(K1$Z)[1]))
K1.sim <- with(p.fit1, simulateRACVM(eta=eta, tau=tau, mu = mu, omega=omega, 
                                     Tmax = nrow(K1), v0 = v0, dt = 1))
with(K1.sim, scan_track(z=Z))

## ----TwoPhaseCVM, fig.height = 4, echo=-1, fig.width = 6, cache=TRUE, fig.align="center"----
par(bty="l")
ucvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=1, method="exact")
ucvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=2, tau=10, v0 = ucvm1$V[100], method="exact")
T <- c(ucvm1$T, ucvm1$T[100] + ucvm2$T)
Z <- c(ucvm1$Z, ucvm1$Z[100] + ucvm2$Z)
plot_track(Z)

## ----fig.height = 4, echo=-1, cache=FALSE, fig.width = 6----------------------
par(bty="l")
findSingleBreakPoint(Z,T, method = "sweep")

## ----simSweepGenerate, echo=-1------------------------------------------------
par(bty="l"); palette(rainbow(4)); set.seed(7)
taus <- c(3, 3, 1)
mus <- c(2, 0, 0)
etas <- c(2, 1, 1)
durations <- c(40,60,100)

Z.raw <- 0
T.raw <- 0
mycvm <- list()

for(i in 1:length(taus)){
  if(i > 1)  v0 <- mycvm$V[length(mycvm)]  else v0 = mus[1]
  mycvm <- simulateRACVM(tau = taus[i], eta = etas[i], mu = mus[i], v0 = v0,
                         Tmax = durations[i], dt = 0.01)
  Z.raw <- c(Z.raw, mycvm$Z + Z.raw[length(Z.raw)])
  T.raw <- c(T.raw, mycvm$T + T.raw[length(T.raw)])
}

## ----plotMulticvm, echo =-1, fig.height = 4, fig.width = 6, cache=TRUE, fig.align="center"----
par(bty = "l", mar = c(0,4,0,0), oma = c(4,0,2,2), xpd = NA)
multicvm <- data.frame(Z = Z.raw, T = T.raw)[sample(1:length(Z.raw), 400),] %>% arrange(T)
with(multicvm, scan_track(z = Z, time = T))

## ----simSweep1, eval=FALSE----------------------------------------------------
#  simSweep <- with(multicvm, sweepRACVM(Z = Z, T = T,
#                                        windowsize = 80, windowstep = 5,
#                                        model = "ACVM", progress=FALSE))

## ----simSweep.parallel, cache=TRUE, warning = FALSE, eval=FALSE, message = FALSE----
#  require(foreach); require(doParallel)
#  cl <- makeCluster(detectCores())
#  registerDoParallel(cl)
#  simSweep <- with(multicvm, sweepRACVM(Z = Z, T = T, windowsize = 80, windowstep = 5,
#                                        model = "ACVM", .parallel = TRUE))

## ----eval=FALSE, include=FALSE------------------------------------------------
#  #save(simSweep, file = "simSweep.robj")

## ----loadsimSweep, echo=-1, fig.height=3, fig.width = 5-----------------------
par(bty="l"); data(simSweep)
plotWindowSweep(simSweep)

## ----simCP--------------------------------------------------------------------
CP.all <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 0)
CP.all %>% as.vector

## ----simCP.clustered----------------------------------------------------------
CP.clustered <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 4)
CP.clustered %>% as.vector

## ----simGetCPtable------------------------------------------------------------
getCPtable(CPs = CP.clustered, modelset = c("UCVM", "ACVM"), tidy = NULL)

## ----simGetCPtable2, cache=TRUE-----------------------------------------------
getCPtable(CPs = CP.clustered, modelset = c("UCVM", "ACVM"),  iterate = TRUE)

## ----exampleCPtables----------------------------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 2) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))
simSweep %>% findCandidateChangePoints(clusterwidth = 10) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))

## ----simSweep.AIC-------------------------------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")

## ----simSweepAllModels, cache=TRUE--------------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 10, verbose = FALSE) %>% 
  getCPtable(modelset ="all", criterion = "AIC")

## ----simPhases----------------------------------------------------------------
simCP.table <- simSweep %>% 
  findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
simPhaselist <- estimatePhases(simCP.table)

## -----------------------------------------------------------------------------
simPhaselist[[1]]

## -----------------------------------------------------------------------------
summarizePhases(simPhaselist)

## ----SimPhasePlot, message=FALSE, echo = -1, fig.width = 6, fig.height = 6----
  par(mar=c(4,0,1,0), oma = c(5,5,1,2), xpd=NA, las = 1, cex.lab = 1.5, tck = 0.01, mgp = c(1.5,0.25,0), bty="n")
  layout(c(1,1,1,2:6))
  # extract locations and times
  Z <- multicvm$Z
  T <- multicvm$T
  
  # Note, these are also contained in the attributes of *any* of the 
  # intermediate objects, e.g.
  Z <- attributes(simPhaselist)$Z
  T <- attributes(simPhaselist)$time
    
  require(gplots) # for rich colors
  cols <- rich.colors(length(simPhaselist))
  T.cuts <- c(T[1], simCP.table$CP, T[length(T)])
  Z.cols <- cols[cut(T, T.cuts, include.lowest = TRUE)]
  
  phaseTable <- summarizePhases(simPhaselist)
  
  plot(Z, asp=1, type="l", xpd=FALSE)
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  legend("top", legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
         fill=cols, ncol=3, bty="n", title = "Phase: model")

  par(mar=c(0,0,1,0), xpd=NA)
  plotPhaseParameter("tau", simPhaselist, ylab="", xaxt="n", xlab="", col=cols, log="y")
  plotPhaseParameter("eta", simPhaselist,  ylab="", xaxt="n", xlab="", col=cols)
  plotPhaseParameter("mu.x", simPhaselist,  ylab= "", xaxt="n", xlab="", col=cols)
  plotPhaseParameter("mu.y", simPhaselist,  ylab= "", xaxt="n", xlab="", col=cols)
  plotPhaseParameter("rms", simPhaselist,  ylab= "", xlab="time", col=cols)

## -----------------------------------------------------------------------------
data.frame(durations, taus, etas, mus)

## ----subsetKestrel, echo=-1, fig.height=4, fig.width = 6----------------------
par(bty="l", mar = c(0,5,0,0), xpd=NA, oma= c(4,0,4,2))
data(Kestrel)
k <- Kestrel[3730:4150,]
head(k)
with(k, scan_track(x=X, y=Y, time = timestamp))

## ----echo = FALSE, eval= FALSE------------------------------------------------
#  require(doParallel)
#  cl <- makeCluster(detectCores())
#  registerDoParallel(cl)

## ----sweepKestrel, cache=TRUE, message=FALSE, eval = FALSE--------------------
#  k.sweep <- with(k, sweepRACVM(Z = cbind(X,Y), T=timestamp, windowsize = 50, windowstep = 5,
#                              model = "RACVM", time.unit = "secs", progress=FALSE,
#                              .parallel = TRUE))

## ----eval=FALSE, include=FALSE------------------------------------------------
#  #save(k.sweep, file = "./vignettes/k.sweep.robj")

## ----plotKestrelSweep, echo=-1, fig.height=3, fig.width = 6-------------------
par(bty="l"); load("k.sweep.robj")
plotWindowSweep(k.sweep)

## ----findKestrelCPs-----------------------------------------------------------
k.CPs <- findCandidateChangePoints(windowsweep = k.sweep, clusterwidth = 4)

## ----filterKestrelCPs---------------------------------------------------------
k.CPtable <- getCPtable(CPs = k.CPs, modelset = "all", spline=TRUE)

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------
options(width=90)

## ----width=100--------------------------------------------------------------------------
k.phases <- estimatePhases(k.CPtable, verbose=FALSE)
summarizePhases(k.phases)

## ----kestrelMegaPlot, echo=-1, eval=TRUE, warning = FALSE, fig.width = 6, fig.height = 6----
par(mar=c(4,0,1,0), oma = c(5,5,1,2), xpd=NA, las = 1, cex.lab = 1.25, tck = 0.01, mgp = c(1.5,0.25,0), bty="n")


cols <- rich.colors(length(k.phases)/2)

XY <- cbind(k$X, k$Y)
time <- k$timestamp

T.cuts <- c(time[1], k.CPtable$CP, time[nrow(k)])
XY.cols <- c(cols,cols)[cut(time, T.cuts, include.lowest = TRUE)]

phaseTable <- summarizePhases(k.phases)

layout(c(1,1,1,1,2:6)) 
plot(XY, asp=1, type="l", xpd=FALSE, xlab="X (m)")
points(XY, col=XY.cols, pch=21, bg = alpha(XY.cols, 0.5), cex=0.8)
legend("topleft", legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
       fill=cols, ncol=2, bty="n", title = "Phase: model")

par(mar=c(1,0,1,0), xpd=NA)
plotPhaseParameter("tau", k.phases, ylab="sec", xaxt="n", xlab="", col=cols, log="y")
plotPhaseParameter("eta", k.phases,  ylab="m / sec", xaxt="n", xlab="",  col=cols)
plotPhaseParameter("omega", k.phases,  ylab="rad / sec", xaxt="n", xlab="", col=cols)
plotPhaseParameter("mu.x", k.phases,  ylab="m / sec", xaxt="n", xlab="", col=cols)
plotPhaseParameter("mu.y", k.phases,  ylab="m / sec", xlab="", col=cols)

