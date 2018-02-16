## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ----loading_cvm,warning=FALSE,message=FALSE,cache=FALSE-----------------
require(smoove)
require(magrittr)
require(plyr)
require(scales)

## ----cache=FALSE---------------------------------------------------------
ucvm1$parameters[1:2]

## ----fig.height = 4, echo=-1, cache=FALSE, fig.width = 6-----------------
par(bty="l")
findSingleBreakPoint(Z,T, method = "sweep")

## ----loadsimSweep, echo=-1, fig.height=3, fig.width = 5------------------
par(bty="l"); data(simSweep)
plotWindowSweep(simSweep)

## ----simCP---------------------------------------------------------------
CP.all <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 0)
CP.all %>% as.vector

## ----simCP.clustered-----------------------------------------------------
CP.clustered <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 4)
CP.clustered %>% as.vector

## ----simGetCPtable-------------------------------------------------------
getCPtable(CPs = CP.clustered, modelset = c("UCVM", "ACVM"), tidy = NULL)

## ----simGetCPtable2, cache=TRUE------------------------------------------
getCPtable(CPs = CP.clustered, modelset = c("UCVM", "ACVM"),  iterate = TRUE)

## ----exampleCPtables-----------------------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 2) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))
simSweep %>% findCandidateChangePoints(clusterwidth = 10) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"))

## ----simSweep.AIC--------------------------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 4) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")

## ----simSweepAllModels, cache=TRUE---------------------------------------
simSweep %>% findCandidateChangePoints(clusterwidth = 10, verbose = FALSE) %>% 
  getCPtable(modelset ="all", criterion = "AIC")

## ----simPhases-----------------------------------------------------------
simCP.table <- simSweep %>% 
  findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>% 
  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
simPhaselist <- estimatePhases(simCP.table)

## ------------------------------------------------------------------------
simPhaselist[[1]]

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
data.frame(durations, taus, etas, mus)

## ----subsetKestrel, echo=-1, fig.height=4, fig.width = 6-----------------
par(bty="l", mar = c(0,5,0,0), xpd=NA, oma= c(4,0,4,2))
data(Kestrel)
k <- Kestrel[3730:4150,]
head(k)
with(k, scan_track(x=X, y=Y, time = timestamp))

## ----echo = FALSE, eval= FALSE-------------------------------------------
#  require(doParallel)
#  cl <- makeCluster(detectCores())
#  registerDoParallel(cl)

## ----sweepKestrel, cache=TRUE, message=FALSE, eval = FALSE---------------
#  k.sweep <- with(k, sweepRACVM(Z = cbind(X,Y), T=timestamp, windowsize = 50, windowstep = 5,
#                              model = "RACVM", time.unit = "secs", progress=FALSE,
#                              .parallel = TRUE))

## ----eval=FALSE, include=FALSE-------------------------------------------
#  #save(k.sweep, file = "./vignettes/k.sweep.robj")

## ----plotKestrelSweep, echo=-1, fig.height=3, fig.width = 6--------------
par(bty="l"); load("k.sweep.robj")
plotWindowSweep(k.sweep)

## ----findKestrelCPs------------------------------------------------------
k.CPs <- findCandidateChangePoints(windowsweep = k.sweep, clusterwidth = 4)

## ----filterKestrelCPs----------------------------------------------------
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

