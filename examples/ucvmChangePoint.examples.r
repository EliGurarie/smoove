  

require(smoove)

mycvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=1, tau=10, method="exact")
mycvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=5, tau=4, v0 = mycvm1$V[100], method="exact")
mycvm3 <- simulateUCVM(T=cumsum(rexp(100)), nu=1, tau=1, v0 = mycvm2$V[100], method="exact")

T <- c(mycvm1$T, mycvm1$T[100] + mycvm2$T, mycvm1$T[100] + mycvm2$T[100] + mycvm3$T)
Z <- c(mycvm1$Z, mycvm1$Z[100] + mycvm2$Z, mycvm1$Z[100] + mycvm2$Z[100] + mycvm3$Z)

plot(Z, type="o", asp=1)
par(mfrow=c(2,1))

ws <- sweepRACVM(Z = Z, T = T, windowsize = 50, windowstep = 5, model = "UCVM")

# select change points

starts <- colnames(ws) %>% as.numeric
image(T, starts, ws)

CPs <- apply(ws, 2, 
             function(ll) T[which.max(ll)]) %>% clusters(10) %>% sapply(mean)

SelectTable <- selectRACVM(Z,T,CPs, modelset = "UCVM")

while("" %in% SelectTable$extremes){
  CPs <- subset(SelectTable, extremes != "")$CP
  SelectTable <- selectRACVM(Z,T,CPs, modelset = "UCVM")
}



CPs <- SelectTable$CP

phase <- cut(T, c(0, CPs, max(T)))


plot(Z, asp=1, col=phase)


WindowSweepCVM(Z, T, plotme=FALSE, windowstep=1)
SelectPhases(ws, 2)

#################################################################
#  this is the vaunted simulation study ... we should revisit!

E <- GetCVMforPhases(Z,T,SelectPhases(ws, 2), method="crawl")
PlotResults(E)
