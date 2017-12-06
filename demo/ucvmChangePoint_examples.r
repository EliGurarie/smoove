if(interactive()){
require(smoove)
require(magrittr)

mycvm1 <- simulateUCVM(T=cumsum(rexp(100)), nu=1, tau=10, method="exact")
mycvm2 <- simulateUCVM(T=cumsum(rexp(100)), nu=5, tau=4, v0 = mycvm1$V[100], method="exact")
mycvm3 <- simulateUCVM(T=cumsum(rexp(100)), nu=1, tau=1, v0 = mycvm2$V[100], method="exact")

T <- c(mycvm1$T, mycvm1$T[100] + mycvm2$T, mycvm1$T[100] + mycvm2$T[100] + mycvm3$T)
Z <- c(mycvm1$Z, mycvm1$Z[100] + mycvm2$Z, mycvm1$Z[100] + mycvm2$Z[100] + mycvm3$Z)

plot(Z, type="o", asp=1)
par(mfrow=c(2,1))

ws <- sweepRACVM(Z = Z, T = T, windowsize = 100, windowstep = 5, model = "UCVM")

# select change points
par(mfrow=c(1,2))
require(rgl)

starts <- colnames(ws) %>% as.numeric
image(T, starts, ws)
surface3d(T, starts, ws)
require(fields)
image.plot(T, starts, ws)


require(fields)
ws2 <- apply(ws, 2, function(x) x - min(x, na.rm=TRUE))
ws2.range <- range(ws2, na.rm=TRUE)
cols <- topo.colors(round(diff(ws2.range)))[round(ws2.range)]
image(T, starts, ws2)
surface3d(T, starts, ws2, col=cols)
image.plot(T, starts, ws2)

CPs <- SelectTable$CP

phase <- cut(T, c(0, CPs, max(T)))


plot(T, apply(ws2, 1, mean, na.rm=TRUE), type="l")
plot(Z, asp=1, col=phase)


WindowSweepCVM(Z, T, plotme=FALSE, windowstep=1)
SelectPhases(ws, 2)

#################################################################
#  this is the vaunted simulation study ... we should revisit!

E <- GetCVMforPhases(Z,T,SelectPhases(ws, 2), method="crawl")
PlotResults(E)
}