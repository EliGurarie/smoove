if(interactive()){
require(smoove)
ucvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 0, Tmax = 100, dt = .1)
acvm <- simulateRACVM(tau = 5, eta = 2, omega = 0, mu = 2, Tmax = 100, dt = .1)
rcvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 0, Tmax = 100, dt = .1)
racvm <- simulateRACVM(tau = 5, eta = 2, omega = 2, mu = 2, Tmax = 100, dt = .1)

# estimating a model with no advection
plot_track(ucvm$Z)
with(ucvm, estimateRACVM(Z=Z, T=T, model = "UCVM", compare.models=TRUE))

# estimating a model with advection
plot_track(acvm$Z)
## compare all models
with(acvm, estimateRACVM(Z=Z, T=T, compare.models=TRUE))
with(acvm, estimateRACVM(Z=Z, T=T, model = "ACVM", compare.models=FALSE))

# estimating a model with rotation
plot_track(rcvm$Z)
## compare all models
with(rcvm, estimateRACVM(Z=Z, T=T, compare.models=TRUE))
with(rcvm, estimateRACVM(Z=Z, T=T, model = "RCVM", compare.models=FALSE))

# estimating a model with both rotation and advection
plot_track(racvm$Z)
## compare all models
with(racvm, estimateRACVM(Z=Z, T=T, compare.models=TRUE))
with(racvm, estimateRACVM(Z=Z, T=T, model = "RCVM", compare.models=FALSE))

## Using the kestrel data

data(Kestrel)
K1 <- Kestrel[3360:3450,]

# 90 data points - rotational and advective

K1$Z <- K1$X + 1i*K1$Y

par(mar = c(0,2,0,0), oma = c(4,4,2,2))
with(K1, scan_track(x=X, y=Y))

(fit1 <- with(K1, estimateRACVM(Z = X + 1i*Y, T = timestamp, 
                                spline=TRUE, compare.models=TRUE, 
                                time.units = "sec")))

# Note: the rotational + advective model was selected both by BIC and AIC

(fit1 <- with(K1, estimateRACVM(Z = X + 1i*Y, T = timestamp, 
                                spline=TRUE, model = "RACVM", 
                                time.units = "sec")))


# simulate from this fit

p.fit1 <- with(fit1$results, list(eta=eta[1],
                                tau = tau[1],
                                mu = mu.x[1] + 1i*mu.y[1],
                                omega = omega[1],
                                v0 = diff(K1$Z)[1]))

K1.sim <- with(p.fit1, simulateRACVM(eta=eta, tau=tau, mu = mu, 
                                     omega=omega, Tmax = nrow(K1), v0 = v0, dt = 1))
with(K1.sim, scan_track(z=Z))


# Analyze a second portion: 

K2 <- Kestrel[3470:3590,]
K2$Z <- K2$X + 1i*K2$Y
par(mar = c(0,2,0,0), oma = c(4,4,2,2))
with(K2, scan_track(x=X, y=Y))

with(K2, estimateRACVM(Z = X + 1i*Y, T = timestamp, spline=TRUE, 
                       compare.models=TRUE, time.units = "sec"))
# Note: Here, the advective model is selected by AIC, and the simple CVM by BIC.  

}