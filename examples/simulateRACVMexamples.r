racvm1 <- simulateRACVM(tau = 4, eta = 1, omega = 2, mu = 0, Tmax = 100, dt = .01)
racvm2 <- simulateRACVM(tau = 4, eta = 1, omega = 0, mu = 1, Tmax = 100, dt = .01)
racvm3 <- simulateRACVM(tau = 4, eta = 1, omega = 2, mu = 1, Tmax = 100, dt = .01)

layout(rbind(c(1,2),c(1,3)))
sapply(list(racvm1, racvm2, racvm3), 
       function(x) with(x, plot(Z, asp=1, type="l", main = paste0("RACVM(", paste(unlist(parameters), collapse = ", "),")"))))


racvm1 <- simulateRACVM(tau = 3, eta = 32, omega = .5, mu = 0, Tmax = 10, dt = .1)
with(racvm1, scan.track(x = Re(Z), y = Im(Z), time = T))


# Compare with simulateRACVM2

racvm1b <- simulateRACVM2(tau = 4, eta = 1, omega = 2, mu = 0, Tmax = 100, dt = .01)
racvm2b <- simulateRACVM2(tau = 4, eta = 1, omega = 0, mu = 1, Tmax = 100, dt = .01)
racvm3b <- simulateRACVM2(tau = 4, eta = 1, omega = 2, mu = 1, Tmax = 100, dt = .01)

racvm1 <- simulateRACVM(tau = 4, eta = 1, omega = 1, mu = 0, Tmax = 10, dt = .01)
racvm1b <- simulateRACVM2(tau = 4, eta = 1, omega = 1, mu = 0, Tmax = 10, dt = .01)
with(racvm1, scan.track(x = Re(Z), y = Im(Z), time = T))
with(racvm1b, scan.track(x = Re(Z), y = Im(Z), time = T))
