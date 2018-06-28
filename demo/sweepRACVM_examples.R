if(interactive() && FALSE){
  # Three stage example (from vignette)
  require(smoove)
  require(plyr)
  require(magrittr)
  
  taus <- c(3, 3, 1)
  mus <- c(3, 0, 0)
  etas <- c(2, 1, 1)
  tmaxes <- c(40,60,100)
  
  Z.raw <- 0
  T.raw <- 0
  mycvm <- list()
  
  for(i in 1:length(taus)){
    if(i > 1)  v0 <- mycvm$V[length(mycvm)]  else v0 = mus[1]
    mycvm <- simulateRACVM(tau = taus[i], eta = etas[i], mu = mus[i], v0 = v0,
                           Tmax = tmaxes[i], dt = 0.01)
    Z.raw <- c(Z.raw, mycvm$Z + Z.raw[length(Z.raw)])
    T.raw <- c(T.raw, mycvm$T + T.raw[length(T.raw)])
  }
  
  multicvm <- data.frame(Z = Z.raw, T = T.raw)[sample(1:length(Z.raw), 400),] %>% 
    arrange(T)
  plot_track(multicvm$Z)
  
  # Perform sweep
  simSweep <- with(multicvm,
                   sweepRACVM(
                     Z = Z,
                     T = T,
                     windowsize = 80,
                     windowstep = 10,
                     model = "ACVM"
                   ))
  
  # Perform sweep in parallel (can greatly improve speed)
  require(doParallel)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  simSweep <- with(
    multicvm,
    sweepRACVM(
      Z = Z,
      T = T,
      windowsize = 80,
      windowstep = 5,
      model = "UCVM",
      .parallel = TRUE
    )
  )
  # plot result of window sweep
  plotWindowSweep(simSweep)
}
