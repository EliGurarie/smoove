estimateCVM.fullL <- function(Z, T, CI = FALSE, parameters = c("tau", "nu"), type=c("optim","stan")[1], diagnose = FALSE, iter=100, chains=4, max.attempts=10)
{   
  v0.null <- diff(Z[1:2])/diff(T[1:2])
  v0x.null <- Re(v0.null)
  v0y.null <- Im(v0.null)
  
  tau.null <- mean(diff(T))
  nu.null <- mean(Mod(diff(Z))/diff(T))
  
  if(type == "optim")
  {
    if("v0"%in%parameters){
      Z.fit <- optim(c(nu.null,tau.null,v0x.null,v0y.null), Z.like2D, Z=Z[-1], T=T[-1], v0=NULL, hessian=TRUE,
                   method = "L-BFGS-B", lower = c(0,0,-Inf, -Inf), upper = c(Inf,Inf,Inf, Inf),
                   control = list(fnscale = -1))
    } else {
      Z.fit <- optim(c(nu.null,tau.null), Z.like2D, Z=Z[-1], T=T[-1], v0 = v0.null, hessian=TRUE,
                     method = "L-BFGS-B", lower = c(0,0), upper = c(Inf,Inf),
                     control = list(fnscale = -1))
    }
  
  # nu estimates
    nu.hat <- Z.fit$par[1]
    nu.se <- sqrt(-1/Z.fit$hessian[1,1])
    nu.CI <- nu.hat + c(-1,1)*qnorm(0.975)*nu.se
    
  # tau estimates
    tau.hat <- Z.fit$par[2]
    tau.se <- sqrt(-1/Z.fit$hessian[2,2])
    tau.CI <- tau.hat + c(-1,1)*qnorm(0.975)*tau.se
    
  # compile results
    results <- data.frame(c(tau.hat, nu.hat), rbind(tau.CI, nu.CI))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
    
    if("v0"%in%parameters)
    {
      # v0x estimates
      v0x.hat <- Z.fit$par[3]
      v0x.se <- sqrt(-1/Z.fit$hessian[3,3])
      v0x.CI <- v0x.hat + c(-1,1)*qnorm(0.975)*v0x.se
      
      # v0y estimates
      v0y.hat <- Z.fit$par[4]
      v0y.se <- sqrt(-1/Z.fit$hessian[4,4])
      v0y.CI <- v0y.hat + c(-1,1)*qnorm(0.975)*v0y.se
      
      results <- rbind(results, 
                       v0x = c(v0x.hat, v0x.CI),
                       v0y = c(v0y.hat, v0y.CI))
    }
  if(!CI) results <- t(results)[1,] 
  return(results)
  }
  
  if(type == "stan")
  {
    data(CVMstan2D)
    CVM.data <- list(N = length(Z)-1, XY = c(Re(Z)[-1], Im(Z)[-1]), T=T[-1], v0x=Re(v0), v0y = Im(v0), tau0 = tau.null, nu0 = nu.null)
    
    CVM.fit <- NULL
    attempt <- 0
    while(1)
    {
      attempt <- attempt+1
      print(paste("Attempt #", attempt))
      CVM.fit <- try(sampling(CVMstan2D, data = CVM.data, iter = iter, pars=c("tau", "nu"), chains = chains))
      if(is(CVM.fit)[1] != "try-error") break()
      if(attempt == max.attempt) stop("Dang it, I couldn't perform the sampling!  See error message above.")
    }
    
    tau.sim <- CVM.fit@sim$samples[[1]]$tau
    tau.hat <- mean(tau.sim[50:iter])
    tau.CI <- quantile(tau.sim[50:iter], c(0.025, 0.975))
    
    nu.sim <- CVM.fit@sim$samples[[1]]$nu
    nu.hat <- mean(nu.sim[50:iter])
    nu.CI <- quantile(nu.sim[50:iter], c(0.025, 0.975))
    
    results <- data.frame(rbind(c(tau.hat, tau.CI), c(nu.hat, nu.CI)))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
    
    if(diagnose) diagnoseSTANfit(CVM.fit, iter)

    if(!CI) results <- t(results)[1,] 
    return(results)
  }
}