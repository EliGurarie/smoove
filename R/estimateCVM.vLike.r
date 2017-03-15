estimateCVM.vLike <- function(Z, T, CI = FALSE, parameters = c("tau", "nu"), spline = FALSE, like=FALSE)
{   
  V <- diff(Z)/diff(T)
  T.mid <- (T[-1] + T[-length(T)])/2
  
  if(spline){
    VZT <- getV.spline(Z,T)
    V <- VZT$V
    T <- VZT$T
    Z <- VZT$Z
  }
  
  V.x1 <- Re(V)[-length(V)] 
  V.x2 <- Re(V)[-1]
  V.y1 <- Im(V)[-length(V)]
  V.y2 <- Im(V)[-1]
  
  dT <- diff(T.mid)
  
  f.V <- function(v1, v2, dt, tau, nu)
  {
    v.mean <-  v1 * exp(-dt/tau)
    v.var <- (2/pi) * nu^2 * (1 - exp(-2*dt/tau))
    sum(dnorm(x = v2, mean = v.mean, sd = sqrt(v.var), log=TRUE))
  }
  
  V.LogLikelihood.Tau <- function(p)
  {
    logtau <- p[1]
    L.x <- f.V(V.x1, V.x2, dt=dT, exp(logtau), nu.hat)
    L.y <- f.V(V.y1, V.y2, dt=dT, exp(logtau), nu.hat)
    return(-L.x-L.y)
  }
  
  V.LogLikelihood.NuTau <- function(p)
  {
    logtau <- p[1]
    nu <- p[2]
    L.x <- f.V(V.x1, V.x2, dt=dT, exp(logtau), nu)
    L.y <- f.V(V.y1, V.y2, dt=dT, exp(logtau), nu)
    return(-L.x-L.y)
  }
  
  nu.hat <- mean(Mod(V))
  
  if(!"nu" %in% parameters)
  {
    
    logtau.fit <- optimize(V.LogLikelihood.Tau, c(-1e10, log(diff(range(T))/2)))
    logtau.hat <- logtau.fit$min
    logtau.sd <- 1/sqrt(hessian(V.LogLikelihood.Tau, logtau.hat))
    tau.hat <- exp(logtau.hat)
        
    LL <- -logtau.fit$obj
    
    if(CI)
    {
      tau.CIs <- exp(logtau.hat + c(-1,1)*1.96*logtau.sd)

      # nu estimates
      rho.hat <- cor(Mod(V[-1]), Mod(V[-length(V)]))
      nu.se <- sd(Mod(V))/sqrt(length(V))/(1-rho.hat)/2
      nu.CI <- nu.hat +  c(-1,1) * qnorm(0.975) * nu.se
    }
  } else
  {
    logtau.null <- log(-1/log(max(cor(V.x1, V.x2), 1e-10))*mean(diff(T)))
    nutau.hat <- try(optim(c(logtau.null,nu.hat), V.LogLikelihood.NuTau, 
                           lower=c(-Inf,1e-10), upper=c(Inf, Inf), method="L-BFGS-B", 
                           hessian=TRUE)) 
    
    if(is(nutau.hat)[1] == "try-error") nutau.hat <- list(par = c(NA, NA), value=NA, hessian=matrix(NA, 2,2))
    nu.hat <- nutau.hat$par[2]
    logtau.hat <- nutau.hat$par[1]
    tau.hat <- exp(logtau.hat)
    Sigma <- solve(nutau.hat$hessian)
    
    nu.CI <- nu.hat + c(-1,1)*qnorm(0.975)*sqrt(Sigma[2,2])
    tau.CI <- exp(logtau.hat + c(-1,1)*qnorm(0.975)*sqrt(Sigma[1,1]))
    
    LL <- -nutau.hat$value
  }  
  
  results <- data.frame(tau.hat = tau.hat, nu.hat = nu.hat)
  if(CI) 
  {
    results <- data.frame(t(results), rbind(tau.CI, nu.CI))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
  }
  if(like) return(list(results=results, LL = LL)) else return(results)
}

