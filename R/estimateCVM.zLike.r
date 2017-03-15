estimateCVM.zLike <- function(Z, T, CI = FALSE, tau.min=1e-10, tau.max = max(T)/2)
{   
  if(T[1] == 0) {T <- T[-1]; Z <- Z[-1]}
  
  logtau.hat <- 	optimize(Z.like2D, interval = c(log(tau.min), log(tau.max)), Z = Z, T = T)$minimum 
  logtau.sd <- 1/sqrt(hessian(Z.like2D, logtau.hat, Z=Z, T=T))
  
  tau.hat <- exp(logtau.hat)
  tau.CIs <- exp(logtau.hat + c(-1,1)*1.96*logtau.sd)
  
  M <- tau.hat * (1 - exp(-T/tau.hat))
  S <- getS.zz(T, tau.hat)
  S.inv <- solve(S)
  
  n <- length(Z)
  
  MSM <- as.numeric(t(M) %*% S.inv %*% M)
  X <- Re(Z)
  Y <- Im(Z)
  
  v0x.hat <- as.numeric((t(X) %*% S.inv %*% M)[1,1]/MSM)
  v0y.hat <- as.numeric((t(Y) %*% S.inv %*% M)[1,1]/MSM)
  
  nu2x.hat <- as.numeric(t(X - v0x.hat * M) %*% S.inv %*% (X - v0x.hat * M)/n)
  nu2y.hat <- as.numeric(t(Y - v0y.hat * M) %*% S.inv %*% (Y - v0y.hat * M)/n)
  nu2xy.hat <- (nu2x.hat + nu2y.hat)/2
  nu.hat <- sqrt( (pi / 2) * nu2xy.hat)
  nu.sd <- sqrt(pi*nu2xy.hat/(2*n))
  
  v0.sd <- sqrt(nu2xy.hat / MSM)
  
  hats <- c(tau.hat, nu.hat, v0x.hat, v0y.hat)
  sds <- c(NA, nu.sd, v0.sd, v0.sd)
  CIs <- cbind(hats - 1.96*sds, hats+1.96*sds)
  CIs[1,] <- tau.CIs
  
  results <- data.frame(hats, CIs)
  row.names(results) <- c("tau", "nu", "v0x", "v0y")
  names(results) <- c("Estimate", "C.I.low", "C.I.high")
  
  return(results)
}

Z.like2D <- function(logtau, Z, T){
  
  X <- Re(Z)
  Y <- Im(Z)
  n <- length(Z)
  tau <- exp(logtau)
  
  M <- tau * (1 - exp(-T/tau))
  S <- getS.zz(T, tau)
  S.inv <- Matrix::solve(S)
  MSM <- as.numeric(t(M) %*% S.inv %*% M)
  
  v0x.hat <- as.numeric(t(X) %*% S.inv %*% M)/MSM
  v0y.hat <- as.numeric(t(Y) %*% S.inv %*% M)/MSM
  
  DSD.x <- as.numeric((t(X- v0x.hat * M) %*% S.inv %*% (X-v0x.hat * M)))
  DSD.y <- as.numeric((t(Y- v0y.hat * M) %*% S.inv %*% (Y-v0y.hat * M)))
  
  # average of the two DSD estimates
  nu2.hat <- (DSD.x + DSD.y)/n/2
  
  # compute the likelihood (ignoring -2*pi, cancelling nu.hat with the DSD's, etc.)
  n*log(nu2.hat) + Matrix::determinant(S)$mod #+ (DSD.x + DSD.y)/nu2.hat/2   
}
