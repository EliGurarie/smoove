getUCVMLikelihoodAtBreak <- function(tbreak, T, Z, ...)
{
  nbreak <- max(which((T-tbreak)<0))
  n <- length(Z)
  
  Hat1 <- estimateUCVM(z = Z[1:nbreak],     t = T[1:nbreak],     like = TRUE, ...)
  Hat2 <- estimateUCVM(z = Z[(nbreak+1):n], t = T[(nbreak+1):n], like = TRUE, ...)
  
  Hat1$LL + Hat2$LL
}


findSingleBreakPoint <- function(Z,T, plotme=TRUE, method = "sweep", ...){
  
  T.mids <- (T[-1] + T[-length(T)])/2
  which.min <- round(length(T)*.2)
  which.max <- round(length(T)*.8)
  
  if(method == "optimize"){
    tbreak.hat <- optimize(f = getUCVMLikelihoodAtBreak, 
                           interval = c(T[which.min], T[which.max]), 
                           Z = Z, T = T, maximum = TRUE, ...)$maximum
  }
  
  T.test <- T.mids[T.mids > T[which.min] & T.mids < T[which.max]]
  LLs <- rep(NA, length(T.test))
  
  if(method == "sweep"){
    
    for(i in 1:length(T.test))
      LLs[i] <- getUCVMLikelihoodAtBreak(tbreak = T.test[i], Z=Z, T=T, parameters ="tau", CI=FALSE)
    
    # use smooth spline
    ss <- smooth.spline(T.test, LLs, nknots=round(length(T)/4))
    tbreak.hat <- ss$x[ss$y==max(ss$y)]
    
    if(plotme){
      plot(T.test, LLs)
      lines(ss, col=4)
      abline(v=tbreak.hat, col=3)
    }
  }
  return(tbreak.hat)
}