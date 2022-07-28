#' Find single change point
#' 
#' Finds a single change point in UCVM parameters (time scale \eqn{tau} and rms 
#' speed \eqn{eta}) of a movement.  
#' 
#' @details Two methods are provided: "sweep", which scans a set of possible 
#' change points, smooths the likelihoods and selects the maximum, or "optimize" 
#' which uses R's single dimension optimization algorithms to find the most 
#' likely change point. The latter is faster, but can be unreliable because the 
#' likelihood profiles are typically quite rough.  
#' 
#' 
#' @param Z complex vector of locations
#' @param T vector of times
#' @param k tuning parameter for the smoothing of the likelihood profile spline. 
#' The number of knots is "length(T)/4 * k" - the lower the value of k, the smoother the spline. 
#' @param method one of "sweep" or "optimize". See details. 
#' @param plotme whether to plot the resulting likelihood (only if method is "sweep"). 
#' @param ... additional parameters to pass to \code{\link{estimateUCVM}} function, in particular the method of estimation.  Under most conditions, fairly reliable and fast results are provided by the default \code{vLike} (velocity likelihood) method. 
#' 
#' @example demo/findSingleBreakPoint_examples.R
#' @export
findSingleBreakPoint <- function(Z, T, k = 1, method = "sweep", plotme=TRUE, ...){
  
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
      LLs[i] <- getUCVMLikelihoodAtBreak(tbreak = T.test[i], Z=Z, T=T, 
                                         parameters ="tau", CI=FALSE)
    
    # use smooth spline
    ss <- smooth.spline(T.test, LLs, nknots=round(length(T)/4 * k))
    tbreak.hat <- ss$x[ss$y==max(ss$y)]
    
    if(plotme){
      plot(T.test, LLs)
      lines(ss, col=4)
      abline(v=tbreak.hat, col=3)
    }
  }
  return(tbreak.hat)
}

getUCVMLikelihoodAtBreak <- function(tbreak, T, Z, ...)
{
  nbreak <- max(which((T-tbreak)<0))
  n <- length(Z)
  
  Hat1 <- estimateUCVM(Z = Z[1:nbreak],     T = T[1:nbreak],     like = TRUE, ...)
  Hat2 <- estimateUCVM(Z = Z[(nbreak+1):n], T = T[(nbreak+1):n], like = TRUE, ...)
  
  Hat1$LL + Hat2$LL
}
