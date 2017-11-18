#' Sweep RACVM 
#' 
#' Moves a window, computes likelihoods for all change points, and fills out a likelihood matrix.
#' 
#' @param XY two column location data (if Z is not provided)
#' @param Z complex location data (if XY is not provided)
#' @param T time vector
#' @param model model to fit for the change point sweep - typically the most complex model in the candidate model set. 
#' @param windowsize size of window to scan
#' @param windowstep step by which the window advances
#' @param progress whether or not to show a progress bar
#' @param ... additional parameters to pass to the \code{\link{estimateRACVM}} function
#' @export
sweepRACVM <- function(XY = NULL, Z=NULL, T, windowsize, windowstep, 
                       model = "UCVM", 
                       progress = TRUE, ...){
  
  if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
  
  n <- length(Z)
  starts <- seq(1,n-windowsize, windowstep)
  
  breaks <- round(windowsize*c(.2)):round(windowsize*(.8))
  
  if (progress) 
    pb <- txtProgressBar(min = 0, max = length(breaks), style = 3)
  
  LLs <- matrix(NA, ncol=length(Z), nrow = length(Z) - windowsize)
  for(start in starts){
    if(progress) print(paste(start, "of", max(starts)))
    
    lls <- breaks*NA
    for(i in 1:length(breaks))
    {	
      left <- start:(start + breaks[i])
      right <- (start + breaks[i]):(start + windowsize)
      
      lls[i] <- 
        estimateRACVM(Z = Z[left], T = T[left], model = model, compare.models = FALSE, spline = FALSE, ...)$LL + 
        estimateRACVM(Z = Z[right], T = T[right], model = model, compare.models = FALSE, spline = FALSE, ...)$LL
      
      if (progress)  setTxtProgressBar(pb, i)
    }
    LLs[start, start + breaks] <- lls
  }
  
  LL <- t(LLs[starts,])
  colnames(LL) <- starts
  rownames(LL) <- T
  
  return(LL)
}
