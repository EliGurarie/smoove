#' Sweep RACVM 
#' 
#' Sets a window (a subset of movement data), computes likelihoods for a set of candidate change points within the window, and steps the window forward, filling out a likelihood matrix.
#' 
#' @param XY two column location data (if Z is not provided)
#' @param Z complex location data (if XY is not provided)
#' @param T time vector
#' @param {model} model to fit for the change point sweep - typically the most complex model in the candidate model set. 
#' @param {windowsize} size of window to scan
#' @param {windowstep} step by which the window advances
#' @param {progress} whether or not to show a progress bar
#' @param {...} additional parameters to pass to the \code{\link{estimateRACVM}} function

sweepRACVM <- function(XY = NULL, Z=NULL, T, windowsize, windowstep, 
                       model = "UCVM", 
                       progress = TRUE, ...){
  
  if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
  
  n <- length(Z)
  
  cut.wstep <- cut(T, seq(min(T), max(T) - windowsize, windowstep), include.lowest = TRUE, labels = FALSE) %>% na.omit
  starts <- c(1, which(diff(cut.wstep) == 1))
  
  ends <- c(sapply(starts[-1], function(s) which.max(T[T < (T[s] + windowsize)])), length(T))
  
  #starts <- seq(1,n-windowsize, windowstep)
  #breaks <- round(windowsize*c(.2)):round(windowsize*(.8))
  
  if (progress) 
    pb <- txtProgressBar(min = 0, max = end-starts, style = 3)
  
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

windowstep <- 5
windowsize <- 50


ends - starts
for(i in 1:length(starts))
  print(T[ends[i]] - T[starts[i]])
