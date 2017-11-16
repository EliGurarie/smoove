#' Sweep RACVM 
#' 
#' Sets a window (a subset of movement data within specific time window), computes likelihoods for a set of candidate change points within the window, and steps the window forward, filling out a likelihood matrix. 
#' 
#' @param XY two column location data (if Z is not provided)
#' @param Z complex location data (if XY is not provided)
#' @param T time vector
#' @param model model to fit for the change point sweep - typically the most complex model in the candidate model set. 
#' @param windowsize time window of analysis to scan, IMPORTANTLY: in units of time (T).
#' @param windowstep step (in time) by which the window advances.  The smaller the step, the slower but more thorough the estimation. 
#' @param time.unit of the windowsize and windowstep.  Only needed if time is a POSIX. Must be one of "auto", "secs", "mins", "hours", "days", "weeks" (See \code{\link{difftime}}).
#' @param {progress} whether or not to show a progress bar
#' @param {...} additional parameters to pass to the \code{\link{estimateRACVM}} function, notably the option "criterion" allows you to select models based on AIC or BIC (the former is more liberal with more complex models).
#' @param .parallel if set TRUE, will use \code{\link{foreach}} to parallelize the optimization.  Requires establishing the 
#' @seealso \code{\link{plotWindowSweep}}, \code{\link{estimateRACVM}}, \code{\link{testCP}}
#' @example ./examples/sweepRACVMexamples.R

sweepRACVM <- function(XY = NULL, Z=NULL, T,
                       windowsize, windowstep, time.unit = "auto",
                       model = "UCVM", 
                       progress = TRUE,  ..., 
                       .parallel = FALSE){
  
  if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
  
  T.raw <- T
  if(inherits(T.raw, "POSIXt"))
    T <- difftime(T.raw, T.raw[1], unit = time.unit) %>% as.numeric
  
  cut.wstep <- cut(T, c(seq(min(T), max(T) - windowsize, windowstep), max(T)+1), include.lowest = TRUE, labels = FALSE)
  starts <- c(1, which(diff(cut.wstep) == 1))
  ends <- c(sapply(starts[-1], function(s) which.max(T[T < (T[s] + windowsize)])), length(T))

  getLLbreaks <- function(start, end, breaks, Z, T, ...){
    require(smoove)
    lls <- breaks*NA
    ll.row <- T*NA
    if((end - start) < 30){
      warning("Too few (less than 30) data points in this analysis window. I'll skip it and move on.")
      return(ll.row)
    }
    
    for(i in 1:length(breaks)){	
      left <- start:(start + breaks[i])
      right <- (start + breaks[i]):end
      lls[i] <- estimateRACVM(Z = Z[left], T = T[left], model = model, compare.models = FALSE, ...)$LL + 
        estimateRACVM(Z = Z[right], T = T[right], model = model, compare.models = FALSE, ...)$LL
    }
    ll.row[start+breaks] <- lls
    return(ll.row)
  }
  
  if(.parallel){
    LLs <- foreach(start = starts, end = ends, .combine = "cbind")  %dopar% {
      mywindow <- end - start
      breaks <- round(mywindow*.2):round(mywindow*.8)
      getLLbreaks(start, end, breaks, Z, T, ...)
    }
  }

  if(!.parallel){
    LLs <- matrix(NA, ncol = length(starts), nrow = length(T))
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- ends[i]
      mywindow <- end - start
      breaks <- round(mywindow*.2):round(mywindow*.8)
      if(progress)
      cat(paste("sweep", i, "of", length(starts), "- data points", start, "to", end, "\n"))
      if(mywindow < 30)
          warning("Too few (less than 30) data points in this analysis window. I'll skip it and move on.") else
      LLs[,i] <- getLLbreaks(start, end, breaks, Z, T, ...)
    }
  }
  colnames(LLs) <- starts
  rownames(LLs) <- T.raw
  return(LLs)
}
