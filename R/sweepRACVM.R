#' Sweep RACVM 
#' 
#' Sets a window (a subset of movement data within specific time window), computes likelihoods for a set of candidate change points within the window, and steps the window forward, filling out a likelihood matrix. 
#' 
#' @param Z location data.  Can be: a complex vector, a two-column matrix or data frame, an \code{\link[adehabitatLT]{ltraj}} object from \code{adehabitatLT} or a \code{\link[move]{move}} object from the \code{move} package.  
#' @param T time vector, ignored if \code{Z} is an ltraj or move
#' @param model model to fit for the change point sweep - typically the most complex model in the candidate model set. 
#' @param windowsize time window of analysis to scan, IMPORTANTLY: in units of time (T).
#' @param windowstep step (in time) by which the window advances.  The smaller the step, the slower but more thorough the estimation. 
#' @param time.unit of the windowsize AND the windowstep. The default is "hours" - can be any of "secs", "mins", "hours", "days", "weeks" (See \code{\link{difftime}}). Ignored if time is not POSIX. 
#' @param progress whether or not to show a progress bar
#' @param ... additional parameters to pass to the \code{\link{estimateRACVM}} function, notably the option "criterion" allows you to select models based on AIC or BIC (the former is more liberal with more complex models).
#' @param .parallel if set TRUE, will use \code{\link{foreach}} to parallelize the optimization.  Requires establishing the 
#' @seealso \code{\link{plotWindowSweep}}, \code{\link{estimateRACVM}}, \code{\link{testCP}}
#' @example ./demo/sweepRACVM_examples.R
#' @export
sweepRACVM <- function(Z, ...) UseMethod("sweepRACVM")

#' @export
#' @rdname sweepRACVM
sweepRACVM.default <- function(Z, T,
                       windowsize, 
                       windowstep, 
                       model = "UCVM", 
                       progress = TRUE,  
                       time.unit = "hours",
                       ..., 
                       .parallel = FALSE){
  if(!is.complex(Z) && (ncol(Z) == 2)) Z <- Z[,1] + 1i*Z[,2]
  T.raw <- T
  if(inherits(T.raw, "POSIXt"))
    T <- difftime(T.raw, T.raw[1], units = time.unit) %>% as.numeric
  
  if(any(diff(T) < 0)) stop("Times must be unique and strictly increasing. Check to see if there are some issues with your data (e.g. via plot(T)).")
  
  cut.wstep <- cut(T, c(seq(min(T), max(T) - windowsize, windowstep), max(T)+1), include.lowest = TRUE, labels = FALSE)
  starts <- c(0, which(diff(cut.wstep) == 1))
  ends <- c(sapply(starts[-1], function(s) which.max(T[T < (T[s] + windowsize)])), length(T))

  getLLbreaks <- function(start, end, breaks, Z, T, ...){
    require(smoove)
    lls <- breaks*NA
    ll.row <- T*NA
    if((end - start) < 10){
      warning("Too few (less than 10) data points in this analysis window. I'll skip it and move on.")
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
      if(mywindow < 10)
          warning("Too few (less than 10) data points in this analysis window. I'll skip it and move on.") else
      LLs[,i] <- getLLbreaks(start, end, breaks, Z, T, ...)
    }
  }
  
  colnames(LLs) <- starts
  rownames(LLs) <- T.raw
  attr(LLs,"time") <- T.raw
  attr(LLs,"Z") <- Z
  attr(LLs,"time.unit") <- time.unit
  
  return(LLs)
}

#' @export
#' @rdname sweepRACVM
sweepRACVM.data.frame <- function(Z, ...){
  xy <- Z[,1] + 1i*Z[,2]
  do.call(sweepRACVM.default, list(Z=xy, ...))
}

#' @export
#' @rdname sweepRACVM
sweepRACVM.ltraj <- function(Z, ...){
  if(requireNamespace("adehabitatLT",quietly = TRUE)){
  ltraj <- adehabitatLT::ld(Z)
  sweepRACVM(Z= ltraj[,c('x','y')],
             T=ltraj$date, ...)
  }
  else
    warning("First load adehabitatLT package using library(adehabitatLT) and then call this function on ltraj objects")
}

#' @export
#' @rdname sweepRACVM
sweepRACVM.Move <- function(Z, ...){
  xy <- Z@coords  
  tt <- Z@timestamps
  sweepRACVM(Z=xy, T = tt, ...)
}