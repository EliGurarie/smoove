#' Plot Window Sweep
#' 
#' Plots a relative likelihood plot for the windowsweep
#' 
#' @param windowsweep output of \code{\link{sweepRACVM}} function
#' @param ... additional parameters to pass to \code{\link{matplot}}
#' @examples 
#' library(smoove)
#' data(simSweep, package="smoove")
#' plotWindowSweep(simSweep)
#' 
#' @export
plotWindowSweep <- function(windowsweep, ...){
  windowsweep.zeroed <- plyr::aaply(windowsweep, 2, function(x) x - min(x, na.rm=TRUE)) %>% t
  starts <- colnames(windowsweep) %>% as.numeric
  T <- attributes(windowsweep)$time #%>% as.numeric
  mean.rel.ll <- apply(windowsweep.zeroed, 1, mean, na.rm=TRUE)
  matplot(T, windowsweep.zeroed, type="l", lty = 1, col=rainbow(length(starts)), ylab = "relative log likelihood", ..., xaxt = "n")
  
  if(inherits(T, "POSIXt"))  axis.POSIXct(side = 1, x = T) else axis(1)
}