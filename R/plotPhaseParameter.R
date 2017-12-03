#' Plot Estimates From RACVM Partition
#' 
#' @details \code{getVariable} takes a partitionlist and generates a table indicating the start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter. 
#' @details \code{plotVariable} 
#' 
#' @param variable one of: \code{omega}, \code{mu.x}, \code{mu.y}, \code{eta}, \code{tau}
#' @param phaselist output of \code{\link{estimatePhases}}, i.e. a list of information on each of the selected phases
#' @param cols colors of bars (by default - a rich color palette)
#' @param label whether to label the plotted parameter (with the word "tau", "eta", etc.)
#' @param ... additional arguments to pass to plotting function.  A useful one is \code{log = "y"} for highly skewed data.
#' @return \code{getVariable} returns a data.frame with start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter, with each row corresponding to an enumerated partition phase.  
#' @aliases getPhaseParameter

plotPhaseParameter <- function(variable, phaselist, cols = 1:length(phaselist), 
                               label = TRUE, ymax = 1e3, ...){
  variabletable <- getPhaseParameter(variable, phaselist)
  
  low.plot <- variabletable$low
  high.plot <- variabletable$high
  high.plot[high.plot > ymax] <- ymax
  
  with(variabletable,{
    plot(range(start, end), 
         range(low.plot, high.plot, na.rm=TRUE), 
         type="n", ...)
    if(!is.na(low[1])) rect(start, low.plot, end, high.plot, col = alpha(cols, .5), bor=NA) 
    
    segments(start, hat, end, hat, lwd = 2, col=cols)
    segments(end[-length(end)], hat[-length(end)], start[-1], hat[-1], col="grey")
    
    if(label) mtext(variable, side = 3, at = start[1], font = 3, adj = 0)
  })
}
