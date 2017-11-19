#' Plot Estimates From RACVM Partition
#' 
#' @details \code{getVariable} takes a partitionlist and generates a table indicating the start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter. 
#' @details \code{plotVariable} 
#' 
#' @param variable one of: \code{omega}, \code{mu.x}, \code{mu.y}, \code{eta}, \code{tau}
#' @param partitionlist output of \code{\link{getPhases}}, i.e. a list of information on each of the selected phases
#' @param label whether to label the plotted parameter
#' @param log default behavior is to plot the log of tau, but not of other parameters. ANy other value of log
#' @param ... additional arguments to pass to plotting function. 
#' @return \code{getVariable} returns a data.frame with start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter, with each row corresponding to an enumerated partition phase.  
#' @aliases getPhaseParameter

plotPhaseParameter <- function(variable, partitionlist, cols = 1:length(partitionlist), extra = TRUE, label = TRUE, log = NULL, ...){
  variabletable <- getPhaseParameter(variable, partitionlist)
  
  low.plot <- pmax(1e-3, variabletable$low)
  high.plot <- variabletable$high
  high.plot[high.plot > 1e2] <- 1e2
  
  if(variable == "tau" & is.null(log)) log = "y" else log = ""
  
  with(variabletable,{
    plot(range(start, end), range(low.plot, high.plot, na.rm=TRUE), type="n", log = log, ...)
    if(!is.na(low[1])) rect(start, low.plot, end, high.plot, col = alpha(cols, .5), bor=NA)  
    segments(start, hat, end, hat, lwd = 2, col=cols)
    if(extra)
      segments(end[-length(end)], hat[-length(end)], start[-1], hat[-1], col="grey")
    
    if(label) mtext(variable, side = 3, at = start[1], font = 3, adj = 0)
  }
  )
}
