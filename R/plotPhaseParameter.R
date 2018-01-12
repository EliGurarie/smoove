#' Plot Estimates From RACVM Partition
#' 
#' @details \code{getVariable} takes a partitionlist and generates a table indicating the start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter. 
#' @details \code{plotVariable} 
#' 
#' @param variable one of: \code{omega}, \code{mu.x}, \code{mu.y}, \code{eta}, \code{tau}
#' @param phaselist output of \code{\link{estimatePhases}}, i.e. a list of information on each of the selected phases
#' @param cols colors of bars (by default - a rich color palette)
#' @param label whether to label the plotted parameter (with the word "tau", "eta", etc.)
#' @param ymax maximum y
#' @param ... additional arguments to pass to plotting function.  A useful one is \code{log = "y"} for highly skewed data.
#' @return \code{getVariable} returns a data.frame with start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter, with each row corresponding to an enumerated partition phase.  
#' @examples 
#' library(smoove)
#' library(gplots) # for rich colors
#' library(magrittr)
#' data(simSweep, package="smoove")
#' 
#' layout(c(1,1,1,2:6))
#' simCP.table <- simSweep %>%
#'   findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>%
#'   getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
#' 
#' Z <- multicvm$Z
#' T <- multicvm$T
#' # Note, these are also contained in the attributes of *any* of the
#' # intermediate objects, e.g.
#' Z <- attributes(simPhaselist)$Z
#' T <- attributes(simPhaselist)$time
#' 
#' cols <- rich.colors(length(simPhaselist))
#' T.cuts <- c(T[1], simCP.table$CP, T[length(T)])
#' Z.cols <- cols[cut(T, T.cuts, include.lowest = TRUE)]
#' 
#' phaseTable <- summarizePhases(simPhaselist)
#' plot(Z, asp=1, type="l", xpd=FALSE)
#' points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
#' legend("top", legend = paste0(phaseTable$phase, ": ", phaseTable$model),
#'        fill=cols, ncol=3, bty="n", title = "Phase: model")
#' 
#' par(mar=c(2,2,1,0), xpd=NA)
#' plotPhaseParameter("tau", simPhaselist, ylab="", xaxt="n", xlab="", col=cols, log="y")
#' plotPhaseParameter("eta", simPhaselist, ylab="", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("mu.x", simPhaselist, ylab= "", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("mu.y", simPhaselist, ylab= "", xaxt="n", xlab="", col=cols)
#' plotPhaseParameter("rms", simPhaselist, ylab= "", xlab="time", col=cols)
#' @export
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
