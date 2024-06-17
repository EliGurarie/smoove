#' Plot Phase Partitioning of RACVM analysis

#' @param phaselist Complete "smoove" partitioning output - returned by \link{estimatePhases}
#' @param cols colors for phases (by default uses the `rich.colors` palette from `gplots`)
#' @param legend.where location of legend
#' @param plot.parameters whether the parameters are plotted
#' @param parameters which parameters to plot (by default - ALL of the estimated parameters)
#' @export
#' @example demo/plotPhaseList_example.R
 
plotPhaseList <- function(phaselist,
                          cols = gplots::rich.colors(length(phaselist)),
                          legend.where = "bottomright",
                          parameters = NULL, 
                          plot.parameters = TRUE){
  
  Z <- attr(phaselist, "Z")
  time <- attr(phaselist, "time")
  phaseTable <- summarizePhases(phaselist)
  
  if(is.null(parameters)){
    allparameters <-  c("eta", "tau", "rms", "mu.x", "mu.y", "omega.x", "omega.y")
    parameters <- names(phaseTable)[names(phaseTable) %in% allparameters]
  }
  
  if(plot.parameters)
    layout(1:(length(parameters) + 1), 
           heights = c(1, rep(1/length(parameters), length(parameters))))
    
  T.cuts <- c(phaseTable$start, max(time))
  Z.cols <- cols[cut(time, T.cuts, include.lowest = TRUE)] 
  
  mars <- par()$mar
  par(mar = c(1, mars[2], 2, mars[4]), oma = c(2,0,0,0))
  
  plot(Z, asp=1, type="l", xpd=FALSE)
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  legend(legend.where, legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
         fill=cols, ncol=3, bty="n", title = "Phase: model")

  if(plot.parameters){  
    mars <- par()$mar
    par(mar = c(0, mars[2], 1.5, mars[4]))
    for(p in parameters)
    plotPhaseParameter(p, phaselist, ylab="", xlab="", col=cols, 
                       xaxt= ifelse(p == parameters[length(parameters)], "s", "n"),
                       log = ifelse(p =="tau", "y", ""))
  }
}

