#' Plot Phase Partitioning of RACVM analysis

#' @param phaselist Complete "smoove" partitioning output - returned by \link{estimatePhases}
#' @param cols colors for phases (by default uses the `rich.colors` palette from `gplots`)
#' @param plot.parameters whether the parameters are plotted
#' @param parameters which parameters to plot (by default - ALL of the estimated parameters)
#' @param plot.legend whether to plot a legend
#' @param legend.where location of legend
#' @param layout  "horizontal" (default) or "vertical"- as preferred (partial string matching accepted)
#' @export
#' @example demo/plotPhaseList_example.R
 
plotPhaseList <- function(phaselist,
                          cols = gplots::rich.colors(length(phaselist)),
                          plot.parameters = TRUE,
                          parameters = NULL, 
                          plot.legend = TRUE, 
                          legend.where = "bottomright",
                          layout = c("horizontal","vertical")){
  
  Z <- attr(phaselist, "Z")
  time <- attr(phaselist, "time")
  phaseTable <- summarizePhases(phaselist)
  
  if(is.null(parameters)){
    allparameters <-  c("eta", "tau", "rms", "mu.x", "mu.y", "omega.x", "omega.y")
    parameters <- names(phaseTable)[names(phaseTable) %in% allparameters]
  }

  mars <- par()$mar
  omas <- par()$oma
  omas[1] <- 2
  
  n.param <- length(parameters)
  
  if(plot.parameters){
    if(grepl(layout[1], "vertical")){
      layout(1:(n.param + 1), 
             heights = c(1, rep(1/n.param, n.param)))
      par(mar = c(1, mars[2], 2, mars[4]), oma = omas)
      }
    else{
      par(mar = c(0, mars[2], 2, mars[4]), oma = omas)
      layout(cbind(rep(1, n.param), 1:n.param+1))
    }
  }
  
  T.cuts <- c(phaseTable$start, max(time))
  Z.cols <- cols[cut(time, T.cuts, include.lowest = TRUE)] 
  
  # plot Track
  plot(Z, asp=1, type="l", xpd=FALSE, xlab = "X", ylab = "Y")
  points(Z, col=Z.cols, pch=21, bg = alpha(Z.cols, 0.5), cex=0.8)
  if(plot.legend){
    legend(legend.where, legend = paste0(phaseTable$phase, ": ", phaseTable$model), 
         fill=cols, ncol=3, bty="n", title = "Phase: model")
    }

  # plotParameters
  if(plot.parameters){  
    mars <- par()$mar
    par(mar = c(0, mars[2], 1.5, mars[4]))
    for(p in parameters)
    plotPhaseParameter(p, phaselist, ylab="", xlab="", col=cols, 
                       xaxt= ifelse(p == parameters[length(parameters)], "s", "n"),
                       log = ifelse(p =="tau", "y", ""))
  }
}

