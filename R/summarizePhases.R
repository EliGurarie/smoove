#' Summarize phases
#' 
#' Takes a phase list and obtains a tidy table of point estimates of relevant parameters, 
#' 
#' @param phaselist a phase list (output of \code{\link{estimatePhases}}) 
#' @return table of start and end times and point estimates of relevant parameters for each phase
#' @examples 
#' library(smoove)
#' library(gplots) # for rich colors
#' library(magrittr)
#' data(simSweep, package="smoove")
#' 
#' simCP.table <- simSweep %>%
#'   findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>%
#'   getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
#'   
#' simPhaselist <- estimatePhases(simCP.table)
#' Z <- attributes(simPhaselist)$Z
#' T <- attributes(simPhaselist)$time
#'  
#' cols <- rich.colors(length(simPhaselist))
#' T.cuts <- c(T[1], simCP.table$CP, T[length(T)])
#' Z.cols <- cols[cut(T, T.cuts, include.lowest = TRUE)]
#' 
#' phaseTable <- summarizePhases(simPhaselist)
#' plot(Z, asp=1, type="l", xpd=FALSE)
#' points(Z, col=Z.cols, pch=21, bg = scales::alpha(Z.cols, 0.5), cex=0.8)
#' legend("top", legend = paste0(phaseTable$phase, ": ", phaseTable$model),
#'        fill=cols, ncol=3, bty="n", title = "Phase: model")
#' @export
summarizePhases <- function(phaselist){
  estimates <- plyr::llply(phaselist, function(a){
    if(!("omega" %in% names(a$estimates))) a$estimates$omega = NA
    if(!("mu.x" %in% names(a$estimates))) a$estimates$mu.x  = NA
    if(!("mu.y" %in% names(a$estimates))) a$estimates$mu.y = NA
    a$estimates
  })
  
  summary <- data.frame(ldply(phaselist, 
                              function(a) with(a, data.frame(start, end, model))),
                        ldply(estimates, function(a) a[1,])) %>% mutate(.id.1 = NULL) %>% 
    plyr::rename(c(".id" = "phase"))
  
  which.NA <- apply(summary, 2, function(x) all(is.na(x)))
  if(sum(which.NA)>0) summary <- summary[,!which.NA]
  
  return(summary)
}