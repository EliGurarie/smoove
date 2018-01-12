#' CVM change point model selection table
#' 
#' Compares selected models with and without changes across a set of change points. 
#' 
#' @param CPs candidate change points
#' @param modelset set of models to compare (combination of UCVM, ACVM, RCVM, RACVM, or \code{all}, which includes all of them)
#' @param tidy criteria by which change points are retained or rejected.  By default, filters by \code{differences}, meaning that if estimates on either side of a change point are outside of the 95\% confidence interval bounds, the change point is considered significant. Other options are \code{AIC} and \code{BIC}, meaning that if the dAIC or dBIC of the change point model is negative, the change point is thrown out. 
#' @param iterate whether or not to continue removing candidate change points until the most parsimonious set is selected.  Should, generally, be true. 
#' @param spline whether or not to use the spline approximation for the final estimate. 
#' @param criterion model selection criterion (choosing between UCVM, ACVM, etc.)- can be either BIC, AIC.  Note, this is different from Change Point retention criterion (in \code{tidy})
#' @param ... arguments to pass the \code{\link{testCP}} function, notably:
#' @export
#' @return a data frame with: \describe{
#' \item{\code{CP}}{index of change point}
#' \item{\code{start,end}}{index of beginning and end of change point window}
#' \item{\code{dAIC,dBIC}}{delta AIC and BIC values}
#' \item{\code{differences}}{which - if any - of the estimated parameters are outside of the 95\% range across the change point}
#' \item{\code{models}}{which models are selected on either side of change point.}}
#' 
#' @examples
#' library(smoove)
#' library(magrittr)
#' data(simSweep, package="smoove")
#' simSweep %>% findCandidateChangePoints(clusterwidth = 2) %>%
#' getCPtable(modelset = c("UCVM", "ACVM"))

getCPtable <- function(CPs, modelset, tidy = "strict", iterate = TRUE, spline = FALSE, criterion = "BIC", ...)
{
  Z <- attributes(CPs)$Z
  T <- attributes(CPs)$time
  time.unit <- attributes(CPs)$time.unit
  T.raw <- T
  
  if(inherits(T, "POSIXt")){
    CPs.raw <- CPs
    T <- as.numeric(T)
    CPs <- as.numeric(CPs)
    T.min <- min(T)
  } 
  
  stop <- FALSE
  while(!stop){
    starts <- c(min(T), CPs[-length(CPs)])
    ends <- c(CPs[-1], max(T))
    SelectTable <- data.frame(NULL)
    
    for(i in 1:length(CPs)){
      CPanalysis <- testCP(Z, T, CPs[i], starts[i], ends[i], modelset = modelset, spline = spline, criterion = criterion, ...)
      models <- CPanalysis$models
      testtable <- CPanalysis$testtable
      SelectTable <- rbind(SelectTable, 
                           cbind(data.frame(CP = CPs[i],
                                            start = starts[i],
                                            end = ends[i],
                                            dAIC = diff(testtable$AIC),
                                            dBIC = diff(testtable$BIC),
                                            differences = testtable$differences[1]),
                                 models))
    }
    
    if(!is.null(tidy)){
      if(tidy == "strict") SelectTable <- subset(SelectTable, SelectTable$differences != "" & SelectTable$dBIC > 0)
      if(grepl("diff", tidy)) SelectTable <- subset(SelectTable, SelectTable$differences != "")
      if("AIC" %in% tidy) SelectTable <- subset(SelectTable, SelectTable$dAIC > 0)
      if("BIC" %in% tidy) SelectTable <- subset(SelectTable, SelectTable$dBIC > 0)
      CPs.new <- SelectTable$CP
      if(!iterate | identical(CPs.new, CPs)) stop <- TRUE else CPs <- CPs.new
    } else stop <- TRUE
  }
  
  if(inherits(T.raw, "POSIXt")){
    SelectTable$CP <- T.raw[1] + SelectTable$CP - T.min
    SelectTable$start <- T.raw[1] + SelectTable$start - T.min
    SelectTable$end <- T.raw[1] + SelectTable$end - T.min
  } 
  
  attr(SelectTable, "Z") <- Z
  attr(SelectTable, "time.unit") <- time.unit
  attr(SelectTable, "time") <- T.raw
  
  return(SelectTable)
}

