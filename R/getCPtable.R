#' CVM change point model selection table
#' 
#' Compares selected models with and without changes across a set of change points. 
#' 
#' @param CPs candidate change points
#' @param {XY,Z} XY two-column matrix or complex location vector.  One of these must be specified.    
#' @param T times of observations
#' @param modelset set of models to compare (combination of UCVM, ACVM, RCVM, RACVM, or \code{all}, which includes all of them)
#' @param tidy criteria by which change points are retained or rejected.  By default, filters by \code{differences}, meaning that if estimates on either side of a change point are outside of the 95\% confidence interval bounds, the change point is considered significant. Other options are \code{AIC} and \code{BIC}, meaning that if the dAIC or dBIC of the change point model is negative, the change point is thrown out. 
#' @param iterate whether or not to continue removing candidate change points until the most parsimonious set is selected.  Should, generally, be true. 
#' @param spline whether or not to use the spline approximation for the final estimate. 
#' @param criterion model selection criterion (choosing between UCVM, ACVM, etc.)- can be either BIC, AIC.  Note, this is different from Change Point retention criterion (in \code{tidy})
#' @return a data frame with: \describe{
#' \item{\code{CP}}{index of change point}
#' \item{\code{start,end}}{index of beginning and end of change point window}
#' \item{\code{dAIC,dBIC}}{delta AIC and BIC values}
#' \item{\code{differences}}{which - if any - of the estimated parameters are outside of the 95\% range across the change point}
#' \item{\code{models}}{which models are selected on either side of change point.}}

getCPtable <- function(CPs, XY = NULL, Z = NULL, T, modelset, tidy = "strict", iterate = TRUE, ...)
{
  if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
  
  stop <- FALSE
  while(!stop){
    starts <- c(min(T), CPs[-length(CPs)])
    ends <- c(CPs[-1], max(T))
    SelectTable <- data.frame(NULL)
    
    for(i in 1:length(CPs)){
      CPanalysis <- testCP(Z, T, CPs[i], starts[i], ends[i], modelset = modelset, ...)
      
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
      if(tidy == "strict") SelectTable <- subset(SelectTable, differences != "" & dBIC > 0)
      if(grepl("diff", tidy)) SelectTable <- subset(SelectTable, differences != "")
      if("AIC" %in% tidy) SelectTable <- subset(SelectTable, dAIC > 0)
      if("BIC" %in% tidy) SelectTable <- subset(SelectTable, dBIC > 0)
      CPs.new <- SelectTable$CP
      if(!iterate | identical(CPs.new, CPs)) stop <- TRUE else CPs <- CPs.new
    } else stop <- TRUE
  }
  
  return(SelectTable)
}

