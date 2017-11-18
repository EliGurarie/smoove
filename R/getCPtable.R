#' CVM change point model selection table
#' 
#' Compares selected models with and without changes across a set of change points. 
#' 
#' @param CPs candidate change points
#' @param XY two-column matrix of location vector.
#' @param Z  Complex location vector (instead of XY).
#' @param T time vector
#' @param modelset set of models to compare (combination of UCVM, ACVM, RCVM, RACVM, or \code{all}, which includes all of them)
#' @param tidy criteria by which to tidy the change point selection table.  By default, filters by \code{extremes}, meaning if estimates on either side of a change point are outside of the 95\% confidence interval bounds, the change point is considered no significant, and \code{AIC} and \code{BIC}, meaning that if the dAIC or dBIC of the change point model is negative, the change point is thrown out. 
#' @param iterate whether to iterate
#' @param spline whether or not to use the spline approximation for the final estimate. 
#' @param criterion selection criterion - either BIC (or 'bic')
#' @param ... other optional arguments to pass to \code{\link{testCP}}
#'
#' @return a data frame with: \describe{
#' \item{\code{CP}}{index of change point}
#' \item{\code{start,end}}{index of beginning and end of change point window}
#' \item{\code{dAIC,dBIC}}{delta AIC and BIC values}
#' \item{\code{extremes}}{which - if any - of the estimated parameters are outside of the 95\% range across the change point}
#' \item{\code{models}}{which models are selected on either side of change point.}}
#' @export
getCPtable <-
  function(CPs,
           XY = NULL,
           Z = NULL,
           T,
           modelset,
           tidy = "extremes",
           iterate = TRUE,
           spline = FALSE,
           criterion = 'BIC',
           ...)
{
  if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
  
  stop <- FALSE
  while(!stop)
  {
    starts <- c(min(T), CPs[-length(CPs)])
    ends <- c(CPs[-1], max(T))
    SelectTable <- data.frame(NULL)
    for(i in 1:length(CPs))
    {
      CPanalysis <- testCP(Z, T, CPs[i], starts[i], ends[i], modelset = modelset, spline=spline, criterion=criterion)
      
      models <- CPanalysis$models
      testtable <- CPanalysis$testtable
      
      SelectTable <- rbind(SelectTable, 
                           cbind(data.frame(CP = CPs[i],
                                            start = starts[i],
                                            end = ends[i],
                                            dAIC = diff(testtable$AIC),
                                            dBIC = diff(testtable$BIC),
                                            extremes = testtable$extremes[1]),
                                 models))
    }
    
    if("extremes" %in% tidy) SelectTable <- subset(SelectTable, 'extremes' != "")
    if("AIC" %in% tidy) SelectTable <- subset(SelectTable, 'dAIC' > 0)
    if("BIC" %in% tidy) SelectTable <- subset(SelectTable, 'dBIC' > 0)
    CPs.new <- SelectTable$CP
    if(!iterate | identical(CPs.new, CPs)) stop <- TRUE else CPs <- CPs.new
  }
  
  return(SelectTable)
}