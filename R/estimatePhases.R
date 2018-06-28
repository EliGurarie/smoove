#' Obtain multi-state RACVM estimates
#' 
#' Takes a set of change points and fits the best model to each phase in an CVM changepoint analysis.  
#' 
#' The output of this function is generally passed to the \code{\link{summarizePhases}} function to obtain a tidy summary of the estiamted phases. 
#' 
#' @param CP.table change point table, i.e. output of \code{\link{getCPtable}} which contains a column of change points and a column of selected models. 
#' @param criterion an information criterion such as BIC, AIC etc.
#' @param verbose whether to print a summary of the results
#' @return a named list of phases (numbered by roman numerals) containing the selected model, estimates, and confidence intervals for each parameter.
#' @seealso \code{\link{summarizePhases}}
#' @examples
#' library(smoove)
#' library(magrittr)
#' data(simSweep)
#' simCP.table <- simSweep %>%
#' findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>%
#'  getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
#' (simPhaselist <- estimatePhases(simCP.table))
#' @export
estimatePhases <- function(CP.table, criterion = "BIC", verbose = TRUE){
  CPs <- CP.table$CP
  
  Z <- attributes(CP.table)$Z
  T <- attributes(CP.table)$time
  time.unit <- attributes(CP.table)$time.unit
  
  phase.start <- c(T[1], CPs)
  phase.end <- c(CPs, T[length(T)])
  
  if(inherits(T, "POSIXt")){
    phase.start <- with_tz(phase.start, tz(T))
    phase.end <- with_tz(phase.end, tz(T))
  }
  
  phases <- list()
  
  M1 <- as.character(CP.table$M1)
  M2 <- as.character(CP.table$M2)
  
  models <- c(M1, M2[length(M2)])
  
  for(i in 1:length(phase.start)){
    z <- Z[ T >= phase.start[i] & T <= phase.end[i]]
    t <- T[ T >= phase.start[i] & T <= phase.end[i]]
    fit <- estimateRACVM(Z = z, T = t, compare.models = FALSE, model = models[i], time.units = time.unit)$results
    phases[[i]] <- list(estimates = fit, start = phase.start[i], end = phase.end[i], model = models[i])
  }
  names(phases) <- as.roman(1:length(phases))

  attr(phases,"time.unit") <- time.unit
  attr(phases,"time") <- T
  attr(phases,"Z") <- Z
  
  if(verbose) print(summarizePhases(phases))
  return(phases)
}
