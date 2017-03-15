#' Obtain multi-state RACVM estimates
#' 
#' Takes a set of change points and fits the best model to each phase
#' 
#' @param {Z,T} locations and times
#' @param {CP.table} change point table, i.e. output of \code{\link{getCPtable}} which contains a column of change points and a column of selected models. 
#' @param verbose whether to print a summary of the results
#' @return a named list of phases (numbered by roman numerals) containing the selected model, estimates, and confidence intervals for each parameter.

getPhases <- function(CP.table, Z, T, criterion = "BIC", verbose = TRUE){
  CPs <- CP.table$CP
  
  phase.start <- c(T[1], CPs)
  phase.end <- c(CPs, T[length(T)])
  phases <- list()
  
  M1 <- as.character(CP.table$M1)
  M2 <- as.character(CP.table$M2)
  
  models <- c(M1, M2[length(M2)])
  
  for(i in 1:length(phase.start)){
    z <- Z[ T>=phase.start[i] & T <= phase.end[i]]
    t <- T[ T>=phase.start[i] & T <= phase.end[i]]
    fit <- estimateRACVM(Z = z, T= t, compare.models = FALSE, model = models[i])$results
    phases[[i]] <- list(estimates = fit, start = phase.start[i], end = phase.end[i], model = models[i])
  }
  names(phases) <- as.roman(1:length(phases))
  if(verbose) print(summarizePhases(phases))
  return(phases)
}

summarizePhases <- function(phases){
  estimates <- plyr::llply(phases, function(a){
    if(!("omega" %in% names(a$estimates))) a$estimates$omega = NA
    if(!("mu.x" %in% names(a$estimates))) a$estimates$mu.x  = NA
    if(!("mu.y" %in% names(a$estimates))) a$estimates$mu.y = NA
    a$estimates
  })
  
  summary <- data.frame(ldply(phases, 
                              function(a) with(a, data.frame(start, end, model))),
                        ldply(estimates, function(a) a[1,])) %>% mutate(.id.1 = NULL) %>% 
    plyr::rename(c(".id" = "phase"))
  
  which.NA <- apply(summary, 2, function(x) all(is.na(x)))
  print(sum(which.NA)>0)
  if(sum(which.NA)>0) summary <- summary[,!which.NA]
  
  return(summary)
}