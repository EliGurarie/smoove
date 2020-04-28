#' Obtain multi-state RACVM estimates
#' 
#' Takes a set of change points and fits the best model to each phase
#' 
#' @param Z locations as a complex vector
#' @param T times vector
#' @param CPs change points (as indices)
#' @param criterion One of \code{"AIC"} or \code{"BIC"} - which information criterion to use for the model selection.
#' @return a named list of phases (numbered by roman numerals) containing the selected model, estimates, and confidence intervals for each parameter.
#' @export
partitionRACVM <- function(Z,T,CPs, criterion = "BIC"){
  phase.start <- c(T[1], CPs)
  phase.end <- c(CPs, T[length(T)])
  
  partition <- list()
  
  for(i in 1:length(phase.start)){
    
    z <- Z[ T>=phase.start[i] & T <= phase.end[i]]
    t <- T[ T>=phase.start[i] & T <= phase.end[i]]
    
    fit <- estimateRACVM(Z = z, T= t, compare.models = TRUE, spline=TRUE)
    
    lowAIC <- which.min(fit$CompareTable$deltaAIC)
    lowBIC <- which.min(fit$CompareTable$deltaBIC)
    
    pick <- ifelse(criterion == "AIC", lowAIC, lowBIC)
    
    model <- strip.model(row.names(fit$CompareTable)[pick])
    if(model$fit.omega & model$fit.mu) 
      myresults <- fit$results 
    else
      myresults <- with(model, estimateRACVM(Z = z, T= t, 
                                             #fit.mu = fit.mu,  #unused argument
                                             #fit.omega = fit.omega,  #unused argument
                                             compare.models = FALSE, spline=TRUE)$results)
    partition[[i]] <- list(estimates = myresults, start = phase.start[i], end = phase.end[i], model = model$model)
  }
  names(partition) <- as.roman(1:length(partition))
  return(partition)
}
