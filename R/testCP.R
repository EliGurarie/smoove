#' Test RACVM change point
#' 
#' Identifies appropriate models on either side of a change point
#' 
#' @param {Z,T} location and time of data
#' @param {cp} change point
#' @param {start,end} beginning and end time of segment to analyze
#' @param modelset set of models to compare (combination of UCVM, ACVM, RCVM, RACVM, or \code{all}, which includes all of them)
#' @param spline whether or not to use the spline approximation for the final estimate. 
#' @param criterion selection criterion - either BIC (or 'bic')
#' 
testCP <- function(Z, T, cp, start, end, modelset = "all", ...){
  
  if(identical(modelset, "all")) modelset <- c("UCVM", "ACVM", "RCVM", "RACVM")
  
  i1 <- which(T >= start & T <= cp)
  i2 <- which(T >= cp & T <= end)
  iboth <- min(i1):max(i2)
  
  Z1 <- Z[i1]
  Z2 <- Z[i2]
  Zboth <- Z[iboth]
  
  T1 <- T[i1]
  T2 <- T[i2]
  Tboth <- T[iboth]
  
  fit1 <- getFit(Z1, T1, modelset = modelset, ...)
  fit2 <- getFit(Z2, T2, modelset = modelset, ...)
  fitboth <- getFit(Zboth, Tboth, modelset = modelset, ...)
  
  #  Which (if any) of the parameters in the most complex SHARED model are significantly different
  
  r1 <- fit1$results
  r2 <- fit2$results
  
  extremes <- names(r1)[(r1[1,] < r2[2,] | r1[1,] > r2[3,]) | (r2[1,] < r1[2,] | r2[1,] > r1[3,])]
  
  #lo.high <- smartbind(r1[2,], r2[3,], fill=0)
  #high.lo <- smartbind(r1[3,], r2[2,], fill=0)
  #which.extreme <- (lo.high[1,] > lo.high[2,]) | (high.lo[1,] < high.lo[2,])
  # extremes <- paste(names(r1)[which.extreme], collapse = "-")
  
  testtable <- data.frame(
    AIC = c(Changepoint = -2*(fit1$LL + fit2$LL) + 2*(fit1$k+fit2$k+1), 
            NoChangepoint =  -2*fitboth$LL + 2 * fitboth$k),
    BIC = c(Changepoint = -2*(fit1$LL + fit2$LL) + log(length(Zboth))*(fit1$k+fit2$k+1), 
            NoChangepoint =  -2*fitboth$LL + log(length(Zboth)) * fitboth$k),
    K = c(Changepoint = fit1$k + fit2$k + 1, 
          NoChangepoint = fitboth$k),
    extremes = c(paste(extremes, collapse = ", "), NA))
  
  models <- data.frame(M1 = fit1$model, M2 = fit2$model, Mboth = fitboth$model)
  
  return(list(testtable=testtable, models = models))
}


getFit <- function(z, t, modelset, criterion = "BIC", spline = FALSE){
  
  fit <- estimateRACVM(Z = z, T = t, model = "UCVM", modelset = modelset, compare.models = TRUE, spline = spline)
  
  ct <- fit$CompareTable
  
  if(criterion %in% c("bic", "BIC")) bestmodel <- row.names(ct)[which.min(ct$deltaBIC)] else
    if(criterion %in% c("aic", "AIC")) bestmodel <- row.names(ct)[which.min(ct$deltaAIC)] else stop("Criterion should be one of `AIC' or `BIC'.")
  
  if(bestmodel != "UCVM")
    fit <- estimateRACVM(Z = z, T = t, 
                         model = bestmodel, modelset = modelset, 
                         compare.models = TRUE, spline = FALSE)
  
  fit$model <- bestmodel
  fit$k <- fit$CompareTable[bestmodel,"k"]
  
  return(fit)
}  
