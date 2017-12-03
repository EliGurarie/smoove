#' Summarize phases
#' 
#' Takes a phase list and obtains a tidy table of point estimates of relevant parameters, 
#' 
#' @param phaselist a phase list (output of \code{\link{estimatePhases}}) 
#' @return table of start and end times and point estimates of relevant parameters for each phase
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