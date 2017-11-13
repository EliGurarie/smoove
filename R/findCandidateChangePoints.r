#' Find Candidate Change Points
#' 
#' @details The raw output of the window sweep (\code{\link{sweepRACVM}})
#' 
#' @param windowsweep A windowsweep opject (matrix), output of \code{\link{sweepRACVM}} function
#' @param clusterwidth A time span within which very close change points are considered a single chagne point.  
#' @param verbose Whether or not to report the number of change points that are clustered away. 

findCandidateChangePoints <- function(windowsweep, clusterwidth=0, verbose = TRUE){
  
  T <- row.names(windowsweep) %>% as.numeric
  starts <- colnames(windowsweep) %>% as.numeric
  
  candidate.CPs <- apply(windowsweep, 2, function(ll) T[which.max(ll)]) 
  n.raw <- length(candidate.CPs)
  
  if(clusterwidth > 0)  candidate.CPs <- clusters(rep(candidate.CPs,2), clusterwidth) %>% 
    sapply(mean) else
    candidate.CPs <- unique(candidate.CPs)
  
  candidate.CPs <- sort(candidate.CPs)
  n.new <- length(candidate.CPs)
  
  if(verbose)
  message(paste("Note: clustering candidate change points at", clusterwidth, "time units collapsed", n.raw, "candidate change points to", n.new, "change points.\n"))
  
  # compute number of points per segment
  dTs <- table(cut(T, c(T[1]-1, candidate.CPs, T[length(T)]+1)))
  n.min <- min(dTs)
  if(n.min < 5)
    warning(paste0("\n Some of your partitions are very small - probably too small. You might consider re-clustering the change points with a threshold of at least ", signif(min(diff(candidate.CPs)),3), "."))
  
  return(candidate.CPs)
}
