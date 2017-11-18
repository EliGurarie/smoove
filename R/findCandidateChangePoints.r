#' findCandidateChangePoints
#' 
#' @param windowsweep windowsweep
#'
#' @param clusterwidth clusterwidth
#'
#' @export
findCandidateChangePoints <- function(windowsweep, clusterwidth=0){
  
  T <- row.names(windowsweep) %>% as.numeric
  starts <- colnames(windowsweep) %>% as.numeric
  
  candidate.CPs <- apply(windowsweep, 2, function(ll) T[which.max(ll)]) 
  
  if(clusterwidth > 0)  candidate.CPs <- clusters(candidate.CPs, clusterwidth) %>% sapply(mean) else
    candidate.CPs <- unique(candidate.CPs)
  
  candidate.CPs<- sort(candidate.CPs)
  
  # compute number of points per segment
  dTs <- table(cut(T, c(T[1]-1, candidate.CPs, T[length(T)]+1)))
  n.min <- min(dTs)
  if(n.min < 5)
    warning(paste0("\n Some of your partitions are very small - probably too small. You might consider re-clustering the change points with a threshold of at least ", signif(min(diff(candidate.CPs)),3), "."))
  
  return(candidate.CPs)
}
