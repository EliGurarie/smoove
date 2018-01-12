#' Find Candidate Change Points
#' 
#' @details The raw output of the window sweep (\code{\link{sweepRACVM}})
#' 
#' @param windowsweep A windowsweep opject (matrix), output of \code{\link{sweepRACVM}} function
#' @param clusterwidth A time span within which very close change points are considered a single chagne point.  
#' @param verbose Whether or not to report the number of change points that are clustered away. 
#' @export
#' @examples
#' library(smoove)
#' library(magrittr)
#' data(simSweep,package = "smoove")
#' #The warning lets us know that some of the candidate change points are rather too close (in time) to each other
#' CP.all <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 0)
#' CP.all %>% as.vector
#' CP.clustered <- findCandidateChangePoints(windowsweep = simSweep, clusterwidth = 4)
#' CP.clustered %>% as.vector
findCandidateChangePoints <- function(windowsweep, clusterwidth=0, verbose = TRUE){
  
  T.raw <- attr(windowsweep, "time")
  starts <- colnames(windowsweep) %>% as.numeric
  time.unit <- attr(windowsweep, "time.unit")
  
  if(inherits(T.raw, "POSIXt"))
    T <- difftime(T.raw, T.raw[1], units = time.unit) %>% as.numeric else 
      T <- T.raw
    
  candidate.CPs <- apply(windowsweep, 2, function(ll) T[which.max(ll)]) %>% unique %>% unlist %>% sort
  n.raw <- length(candidate.CPs)
  
  if(clusterwidth > 0)  candidate.CPs <- clusters(rep(candidate.CPs,2), clusterwidth) %>% 
    sapply(mean) else
    candidate.CPs <- unique(candidate.CPs)
  
  candidate.CPs <- sort(candidate.CPs)
  n.new <- length(candidate.CPs)
  
  if(verbose)
  message(paste("Note: clustering candidate change points at", clusterwidth, "time units collapsed", n.raw, "candidate change points to", n.new, "change points.\n"))
  
  # compute number of points per segmen
  dTs <- table(cut(T, c(T[1]-1, candidate.CPs, T[length(T)]+1)))
  n.min <- min(dTs)
  if(n.min < 5)
    warning(paste0("\n Some of your partitions are very small - probably too small. You might consider re-clustering the change points with a threshold of at least ", signif(min(diff(candidate.CPs)),3), "."))
  
  allunits <- c("secs", "mins", "hours", "days", "weeks")
  allfactors <- c(1, 60, 60*60, 60*60*24, 60*60*24*7)
  
  if(inherits(T.raw, "POSIXt"))
    candidate.CPs <- T.raw[1] + candidate.CPs * allfactors[match(time.unit, allunits)]
  
  attr(candidate.CPs,"time.unit") <- time.unit
  attr(candidate.CPs,"time") <- T.raw
  attr(candidate.CPs,"Z") <- attributes(windowsweep)$Z
  
  return(candidate.CPs)
}
