#' Obtain and Plot Estimates From RACVM Partition
#' 
#' @details \code{getVariable} takes a phase list  and generates a table indicating the start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter. 
#' @details \code{plotVariable} 
#' 
#' @param variable one of: \code{omega}, \code{mu.x}, \code{mu.y}, \code{eta}, \code{tau}
#' @param phaselist output of \code{\link{estimatePhases}}, i.e. a list of information on each of the selected phases
#' @return \code{getPhaseParameter} returns a data.frame with start time, end time, estimate, low and high 95\% C.I. and selected model for a given parameter, with each row corresponding to an enumerated partition phase.  
#' @examples 
#' library(smoove)
#' library(magrittr)
#' data(simSweep)
#' 
#' simCP.table <- simSweep %>%
#'   findCandidateChangePoints(clusterwidth = 4, verbose = FALSE) %>%
#'   getCPtable(modelset = c("UCVM", "ACVM"), criterion = "AIC")
#' simPhaselist <- estimatePhases(simCP.table)
#' 
#' getPhaseParameter("omega",simPhaselist)
#' @export
getPhaseParameter <- function(variable, phaselist)
{
  ldply(phaselist, 
        function(pl){
          e <- pl$estimates
          if(variable %in% names(e)){ 
            hat <- e[1,variable]
            low <- e[2,variable]
            high <- e[3,variable]} else {
              hat <- 0; low <- 0; high <- 0
            }
          
          data.frame(start = pl$start,
                     end = pl$end,
                     hat, low, high, 
                     model = pl$model)
        }) %>% 
    plyr::rename(c(".id" = "phase"))
}
