#' Kestrel
#' 
#' For more information about Kestrel movement behavior see the references.
#' 
#' @format A data frame with xx rows and n columns
#'  \describe{
#'  \item{event.id}{event.id}
#'  \item{visible}{visible}
#'  \item{timestamp}{timestamp}
#'  \item{longitude}{longitude}
#'  \item{latitude}{latitude}
#'  \item{manually.marked.outlier}{manually.marked.outlier}
#'  \item{sensor.type}{sensor.type}
#'  \item{individual.taxon.canonical.name}{individual.taxon.canonical.name}
#'  \item{tag.local.identifier}{tag.local.identifier}
#'  \item{ID}{ID of individual}
#'  \item{study.name}{study.name}
#'  \item{X}{X in UTM coordinate system}
#'  \item{Y}{Y in UTM coordinate system}
#'  
#'  }
#'
#' @references 
#' Hernandez-Pliego J, Rodriguez C, Bustamante J (2015) Why do kestrels soar? PLOS ONE. 10(12): e0145402. doi:10.1371/journal.pone.0145402
#' Hernandez-Pliego J, Rodriguez C, Bustamante J (2015) Data from: Why do kestrels soar? Movebank Data Repository. doi:10.5441/001/1.sj8t3r11
#' 
#' @source
#' https://www.datarepository.movebank.org/handle/10255/move.487
#'
"Kestrel"

#' simSweep
#' 
#' simSweep is a simulated dataset that could be generate by calling \code{\link{simulateRACVM}} iteratively and then calling \code{\link{sweepRACVM}}. See the provided example.
#' 
#' @format A matrix with 400 rows and 24 columns with the following notable attributes:
#'  \describe{
#'  \item{time}{a vector of times}
#'  \item{Z}{a complex vector of locations}
#'  
#'  }
#' 
#' @examples
#' taus <- c(3, 3, 1)
#' mus <- c(2, 0, 0)
#' etas <- c(2, 1, 1)
#' durations <- c(40,60,100)
#' 
#' Z.raw <- 0
#' T.raw <- 0
#' mycvm <- list()
#' 
#' for(i in 1:length(taus)){
#'   if(i > 1)  v0 <- mycvm$V[length(mycvm)]  else v0 = mus[1]
#'   mycvm <- simulateRACVM(tau = taus[i], eta = etas[i], mu = mus[i], v0 = v0,
#'                          Tmax = durations[i], dt = 0.01)
#'   Z.raw <- c(Z.raw, mycvm$Z + Z.raw[length(Z.raw)])
#'   T.raw <- c(T.raw, mycvm$T + T.raw[length(T.raw)])
#' }
#' 
#' multicvm <- data.frame(Z = Z.raw, T = T.raw)[sample(1:length(Z.raw), 400),] %>% 
#'             dplyr::arrange(T)
#' #a time consuming line of code
#' simSweep <- with(multicvm, sweepRACVM(Z = Z, T = T, 
#'                  windowsize = 80, windowstep = 5, 
#'                  model = "ACVM", progress=FALSE))
#'
"simSweep"