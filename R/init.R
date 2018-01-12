#' @useDynLib smoove
#' @import graphics
#' @importFrom methods is
#' @importFrom grDevices rainbow rgb
#' @importFrom utils as.roman txtProgressBar setTxtProgressBar
#' @importFrom stats acf optim rnorm quantile dnorm optimize cor sd qnorm lm qt smooth.spline uniroot splinefun runif na.omit
#' @importFrom crawl crwMLE
#' @importFrom CircStats dwrpcauchy
#' @importFrom numDeriv hessian
#' @importFrom nlme gls corAR1 varExp intervals
#' @importFrom intervals clusters
#' @importFrom plyr mutate ldply aaply
#' @importFrom Matrix Matrix forceSymmetric
#' @importFrom gtools smartbind
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom magrittr %>%
#' @importFrom lubridate tz with_tz
#' @importFrom scales alpha
NULL