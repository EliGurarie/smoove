#' Estimating parameters of unbiased CVM
#' 
#' This  function  estimates the mean speed \eqn{nu}, the time-scale \eqn{tau} and (occasionally) the initial speed \eqn{v_0} of the unbiased correlated velocity movement (UCVM).  See Gurarie et al. (in review) and the \code{vignette{smoove}} vignette for more details. 

#' @param Z location data in complex form (X + iY)
#' @param XY ... or, optionally, as a two column matrix of x and y coordinates. 
#' @param T time vector
#' @param method the method to use for the estimation.  These are (in increasing : velocity auto-correlation fitting (\code{vaf}), correlated random walk matching (\code{crw}), velocity likelihood (\code{vLike}), position likelihood (\code{zLike}) and position likelihood with Kalman filter (\code{crawl}). This last method is generally he best method, since it  fits the position likelihood more efficiently by using a Kalman filter. It is based on Johnson et al (2008) and is a wrapper for the \code{\link{crwMLE}} in the (excellent) \code{\link{crawl}} package.  The default method is \code{vLike}.
#' @param parameters which parameters to estimate.  For most methods "tau" and "nu" are always both estimated, but some computation can be saved for the velocity likelihood method by providing an estimate for "nu".
#' @param CI whether or not to compute 95\% confidence intervals for parameters. In some cases, this can slow the computation down somewhat.
#' @param spline whether or not to use the spline correction (only relevant for \code{vaf} and \code{vLike}).
#' @param diagnose whether to draw a diagnostic plot.  Varies for different methods.
#' @param ... additional parameters to pass to estimation functions.  These are particularly involved in the \code{crawl} method (see \code{\link{crwMLE}}). 
#'
#' @return A data frame with point estimates of mean speed `nu' and time-scale `tau' 
#' @example ./demo/estimateUCVM_examples.r
#' @export
estimateUCVM <- 
  function(Z,
           XY,
           T,
           method = c("vaf", "crw", "vLike", "zLike", "crawl")[3],
           parameters = c("tau", "nu"),
           spline = FALSE,
           CI = FALSE,
           diagnose = FALSE,
           ...
  )
{
   if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
    
  if(method == "crw")
    return(estimateCVM.crw(Z,T,CI,diagnose,...))
    
  if(method == "vaf")
    return(estimateCVM.vaf(Z,T,CI=CI,spline=spline,diagnose=diagnose,...))
  
  if(method == "vLike")
    return(estimateCVM.vLike(Z,T,CI=CI,parameters=parameters,spline=spline,...))
  
  if(method == "zLike")
    return(estimateCVM.zLike(Z,T,CI=CI,...))
  
  if(method == "crawl")
    return(estimateCVM.crawl(Z,T, CI = CI,...))
  }


