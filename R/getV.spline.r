##' Get splined velocity estimates
##'
##' Computes the locations and velocities of a cubic spline of a 2D track over time.
##'
#' @param Z location data in complex form (X + iY)
#' @param T.raw time of observations to feed the spline.
#' @param T.spline times for which the spline is requested.  If NULL (the default) the function returns the velocity at midpoints between consecutive T.raw.
#' @param resolution of spline, i.e. the fraction of the smallest interval in T.raw.
#' @param ... further arguments to \code{\link{splinefun}}.  Importantly, the method. 
#' @return a list with the estimated velocity (V), as well as the position (Z) and time (T).  

getV.spline <- function(Z, T.raw, T.new = NULL, resolution = 1e-2, ... ){
  
  fX.spline <- splinefun(T.raw, Re(Z), ...)
  fY.spline <- splinefun(T.raw, Im(Z), ...)
  
  T.raw <- as.numeric(T.raw)
  if(is.null(T.new)) T.new <- T.raw[-length(T.raw)] + diff(T.raw)/2
  
  dT.min <- min(c(diff(T.new), diff(T.raw)))
  T.spline <- seq(min(T.raw), max(T.raw), dT.min*resolution)
  
  X.spline <- fX.spline(T.spline)
  Y.spline <- fY.spline(T.spline)
  Z.spline <- X.spline + 1i*Y.spline
  V.spline <- diff(Z.spline)/diff(as.numeric(T.spline))
  
  dt.spline <- as.numeric(diff(T.spline))[1]
  
  # find closest match (should be very close!)
  d.new.spline <- outer(T.new, T.spline, function(x,y) abs(as.numeric(y)-as.numeric(x)))
  which.spline<- apply(d.new.spline, 1, which.min)
  
  # to remove stray NA in the velocity spline
  which.spline  <- which.spline[-length(which.spline)]

  return(list(V = V.spline[which.spline], Z = Z.spline[which.spline], T = T.new[-length(T.new)]))
}