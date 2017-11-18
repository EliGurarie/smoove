#' getEVAF
#' 
#' Obtain empirical velocity autocorrelation function.
#' 
#' @details Requires at least one of complex locations, XY coordinates, or velocties to compute.  
#' 
#' @param Z complex vector of locations
#' @param XY two-column matrix of x-y coordinates of locations
#' @param V complex vector of velocities
#' @param T times of observations
#' @param lagmax maximum lag to compute the empirical VAF for
#' @return A data frame with columns: \code{lag} and \code{vaf}
#' @examples
#' nu <- 2; tau <- 5
#' ucvm <- simulateUCVM(nu=nu, tau = tau, T.max = 1000, dt = .1)
#' evaf <- with(ucvm, getEVAF(Z=Z,T=T, lagmax = 20))
#' plot(evaf)
#' eta <- 2/sqrt(pi)*nu
#' curve(eta^2 * exp(-x/tau), add=TRUE, col=2, lwd=2)
#' @export
getEVAF <- function(Z, XY=NULL, V=NULL, T, lagmax = max(T)/2){
  T <- T-T[1]
  dT <- mean(diff(T))
  if( (sd(diff(T)) / dT) > 1e-10) stop("Sorry - time intervals must be constant to compute the velocity autocovariance function.")
  
  if(is.null(V)) V <- diff(Z)/dT
  lag <- T[T < lagmax]
  lag.scalar <- as.integer(lag/dT)
  vaf <- apply(as.matrix(lag.scalar),1,getVaf, V=V)
  
  data.frame(lag, vaf)
}

ComplexDot <- function(a,b)
  #(Re(a) + 1i*Im(a)) * (Re(b) - 1i*Im(b))
  Re(a)*Re(b) + Im(a)*Im(b)
  
getVaf <- function(lag, V){
  if(lag == 0) vaf.all <- Mod(V^2) else
    vaf.all <- ComplexDot(V[-(1:lag)],V[-((length(V)-lag+1):length(V))])
  return(mean(vaf.all))
}

omegaVAF <- function(lag, mu, omega, tau, beta)
  (mu^2 + beta^2 * tau * exp(-lag/tau) * cos(omega*lag)) #/ (mu^2 + beta^2*tau)

ls.omegaVAF <- function(p, lag, vaf, weight=1){
  tau   <- p[1]
  beta  <- p[2]
  omega <- p[3]
  mu    <- p[4]
  sum(weight*(vaf - omegaVAF(lag, mu, omega, tau, beta))^2)
}
