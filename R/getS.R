#' Obtain 1D complete S.zz matrix
#' 
#' Takes vector of times T and parameters nu, tau, v0, and returns the zz portion of the covariance matrix.
#' 
#' @param t1 first time
#' @param t2 second time
#' @param t0 initial time
#' @param tau characteristic time scale.
#'
#' @return either a 2n x 2n VV-VZ-ZZ matrix or n x n ZZ matrix. 
#' 
#' @examples
#' if (requireNamespace('gridExtra',quietly = TRUE))
#'  warning("please install \"gridExtra\" first to run this example")
#' T <- cumsum(rexp(10))
#' # separated into Sigma_vv, Sigma_vz (assymetric), and Sigma_zz
#' Sigma <- Matrix::Matrix(getSigma.VZ(T, nu=2, tau=5, t0=2))
#' #i1 <- image(Sigma[1:10,1:10], main="V-V", colorkey=TRUE, useRaster=TRUE)
#' #i2 <- image(Sigma[11:20,1:10], main="V-Z", colorkey=TRUE, useRaster=TRUE)
#' #i3 <- image(Sigma[11:20,11:20], main="Z-Z", colorkey=TRUE, useRaster=TRUE)
#' #gridExra::grid.arrange(i1,i2,i3,ncol=3)
#' # And the complete matrix (log transformed):
#' #image(log(Sigma), colorkey=TRUE, main="log(Sigma)")
#' 
#' @export
getcov.zz <- function(t1, t2, tau, t0 = 0) {
  eps1 <- exp(-(t1 - t0)/tau)
  kappa <- exp(-(t2 - t1)/tau)
  CovVZ <- tau * (1 - eps1)^2 #* 2 / pi
  VarZ <- tau^2 * ((t1 - t0)/tau - 2 * (1 - eps1) + (1 - eps1^2)/2) *2 # 4 / pi
  return(VarZ + tau * (1 - kappa) * CovVZ)
}

#' @param T time vector of length n 
#'
#' @rdname getcov.zz
#' @export
getS.zz <- function(T, tau, t0 = 0) {
  M <- outer(T,T,getcov.zz, tau=tau)
  M[lower.tri(M)] <- t(M)[lower.tri(M)]
  return(Matrix(M))
}

# getcov.zz <- function(t1, t2, tau, t0 = 0) {
#   eps1 <- exp(-(t1 - t0)/tau)
#   kappa <- exp(-(t2 - t1)/tau)
#   VarV <- (2/pi) * (1 - eps1^2)
#   VarZ <- 4 * tau^2/pi * ((t1 - t0)/tau - 2 * (1 - eps1) + (1 - eps1^2)/2)
#   CovVZ <- (2 * tau/pi) * (1 - exp(-t1/tau))^2
#   return(VarZ + tau * (1 - kappa) * CovVZ)
# }
