#' Obtain 1D complete VV-VZ-ZZ or ZZ covariance matrix
#' 
#' Takes vector of times T and parameters nu, tau, v0, and returns either complete V-Z covariance matrix (if \code{getSigma.VZ}) or Z-Z portion of the covariance matrix (if \code{getSigma.ZZ}).
#' @usage getSigma.VZ(T, nu, tau, v0=nu)
#' getSigma.ZZ(T, nu, tau, v0=nu)
#' @param T vector of times (length n).
#' @param nu mean speed.
#' @param tau characteristic time scale.
#' @return either a 2n x 2n VV-VZ-ZZ matrix or n x n ZZ matrix. 
#' 
#' @aliases getSigma.ZZ
#' @examples
#' require(gridExtra)
#' T <- cumsum(rexp(10))
#' # separated into Sigma_vv, Sigma_vz (assymetric), and Sigma_zz
#' Sigma <- Matrix(getSigma.VZ(T, nu=2, tau=5, v0=2))
#' i1 <- image(Sigma[1:10,1:10], main="V-V", colorkey=TRUE, useRaster=TRUE)
#' i2 <- image(Sigma[11:20,1:10], main="V-Z", colorkey=TRUE, useRaster=TRUE)
#' i3 <- image(Sigma[11:20,11:20], main="Z-Z", colorkey=TRUE, useRaster=TRUE)
#' grid.arrange(i1,i2,i3,ncol=3)
#' # And the complete matrix (log transformed):
#' image(log(Sigma), colorkey=TRUE, main="log(Sigma)")


getSigma.VZ <-
function(T, nu, tau, t0=0)
{
  n <- length(T)
  M <- matrix(0, nrow=n*2, ncol=n*2)
  
  # THIS IS THE SLOW STEP!!!!
  for(i in 0:(length(T)-1))
    for(j in i:(length(T)-1))
      M[(i*2+1):(i*2+2), (j*2+1):(j*2+2)] <- getCov.vz(T[i+1],T[j+1], nu, tau, t0)
  M <- forceSymmetric(M)
  
  # rearrange to separate VV, VZ, ZV and ZZ blocks
  M.new <- M*0
  # Velocity-Velocity
  M.new[1:n,1:n] <- M[seq(1,n*2,2), seq(1,n*2,2)]
  # Velocity-LocationH
  M.new[1:n,(n+1):(2*n)] <- M[seq(2,n*2,2), seq(1,n*2,2)]
  M.new[(n+1):(2*n),1:n] <- M[seq(1,n*2,2), seq(2,n*2,2)]
  # Location-Location
  M.new[(n+1):(2*n),(n+1):(2*n)] <- M[seq(2,n*2,2), seq(2,n*2,2)]
  return(M.new)
}

getSigma.ZZ <-
  function(T, nu, tau, t0=0)
  {
    n <- length(T)
    M <- matrix(0, nrow=n, ncol=n)
    for(i in 1:n)
      for(j in i:n)
        M[i,j] <- getCov.zz(T[i],T[j], nu, tau, t0)
    M[lower.tri(M)] <- t(M)[lower.tri(M)]
    return(M)
  }

getSigma.ZZ2 <-
  function(T, nu, tau, t0=0)
  {
    n <- length(T)
    M <- matrix(0, nrow=n, ncol=n)
    for(i in 1:n)
      for(j in i:n)
        M[i,j] <- getCov.zz2(T[i],T[j], nu, tau, t0)
    M[lower.tri(M)] <- t(M)[lower.tri(M)]
    return(M)
  }
