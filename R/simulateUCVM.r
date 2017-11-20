#' Correlated velocity movement: OU Simulation
#' 
#' Simulates 2D correlated velocity movement model
#' 
#' @details This function simulates an unbiased CVM in one of two ways: either by explicitly sampling from the velocity equation at some small interval dt << tau and integrating to obtain positions, or sampling locations and velocities directly from the true process at arbitrary times. 
#' 
#' @param nu mean speed of movement
#' @param tau characteristic time scale of movement
#' @param v0 initial velocity vector.  Default is randomly oriented vector with magnitude \code{nu}
#' @param T.max maximum time of simulations.
#' @param dt time interval of simulations.
#' @param T time vector
#' @param method one of \code{direct}, referring to a direct numerical integration of the process which requires a fixed (and small) interval, or \code{exact} which samples from the likelihood of the complete process and can take any random vector or times \code{T}.  The "exact" sampling can be much slower for a large vector of observations.  See the package \code{vignette("smoove",package="smoove")} for more details. 
#'
#' @return a list with the following elements \describe{\item{T}{the time vector} \item{V}{the (complex) vector of velocities} \item{Z}{the (complex) vector of locations} \item{XY}{a 2 x n matrix containing columns for the X and Y coordinates of the location} \item{dt, tau, nu,vo}{the parameters of the model.}}
#' 
#' @seealso simulateUCVM, simulateRACVM
#' 
#' @examples
#' # Direct simulation of UCVM
#' nu <- 2; tau <- 5; dt <- .1; ucvm <- simulateUCVM(nu, tau, T.max = 1000, dt = dt)
#' plot(ucvm$XY, asp=1, type="l", main = paste0("UCVM(",nu, tau,")"))
#' 
#' ## Also plotting the velocity and using the plot_track function.
#' par(mfrow=c(1,2))
#' plot(ucvm$V[1:1000], asp=1, type="l", main = "Velocity")
#' plot_track(ucvm$Z[1:1000], pch=19, cex=0.5, col=rgb(0,0,0,.1), main = "Location")
#' 
#' # Exact simulation of UCVM, with illustration of velocity and position samples
#' T <- cumsum(rexp(100))
#' ucvm2 <-  simulateUCVM(nu, tau, T = T, method="exact")
#' layout(rbind(c(1,3,3,2), c(1,4,4,2))); par(bty="l", oma=c(0,0,4,0))
#' with(ucvm2, {
#'     plot(V, type="o", asp=1, main="Velocity",  col=rgb(0,0,0,.5), cex=0.5)
#'     plot(Z, type="o", main="Position", asp=1, col=rgb(0,0,0,.5), cex=0.5)
#'     plot(T, Re(V), col=2, type="o", main="Velocity (decomposed)", 
#'     ylim=range(c(Re(V), Im(V))),  pch=19, cex=0.5)
#'     lines(T, Im(V), col=3, type="o", pch=19, cex=0.5)
#'     plot(T, Re(Z), col=2, type="o", main="Position (decomposed)", 
#'     ylim=range(XY), pch=19, cex=0.5)
#'     lines(T, Im(Z), col=3, type="o", pch=19, cex=0.5)
#' })
#' @export
#' @usage 
#' simulateUCVM(nu = 1, tau = 1, v0 = nu * exp((0+1i) * runif(1, 0, 2 * pi)), 
#' T.max = NULL, dt = NULL, T = NULL, method = c("direct","exact")[1])
#' 
simulateUCVM <- function(nu = 1, tau=1, v0= nu * exp((0+1i) * runif(1, 0, 2 * pi)), 
                         T.max=NULL, dt=NULL, T = NULL,
                         method = c("direct", "exact")[1])
{
  if(is.null(T))  T <- seq(0,T.max,dt)
    
  if(method == "direct" & !zero_range(diff(T)))
     stop("Sorry, you can't directly simulate for an irregular time vector. Try 'method = 'exact''.")
    
  if(method == "direct"){
    n <- length(T)
    V <- T*0
    dW <- (rnorm(n) + 1i*rnorm(n))*sqrt(dt)
    V[1] <- v0
    for(i in 2:n)
      V[i] <-  V[i-1] - V[i-1] * dt/tau + 2*nu/sqrt(pi*tau) * dW[i]              
    Z <- cumsum(V)*dt
    return(list(T = T, V = V, XY = cbind(x = Re(Z), y = Im(Z)), 
                Z = Z,
                parameters = list(tau = tau, nu = nu, v0 = v0)))
  }

  if(method == "exact")
    return(simulateUCVM.exact(T, nu = nu, tau = tau, v0 = v0))
}

zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}