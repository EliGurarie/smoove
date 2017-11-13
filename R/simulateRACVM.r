#' Rotational/Advective Correlated velocity movement
#' 
#' Simulates 2D correlated velocity movement model with rotation and advection
#' 
#' @details This function simulates a Rotational-Advective CVM explicitly using the governing OU equation, using a discretization in which dt << tau.  
#' @param tau characteristic time scale of autocorrelation
#' @param eta random speed parameter. In the case where \eqn{\omega = 0}, the mean square speed of movement is \eqn{E(|V|^2) = \eta^2 + \mu^2}.  In the case were \eqn{\mu = \omega = 0}, the mean tangential speed is \eqn{ E(|V|) = \eta \sqrt(\pi)/2}. 
#' @param omega rotational velocity
#' @param mu advective velocity (can be complex value where real and imaginary parts are x and y components of velocity)
#' @param v0 initial velocity
#' @param Tmax max time
#' @param dt time interval of simulation
#' @return a list with the following elements \describe{\item{T}{the time vector} \item{V}{the (complex) vector of velocities} \item{Z}{the (complex) vector of locations} \item{XY}{a two-column matrix of x-y coordinates of locations} \item{parameters}{a named list of model parameters.}}
#' @seealso \code{\link{estimateRACVM}}
#' @example ./examples/simulateRACVMexamples.r

simulateRACVM <- function(tau=1, eta = 1, omega=0, mu=0, v0=NULL, Tmax=100, dt=.01){
  T <- seq(0,Tmax,dt)
  n <- length(T)
  V <- T*0
  
  if(is.null(v0)) v0 <- sqrt(eta^2 + mu^2)
  
  dW <- (rnorm(n) + 1i*rnorm(n))*sqrt(dt)
  V[1] <- v0
  
  for(i in 2:n)
    V[i] <- V[i-1] * exp( - (1/tau + 1i*omega)*dt)  + eta * dW[i] / sqrt(tau)  
    
  Z <- cumsum(V + mu)*dt
  list(T = T, V = V, Z = Z,  XY = cbind(x = Re(Z), y = Im(Z)), 
       parameters = list(tau=tau, eta = eta, omega = omega, mu=mu))
}

simulateRACVM2 <- function(tau=1, eta = 1, omega=0, mu=0, v0=NULL, Tmax=100, dt=.01){
  T <- seq(0,Tmax,dt)
  n <- length(T)
  
  Vx <- T*0
  Vy <- T*0
   
  if(is.null(v0)) v0 <- sqrt(eta^2 + mu^2)
  
  dWx <- rnorm(n)*sqrt(dt)
  dWy <- rnorm(n)*sqrt(dt)
  
  Vx[1] <- Re(v0)
  Vy[1] <- Im(v0)
  
  for(i in 2:n){
    Vx[i] <- Vx[i-1] + ((1/tau) * (Re(mu) - Vx[i-1]) + omega*(Im(mu) - Vy[i-1]))*dt + eta * dWx[i] / sqrt(tau)  
	  Vy[i] <- Vy[i-1] + ((1/tau) * (Im(mu) - Vy[i-1]) - omega*(Re(mu) - Vx[i-1]))*dt + eta * dWy[i] / sqrt(tau)  
  }
  
  V <- Vx + 1i*Vy
  Z <- cumsum(V)*dt
  list(T = T, V = V, Z = Z, dt = dt, parameters = list(tau=tau, eta = eta, omega = omega, mu=mu))
}


#simulateRACVM2 <- function(tau=1, eta = 1, omega=0, mu=0, v0=NULL, Tmax=100, dt=.01){
#  
#  v.x <- "1/tau * (Re(mu) - x) - omega*(Im(mu) - y)"
#  v.y <- "1/tau * (Im(mu) - y) + omega*(Re(mu) - x)"
#  T <- seq(0,Tmax, dt)
#  
#  if(is.null(v0)) v0 <- sqrt(eta^2 + mu^2)
#  z0 <- c(x = Re(v0), y = Im(v0))
#  
#  model.parms <- c(mu = mu, omega = omega)
#  model.sigma <- eta/sqrt(tau)
#  
#  racvm.sim <- TSTraj(y0 = z0, time = Tmax+dt, deltat = dt,
#                      x.rhs = v.x, y.rhs = v.y, 
#                      parms = model.parms, 
#                      sigma = model.sigma)
#  
#  V <- (racvm.sim[,2] + 1i*racvm.sim[,3])
#  Z <- cumsum(V)*dt
#  list(T = T, V = V, Z = Z, dt = dt, parameters = list(tau=tau, eta = eta, omega = omega, mu=mu))
#}
