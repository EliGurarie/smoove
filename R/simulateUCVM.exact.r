#' Correlated velocity movement: Exact Updating 
#' 
#' Simulates 2D correlated velocity movement model for arbitrary time intervals, using the "exact" updating formulae of Gillespie (1996). 
#'
#' @aliases getX, getSigma
#' @details This function samples from an unbiased CVM process explicitly using the governing OU equation.   
#' @param T vector of times
#' @param nu mean speed of movement
#' @param tau characteristic time scale of movement
#' @param v0 initial velocity vector.  Default is randomly oriented vector with magnitude \code{nu}
#' @return a list with the following elements \describe{\item{T}{the time vector} \item{V}{the (complex) vector of velocities} \item{Z}{the (complex) vector of locations} \item{X}{a 4xn matrix containing columns for, respectively, Vi, Zi, Vj and Zj where i and j refer to the x and y coordinates of the movement} \item{dt, tau, nu,vo}{the parameters of the model.}}
#' @seealso simulateUCVM, simulateRACVM
#' @examples
#' # sampling 100 random times up to (about) 1000:
#' T <- cumsum(rexp(100)*10) 
#' # Simulate
#' cvm2 <- simulateUCVM.exact(T, nu=2, tau=5)
#' # Illustrate
#' layout(rbind(c(1,3,3,2), c(1,4,4,2)))
#' par(bty="l", oma=c(0,0,4,0))
#' plot(cvm2$V, type="l", asp=1, main="Velocity", xlab="", 
#' ylab="", col="darkgrey")
#' points(cvm2$V, pch=19, cex=0.5)
#' plot(cvm2$Z, type="l", main="Position", asp=1, col="darkgrey")
#' points(cvm2$Z, pch=19, cex=0.5)
#' plot(cvm2$T, Re(cvm2$V), col=2, type="l", main="Velocity (decomposed)", 
#' ylim=range(c(Re(cvm2$V), Im(cvm2$V))))
#' lines(cvm2$T, Im(cvm2$V), col=3, type="l")
#' plot(cvm2$T, Re(cvm2$Z), col=2, type="l", main="Position (decomposed)", 
#' ylim=range(c(Re(cvm2$Z), Im(cvm2$Z))))
#' lines(cvm2$T, Im(cvm2$Z), col=3, type="l")
#' title("CVM(2, 5): 0-1000, 100 random samples", outer=TRUE, cex=1.5)
#' @export
simulateUCVM.exact <-  function(T, nu = 1, tau = 1, v0 = nu * exp((0+1i) * runif(1, 0, 2 * pi)))
  {
    X <- matrix(c(Re(v0), 0, Im(v0), 0), nrow=1)
        
    if(T[1]!=0) T <- c(0,T)
    
    dT <- diff(T)
    
    for(i in 1:length(T[-1]))
      X <- rbind(X, getX(X[i,], dT[i], nu, tau))
    
    V <- X[,1]+1i*X[,3]
    Z <- X[,2]+1i*X[,4]
   return(list(T = T[-1], V = V[-1], 
               XY = cbind(x = Re(Z[-1]),  y = Im(Z[-1])), 
               Z = Z[-1], 
               parameters = list(tau = tau, nu = nu, v0 = v0)))
  }


getX <-
  function(x, dt, nu, tau)
  {
    kappa <- exp(-dt/tau)
    Sigma <- matrix(0,nrow=4,ncol=4)
    Sigma[1:2,1:2] <- getSigma(dt, nu, tau)
    Sigma[3:4,3:4] <- getSigma(dt, nu, tau)
    
    #Sigma <- bdiag(list(getSigma(dt, nu, tau), getSigma(dt, nu, tau)))
    
    mu <- c(x[1] * kappa, 
            x[2] + x[1]*tau*(1-kappa), 
            x[3] * kappa, 
            x[4] + x[3]*tau*(1-kappa))
    
    mvrnorm2(n = 1, mu, Sigma)
  }


getSigma <-  function(dt, nu, tau)
  {
    kappa <- exp(-dt/tau)
    varV <- (2*nu^2 / pi) * (1 - kappa^2)
    covVX <- (2*nu^2 * tau / pi) * (1 - kappa)^2
    varX <- (4*nu^2 * tau^2/ pi) * (dt/tau - 2*(1 - kappa) + (1-kappa^2)/2)
    rbind(c(varV, covVX), c(covVX, varX))
  }

mvrnorm2 <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE) 
{
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE, EISPACK = FALSE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
    t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}

