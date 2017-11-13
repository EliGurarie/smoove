#' Estimate RACVM parameters
#'
#' Estimate rotational-advectice correlated velocity movement model 
#' 
#' This group of functions estimate the parameters of a rotational and advective CVM using a one-step velocity likelihood.  It is best implemented on relatively high resolution data from which one can obtain good estimates of velocity.  The observations can, however, be irregularly sampled.  
#' 
#' The parameterization is: \eqn{\tau} - characteristic time scale, \eqn{\mu} - advective velocity, \eqn{\eta} - random rms speed, \eqn{\omega} - angular speed. 
#' 
#' The \code{fitRACVM} function is an (internal) helper function.  

#' @param {XY,Z,T} respectively, XY two-column matrix, optional complex location matrix, and times of observations
#' @param {track} a possible \code{track} object (i.e. data frame with columns called X, Y and Time) to provide instead of Z and T.
#' @param {model} one of UCVM, RCVM, ACVM or RACVM. 
#' @param {compare.models} whether to compare four models: with both rotation and advection, only rotation, only advection, or neither.  The comparison provides a table with the log likelihood, number of parameters, AIC, BIC, delta AIC and delta BIC values.  A limited comparison set may be useful when running the fit many times (e.g. when performing change point analysis). 
#' @param {modelset} which models to fit and compare (if \code{compare.models}) is TRUE)
#' @param {p0} optional named list of initial parameter values in the form: c(tau, eta, omega, mu.x, mu.y).
#' @param {spline} whether to implement the spline correction on the positions 
#' @param {spline.res} resolution of spline (see \code{\link{getV.spline}})
#' @param {T.spline} new times for spline estimation (best left as NULL)
#' @param {time.units} time units of calculations (e.g. "sec", "min", "hour", "day")
#' @aliases fitRACVM 
#' @seealso \code{\link{simulateRACVM}}
#' @example ./examples/estimateRACVMexamples.r

estimateRACVM <- function(XY, Z=NULL, T, track = NULL, 
                          model = "RACVM", 
                          compare.models = TRUE, 
                          modelset = c("UCVM", "ACVM", "RCVM", "RACVM"),
                          p0 = NULL, 
                          spline = FALSE, spline.res = 1e-2, T.spline= NULL,  
                          time.units = "day", verbose = FALSE){  
    
    all <- c("UCVM", "ACVM", "RCVM", "RACVM")
    if(identical(modelset, "all")) modelset <- all
    
    if(!is.null(track)){
      Z <- with(track, X+1i*Y)
      T <- track$Time
    }
    
    if("POSIXt" %in% is(T))
      T <- difftime(T, T[1], units = time.units) %>% as.numeric
    
    if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
    
    if(spline){
      if(is.null(T.spline)) T.spline <- seq(min(T), max(T), length = length(T))
      VZT <- getV.spline(Z,T, resolution = spline.res, T.new = T.spline)
      V <- VZT$V
      T <- VZT$T
    } else {
      V <- diff(Z)/diff(T)
      T <- (T[-1] + T[-length(T)])/2
    }

    # initial parameter values
    
    if(is.null(p0)){
        mu0 <- mean(V)
        tau0 <-  diff(range(T))/10
        eta0 <- sqrt(mean(Mod(V)^2))
        
        # using simple acf to initialize omega
        
        logv <- log(Mod(V))
        logv.detrend <- logv - mean(logv[Mod(V) > 0])
        logv.acf <- acf(logv.detrend, lag.max = round(length(Z)/2), plot=FALSE)$acf
        logv.localminima <- (which(diff(sign(diff(logv.acf)))==2)+1)
        logv.minima <- which(logv.acf < 0)[which(logv.acf < 0) %in% logv.localminima]
        if(length(logv.minima) > 0) 
          omega0 <- pi/min(logv.minima) * mean(diff(T)) else 
            omega0 <- pi/(length(Z)*mean(diff(T))/2)
        
        p0 <- c(logtau = log(tau0), eta = eta0, omega = omega0, mu.x = Re(mu0), mu.y = Im(mu0))
    }
    
    if(verbose){
      cat("initial values:\n")
      print(p0)
    }
    fit <- fitRACVM(T, V, p0, model = model)
    
    if(compare.models){
      fit.list <- list()
      for(m in modelset){
          if(m == model)  fit.list[[m]] <- fit else
            fit.list[[m]] <- fitRACVM(T, V, p0, model = m)
        }
          
      logLike <- sapply(fit.list, function(fit) fit$LL)  
      
      k <- c(2, 4, 3, 5)[match(modelset, all)]
      
      AIC <- -2*logLike + 2 * k
      BIC <- -2*logLike + k * log(length(V))
      deltaAIC <- AIC - min(AIC)
      deltaBIC <- BIC - min(BIC)
      
      fit$CompareTable <- data.frame(logLike, k, AIC, deltaAIC, BIC, deltaBIC)
    }
    return(fit)
  }


fitRACVM <- function(T, V, p0, model = "UCVM"){
  
   if(model == "UCVM"){fit.omega = FALSE; fit.mu = FALSE} else
      if(model == "ACVM"){fit.omega = FALSE; fit.mu = TRUE} else
        if(model == "RCVM"){fit.omega = TRUE; fit.mu = FALSE} else
          if(model == "RACVM"){fit.omega = TRUE; fit.mu = TRUE} else
            stop("Please choose one of the model options: UCVM, ACVM, RCVM, or RACVM.")
  
  V.like <- function(p, V, T, fit.omega, fit.mu){
    
    tau <- exp(p["logtau"])
    eta <- p["eta"]
     
    if(fit.omega) omega <- p["omega"] else omega <- 0
    if(fit.mu) mu <- p["mu.x"] + 1i*p["mu.y"] else mu <- 0
    
    dt <- diff(T)
    V1 <- V[-length(V)] - mu
    V2 <- V[-1] - mu
    V.mu <- V1 * exp(-(1/tau + 1i*omega)*dt)
    V.sigma2 <- (eta^2/2) * (1 - exp(-2*dt/tau)) 
   
    - sum(c(dnorm(Re(V2), Re(V.mu), sqrt(V.sigma2), log = TRUE),
          dnorm(Im(V2), Im(V.mu), sqrt(V.sigma2), log = TRUE)))
  }

  if(!fit.omega) p0 <- p0[-which(names(p0) == "omega")]
  if(!fit.mu) p0 <- p0[-match(c("mu.x", "mu.y"), names(p0))]
  
  p.fit <- optim(p0, V.like, V = V, T = T, 
                 fit.omega = fit.omega, fit.mu = fit.mu, hessian = TRUE)
  
  p.hat <- p.fit$par
  keep.se.params <- which(diag(p.fit$hessian) != 0)
  hessian <- p.fit$hessian[keep.se.params, keep.se.params]
  p.se <- rep(NA, length(p.hat))
  
  ses <- try(diag(solve(hessian)^(1/2)))
  if(inherits(ses, "try-error")) ses <- rep(NA, length(keep.se.params))
  p.se[keep.se.params] <- ses
  
  CI.low <- p.hat - 2*p.se
  CI.high <- p.hat + 2*p.se
  
  p.hat["tau"] <- exp(p.hat["logtau"])
  CI.low["tau"] <- exp(CI.low["logtau"])
  CI.high["tau"] <- exp(CI.high["logtau"])
  
  LL <- -p.fit$value
  
  results <- rbind(Estimate = p.hat, CI.low, CI.high) %>% 
    as.data.frame %>% mutate(logtau=NULL, logeta=NULL)
  
  # Computing approximate confidence intervals around eta (propagating the error)
  # W = eta^2 + mu.x^2 + mu.y^2
  # V = sqrt(W)
  # EW and VarW come from (transformations) of non-central Chi distribution
  # EV and VarV come from Taylor expansion around moments
  
  if(!("mu.x" %in% names(results))) results$rms = results$eta else{
    r <- mutate(results, tau = NULL, omega = NULL)
    
    mu2 <- r[1,]^2
    sigma2 <- ((r[2,] - r[1,])/2)^2
    ms <- 2 * sum(sigma2 *(1 + 2*mu2))
    
    EW <- sum(mu2 + sigma2)
    varW <- 2 * sum(sigma2 * (sigma2 + 2*mu2)) 
    EV <- sqrt(EW) - (varW/8) * EW^(-3/2)
    varV <- (1/(4*EW)) * varW
    sdV <- sqrt(varV)
    
    results$rms = c(EV, max(EV - 2*sdV, 0), EV + 2*sdV)
  }
  
  return(list(results=results, LL = LL)) 
}
