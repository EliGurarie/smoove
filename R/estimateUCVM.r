#' Estimating parameters of unbiased CVM
#' 
#' This  function  estimates the mean speed \eqn{nu}, the time-scale \eqn{tau} and (occasionally) the initial speed \eqn{v_0}of the unbiased correlated velocity movement (UCVM).  See Gurarie et al. (in review) and the \code{vignette("smoove", package = "smoove")} vignette for more details. 
#' @param Z location data in complex form (X + iY)
#' @param XY ... or, optionally, as a two column matrix of x and y coordinates. 
#' @param T time of observations (NOTE: must be regularly spaced for methods "vaf" and "crw")
#' @param method the method to use for the estimation.  These are (in increasing : velocity auto-correlation fitting (\code{vaf}), correlated random walk matching (\code{crw}), velocity likelihood (\code{vLike}), position likelihood (\code{zLike}) and position likelihood with Kalman filter (\code{crawl}). This last method is generally he best method, since it  fits the position likelihood more efficiently by using a Kalman filter. It is based on Johnson et al (2008) and is a wrapper for the \code{\link{crwMLE}} in the (excellent) \code{\link{crawl}} package.  The default method is \code{vLike}.
#' @param parameters which parameters to estimate.  For most methods "tau" and "nu" are always both estimated, but some computation can be saved for the velocity likelihood method by providing an estimate for "nu".
#' @param CI whether or not to compute 95\% confidence intervals for parameters. In some cases, this can slow the computation down somewhat.
#' @param spline whether or not to use the spline correction (only relevant for \code{vaf} and \code{vLike}).
#' @param diagnose whether to draw a diagnostic plot.  Varies for different methods.
#' @param ... additional parameters to pass to estimation functions.  These are particularly involved in the \code{crawl} method (see \code{\link[crawl]{crwMLE}}). 
#' @return A data frame with point estimates of mean speed `nu' and time-scale `tau' 
#' @example ./demo/estimateUCVM_examples.r
#' @export
estimateUCVM <- function(Z,
           XY,
           T,
           method = c("vaf", "crw", "vLike", "zLike", "crawl")[3],
           parameters = c("tau", "nu"),
           CI = FALSE,
           spline = FALSE,
           diagnose = FALSE,
           ...)
{
   if(is.null(Z)) Z <- XY[,1] + 1i*XY[,2]
    
  if(method == "crw")
    return(estimateCVM.crw(Z,T, CI = CI, diagnose = diagnose, ...))
    
  if(method == "vaf")
    return(estimateCVM.vaf(Z,T, CI = CI, diagnose = diagnose, spline = spline, ...))
  
  if(method == "vLike")
    return(estimateCVM.vLike(Z,T, CI = CI, parameters = parameters, spline = spline, ...))
  
  if(method == "zLike")
    return(estimateCVM.zLike(Z,T, CI = CI, ...))
  
  if(method == "crawl")
    return(estimateCVM.crawl(Z,T, CI = CI,...))
  }


estimateCVM.crawl <- function(Z, T, p0 = NULL, verbose = TRUE, CI = NULL, ...)
{
  X = Re(Z)
  Y = Im(Z)
  data <- data.frame(X, Y, T)
  
  initial.state <- list(
    a1.x=c(X[1],0),
    a1.y=c(Y[1],0),
    P1.x=diag(c(1,1)),
    P1.y=diag(c(1,1)))
  
  initial.state <- list(
    a=c(X[1],0,Y[1],0),
    P=diag(rep(1,4)))
  
  Fit.crawl <- crwMLE(
    mov.model=~1, 
    data=data, coord=c("X","Y"), 
    Time.name="T", 
    #theta = ,# initial parameters,
    initial.state=initial.state, 
    fixPar=c(NA, NA), ...)
  
  Get.nutau <- function(fit)
  {
    sigma.hat <- exp(fit$par[1])
    sigma.CI <- exp(fit$ci[1,])
    
    tau.hat <- 1/exp(fit$par[2])
    tau.CI <- sort(1/exp(fit$ci[2,]))
    
    nu.hat <- sqrt(pi/tau.hat)*sigma.hat/2
    nu.CI <- sqrt(pi/tau.hat)*sigma.CI/2
    
    results <- data.frame(Estimate = c(tau.hat, nu.hat), rbind(tau.CI, nu.CI))
    names(results) <- c("Estimate", "L", "U")
    return(results)
  }
  
  nutau <- Get.nutau(Fit.crawl)
  row.names(nutau) <- c("tau", "nu")
  
  if(verbose) print(Fit.crawl)
  
  return(nutau)
}

estimateCVM.crw <- function(Z, T, CI = FALSE, diagnose = FALSE)
{
  # extract pieces
  V <- diff(Z)/diff(T)
  lambda <- mean(Mod(V)^2) / mean(Mod(V))^2
  Theta <- diff(Arg(diff(Z)))
  n <- length(Theta)
  dT.mean <- mean(diff(T))
  
  # estimates of kappa  
  if(CI){
    kappaLikelihood <- function(kappa, theta)
      -sum(log(dwrpcauchy(theta, mu=0, rho=kappa)))
    suppressWarnings(kappa.fit <- optim(0.5, kappaLikelihood, theta=Theta, hessian=TRUE))
    kappa.hat <- kappa.fit$par
    kappa.sd <- sqrt(1/kappa.fit$hessian)[1,1]
  } else kappa.hat <- mean(cos(Theta))
  
  # estimates of lambda
  S <- Mod(V)
  s2 <- mean(S^2)
  Sbar2 <- mean(S)^2
  lambda.hat <- (s2 / Sbar2)
  if(CI) lambda.sd <- (s2 / Sbar2) * sqrt((2*(n+1)/n^2 + 4*s2 / (n*Sbar2)))
  
  # conversion function
  
  crw2cvm <- function(kappa, lambda)
  {
    tau <- getTau(lambda, kappa, dT.mean)
    nu <- sqrt(pi/8 * (lambda + 2*kappa/(1-kappa)) / (dT.mean * tau)) * mean(S)
    rbind(nu, tau)
  }
  
  # point estimates
  nutau.hat <- crw2cvm(kappa.hat, lambda.hat)
  
  nu.hat <- nutau.hat[1]
  tau.hat <- nutau.hat[2]
  tau.CI <- rep(NA,2)
  nu.CI <- rep(NA,2)
  
  # confidence intervals
  if(CI)
  {
    kappas <- rnorm(1000, kappa.hat, kappa.sd)
    lambdas <- rnorm(1000, lambda.hat, lambda.sd)
    nutaus <- Vectorize(crw2cvm)(kappas, lambdas)
    nutau.CIs <- apply(nutaus, 1, quantile, prob=c(0.025, 0.975))
    nu.CI <- nutau.CIs[,1]
    tau.CI <- nutau.CIs[,2]
  } 
  
  # diagnostic plot
  if(diagnose) DiagnoseCRW(Z)
  
  results <- data.frame(tau.hat = tau.hat, nu.hat = nu.hat)
  if(CI) 
  {
    results <- data.frame(t(results), rbind(tau.CI, nu.CI))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
  }
  return(results)
}

getTau <- function(lambda, kappa, mu=1)
{
  f <- function(x) exp(-x/mu)*(1+1/lambda*(-1+exp(kappa*x/mu))) - exp(-1)
  uniroot(f, c(0, 100))$root
}

DiagnoseCRW <- function(Z)
{
  # Get non-jumpy theta
  
  phi <- Arg(diff(Z))
  posjumps <- which(diff(phi) > pi)+1
  negjumps <- which(diff(phi) < -pi)+1
  l <- length(phi)
  for(i in posjumps)
    phi[i:l] <- phi[i:l]-2*pi
  for(i in negjumps)
    phi[i:l] <- phi[i:l]+2*pi
  theta <- diff(Arg(diff(Z)))
  theta[theta < pi] <- theta[theta < pi] + 2*pi
  theta[theta > pi] <- theta[theta > pi] - 2*pi
  
  
  # plot acf's
  par(mfrow=c(2,1), bty="l", cex.lab=1.25, mar=c(0,4,1,2), oma=c(4,0,4,0))
  acf(Mod(diff(Z)), main="", xaxt="n", xlab=""); mtext("Step length", line=0, cex=1.25, font=3)
  acf(theta, main=""); mtext("Turning angle", line=-1, cex=1.25, font=3)
  title("Autocorrelations should be near 0 at lag>0", outer=TRUE, font=2)
}

estimateCVM.vaf <- function(Z, T, lagmax = (max(T) - min(T))/2, CI = FALSE, diagnose = FALSE, spline=FALSE)
{
  T <- T-min(T)
  dT <- mean(diff(T))
  if(sd(diff(T))/mean(diff(T)) > 1e-10) stop("Sorry - time intervals must be constant to use this method.")
  
  lag <- T[T < lagmax]
  V <- diff(Z)/diff(T)
  
  if(spline)
  {
    VZT <- getV.spline(Z,T)
    V <- VZT$V
    T <- VZT$T
    Z <- VZT$Z
  }
  
  lag.scalar <- lag/dT
  vaf <- apply(as.matrix(lag.scalar),1,getVaf, V=V)
  
  truncate <- 30
  if(min(vaf) < exp(-3))
  {
    truncate <- min(which(vaf<exp(-3)))
    vaf <- vaf[1:(truncate-1)]
    lag <- lag[1:(truncate-1)]
  }
  
  if(truncate < 3 | truncate == "Inf"){ 
    warning("Data are too sparcely sampled for any estimate of tau.")
    beta.hat <- NA; beta.CI <- NA;
  } else if(truncate < 10){ 
    warning("Data are too sparcely sampled for robust estimate of tau.")
    beta.hat <- lm(log(vaf)~lag-1)$coef; beta.CI <- NA; 
  } else {
    y <- log(vaf/vaf[1])
    x <- lag
    
    tau.gls <- try(
      gls(y~x-1, correlation = corAR1(form=~1), weights=varExp()), silent=TRUE)
    
    if(is(tau.gls)!="try-error")
    {
      beta.hat <- tau.gls$coef
      beta.CI <- intervals(tau.gls)[[1]][c(1,3)]
    } else {
      tau.lm <- lm(log(vaf[vaf>0])~lag[vaf>0]-1)
      beta.hat <- tau.lm$coef
      beta.CI <- beta.hat + c(-1,1) *qt(0.975, df=length(vaf[vaf > 0])-1)*summary(tau.lm)$coef[1,2]
      warning("Generalized least squares fit failed - confidence intervals are probably suspect.")
    }}
  
  tau.hat <- as.numeric(-1/beta.hat)
  nu.hat <- mean(Mod(V))
  results <- data.frame(tau.hat = tau.hat, nu.hat = nu.hat)
  
  if(CI)
  {
    # nu estimates
    rho.hat <- cor(Mod(V[-1]), Mod(V[-length(V)]))
    nu.se <- sd(Mod(V))/sqrt(length(V))/(1-rho.hat)/2
    nu.CI <- nu.hat +  c(-1,1) * qnorm(0.975) * nu.se
    
    # tau estimates
    tau.CI <- -1/beta.CI
    
    # compile results
    results <- data.frame(t(results), rbind(tau.CI, nu.CI))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
  }
  
  if(diagnose){   
    plot(lag, vaf, bty="l", cex.lab=1.25, cex.sub=1.25, font.sub = 2, col=rgb(0,0,0,.5), pch=19)
    curve(nu.hat^2*4/pi * exp(-x/tau.hat), add=TRUE, col=2, lwd=3, lty=2)
    legend("topright",lty = c(NA,2), pch = c(1,NA), legend = c("observed", "fitted"), col=1:2, lwd=1:2, bty="n")
  }
  
  return(results)
}

estimateCVM.vLike <- function(Z, T, CI = FALSE, parameters = c("tau", "nu"), spline = FALSE, like=FALSE)
{   
  V <- diff(Z)/diff(T)
  T.mid <- (T[-1] + T[-length(T)])/2
  
  if(spline){
    VZT <- getV.spline(Z,T)
    V <- VZT$V
    T <- VZT$T
    Z <- VZT$Z
  }
  
  V.x1 <- Re(V)[-length(V)] 
  V.x2 <- Re(V)[-1]
  V.y1 <- Im(V)[-length(V)]
  V.y2 <- Im(V)[-1]
  
  dT <- diff(T.mid)
  
  f.V <- function(v1, v2, dt, tau, nu)
  {
    v.mean <-  v1 * exp(-dt/tau)
    v.var <- (2/pi) * nu^2 * (1 - exp(-2*dt/tau))
    sum(dnorm(x = v2, mean = v.mean, sd = sqrt(v.var), log=TRUE))
  }
  
  V.LogLikelihood.Tau <- function(p)
  {
    logtau <- p[1]
    L.x <- f.V(V.x1, V.x2, dt=dT, exp(logtau), nu.hat)
    L.y <- f.V(V.y1, V.y2, dt=dT, exp(logtau), nu.hat)
    return(-L.x-L.y)
  }
  
  V.LogLikelihood.NuTau <- function(p)
  {
    logtau <- p[1]
    nu <- p[2]
    L.x <- f.V(V.x1, V.x2, dt=dT, exp(logtau), nu)
    L.y <- f.V(V.y1, V.y2, dt=dT, exp(logtau), nu)
    return(-L.x-L.y)
  }
  
  nu.hat <- mean(Mod(V))
  
  if(!"nu" %in% parameters)
  {
    
    logtau.fit <- optimize(V.LogLikelihood.Tau, c(-1e10, log(diff(range(T))/2)))
    logtau.hat <- logtau.fit$min
    logtau.sd <- 1/sqrt(hessian(V.LogLikelihood.Tau, logtau.hat))
    tau.hat <- exp(logtau.hat)
    
    LL <- -logtau.fit$obj
    
    if(CI)
    {
      tau.CIs <- exp(logtau.hat + c(-1,1)*1.96*logtau.sd)
      
      # nu estimates
      rho.hat <- cor(Mod(V[-1]), Mod(V[-length(V)]))
      nu.se <- sd(Mod(V))/sqrt(length(V))/(1-rho.hat)/2
      nu.CI <- nu.hat +  c(-1,1) * qnorm(0.975) * nu.se
    }
  } else
  {
    logtau.null <- log(-1/log(max(cor(V.x1, V.x2), 1e-10))*mean(diff(T)))
    nutau.hat <- try(optim(c(logtau.null,nu.hat), V.LogLikelihood.NuTau, 
                           lower=c(-Inf,1e-10), upper=c(Inf, Inf), method="L-BFGS-B", 
                           hessian=TRUE)) 
    
    if(is(nutau.hat)[1] == "try-error") nutau.hat <- list(par = c(NA, NA), value=NA, hessian=matrix(NA, 2,2))
    nu.hat <- nutau.hat$par[2]
    logtau.hat <- nutau.hat$par[1]
    tau.hat <- exp(logtau.hat)
    Sigma <- solve(nutau.hat$hessian)
    
    nu.CI <- nu.hat + c(-1,1)*qnorm(0.975)*sqrt(Sigma[2,2])
    tau.CI <- exp(logtau.hat + c(-1,1)*qnorm(0.975)*sqrt(Sigma[1,1]))
    
    LL <- -nutau.hat$value
  }  
  
  results <- data.frame(tau.hat = tau.hat, nu.hat = nu.hat)
  if(CI) 
  {
    results <- data.frame(t(results), rbind(tau.CI, nu.CI))
    row.names(results) <- c("tau", "nu")
    names(results) <- c("Estimate", "C.I.low", "C.I.high")
  }
  if(like) return(list(results=results, LL = LL)) else return(results)
}


estimateCVM.zLike <- function(Z, T, CI = FALSE, tau.min=1e-10, tau.max = max(T)/2)
{   
  if(T[1] == 0) {T <- T[-1]; Z <- Z[-1]}
  
  logtau.hat <- 	optimize(Z.like2D, interval = c(log(tau.min), log(tau.max)), Z = Z, T = T)$minimum 
  logtau.sd <- 1/sqrt(hessian(Z.like2D, logtau.hat, Z=Z, T=T)) %>% as.vector
  
  tau.hat <- exp(logtau.hat)
  tau.CIs <- exp(logtau.hat + c(-1,1)*1.96*logtau.sd)
  
  M <- tau.hat * (1 - exp(-T/tau.hat))
  S <- getS.zz(T, tau.hat)
  S.inv <- solve(S)
  
  n <- length(Z)
  
  MSM <- as.numeric(t(M) %*% S.inv %*% M)
  X <- Re(Z)
  Y <- Im(Z)
  
  v0x.hat <- as.numeric((t(X) %*% S.inv %*% M)[1,1]/MSM)
  v0y.hat <- as.numeric((t(Y) %*% S.inv %*% M)[1,1]/MSM)
  
  nu2x.hat <- as.numeric(t(X - v0x.hat * M) %*% S.inv %*% (X - v0x.hat * M)/n)
  nu2y.hat <- as.numeric(t(Y - v0y.hat * M) %*% S.inv %*% (Y - v0y.hat * M)/n)
  nu2xy.hat <- (nu2x.hat + nu2y.hat)/2
  nu.hat <- sqrt( (pi / 2) * nu2xy.hat)
  nu.sd <- sqrt(pi*nu2xy.hat/(2*n))
  
  v0.sd <- sqrt(nu2xy.hat / MSM)
  
  hats <- c(tau.hat, nu.hat, v0x.hat, v0y.hat)
  sds <- c(NA, nu.sd, v0.sd, v0.sd)
  CIs <- cbind(hats - 1.96*sds, hats+1.96*sds)
  CIs[1,] <- tau.CIs
  
  results <- data.frame(hats, CIs)
  row.names(results) <- c("tau", "nu", "v0x", "v0y")
  names(results) <- c("Estimate", "C.I.low", "C.I.high")
  
  return(results)
}

Z.like2D <- function(logtau, Z, T){
  
  X <- Re(Z)
  Y <- Im(Z)
  n <- length(Z)
  tau <- exp(logtau)
  
  M <- tau * (1 - exp(-T/tau))
  S <- getS.zz(T, tau)
  S.inv <- Matrix::solve(S)
  MSM <- as.numeric(t(M) %*% S.inv %*% M)
  
  v0x.hat <- as.numeric(t(X) %*% S.inv %*% M)/MSM
  v0y.hat <- as.numeric(t(Y) %*% S.inv %*% M)/MSM
  
  DSD.x <- as.numeric((t(X- v0x.hat * M) %*% S.inv %*% (X-v0x.hat * M)))
  DSD.y <- as.numeric((t(Y- v0y.hat * M) %*% S.inv %*% (Y-v0y.hat * M)))
  
  # average of the two DSD estimates
  nu2.hat <- (DSD.x + DSD.y)/n/2
  
  # compute the likelihood (ignoring -2*pi, cancelling nu.hat with the DSD's, etc.)
  n*log(nu2.hat) + Matrix::determinant(S)$mod #+ (DSD.x + DSD.y)/nu2.hat/2   
}
