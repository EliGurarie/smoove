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
