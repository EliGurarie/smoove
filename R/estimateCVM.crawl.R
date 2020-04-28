estimateCVM.crawl <- function(Z, T, p0 = NULL, verbose = TRUE, CI = NULL, ...) {
  x <- Re(Z)
  y <- Im(Z)
  data <- data.frame(x, y, T)

  initial.state <- list(
    a1.x = c(x[1], 0),
    a1.y = c(y[1], 0),
    P1.x = diag(c(1, 1)),
    P1.y = diag(c(1, 1))
  )

  initial.state <- list(
    a = c(x[1], 0, y[1], 0),
    P = diag(rep(1, 4))
  )

  Fit.crawl <- crwMLE(
    mov.model = ~ 1,
    data = data, coord = c("x", "y"),
    Time.name = "T",
    # theta = ,# initial parameters,
    initial.state = initial.state,
    fixPar = c(NA, NA), ...
  )

  Get.nutau <- function(fit) {
    sigma.hat <- exp(fit$par[1])
    sigma.CI <- exp(fit$ci[1, ])

    tau.hat <- 1 / exp(fit$par[2])
    tau.CI <- sort(1 / exp(fit$ci[2, ]))

    nu.hat <- sqrt(pi / tau.hat) * sigma.hat / 2
    nu.CI <- sqrt(pi / tau.hat) * sigma.CI / 2

    results <- data.frame(Estimate = c(tau.hat, nu.hat), rbind(tau.CI, nu.CI))
    names(results) <- c("Estimate", "L", "U")
    return(results)
  }

  nutau <- Get.nutau(Fit.crawl)
  row.names(nutau) <- c("tau", "nu")

  if (verbose) print(Fit.crawl)

  return(nutau)
}
