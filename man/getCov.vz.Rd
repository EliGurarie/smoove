% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCov.vz.R
\name{getCov.vz}
\alias{getCov.vz}
\alias{getCov.zz}
\title{Var-Cov block of 1-D V and Z}
\usage{
getCov.vz(t1, t2, nu, tau, t0 = 0)
}
\arguments{
\item{t1}{first time}

\item{t2}{second time}

\item{nu}{cvm paramter}

\item{tau}{cmv parameter}

\item{t0}{initial t value}
}
\description{
Takes two times (ti, tj) and the CVM parameters, and returns either the 2x2 matrix of the (vi,zi)x(vj,zj) covariances - if \code{getCov.vz}, or only the zi-zj component, i.e. cov(z_i, z_j) - if \code{getCov.zz}.  This covariance does not depend at all on the v0 parameter.
}
\examples{
getCov.vz(t1=5,t2=5, nu=2, tau=10)
getCov.vz(t1=5,t2=10, nu=2, tau=10)
getCov.vz(t1=95,t2=100, 2,10)
}
