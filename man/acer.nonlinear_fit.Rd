\name{acer.nonlinear_fit}
\alias{acer.nonlinear_fit}
\title{Perform a nonlinear fit to the estimated ACER function (internal)}
\description{This function performs a nonlinear fit to the estimated
  ACER function.}
\usage{acer.nonlinear_fit (startpar, eta, epsilon, weights,
eta_min, eta1)}
\arguments{
  \item{startpar}{start values of the curve parameters, passed from the
    linear fit to the ACER function.}
  \item{eta}{barrier levels.}
  \item{epsilon}{the estimated ACER function.}
  \item{weights}{weights to be used in the nonlinear fit.}
  \item{eta_min}{The smallest eta value.}
  \item{eta1}{The tail marker.}
  }
  \value{the estimated parameters.}