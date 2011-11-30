\name{acer.estimate_confidence_intervals}
\alias{acer.estimate_confidence_intervals}
\title{Estimate confidence intervals (internal)}
\description{Performs nonlinear fits on the confidence bounds of the
  ACER function}
\usage{acer.estimate_confidence_intervals (eta, par, confint, weights, eta_min, eta1,
peaks_mean, N, return_periods)}
\arguments{
  \item{eta}{a vector containing barrier levels.}
  \item{par}{a list containing the parameters of the fitted ACER
    function.}
  \item{confint}{a vector containing the estimated confidence width of
    the ACER function}
  \item{weights}{a vector containing weights of the to be used in the
    nonlinear fit.}
  \item{eta_min}{The smallest eta value.}
  \item{eta1}{The tail marker.}
  \item{peaks_mean}{the mean of the data used in the analysis.}
  \item{N}{the number of events per time period.}
  \item{return_periods}{the return periods for which return levels have
    been estimated.}
}
\value{
  A list containing the following items:
  \item{ci_upper}{upper confidence bounds for the estimated return
    levels.}
  \item{ci_lower}{lower confidence bounds for the estimated return
    levels.}
  \item{par_upper}{a list containing the parameters of the nonlinear fit
    to the upper confidence bounds of the ACER function.}
  \item{par_upper}{a list containing the parameters of the nonlinear fit
    to the lower confidence bounds of the ACER function.}
  }