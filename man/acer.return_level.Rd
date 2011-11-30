\name{acer.return_level}
\alias{acer.return_level}
\title{Calculate the return level corresponding to a return period
  (internal)}
\description{This function calculates the return level(s) corresponding to
  given return period(s), using the parameters of the fitted ACER function.}
\usage{acer.return_level (par, N, return_periods)}
\arguments{
  \item{par}{a list containing the parameters of the fitted ACER
    function.}
  \item{N}{the number of events per time period.}
  \item{return_periods}{the return period(s) for which the
    return_level(s) are to be estimated.}
}
\value{
  A list containing the following items:
  \item{return_levels}{the estimated return level(s).}
  \item{epsilon_return}{the value(s) of the ACER function at the given
    return level(s).}
  }