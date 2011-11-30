\name{acer.prepare}
\alias{acer.prepare}
\title{Estimation of the ACER functions}
\description{Estimate the ACER functions for different \eqn{k} values}
\usage{
  acer.prepare (x, eta.length = 300, find_peaks = FALSE, plot
= TRUE, weighted_mean = FALSE, conf_level = 0.95, k_values = NULL)
}
\arguments{
  \item{x}{A matrix containing the data to be analyzed, each column containg data from one period.}
  \item{eta.length}{The number of barriers to be used in estimating the ACER function. Default 300.}
  \item{find_peaks}{TRUE if the analysis is to be performed on extracted peaks only, else FALSE. Default FALSE.}
  \item{plot}{TRUE if a plot of the estimated ACER functions is to be generated, else FALSE. Default TRUE.}
  \item{weighted_mean}{TRUE if the ACER function is to be estimated using a weighted mean and standard deviation, else FALSE. Default FALSE.}
  \item{conf_level}{Confidence level expressed as a number between 0 and
    1. Default 0.95.}
  \item{k_values}{A vector containing the values of \eqn{k} for which the ACER functions should be estimated.}
}
\value{
  An object containing the following components:
  \item{peaks}{if peaks=TRUE, A vector containing the peaks extracted
    from the data, else the data set itself.}
  \item{peaks}{The indices of the peaks used in the analysis}
  \item{indices}{If find_peaks = TRUE, an array containing the indices
    of the extracted peaks from the matrix x. Otherwise empty.}
  \item{eta}{a vector containing the barrier levels.}
  \item{k}{A vector containg the k values.}
  \item{epsilon}{a matrix containing the estimated ACER functions, with
    each column containing the ACER function esimated from the
    corresponding value of k.}
  \item{conf_int}{confidence intervals for the estimated ACER function.}
  \item{eta_min}{the minimum value among the peaks.}
  \item{epsilon_estimates}{a matrix containing ACER estimates for each value of k.}
  \item{conf_level}{The confidence level expressed as a number between 0
    and 1.}
  \item{events_per_time_unit}{The mean number of events per time unit.}
}
