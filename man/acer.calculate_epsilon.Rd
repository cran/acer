\name{acer.calculate_epsilon}
\alias{acer.calculate_epsilon}
\title{Estimation of the ACER function for a given value of k}
\description{Estimate the ACER function for a given value of k.}
\usage{acer.calculate_epsilon (x, k_value, eta, weighted_mean,
conf_level)}
\arguments{
  \item{x}{a matrix, with each column containing one period of data.}
  \item{k_value}{the value of \eqn{k} for which the ACER function is to be
    estimated.}
  \item{eta}{a vector of barrier levels on which to estimate the ACER
    function.}
  \item{weighted_mean}{TRUE if the ACER function is to be estimated by
    using a weighted mean, else FALSE.}
  \item{conf_level}{confidence level as a number between 0 and 1.}
}
\value{
  A list containing the following items:
  \item{epsilon}{the estimate of the ACER function.}
  \item{confint}{confidence intervals of epsilon_mean.}
  \item{epsilon_estimates}{a matrix in which each columns contains the
    estimate of the ACER from the corresponding period.}
  }