\name{fr}
\alias{fr}
\title{Function to be minimized be nonlinear optimization (internal)}
\description{This function returns the sum of weighted squares of a fit
  to the estimated ACER function.}
\usage{fr (parvec, eta, epsilon, weights)}
\arguments{
  \item{parvec}{the estimated parameters.}
  \item{eta}{barrier levels.}
  \item{epsilon}{the estimated ACER function.}
  \item{weights}{weights used in the nonlinear fit.}
}
  \value{weighted sum of squares.}