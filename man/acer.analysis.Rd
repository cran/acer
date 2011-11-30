\name{acer.analysis}
\alias{acer.analysis}
\title{ACER analysis}
\description{Perform analysis on the estimate of a given ACER function}
\usage{
acer.analysis (estimates, k_value, eta1, N = NULL, T = NULL, delta = 1,
plot = TRUE, return_periods = NULL, print = TRUE)
}
\arguments{
  \item{estimates}{an object of the class "acer.estimates" containing estimates of the ACER functions, returned from acer.estimate_functions.}
  \item{k_value}{the \eqn{k} value of the ACER function to be used in the analysis.}
  \item{eta1}{the tail marker of the ACER function, a value of \eqn{\eta} above which the ACER function \eqn{\epsilon_k} behaves regularly.}
  \item{delta}{the value of \eqn{\delta}, which decides how many uncertain data points should be left out of the analysis}
  \item{N}{the number of events in a time period, if known.}
  \item{T}{the length of a time period, in time units, to be used in
    calculating the number of events in a time period.}
  \item{plot}{if TRUE, a plot of the results will be shown.}
  \item{return_periods}{a vector containing return periods for which the
    return levels are to be calculated. If not given, a standard set of
    return periods is chosen.}
  \item{print}{if TRUE, the results are printed to the screen.}
}
\value{
  A list of the class "acer.result" containing the following objects:
  \item{par}{a list containing the estimates of the
    parameters \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the nonlinear fit to the ACER function \eqn{\epsilon}_k.}
  \item{epsilon}{the values of the ACER function \eqn{\epsilon_k} used
    in the nonlinear fit}.
  \item{eta}{the corresponding values of \eqn{\eta}.}
  \item{confint}{the corresponding confidence intervals}.
  \item{weights}{the corresponding weights produced from the confidence
    intervals of \eqn{\epsilon_k}.}
  \item{return_periods}{time periods for which return levels were
    estimated.}
  \item{epsilon_return}{the values of \eqn{epsilon_k} corresponding to
    the return periods.}
  \item{return_periods}{the estimated return levels}.
  \item{ci_upper}{the upper confidence limits corresponding to the
    estimated return levels.}
  \item{ci_lower}{the lower confidence limits corresponding to the
    estimated return levels.}
  \item{startpar}{a list containing the estimates of the parameters
    \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the linear fit tot the ACER
    function.}
  \item{par_upper}{a list containing the estimates of the parameters
    \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the nonlinear fit to the
    upper confidence limits.}
  \item{par_upper}{a list containing the estimates of the parameters
    \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the nonlinear fit to the
    lower confidence limits.}
  \item{k}{the k value used in the analysis.}
  \item{eta1}{The tail marker used in the analysis.}
  \item{N}{the number of events per period.}
  \item{par_upper}{a list containing the estimates of the
    parameters \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the nonlinear
    fit to the upper confidence interval of \eqn{\epsilon_k}.}
  \item{par_lower}{a list containing the estimates of the
    parameters \eqn{q},\eqn{b}, \eqn{a}, \eqn{c} from the nonlinear
    fit to the lower confidence interval of \eqn{\epsilon_k}.}
  \item{conf_level}{the level of the confidence limits.}
}
\references{
  Naess, A. and Gaidai, O.: Estimation of extreme values
    from sampled time series, in: Structural Safety 31 (2009),
    p. 325-334
    }