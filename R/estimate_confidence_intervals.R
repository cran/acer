acer.estimate_confidence_intervals <- function (eta, par, confint, weights, eta_min, eta1, peaks_mean, N, return_periods)
{
  
  fit <- acer.evaluate (eta, par)

  # Find upper confidence limits
  confint_anch_upper <- fit + confint
  startpar_upper <- acer.linear_fit (eta, confint_anch_upper, peaks_mean)
  par_upper <- acer.nonlinear_fit (startpar_upper, eta, confint_anch_upper, weights, eta_min, eta1)
  
  retlev <- acer.return_level (par_upper, N, return_periods)
  ci_upper <- retlev$return_levels

  # Find lower confidence limits

  confint_anch_lower <- fit - confint
  startpar_lower <- acer.linear_fit (eta, confint_anch_lower, peaks_mean)
  par_lower <- acer.nonlinear_fit (startpar_lower, eta, confint_anch_lower, weights, eta_min, eta1)
  retlev <- acer.return_level (par_lower, N, return_periods)
  ci_lower <- retlev$return_levels

  return (list (ci_upper = ci_upper, ci_lower = ci_lower, par_upper = par_upper, par_lower = par_lower))
}      
