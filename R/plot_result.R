acer.plot_result <- function (result)
{
  if (!inherits (result, "acer.result"))
    stop ("input is not an ACER result")
  
  par <- result$par
  par_upper <- result$par_upper
  par_lower <- result$par_lower
  confint <- result$confint
  eta <- result$eta
  epsilon <- result$epsilon
  ci_upper <- result$ci_upper
  N <- result$N
  k <- result$k

  fit <- acer.evaluate (eta, par)
  confint_anch_upper <- fit + confint # Reanchored confidence intervals
  confint_anch_lower <- fit - confint

  tic_return_periods <- c (1, 2, 5, 10, 25, 50, 100, 200, 500, 1000)
  tic_retlev <- acer.return_level (par, N, tic_return_periods)
  tic_epsilon_return <- tic_retlev$epsilon_return

  extended_eta <- seq (min (eta), max (ci_upper) + (max (ci_upper) - min (eta)) / 20, length.out = 300)
  
  extended_fit <- acer.evaluate (extended_eta, par)

  fitted_confint_lower <- acer.evaluate (extended_eta, par_lower)
  fitted_confint_upper <- acer.evaluate (extended_eta, par_upper)
  
  par (mar = c (5, 4, 4, 4) + 0.1)
  plot (extended_eta [extended_fit > 0], extended_fit [extended_fit > 0], type = "l", xlab = expression (eta), ylab = expression (hat (epsilon) [k]), main = paste ("Nonlinear fit to the ACER function, k = ", k, sep =""), col = "blue", log = "y")
  axis (4, at = tic_epsilon_return, labels = tic_return_periods)
  mtext ("Return periods", side = 4, line = 2)
  points (eta, epsilon, cex = 0.5)

  lines (eta, confint_anch_upper, lty = 2)
  lines (eta, confint_anch_lower, lty = 2)
  lines (extended_eta [fitted_confint_upper > 0], fitted_confint_upper [fitted_confint_upper > 0], lty = 2, col = "red")
  lines (extended_eta [fitted_confint_lower > 0], fitted_confint_lower [fitted_confint_lower > 0], lty = 2, col = "red")
}
