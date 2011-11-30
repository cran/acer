acer.analysis <- function (estimates, k_value, eta1, N = NULL, T = NULL, delta = 1, plot = TRUE, return_periods = NULL, print = TRUE)
  {
    if (! inherits (estimates, "acer.estimates"))
        stop ("not an ACER estimate")

    k <- which (estimates$k_values == k_value)
    if (length (k) != 1)
      stop ("k value not valid")

    eta <- estimates$eta
    epsilon <- estimates$epsilon [, k]
    confint <- estimates$confint [, k]
    peaks <- estimates$peaks
    eta_min <- estimates$eta_min
    peaks_mean <- estimates$peaks_mean
    conf_level <- estimates$conf_level

    # Remove the very tail and values where eta < eta1
    not_tail <- which (confint / epsilon < delta)
    select <- intersect (which (eta >= eta1 & epsilon > 0), not_tail)
    
    epsilon <- epsilon [select]
    eta <- eta [select]
    confint <- confint [select]

    ci_plus <- epsilon + confint
    ci_minus <- epsilon - confint

    # Calculate weights on the log scale
    weights <- rep (0, length (epsilon))
    weights [ci_minus > 0] <- 1 / (abs (log (ci_plus [ci_minus > 0]) - log (ci_minus [ci_minus > 0])))^2
    weights <- weights / sum (weights)

    # Perform linear fit on the loglog scale to get start values for the parameters
    startpar <- acer.linear_fit (eta, epsilon, peaks_mean)

    # Perform nonlinear fit on the log scale using start values from linear fit
    par <- acer.nonlinear_fit (startpar, eta, epsilon, weights, eta_min, eta1)
    ssq = fr (c (par$q, par$b, par$a, par$c), eta, epsilon, weights)

    # Find the number of events in one time period
    if (is.null (N))
      {
        if (! is.null (T))
            # We use the mean number of events per time unit to find an estimate
          N <- T * estimates$events_per_time_unit
        else
              # The average number of events is used as an estimate
          N <- length (na.omit (c (peaks))) / ncol (peaks)
      }
    
    if (is.null (return_periods))
        # Standard set of return periods
      return_periods <- c (5, 10, 20, 100, 200)
    
    # Find return levels and the corresponding values of epsilon
    retlev <- acer.return_level (par, N, return_periods)
    return_levels <- retlev$return_levels
    epsilon_return <- retlev$epsilon_return

    # Find confidence intervals by extrapolating the confidence intervals of epsilon, reanchored to the fitted curve
    confres <- acer.estimate_confidence_intervals (eta, par, confint, weights, eta_min, eta1, peaks_mean, N, return_periods)
    par_upper <- confres$par_upper
    par_lower <- confres$par_lower
    ci_upper <- confres$ci_upper
    ci_lower <- confres$ci_lower

    result <- list (par = par, k = k_value, eta1 = eta1, eta = eta, epsilon = epsilon, confint = confint, weights = weights, return_periods = return_periods, return_levels = return_levels, ci_upper = ci_upper, ci_lower = ci_lower, epsilon_return = epsilon_return, startpar = startpar, par_upper = par_upper, par_lower = par_lower, N = N, conf_level = conf_level, ssq = ssq)

    class (result) <- "acer.result"

    if (plot)
      acer.plot_result (result) # Plot results

    if (print)
      acer.print_to_screen (result) # Print results to screen
        
    return (result)
  }
