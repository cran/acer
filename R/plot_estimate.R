acer.plot_estimate <- function (estimates, k_value)
  {
    if (!inherits (estimates, "acer.estimates"))
      {
        stop ("not an ACER estimate")
      }

    k <- which (estimates$k_values == k_value)
    if (length (k) != 1)
      stop ("k value not valid")

    epsilon <- estimates$epsilon [, k]
    confint <- estimates$confint [, k]
    select <- which (epsilon > 0)
    eta <- estimates$eta [select]
    epsilon <- epsilon [select]
    confint <- confint [select]
    
    ci_plus <- epsilon + confint
    ci_minus <- epsilon - confint

    plot (eta [epsilon > 0], epsilon [epsilon > 0], log = 'y', xlab = expression (eta), ylab = expression (hat (epsilon) [k]), main = paste ("Estimated ACER function, k = ", k, sep = ""), cex = 0.5)

    lines (eta [ci_plus > 0], ci_plus [ci_plus > 0], lty = 2)
    lines (eta [ci_minus > 0], ci_minus [ci_minus > 0], lty = 2)
  }
