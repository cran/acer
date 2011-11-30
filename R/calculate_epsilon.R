acer.calculate_epsilon <- function (x, k_value, eta, weighted_mean, conf_level)
{
  x <- as.matrix (x)
  R <- ncol (x)
        
  epsilon_estimates <- matrix (nrow = length (eta), ncol = R)
  totallength <- length (na.omit (c (x)))
  
  # estimate the ACER function for each realization
  epsilon_estimates <- apply (x, 2, acer.estimate_acer, eta, k_value)
  
  # estimate the mean and confidence intervals of the ACER functions
  if (R > 1) # multi-realization
    {
      if (weighted_mean)
        {
          weights <- (nrow (x) - colSums (apply (x, 2, is.na))) / totallength
          epsilon_mean <- apply (epsilon_estimates, 1, weighted.mean, weights)
          epsilon_std <- sqrt (colSums (weights * t ((epsilon_estimates - epsilon_mean)^2)) / (1 - sum (weights^2)))
        }
      else
        {
          epsilon_mean <- rowMeans (epsilon_estimates)
          epsilon_std <- sqrt (apply (epsilon_estimates, 1, var))
        }
      conf_coef <- qt ((1 + conf_level) / 2, df = R - 1)
      confint <- conf_coef * epsilon_std / sqrt (R)
    }
  else # single realization
    {
      epsilon_mean = epsilon_estimates
      conf_coef <- qnorm ((1 + conf_level) / 2)
      confint <- conf_coef * sqrt (epsilon_mean) / sqrt (totallength - k_value + 1)
    }
   
  return (list (epsilon = epsilon_mean, confint = confint, epsilon_estimates = epsilon_estimates))
}
