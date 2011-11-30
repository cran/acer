acer.prepare <- function (x, eta.length = 300, find_peaks = FALSE, plot = TRUE, weighted_mean = FALSE, conf_level = 0.95, k_values= NULL)
  {
    if (conf_level <= 0 || conf_level >= 1)
      stop ("Parameter conf_level not in range (0, 1)")
    if (find_peaks == TRUE)
        # extract peaks from the data set and perform the analysis on those data points only
      peaks <- apply (as.matrix (x), 2, acer.peaks)

    else
       # perform analysis on all data points
      peaks <- x
    length_raw <- sum (! is.na (c (x))) # lengths of the raw series
    length_peaks <- sum (! is.na (c (peaks)))
    events_per_time_unit <- length_peaks / length_raw
       
    eta_min <- min (c (peaks), na.rm = TRUE) # find the smallest value among the data to be used in the analysis
    peaks_mean <- mean (peaks, na.rm = TRUE) # mean value among the peaks; to be used later in the analysis

    if (is.null (k_values))
      k_values <- seq (1, 6)
    else if (any (k_values < 0))
      stop ("k values must be non-negative")

    epsilon <- matrix (nrow = eta.length, ncol = length (k_values))
    confint <- matrix (nrow = eta.length, ncol = length (k_values))
    epsilon_estimates <- list ()

    eta <- seq (eta_min, max (peaks, na.rm = TRUE), length.out = eta.length)
    
    for (i in 1:length (k_values))
      {
        res <- acer.calculate_epsilon (peaks, k_values [i], eta, weighted_mean, conf_level)
        epsilon [, i] <- res$epsilon
        confint [, i] <- res$confint
        epsilon_estimates [[i]] <- res$epsilon_estimate
      }

    if (plot) # plot estimates
      {
        acer.plot_all_estimates (eta, epsilon, k_values)
      }

    estimates <- list (peaks = peaks, eta = eta, k_values= k_values, epsilon = epsilon, confint = confint, eta_min = eta_min, peaks_mean = peaks_mean, epsilon_estimates = epsilon_estimates, conf_level = conf_level, events_per_time_unit = events_per_time_unit)

    class (estimates) <- "acer.estimates"
    
    return (estimates)
  }
