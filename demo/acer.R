# Generate simulated data from an extreme value distribution (see Naess and Gaidai: Estimation of extreme values from sampled time series, in Structural Safety 31 (2009), p. 329)

# Draw 100 realizations each year in 20 years
N <- 100
M <- 20
q <- 10
testdata <- matrix (nrow = N, ncol = M)

# Generate data from the distribution
# F (eta) = exp { - q * exp (- eta^2 / 2)}

for (i in 1:M)
  {
    u <- runif (N)
    testdata [, i] <- sqrt (abs (2 * log (- log (u) / q)))
  }

# Estimate ACER functions
estimates <- acer.prepare (testdata)

k <- 1

# Plot the ACER function, k = 1
acer.plot_estimate (estimates, k)

# Choose a tail marker
eta1 <- 2.5

# Perform the analysis
result <- acer.analysis (estimates, k, eta1)
