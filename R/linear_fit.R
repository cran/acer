acer.linear_fit <- function (eta, epsilon, peaks_mean)
  {
    eta1 <- min (eta)
    b <- peaks_mean
    if (b > eta1)
      {
        b <- eta1
      }
        
    c <- 2
    Y <- log (epsilon [epsilon > 0])
    X <- (eta [epsilon > 0] - b)^c

    fit <- lm (Y ~ X)
    q <- as.numeric (exp (fit$coefficients [1]))
    a <- -as.numeric (fit$coefficients [2])

    par <- list (q = q, b = b, a = a, c = c)
    
    return (par)
  }
