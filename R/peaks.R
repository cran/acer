acer.peaks <- function (x)
  {
    # Find all peaks in a time series. NA values are ignored.
    N <- length (x)
    peaks <- matrix (nrow = N)
    res <- .Call ("peaks", as.numeric (na.omit (x)), PACKAGE = "acer")

    if (length (res) == 0)
      stop ("no peaks found")
    else
      peaks [1: length (res)] <- res

    return (peaks)
  }
