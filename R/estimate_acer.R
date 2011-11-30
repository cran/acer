acer.estimate_acer <- function (x, eta, k)
  {
    x <- na.omit (x)

    epsilon <- .Call ("estimate_acer", x, eta, k)
    return (epsilon)
  }
