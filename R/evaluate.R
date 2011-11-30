acer.log_evaluate <- function (eta, par)
  {
    q <- par$q
    b <- par$b
    a <- par$a
    c <- par$c
    return (log (q) - a * (eta - b)^c)
  }

acer.evaluate <- function (eta, par)
  {
    q <- par$q
    b <- par$b
    a <- par$a
    c <- par$c
    return (q * exp (- a * (eta - b)^c))
  }
