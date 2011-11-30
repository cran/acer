acer.return_level <- function (par, N, return_periods)
{
  q <- par$q
  b <- par$b
  a <- par$a
  c <- par$c
  
  epsilon_return <- - log (1 - 1 / return_periods) / N
  return_levels <- b + ((log (q * N) - log (- log (1 - 1 / return_periods))) / a)^(1 / c)
  
  return (list (return_levels = return_levels, epsilon_return = epsilon_return))
}
