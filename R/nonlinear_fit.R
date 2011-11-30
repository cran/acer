fr = function (parvec, eta, epsilon, weights)
  {
    par = list (q = parvec [1], b = parvec [2], a = parvec [3], c = parvec [4])
      
    return (sum (weights [epsilon > 0] * (log (epsilon [epsilon > 0]) - acer.log_evaluate (eta [epsilon > 0], par))^2))
  }

acer.nonlinear_fit = function (startpar, eta, epsilon, weights, eta_min, eta1)
  {
    spar = c (startpar$q, startpar$b, startpar$a, startpar$c)
    lower = c (0, eta_min, 0, 0)
    upper = c (Inf, eta1, Inf, 5)
    res = nlminb (spar, fr, lower = lower, upper = upper, eta = eta, epsilon = epsilon, weights = weights)

    par = list (q = res$par [1], b = res$par [2], a = res$par [3], c = res$par [4])
    
    return (par)
  }
