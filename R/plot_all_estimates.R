acer.plot_all_estimates <- function (eta, epsilon, k_values)
  {
    colors <- seq (1, ncol (epsilon))
    select = which (epsilon [,1] > 0)
    plot (eta [select], epsilon [select, 1], type = 'l', xlab = expression (eta), ylab = expression (hat (epsilon) [k]), main = 'Estimates of the ACER functions', log = 'y', col = colors [1])
    legendstring <- paste ("k = ", k_values [1], sep = "")

    if (ncol (epsilon) > 1)
      {
        for (k in 2 : ncol (epsilon))
          {
            select = which (epsilon [,k] > 0)
            legendstring [k] <- paste ("k = ", k_values [k], sep = "")
            lines (eta [select], epsilon [select, k], col = colors [k])
          }
       } 
    legend ("topright", legendstring, col = colors, lty = 1)
    
  }
