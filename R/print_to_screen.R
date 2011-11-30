acer.print_to_screen <- function (result)
{
  par <- result$par
  return_periods <- result$return_periods
  return_levels <- result$return_levels
  conf_level <- result$conf_level
  ci_upper <- result$ci_upper
  ci_lower <- result$ci_lower
  k <- result$k

print (paste ("Result of nonlinear fit to the ACER function, k = ", k, sep = ""), quote = FALSE)
    print (sprintf ("Parameters: q = %.4f, b = %.4f, a = %.4f, c = %.4f", par$q, par$b, par$a, par$c), sep = "", quote = FALSE)
  for (i in 1:length (return_periods))
      {
        print (sprintf ("%d periods: %.2f; %d%% CI: (%.2f, %.2f)", return_periods [i], return_levels [i], 100 * conf_level, ci_lower [i], ci_upper [i]), quote = FALSE)
      }
}
