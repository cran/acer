#include <Rcpp.h>

/*

  Given a time series and a k value, this function estimates the corresponding ACER function at discrete levels. The discrete levels must be equidistant. The ACER function is estimated from the number of upcrossings at the discrete levels. If the k value is equal to 1, all upcrossings are counted. If the k value is greater than 1, the counted upcrossings are conditioned on k - 1 previous non-exceedances of the level in question.

  Literature:
  Naess, Arvid and Gaidai, Oleg: Prediction of Extreme Values by the Average Conditional Exceedance Rate Method. Working paper no. N2-2011. Department of Mathematical Sciences, NTNU.

*/

RcppExport SEXP estimate_acer (SEXP peaks_, SEXP eta_, SEXP k_value_)
{
  using namespace Rcpp;
  using namespace std;

  NumericVector peaks (peaks_); // The time series
  NumericVector eta (eta_); // Vector of discrete levels
  NumericVector k_value (k_value_); // k value

  NumericVector eta_crossings (peaks . size ());
  NumericVector numerator (eta . size ());
  NumericVector denominator (eta . size ());
  NumericVector epsilon (eta . size ());

  if (k_value [0] < 1)
    forward_exception_to_r (logic_error ("k less than 1"));

  double diff = eta [1] - eta [0]; // Distance between discrete levels. The levels are assumed to be equidistant.

  for (int i = 0; i < peaks . size (); i ++)
    {
      // Find the number of levels crossed by this peak
      eta_crossings [i] = (int) floor ((peaks [i] - eta [0]) / diff) + 1;
    }

  if (k_value [0] == 1) // count non-conditional upcrossings
    {
      // The upcrossing rate is calculated by dividing the number of upcrossings on the total number of peaks

      for (int i = 0; i < numerator . size (); i ++)
	{
	  numerator [i] = 0;
	  denominator [i] = peaks . size ();
	}
      
      for (int i = 0; i  < peaks . size (); i ++)
	{
	  // For each level crossed by this peak, add one upcrossing at that level
	  for (int j = 0; j < eta_crossings [i]; j ++)
	    numerator [j] = numerator [j] + 1;
	}
    }

  else // k > 1, count upcrossings conditioned on k - 1 nonexceedances
    {
      // The upcrossing rate is calculated by dividing the number of conditional upcrossings on the number of non-exceedances (that is, k - 1 sequential non-exceedances)
      for (int i = 0; i < numerator . size (); i ++)
	{
	  numerator [i] = 0;
	  denominator [i] = 0;
	}
      int kk = k_value [0] - 1;
      int counter_den;
      for (int i = kk; i < peaks . size (); i ++)
	{
	  counter_den = 0;
	  for (int j = i - kk; j < i; j ++)
	    {
	      // Find the index of the highest eta level crossed among the previous k - 1 peaks
	      if (eta_crossings [j] > counter_den)
		counter_den = eta_crossings [j];
	    }
	  for (int j = counter_den; j < eta . size (); j ++)
	    {
	      // Add one to the number of non-exceedances among the levels higher than the highest crossed among the previous k - 1 peaks
	      denominator [j] = denominator [j] + 1;
	    }
	  if (eta_crossings [i] - 1 - counter_den > 0)
	    {
	      // If the current peak is higher than the highest crossed level among the previous k - 1 peaks, add one to the number of conditional exceedances of the levels in between
	      for (int k = counter_den; k < eta_crossings [i]; k ++)
		numerator [k] = numerator [k] + 1;
	    }
	}
    }

  // The estimated ACER function
  for (int i = 0; i < eta . size (); i ++)
    {
      if (denominator [i] == 0)
	epsilon [i] = NA_REAL;
	//	forward_exception_to_r (logic_error ("No non-exceedances found at level. Trying to divide by 0"));
      else
	epsilon [i] = numerator [i] / denominator [i];
    }

  return (epsilon);

}
