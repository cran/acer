#include <Rcpp.h>

/*
  This function finds the number of peaks in a time series.
*/

RcppExport SEXP peaks (SEXP xin) {
  using namespace Rcpp;
  using namespace std;
  
  NumericVector x (xin); // time series

  NumericVector peaks;
  
  for (int j = 1; j < x . size () - 1; j ++)
    {
      if (x [j - 1] <= x [j] && x [j] > x [j + 1])
	peaks . push_back (x [j]);
    }

  return peaks;
}
