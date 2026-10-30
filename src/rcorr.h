#ifndef CFB_RCORR_H
#define CFB_RCORR_H

#include <Rcpp.h>

// Weighted concordance between predicted and observed benefits.
// Returns an Rcpp::List with elements "C Index" and "S.D.".
// The S.D. field preserves the existing rcorr implementation's scaling.
Rcpp::List rcorr(Rcpp::NumericVector tau_hat,
                 Rcpp::NumericVector tau_obs,
                 Rcpp::NumericVector sample_weight);

#endif  // CFB_RCORR_H
