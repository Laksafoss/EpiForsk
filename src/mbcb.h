#ifndef CFB_MBCB_H
#define CFB_MBCB_H

#include <Rcpp.h>

// Weighted model-based concordance for binary potential outcomes.
// Returns an Rcpp::List with elements "C Index" and "S.D.".
// The S.D. field preserves the existing mbcb implementation's scaling.
Rcpp::List mbcb(Rcpp::NumericVector p_0,
                Rcpp::NumericVector p_1,
                Rcpp::NumericVector tau_hat,
                Rcpp::NumericVector sample_weight);

#endif  // CFB_MBCB_H
