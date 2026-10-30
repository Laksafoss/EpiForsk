// Model-based C-for-benefit, O(n log n) time and O(n) memory.
// Model: conditional independence of Y(0), Y(1) given X, with binary outcomes.
// Benefit D = Y(1) - Y(0): -1, 0, +1.
// q_minus = P(D=-1), q_zero = P(D=0), q_plus = P(D=+1).
#include "mbcb.h"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <vector>
using namespace Rcpp;

// Create structure to hold each probability of the three possible treatment effect values:
// D_i = Y_i(1) - Y_i(0) where D_i = -1, 0, or 1
namespace {
struct Mass {
  double minus = 0.0, zero = 0.0, plus = 0.0;
  Mass& operator+=(const Mass& b) {
    minus += b.minus; zero += b.zero; plus += b.plus; return *this;
  }
};
// Create function to subtract probabilities
inline Mass difference(const Mass& a, const Mass& b) {
  return {a.minus-b.minus, a.zero-b.zero, a.plus-b.plus};
}
// create function to calculate the probability mass corresponding to D_i < D_j
inline double less_prob(const Mass& a, const Mass& b) {
  return a.minus * (b.zero + b.plus) + a.zero * b.plus;
}
// create function to calculate the probability mass corresponding to D_i > D_j
inline double greater_prob(const Mass& a, const Mass& b) {
  return a.plus * (b.minus + b.zero) + a.zero * b.minus;
}
}

// [[Rcpp::export]]
Rcpp::List mbcb(Rcpp::NumericVector p_0,
                Rcpp::NumericVector p_1,
                Rcpp::NumericVector tau_hat,
                Rcpp::NumericVector sample_weight) {
  // number of observations
  const R_xlen_t n = tau_hat.size();

  // object to hold treatment effect probabilities
  std::vector<Mass> weighted_benefit_prob(n);
  Mass total;

  // indices of ordered treatment effect estimates
  // ordering is O(n*log(n)) complexity and will allow us to aggregate the probability distributions of
  // the three possible comparisons (D_i < D_j, D_i = D_j, D_i > D_j) with O(n) complexity
  std::vector<R_xlen_t> order(n);
  std::iota(order.begin(), order.end(), R_xlen_t(0));
  std::sort(order.begin(), order.end(), [&](R_xlen_t a, R_xlen_t b) {
    return tau_hat[a] < tau_hat[b];
  });

  // check objects
  if (n < 2) Rcpp::stop("At least two observations are required.");
  if (p_0.size() != n || p_1.size() != n || sample_weight.size() != n)
    Rcpp::stop("p_0, p_1, tau_hat, and sample_weight must have equal lengths.");
  // counter of positive weights (used for check)
  int positive_weights = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!R_finite(p_0[i]) || !R_finite(p_1[i]) || !R_finite(tau_hat[i]) || !R_finite(sample_weight[i])) {
      Rcpp::stop("Inputs must contain only finite values.");
    }
    if (p_0[i] < 0 || p_0[i] > 1 || p_1[i] < 0 || p_1[i] > 1) {
      Rcpp::warning("p_0 and p_1 must be probabilities in [0, 1].");
    }
    if (sample_weight[i] < 0) {
      Rcpp::stop("sample_weight must be nonnegative.");
    }
    if (sample_weight[i] > 0) ++positive_weights;
    const double p0i = p_0[i], p1i = p_1[i], weight = sample_weight[i];
    // compute weighted probability of each possible treatment effect
    weighted_benefit_prob[i] = {
      weight * p0i * (1 - p1i), // weighted probability of D_i = -1
      weight * ((1 - p0i) * (1 - p1i) + p0i * p1i), // weighted probability of D_i = 0
      weight * (1 - p0i) * p1i // weighted probability of D_i = 1
      };
    // keep track of total weight for each possible treatment effect
    total += weighted_benefit_prob[i];
  }
  if (positive_weights < 2) {
    Rcpp::stop("At least two observations must have positive weights.");
  }

  // aggregate probability distributions of the three possible comparisons and cumulatively add contributions
  // to the numerator and denominator of the C-for-benefit as well as r and w used to calculate standard error.
  double snum = 0.0, sden = 0.0;
  std::vector<double> r(n), w(n);
  Mass lower;
  R_xlen_t start = 0;
  R_xlen_t next_interrupt_check = 4096;
  while (start < n) {
    R_xlen_t end = start + 1;
    while (end < n && tau_hat[order[end]] == tau_hat[order[start]]) ++end;

    Mass tied;
    for (R_xlen_t k = start; k < end; ++k) tied += weighted_benefit_prob[order[k]];
    const Mass upper = difference(difference(total, lower), tied);

    for (R_xlen_t k = start; k < end; ++k) {
      const R_xlen_t i = order[k];
      const Mass own = weighted_benefit_prob[i];
      const Mass other = difference(total, own);
      const Mass tied_other = difference(tied, own);

      // These are weighted by observation i already, since own contains its weight.
      const double p_less_all = less_prob(own, other);
      const double p_greater_all = greater_prob(own, other);
      const double p_less_upper = less_prob(own, upper);
      const double p_less_tied = less_prob(own, tied_other);
      const double p_greater_upper = greater_prob(own, upper);
      const double p_less_lower = less_prob(own, lower);
      const double p_greater_lower = greater_prob(own, lower);

      snum += p_less_upper + 0.5 * p_less_tied;
      sden += p_less_all;
      r[i] = p_less_all + p_greater_all;
      w[i] = (p_less_upper - p_greater_upper) +
        (p_greater_lower - p_less_lower);
    }
    lower += tied;
    start = end;

    // check for user interrupt
    if (start >= next_interrupt_check) {
      Rcpp::checkUserInterrupt();
      next_interrupt_check = start + 4096;
    }
  }

  // early return if denominator is not a positive real number
  if (!R_finite(sden) || sden <= 0.0) {
    return Rcpp::List::create(Rcpp::_["C Index"] = NA_REAL, Rcpp::_["S.D."] = NA_REAL);
  }

  // compute final C-for-benefit
  const double cindex = snum / sden;

  // Compute standard errors
  const double sumr = std::accumulate(r.begin(), r.end(), 0.0);
  const double sumw = std::accumulate(w.begin(), w.end(), 0.0);
  double sumsq = 0.0;
  for (R_xlen_t i = 0; i < n; ++i) {
    const double influence = w[i] * sumr - r[i] * sumw;
    sumsq += influence * influence;
  }
  const double sd = (sumr > 0.0) ? std::sqrt(sumsq) / (sumr * sumr) : NA_REAL;

  // Return a list with the C-for-benefit and the associated standard error
  return Rcpp::List::create(Rcpp::_["C Index"] = cindex, Rcpp::_["S.D."] = sd);
}
