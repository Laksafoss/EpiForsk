#include "rcorr.h"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <vector>
using namespace Rcpp;

// Fenwick tree for prefix sums of nonnegative observation weights.
class Fenwick {
public:
  explicit Fenwick(std::size_t n) : bit_(n + 1, 0.0) {}
  void add(std::size_t index, double value) {
    for (++index; index < bit_.size(); index += index & -index) bit_[index] += value;
  }
  double prefix(std::size_t count) const { // sum of indices [0, count)
    double result = 0.0;
    for (; count > 0; count -= count & -count) result += bit_[count];
    return result;
  }
private:
  std::vector<double> bit_;
};

// Weighted concordance for observed and predicted treatment benefits.
// O(n log n) time and O(n) memory, including exact handling of ties.
// [[Rcpp::export]]
Rcpp::List rcorr(Rcpp::NumericVector tau_hat,
                 Rcpp::NumericVector tau_obs,
                 Rcpp::NumericVector sample_weight) {
  const std::size_t n = tau_hat.size();
  if (tau_obs.size() != static_cast<R_xlen_t>(n) || sample_weight.size() != static_cast<R_xlen_t>(n)) {
    Rcpp::stop("tau_hat, tau_obs, and sample_weight must have equal lengths.");
  }
  if (n < 2) Rcpp::stop("At least two observations are required.");

  double total_weight = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    if (!R_finite(tau_hat[i]) || !R_finite(tau_obs[i]) || !R_finite(sample_weight[i])) {
      Rcpp::stop("All predictions, observed benefits, and weights must be finite.");
    }
    if (sample_weight[i] < 0.0) Rcpp::stop("Weights must be nonnegative.");
    total_weight += sample_weight[i];
  }
  if (!R_finite(total_weight)) Rcpp::stop("Sum of weights is not finite.");

  // Coordinate-compress observed benefit values; exact equality defines ties.
  std::vector<double> obs_values(tau_obs.begin(), tau_obs.end());
  std::sort(obs_values.begin(), obs_values.end());
  obs_values.erase(std::unique(obs_values.begin(), obs_values.end()), obs_values.end());
  const std::size_t m = obs_values.size();
  std::vector<std::size_t> rank(n);
  std::vector<double> weight_at_rank(m, 0.0);
  for (std::size_t i = 0; i < n; ++i) {
    rank[i] = std::lower_bound(obs_values.begin(), obs_values.end(), tau_obs[i]) - obs_values.begin();
    weight_at_rank[rank[i]] += sample_weight[i];
  }

  // For each observation, r_i is its weighted number of comparable observations (different observed benefit),
  // times its own weight.
  std::vector<double> r(n), w(n, 0.0);
  double R = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    r[i] = sample_weight[i] * (total_weight - weight_at_rank[rank[i]]);
    R += r[i];
  }
  if (!(R > 0.0) || !R_finite(R)) {
    return Rcpp::List::create(_["C Index"] = NA_REAL, _["S.D."] = NA_REAL);
  }

  // Sort indices by predicted benefit. Equal predictions are processed
  // simultaneously, so they contribute zero to w_i (half concordance credit).
  std::vector<std::size_t> order(n);
  std::iota(order.begin(), order.end(), 0);
  std::sort(order.begin(), order.end(), [&](std::size_t a, std::size_t b) {
    return tau_hat[a] < tau_hat[b];
  });

  // Forward sweep: contributions from strictly smaller predicted benefits.
  Fenwick below(m);
  double processed = 0.0;
  std::size_t start = 0;
  while (start < n) {
    std::size_t end = start + 1;
    while (end < n && tau_hat[order[end]] == tau_hat[order[start]]) ++end;
    for (std::size_t k = start; k < end; ++k) {
      const std::size_t i = order[k];
      const double less_obs = below.prefix(rank[i]);
      const double at_most_obs = below.prefix(rank[i] + 1);
      const double greater_obs = processed - at_most_obs;
      w[i] += sample_weight[i] * (less_obs - greater_obs);
    }
    for (std::size_t k = start; k < end; ++k) {
      const std::size_t i = order[k];
      below.add(rank[i], sample_weight[i]);
      processed += sample_weight[i];
    }
    start = end;
    if ((start & 16383U) == 0U) Rcpp::checkUserInterrupt();
  }

  // Reverse sweep: contributions from strictly larger predicted benefits.
  Fenwick above(m);
  processed = 0.0;
  std::size_t end = n;
  while (end > 0) {
    std::size_t start = end - 1;
    while (start > 0 && tau_hat[order[start - 1]] == tau_hat[order[end - 1]]) --start;
    for (std::size_t k = start; k < end; ++k) {
      const std::size_t i = order[k];
      const double less_obs = above.prefix(rank[i]);
      const double greater_obs = processed - above.prefix(rank[i] + 1);
      w[i] += sample_weight[i] * (greater_obs - less_obs);
    }
    for (std::size_t k = start; k < end; ++k) {
      const std::size_t i = order[k];
      above.add(rank[i], sample_weight[i]);
      processed += sample_weight[i];
    }
    end = start;
    if ((end & 16383U) == 0U) Rcpp::checkUserInterrupt();
  }

  const double W = std::accumulate(w.begin(), w.end(), 0.0);
  double cindex = 0.5 + W / (2.0 * R);
  // Clamp only roundoff outside the mathematical range.
  if (cindex < 0.0 && cindex > -1e-12) cindex = 0.0;
  if (cindex > 1.0 && cindex < 1.0 + 1e-12) cindex = 1.0;

  // Standard error calculation
  long double variance_sum = 0.0L;
  for (std::size_t i = 0; i < n; ++i) {
    const long double delta = static_cast<long double>(R) * w[i] - static_cast<long double>(W) * r[i];
    variance_sum += delta * delta;
  }
  const double sd = static_cast<double>(std::sqrt(variance_sum) / (static_cast<long double>(R) * R));

  // Return a list with the C Index and the associated standard error
  return List::create(_["C Index"] = cindex, _["S.D."] = sd);
}
