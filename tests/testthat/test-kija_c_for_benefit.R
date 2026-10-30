cfb_fixture <- function(n = 80L) {
  stopifnot(n %% 2L == 0L)
  i <- seq_len(n)
  W <- rep(c(0, 1), length.out = n)
  X <- cbind(x1 = rep(seq(-1, 1, length.out = n / 2), each = 2),
             x2 = sin(i * 0.37))
  p_0 <- plogis(-0.7 + X[, 1] + 0.15 * X[, 2])
  p_1 <- plogis(-0.5 + 0.4 * X[, 1] - 0.2 * X[, 2])
  tau_hat <- p_1 - p_0 + 0.001 * cos(i)
  # Deterministic outcome variability, across both arms.
  Y <- as.numeric((i %% 7L) %in% c(0L, 1L, 3L))
  list(Y = Y, W = W, X = X, tau_hat = tau_hat,
       p_0 = p_0, p_1 = p_1, sample_weights = rep(1, n))
}

cfb_expect_result <- function(z, method = "model_based", matched = FALSE, interval = FALSE) {
  if (length(method) == 1 && method == "none") method <- "model_based"
  expect_s3_class(z, "c_for_benefit")
  expect_named(z, c("c_for_benefit", "c_for_benefit_se", "lower_ci",
                    "upper_ci", "matched_patients", "matched_pairs",
                    "bootstrap_method", "n_bootstrap_valid", "method"))
  expect_true(is.numeric(z$c_for_benefit) && length(z$c_for_benefit) == 1L)
  expect_true(is.numeric(z$c_for_benefit_se) && length(z$c_for_benefit_se) == 1L)
  if (is.finite(z$c_for_benefit)) {
    expect_gte(z$c_for_benefit, -1e-12)
    expect_lte(z$c_for_benefit, 1 + 1e-12)
  }
  expect_identical(z$method, method)
  if (matched) {
    expect_s3_class(z$matched_pairs, "data.frame")
    expect_gt(nrow(z$matched_pairs), 1L)
    expect_true(all(c("matched_tau_hat", "matched_tau_obs", "sample_weight") %in%
                      names(z$matched_pairs)))
    expect_s3_class(z$matched_patients, "data.frame")
  } else {
    expect_null(z$matched_pairs)
    expect_null(z$matched_patients)
  }
  if (interval) {
    expect_length(z$lower_ci, 1)
    expect_length(z$upper_ci, 1)
    expect_lte(z$lower_ci, z$upper_ci)
  } else {
    expect_true(is.na(z$lower_ci))
    expect_true(is.na(z$upper_ci))
  }
}

# Independent O(n^2) reference for weighted concordance. The denominator
# excludes pairs tied on observed benefit; ties in predicted benefit earn 1/2.
cfb_reference_rcorr <- function(pred, obs, weight = rep(1, length(pred))) {
  pairs <- utils::combn(seq_along(pred), 2L)
  i <- pairs[1L, ]; j <- pairs[2L, ]
  comparable <- obs[i] != obs[j]
  w <- weight[i] * weight[j]
  credit <- ifelse(
    pred[i] == pred[j],
    0.5,
    as.numeric(sign(pred[i] - pred[j]) == sign(obs[i] - obs[j]))
  )
  den <- sum(w[comparable])
  if (den == 0) return(NA_real_)
  sum(w[comparable] * credit[comparable]) / den
}

# Reference for model-based estimator, assuming independent potential outcomes
# conditional on covariates.
cfb_reference_mbcb <- function(p0, p1, tau, weight = rep(1, length(tau))) {
  qminus <- p0 * (1 - p1)
  qzero <- (1 - p0) * (1 - p1) + p0 * p1
  qplus <- (1 - p0) * p1
  pairs <- utils::combn(seq_along(tau), 2L)
  a <- pairs[1L, ]
  b <- pairs[2L, ]
  less <- qminus[a] * (qzero[b] + qplus[b]) + qzero[a] * qplus[b]
  greater <- qplus[a] * (qminus[b] + qzero[b]) + qzero[a] * qminus[b]
  comparable <- less + greater
  credit <- ifelse(
    tau[a] == tau[b],
    0.5 * comparable,
    ifelse(tau[a] < tau[b], less, greater)
  )
  w <- weight[a] * weight[b]
  den <- sum(w * comparable)
  if (den <= 0) return(NA_real_)
  sum(w * credit) / den
}

test_that(
  "default uses model-based estimator and normal CI",
  {
    x <- cfb_fixture()
    z <- CForBenefit(x)
    cfb_expect_result(z, interval = TRUE)
    expect_null(z$bootstrap_method)
    expect_null(z$n_bootstrap_valid)
    expect_equal(
      z$c_for_benefit,
      cfb_reference_mbcb(x$p_0, x$p_1, x$tau_hat, x$sample_weights),
      tolerance = 1e-10
    )
    q <- stats::qnorm(c(0.025, 0.975))
    expect_equal(
      unname(c(z$lower_ci, z$upper_ci)),
      as.numeric(z$c_for_benefit + q * z$c_for_benefit_se)
    )
  }
)

test_that(
  "CI none gives missing bounds and ignores bootstrap settings",
  {
    z <- CForBenefit(cfb_fixture(), CI = "none", bootstrap_method = "full")
    cfb_expect_result(z)
    expect_null(z$bootstrap_method)
    expect_null(z$n_bootstrap_valid)
  }
)

test_that(
  "confidence level controls simple interval",
  {
    x <- cfb_fixture()
    a <- CForBenefit(x, CI = "simple", level = 0.90)
    b <- CForBenefit(x, CI = "simple", level = 0.99)
    expect_equal(a$c_for_benefit, b$c_for_benefit)
    expect_equal(a$lower_ci, a$c_for_benefit + stats::qnorm(0.05) * a$c_for_benefit_se)
    expect_lt(b$lower_ci, a$lower_ci)
    expect_gt(b$upper_ci, a$upper_ci)
  }
)

test_that(
  "all implemented match strategies return well-formed results",
  {
    skip_if_not_installed("MatchIt")
    x <- cfb_fixture()
    methods <- c(
      "covariates", "CATE", "control_risk", "treated_risk",
      "CATE_control_risk", "CATE_treated_risk", "combined_outcome_risk"
    )
    for (method in methods) {
      if (method %in% c("control_risk", "treated_risk")) {
        expect_warning(CForBenefit(x, match = method, CI = "none"))
      }
      z <- suppressWarnings(CForBenefit(x, match = method, CI = "none"))
      cfb_expect_result(z, method = c(method, "nearest", "mahalanobis"), matched = TRUE, interval = FALSE)
      expect_true(all(z$matched_pairs$sample_weight >= 0))
    }
  }
)

test_that(
  "matched predicted effects implement the four aggregation rules",
  {
    skip_if_not_installed("MatchIt")
    x <- cfb_fixture()
    methods <- c("risk_diff", "tau_avg", "tau_treated", "tau_control")
    for (method in methods) {
      if (method %in% c("tau_treated", "tau_control")) {
        expect_warning(CForBenefit(x, match = "CATE", tau_hat_method = method, CI = "none"))
      }
      z <- suppressWarnings(CForBenefit(x, match = "CATE", tau_hat_method = method, CI = "none"))
      m <- z$matched_patients
      groups <- split(m, m$subclass, drop = TRUE)
      expected <- vapply(
        groups,
        function(g) {
          tr <- g[g$W == 1, , drop = FALSE]
          ct <- g[g$W == 0, , drop = FALSE]
          avg <- function(d, v) stats::weighted.mean(d[[v]], d$weights)
          switch(method,
                 risk_diff = avg(tr, "p_1") - avg(ct, "p_0"),
                 tau_avg = (avg(tr, "tau_hat") + avg(ct, "tau_hat")) / 2,
                 tau_treated = avg(tr, "tau_hat"),
                 tau_control = avg(ct, "tau_hat"))
        },
        numeric(1)
      )
      expect_equal(z$matched_pairs$matched_tau_hat, unname(expected), info = method)
      expect_equal(
        z$c_for_benefit,
        cfb_reference_rcorr(z$matched_pairs$matched_tau_hat,
                            z$matched_pairs$matched_tau_obs,
                            z$matched_pairs$sample_weight),
        tolerance = 1e-10,
        info = method
      )
    }
  }
)

test_that(
  "matching estimand ATT and ATC are supported",
  {
    skip_if_not_installed("MatchIt")
    x <- cfb_fixture()
    for (target in c("ATT", "ATC")) {
      z <- CForBenefit(x, match = "CATE", estimand = target, CI = "none")
      cfb_expect_result(z, method = c("CATE", "nearest", "mahalanobis"), matched = TRUE)
    }
  }
)

test_that(
  "fixed bootstrap is reproducible for both estimators",
  {
    skip_if_not_installed("MatchIt")
    x <- cfb_fixture()
    for (m in c("none", "CATE")) {
      set.seed(302)
      a <- CForBenefit(
        x, match = m, CI = "bootstrap",
        bootstrap_method = "fixed", n_bootstraps = 39,
        bootstrap_fail = "NA"
      )
      set.seed(302)
      b <- CForBenefit(
        x, match = m, CI = "bootstrap",
        bootstrap_method = "fixed", n_bootstraps = 39,
        bootstrap_fail = "NA"
      )
      expect_equal(a, b, info = m)
      expect_identical(a$bootstrap_method, "fixed")
      expect_gte(a$n_bootstrap_valid, 1)
      expect_lte(a$n_bootstrap_valid, 39)
      cfb_expect_result(
        a,
        method = if (m == "none") m else c(m, "nearest", "mahalanobis"),
        matched = m != "none",
        interval = TRUE
      )
    }
  }
)

test_that(
  "evaluation and full bootstrap invoke callback with correct inputs",
  {
    x <- cfb_fixture()
    for (mode in c("evaluation", "full")) {
      calls <- list()
      callback <- function(data, indices, replicate, mode) {
        calls[[length(calls) + 1L]] <<- list(indices = indices, replicate = replicate, mode = mode)
        z <- lapply(data, identity)
        z$tau_hat <- z$tau_hat + replicate * 1e-4 * sin(seq_along(z$tau_hat))
        z
      }
      z <- CForBenefit(
        x,
        CI = "bootstrap", bootstrap_method = mode, bootstrap_refit = callback, bootstrap_data = x, n_bootstraps = 15
      )
      expect_length(calls, 15)
      expect_equal(vapply(calls, `[[`, integer(1), "replicate"), seq_len(15))
      expect_true(all(vapply(calls, function(a) identical(a$mode, mode), logical(1))))
      expect_true(
        all(
          vapply(
            calls,
            function(a) length(a$indices) == length(x$Y) && all(a$indices %in% seq_along(x$Y)),
            logical(1)
          )
        )
      )
      expect_identical(z$n_bootstrap_valid, 15L)
      expect_identical(z$bootstrap_method, mode)
      cfb_expect_result(z, interval = TRUE)
    }
  }
)

test_that(
  "C++ concordance agrees with independent weighted/tied reference",
  {
    set.seed(17)
    for (k in seq_len(20)) {
      n <- 12L
      pred <- sample(c(-1, 0, 1, 2), n, replace = TRUE)
      obs <- sample(c(-1, 0, 1), n, replace = TRUE)
      w <- sample(c(0, 1, 2, 3), n, replace = TRUE)
      if (sum(w > 0) < 2 || length(unique(obs[w > 0])) < 2) next
      observed <- rcorr(pred, obs, w)[["C Index"]]
      expected <- cfb_reference_rcorr(pred, obs, w)
      expect_equal(as.numeric(observed), expected, tolerance = 1e-10)
    }
  }
)

test_that(
  "C++ model-based estimator agrees with independent pairwise reference",
  {
    set.seed(19)
    for (k in seq_len(20)) {
      n <- 12L
      p0 <- stats::runif(n, .05, .95)
      p1 <- stats::runif(n, .05, .95)
      tau <- sample(c(-1, 0, 1, 2), n, replace = TRUE)
      w <- sample(c(0, 1, 2, 3), n, replace = TRUE)
      if (sum(w > 0) < 2) next
      observed <- mbcb(p0, p1, tau, w)[["C Index"]]
      expected <- cfb_reference_mbcb(p0, p1, tau, w)
      expect_equal(as.numeric(observed), expected, tolerance = 1e-10)
    }
  }
)

test_that(
  "C++ estimators handle degenerate pairs",
  {
    expect_true(is.na(rcorr(c(0, 1, 2), c(1, 1, 1), rep(1, 3))[["C Index"]]))
    expect_true(is.na(mbcb(rep(0, 3), rep(0, 3), 1:3, rep(1, 3))[["C Index"]]))
    expect_equal(rcorr(c(1, 1), c(0, 1), c(1, 1))[["C Index"]], 0.5)
    expect_equal(rcorr(c(1, 2), c(0, 1), c(1, 1))[["C Index"]], 1)
    expect_equal(rcorr(c(2, 1), c(0, 1), c(1, 1))[["C Index"]], 0)
  }
)

test_that(
  "input validation rejects malformed values",
  {
    x <- cfb_fixture()
    expect_error(CForBenefit(list(), CI = "none"), "Missing inputs")
    expect_error(CForBenefit(x, match = "bad"), "arg")
    expect_error(CForBenefit(x, tau_hat_method = "bad"), "arg")
    expect_error(CForBenefit(x, CI = "bad"), "arg")
    expect_error(CForBenefit(x, bootstrap_method = "bad"), "arg")
    expect_error(CForBenefit(x, level = 0), "level")
    expect_error(CForBenefit(x, level = 1), "level")
    expect_error(CForBenefit(x, level = NA_real_), "level")
    expect_error(CForBenefit(x, CI = "bootstrap", n_bootstraps = 0), "n_bootstraps")
    expect_error(CForBenefit(x, CI = "bootstrap", n_bootstraps = 1.5), "n_bootstraps")
    expect_error(CForBenefit(x, CI = "bootstrap", bootstrap_method = "full"), "bootstrap_refit")
    y <- x; y$tau_hat[1] <- NA_real_
    expect_error(CForBenefit(y, CI = "none"), "tau_hat")
    y <- x; y$p_0 <- y$p_0[-1]
    expect_error(CForBenefit(y, CI = "none"), "p_0")
    y <- x; y$sample_weights[1] <- -1
    expect_error(CForBenefit(y, CI = "none"), "sample_weights")
    y <- x; y$W[] <- 1
    expect_error(CForBenefit(y, match = "CATE", CI = "none"), "W must contain both")
    y <- x; y$X <- y$X[-1, , drop = FALSE]
    expect_error(CForBenefit(y, match = "covariates", CI = "none"), "X must")
  }
)

test_that(
  "unsupported matching configurations fail informatively",
  {
    skip_if_not_installed("MatchIt")
    x <- cfb_fixture()
    expect_error(CForBenefit(x, match = "CATE", replace = TRUE, CI = "none"), "replacement is not supported")
    expect_error(CForBenefit(x, match = "CATE", estimand = "ATE", CI = "none"), "ATT/ATC")
    expect_error(CForBenefit(x, match = "CATE", formula = W ~ tau_hat, CI = "none"), "Do not pass formula")
  }
)

test_that(
  "print method returns object invisibly",
  {
    z <- CForBenefit(cfb_fixture(), CI = "none")
    output <- capture.output(visible <- withVisible(print(z)))
    expect_false(visible$visible)
    expect_identical(visible$value, z)
    expect_match(paste(output, collapse = " "), "C-for-benefit")
  }
)

test_that(
  "bootstrap callback errors identify the replicate", {
    x <- cfb_fixture()
    bad_callback <- function(data, indices, replicate, mode) {
      stop("deliberate callback failure")
    }
    expect_error(
      CForBenefit(x, CI = "bootstrap", bootstrap_method = "full", bootstrap_refit = bad_callback, n_bootstraps = 3),
      "Bootstrap replicate 1: deliberate callback failure"
    )
  }
)

test_that(
  "causal forest dispatch agrees with extracted default-method inputs",
  {
    skip_if_not_installed("grf")
    skip_on_cran()
    set.seed(91)
    x <- cfb_fixture(n = 120)
    forest <- grf::causal_forest(x$X, x$Y, x$W, num.trees = 200, num.threads = 1)
    a <- CForBenefit(forest, CI = "none", n_cores = 1)
    # Test forest data extraction through the named package-internal helper.
    b <- CForBenefit(EpiForsk:::.cfb_forest_data(forest), CI = "none")
    expect_equal(a$c_for_benefit, b$c_for_benefit)
    expect_equal(a$c_for_benefit_se, b$c_for_benefit_se)
  }
)

