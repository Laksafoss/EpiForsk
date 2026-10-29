#' C-for-benefit
#'
#' Calculates the C-for-benefit, as proposed by D. van Klaveren et al. (2018). Either using a model based approach as
#' proposed by Hoogland et al. (2024) or by matching patients based on patient characteristics.
#'
#' @param object For the default method, a named list or data.frame with the following components:
#'   \itemize{
#'     \item{`"tau_hat"` - vector with estimated treatment effect}
#'     \item{`"p_0"` - vector with estimated potential outcome given non-exposure}
#'     \item{`"p_1"` - vector with estimated potential outcome given exposure}
#'     \item{`"Y"` - (only if matching != 'none') vector with outcome data}
#'     \item{`"W"` - (only if matching != 'none') vector with exposure data}
#'     \item{`"X"` - (only if matching == 'covariates')list with covariate data}
#'     \item{`"sample_weights"` - (optional) vector with sample weights}
#'   }
#'   Otherwise a supported fitted model:
#'     - causal_forest
#' @param ... additional arguments passed onto the method.
#'
#' @returns an object of class c_for_benefit with the following components:
#'   \itemize{
#'     \item{`"c_for_benefit"` - the resulting C-for-benefit value}
#'     \item{`"C_for_benefit_se"` - the resulting C-for-benefit standard error}
#'     \item{`"lower_CI"` - the lower bound of the confidence interval (if CI = simple or CI = bootstrap)}
#'     \item{`"upper_CI"` - the upper bound of the confidence interval (if CI = simple or CI = bootstrap)}
#'     \item{`"matched_patients"` - a tibble containing the matched patients}
#'     \item{`"matched_pairs"` - }
#'     \item{`"bootstrap_method"` - NULL if no bootstrap, else a character specifying the bootstrap method used}
#'     \item{`"n_bootstrap_valid"` - }
#'     \item{`"method"` - either 'model-based' or 'matching'}
#'   }
#'
#' @details The C-for-benefit statistic is inspired by the C-statistic used with prediction models to measure
#'   discrimination. The C-statistic takes all pairs of observations discordant on the outcome, and calculates the
#'   proportion of these where the subject with the higher predicted probability was the one who observed the outcome.
#'   In order to extend this to treatment effects, van Klaveren et al. suggest matching a treated subject to a control
#'   subject on the predicted treatments effect (or alternatively the covariates) and defining the observed effect as
#'   the difference between the outcomes of the treated subject and the control subject. The C-for-benefit statistic is
#'   then defined as the proportion of matched pairs with unequal observed effect in which the subject pair receiving
#'   greater treatment effect also has the highest expected treatment effect. \cr When calculating the expected
#'   treatment effect, van Klaveren et al. use the average CATE from the matched subjects in a pair (tau_hat_method =
#'   "tau_avg"). However, this doesn't match the observed effect used, unless the baseline risks are equal. The observed
#'   effect is the difference between the observed outcome for the subject receiving treatment and the observed outcome
#'   for the subject receiving control. Their outcomes are governed by the exposed risk and the baseline risk
#'   respectively. The baseline risks are ideally equal when covariate matching, although instability of e.g. causal
#'   forest estimates can cause significantly different baseline risks due to non-exact matching. When matching on CATE,
#'   we should not expect baseline risks to be equal. Instead, we can more closely match the observed treatment effect
#'   by using the difference between the exposed risk for the subject receiving treatment and the baseline risk of the
#'   subject receiving control (tau_hat_method = "risk_diff").
#'   Hoogland et al. has suggested a model-based approach to define the C-for-benefit, targeting a well-defined estimand
#'   and avoiding the need for matching. This method is the default and recommended method.
#'   Importantly, the C-for-benefit model assumes conditional independence between the potential outcomes Y(0) and Y(1)
#'   given covariates X, with binary outcomes.
#'
#' @author KIJA
#'
#' @examples
#' \donttest{
#'   # Generate example data
#'   set.seed(123)
#'   n <- 800
#'   X <- matrix(rnorm(n * 3), nrow = n)
#'   W <- rbinom(n, 1, 0.5)
#'   p_0 <- plogis(-1 + X[, 1])
#'   p_1 <- plogis(-1 + X[, 1] - 0.5 * X[, 2])
#'   Y <- rbinom(n, 1, ifelse(W == 1, p_1, p_0))
#'
#'   data <- list(
#'     X = X, W = W, Y = Y,
#'     p_0 = p_0, p_1 = p_1,
#'     tau_hat = p_1 - p_0
#'   )
#'
#'   # 1. Model-based C-for-benefit (default)
#'     CForBenefit(data)
#'
#'   # 2. Matching on predicted treatment effects
#'     CForBenefit(
#'       data,
#'       match = "CATE",
#'       tau_hat_method = "tau_avg"
#'     )
#'
#'   # 3. Covariate matching using predicted risk differences
#'     CForBenefit(
#'       data,
#'       match = "covariates",
#'       tau_hat_method = "risk_diff",
#'       estimand = "ATT"
#'     )
#'
#'   # 4. Model-based C-for-benefit with bootstrap CI
#'     CForBenefit(
#'       data,
#'       CI = "bootstrap",
#'       bootstrap_method = "fixed",
#'       n_bootstraps = 100
#'     )
#'
#'   # 5. C-for-benefit using a causal forest
#'     cf <- grf::causal_forest(X, Y, W)
#'     CForBenefit(cf)
#'
#'   # 6. Evaluation on an independent validation sample
#'     idx <- sample.int(n, n / 2)
#'     cf_train <- grf::causal_forest(
#'       X[idx, , drop = FALSE], Y[idx], W[idx]
#'     )
#'     cf_validation <- grf::causal_forest(
#'       X[-idx, , drop = FALSE], Y[-idx], W[-idx]
#'     )
#'     CForBenefit(
#'       cf_train,
#'       validation_forest = cf_validation,
#'       match = "CATE",
#'       CI = "none"
#'   )
#' }
#'
#' @export

CForBenefit <- function(object,
                        ...) {
  UseMethod("CForBenefit")
}

### Internals for CForBenefit implementation
# input validator
.cfb_validate <- function(x,
                          match) {
  if (!is.list(x)) stop("object must be a named list")
  required <- c("tau_hat", "p_0", "p_1")
  if (match != "none") required <- c(required, "Y", "W")
  if (match == "covariates") required <- c(required, "X")
  missing <- required[vapply(required, function(z) is.null(x[[z]]), logical(1))]
  if (length(missing)) stop("Missing inputs: ", paste(missing, collapse = ", "))
  n <- length(x$tau_hat)
  for (nm in setdiff(required, "X")) {
    v <- x[[nm]]
    if (!is.numeric(v) || length(v) != n || any(!is.finite(v)))
      stop(nm, " must be a finite numeric vector of length ", n)
  }
  if (n < 2L) stop("At least two observations are required")
  if (any(x$p_0 < 0 | x$p_0 > 1 | x$p_1 < 0 | x$p_1 > 1))
    warning("p_0 and p_1 must be probabilities in [0,1]")
  if (match != "none" && (any(!x$W %in% 0:1) || length(unique(x$W)) != 2L))
    stop("W must contain both 0 and 1", call. = FALSE)
  if (match == "covariates" && (!is.matrix(x$X) && !is.data.frame(x$X) || nrow(x$X) != n))
    stop("X must be a matrix/data frame with n rows")
  if (is.null(x$sample_weights)) x$sample_weights <- rep(1, n)
  if (!is.numeric(x$sample_weights) || length(x$sample_weights) != n ||
      any(!is.finite(x$sample_weights)) || any(x$sample_weights < 0) ||
      sum(x$sample_weights > 0) < 2L)
    stop("sample_weights must be finite, nonnegative, length n")
  return(x)
}

# subset data according to index
.cfb_subset <- function(x, idx) {
  out <- x
  for (nm in c("Y", "W", "tau_hat", "p_0", "p_1", "sample_weights"))
    if (!is.null(out[[nm]])) out[[nm]] <- out[[nm]][idx]
  if (!is.null(out$X)) out$X <- out$X[idx, , drop = FALSE]
  out
}

# Compute C-for-benefit using the specified method
.cfb_compute <- function(x,
                         match,
                         match_method,
                         match_distance,
                         tau_hat_method,
                         match_args) {
  x <- .cfb_validate(x, match)
  if (match == "none") {
    fit <- mbcb(x$p_0, x$p_1, x$tau_hat, x$sample_weights)
    return(
      list(
        est = as.numeric(fit[["C Index"]]),
        se = as.numeric(fit[["S.D."]]),
        patients = NULL,
        pairs = NULL
      )
    )
  }

  # If x is a list, convert it to data.frame
  df <- data.frame(
    W = x$W,
    Y = x$Y,
    tau_hat = x$tau_hat,
    p_0 = x$p_0,
    p_1 = x$p_1,
    sample_weights = x$sample_weights
  )
  vars <- switch(
    match,
    covariates = {
      cov <- as.data.frame(x$X)
      names(cov) <- paste0("cov_", seq_len(ncol(cov)))
      df <- cbind(df, cov)
      names(cov)
    },
    CATE = "tau_hat",
    control_risk = "p_0",
    treated_risk = "p_1",
    CATE_control_risk = c("p_0", "tau_hat"),
    CATE_treated_risk = c("p_1", "tau_hat"),
    combined_outcome_risk = c("p_0", "p_1")
  )
  if (is.null(match_args$estimand)) {
    match_args$estimand <- if (sum(x$W == 1) <= sum(x$W == 0)) "ATT" else "ATC"
  }
  if (!match_args$estimand %in% c("ATT", "ATC")) {
    stop("Only ATT/ATC matching estimands are supported")
  }
  if (isTRUE(match_args$replace)) {
    stop("Matching with replacement is not supported")
  }
  if (any(c("formula", "data", "method", "distance") %in% names(match_args))) {
    stop("Do not pass formula, data, method or distance through '...'")
  }
  if (match == "combined_outcome_risk") {
    if(!is.null(match_args$estimand)) match_args$estimand <- NULL
    matched_trt <- do.call(
      MatchIt::matchit,
      c(
        list(
          reformulate(vars[1], response = "W"),
          data = df,
          method = match_method,
          distance = match_distance,
          estimand = "ATT"
        ),
        match_args
      )
    )
    matched_ctr <- do.call(
      MatchIt::matchit,
      c(
        list(
          reformulate(vars[2], response = "W"),
          data = df,
          method = match_method,
          distance = match_distance,
          estimand = "ATC"
        ),
        match_args
      )
    )
    mp_trt <- df |>
      dplyr::filter(matched_trt$treat == 1) |>
      dplyr::mutate("subclass" = seq_len(dplyr::n())) |>
      dplyr::bind_rows(
        df |>
          dplyr::slice(as.integer(matched_trt$match.matrix[, 1])) |>
          dplyr::mutate("subclass" = seq_len(dplyr::n()))
      ) |>
      dplyr::arrange(.data$subclass, .data$W) |>
      dplyr::mutate(
        "sample_weights" = sum((.data$W == 1) * .data$sample_weights),
        .by = "subclass"
      ) |>
      dplyr::mutate(
        Y = .data$Y - .data$p_0,
        weights = 1
      )
    mp_ctr <- df |>
      dplyr::filter(matched_ctr$treat == 0) |>
      dplyr::mutate("subclass" = seq_len(dplyr::n()) + sum(df$W == 1)) |>
      dplyr::bind_rows(
        df |>
          dplyr::slice(as.integer(matched_ctr$match.matrix[, 1])) |>
          dplyr::mutate("subclass" = seq_len(dplyr::n()) + sum(df$W == 1))
      ) |>
      dplyr::arrange(.data$subclass, .data$W) |>
      dplyr::mutate(
        "sample_weights" = sum((.data$W == 0) * .data$sample_weights),
        .by = "subclass"
      ) |>
      dplyr::mutate(
        Y = .data$Y - .data$p_1,
        weights = 1
      )
    m <- dplyr::filter(dplyr::bind_rows(mp_trt, mp_ctr), dplyr::n() > 1, .by = "subclass")
  } else {
    fit_match <- do.call(
      MatchIt::matchit,
      c(
        list(
          formula = reformulate(vars, response = "W"),
          data = df,
          method = match_method,
          distance = match_distance
        ),
        match_args
      )
    )
    m <- MatchIt::match.data(fit_match)
  }
  if (!"subclass" %in% names(m)) stop("Matching must produce subclasses")
  groups <- split(m, m$subclass, drop = TRUE)
  if (length(groups) < 2L || any(vapply(groups, function(g) !all(c(0, 1) %in% g$W), logical(1)))) {
    stop("Need at least two subclasses, each with treated and control observations")
  }
  avg <- function(g, nm) weighted.mean(g[[nm]], g$weights)
  pairs <- do.call(
    rbind,
    lapply(
      groups,
      function(g) {
        t <- g[g$W == 1, , drop = FALSE]
        c <- g[g$W == 0, , drop = FALSE]
        observed <- avg(t, "Y") - avg(c, "Y")
        predicted <- switch(
          tau_hat_method,
          risk_diff = avg(t, "p_1") - avg(c, "p_0"),
          tau_avg = (avg(t, "tau_hat") + avg(c, "tau_hat")) / 2,
          tau_treated = avg(t, "tau_hat"),
          tau_control = avg(c, "tau_hat")
        )
        focal <- if (match == "combined_outcome_risk" || match_args$estimand == "ATT") {
          t
        } else {
          c
        }
        data.frame(
          matched_tau_hat = predicted,
          matched_tau_obs = observed,
          sample_weight = sum(focal$sample_weights)
        )
      }
    )
  )
  rownames(pairs) <- NULL
  fit <- rcorr(
    tau_hat = pairs$matched_tau_hat,
    tau_obs = pairs$matched_tau_obs,
    sample_weight = pairs$sample_weight
  )

  return(
    list(
      est = as.numeric(fit[["C Index"]]),
      se = as.numeric(fit[["S.D."]]),
      patients = m,
      pairs = pairs
    )
  )
}

# Compute confidence interval
.cfb_ci <- function(est,
                    se,
                    CI,
                    level,
                    boot = NULL) {
  if (CI == "none") return(c(NA_real_, NA_real_))
  if (CI == "simple") return(est + qnorm(c((1 - level) / 2, (1 + level) / 2)) * se)
  as.numeric(quantile(boot, probs = c((1 - level) / 2, (1 + level) / 2), na.rm = TRUE))
}

# Extract data from a causal forest object (and optional validation forest)
.cfb_forest_data <- function(forest,
                             validation = NULL,
                             n_cores = 1L) {
  eval <- if (is.null(validation)) forest else validation
  pred <- if (is.null(validation)) {
    as.numeric(forest$predictions)
  } else {
    as.numeric(predict(forest, newdata = eval$X.orig, num.threads = n_cores)$predictions)
  }
  if (length(pred) != length(eval$Y.orig)) stop("Predictions and evaluation sample differ in size")
  # When specified, risks use nuisance components from validation forest
  risk_tau <- as.numeric(eval$predictions)
  list(
    Y = eval$Y.orig,
    W = eval$W.orig,
    X = eval$X.orig,
    tau_hat = pred,
    p_0 = as.numeric(eval$Y.hat - eval$W.hat * risk_tau),
    p_1 = as.numeric(eval$Y.hat + (1 - eval$W.hat) * risk_tau),
    sample_weights = if (is.null(eval$sample.weights)) rep(1, length(pred)) else eval$sample.weights
  )
}


#' @rdname CForBenefit
#' @param match Character string specifying the method used to construct matched groups. Options are:
#'   \itemize{
#'     \item{`"none"` - Use model-based c-for-benefit without matching.}
#'     \item{`"covariates"` - Match on the covariates in `object$X`.}
#'     \item{`"CATE"` - Match on predicted conditional average treatment effects (`tau_hat`).}
#'     \item{`"control_risk"` - Match on predicted outcome risk under control (`p_0`).}
#'     \item{`"treated_risk"` - Match on predicted outcome risk under treatment (`p_1`).}
#'     \item{`"CATE_control_risk"` - Match jointly on `tau_hat` and `p_0`.}
#'     \item{`"CATE_treated_risk"` - Match jointly on `tau_hat` and `p_1`.}
#'     \item{`"combined_outcome_risk"` - Match combined on predicted outcome risk under control for controls and
#'           under treatment for treated. Thus each individual in the sample is matched with an individual from the
#'           opposite treatment group.}
#'   }
#'   Defaults to `"none"`.
#' @param match_method Character string specifying the matching algorithm passed to the `method` argument of
#'   \link[MatchIt]{matchit}. Defaults to `"nearest"`. Matching must produce subclasses containing both treated
#'   and control observations. Matching with replacement is not supported.
#' @param match_distance Character string specifying the distance measure passed to the `distance` argument of
#'   \link[MatchIt]{matchit}. Defaults to `"mahalanobis"`.
#' @param tau_hat_method Character string specifying how predicted treatment effects are aggregated within matched
#'   subclasses. Options are:
#'   \itemize{
#'     \item{`"risk_diff"` - Difference between the weighted mean predicted outcome risk under treatment among treated
#'     observations and the weighted mean predicted outcome risk under control among controls.}
#'     \item{`"tau_avg"` - Average of the weighted mean predicted CATE among treated and control observations.}
#'     \item{`"tau_treated"` - Weighted mean predicted CATE among treated observations.}
#'     \item{`"tau_control"` - Weighted mean predicted CATE among control observations.}
#'   }
#'   Defaults to `"risk_diff"`. Ignored when `match = "none"`.
#' @param CI Character string specifying the confidence interval method. Options are:
#'   \itemize{
#'     \item{`"simple"` - A normal-approximation interval based on the estimated standard error}
#'     \item{`"bootstrap"` - A percentile bootstrap interval}
#'     \item{`"none"` - No interval}
#'   }
#'   Defaults to `"simple"`.
#' @param level Numeric scalar specifying the confidence level. Must be strictly between 0 and 1. Defaults to `0.95`.
#' @param n_bootstraps Positive integer specifying the number of bootstrap replicates.
#' Defaults to `999L`. Only used when `CI = "bootstrap"`.
#' @param bootstrap_method Character string specifying the bootstrap procedure. Options are:
#'   \itemize{
#'     \item{`"fixed"` - Resample observation-level prediction vectors for model-based c-for-benefit, or matched
#'     subclasses for matching-based c-for-benefit. Prediction models are not refitted.}
#'     \item{`"evaluation"` - Resample evaluation observations and invoke `bootstrap_refit` to refit evaluation nuisance
#'     models while keeping the treatment-effect model fixed.}
#'     \item{`"full"` - Resample observations and invoke `bootstrap_refit` to repeat the specified modelling and
#'       evaluation pipeline.}
#'   }
#'   Defaults to `"fixed"`. Only used when `CI = "bootstrap"`.
#' @param bootstrap_refit Callback function(data, indices, replicate, mode) returning
#'   a complete named list for .cfb_compute(). Required for evaluation/full.
#'   In 'evaluation', the callback must keep the CATE model fixed and refit
#'   evaluation nuisance models; in 'full', it must repeat the intended pipeline.
#' @param bootstrap_data Raw data made available to the callback. Must have a
#'   meaningful nrow() or be a named list with Y; default is object.
#' @param bootstrap_fail Whether invalid replicates cause an error or return NA.
#' @export
CForBenefit.default <- function(object,
                                match = c("none", "covariates", "CATE", "control_risk",  "treated_risk",
                                          "CATE_control_risk", "CATE_treated_risk", "combined_outcome_risk"),
                                match_method = "nearest",
                                match_distance = "mahalanobis",
                                tau_hat_method = c("risk_diff", "tau_avg", "tau_treated", "tau_control"),
                                CI = c("simple", "bootstrap", "none"),
                                level = 0.95,
                                n_bootstraps = 999L,
                                bootstrap_method = c("fixed", "evaluation", "full"),
                                bootstrap_refit = NULL,
                                bootstrap_data = NULL,
                                bootstrap_fail = c("error", "NA"),
                                ...) {
  # Input validation
  match <- match.arg(match)
  tau_hat_method <- match.arg(tau_hat_method)
  CI <- match.arg(CI)
  bootstrap_method <- match.arg(bootstrap_method)
  bootstrap_fail <- match.arg(bootstrap_fail)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1) {
    stop("level must be in (0,1)")
  }
  if (CI == "bootstrap" && (
    length(n_bootstraps) != 1L || !is.finite(n_bootstraps) ||
    n_bootstraps < 1 || n_bootstraps != as.integer(n_bootstraps)
  )) {
    stop("n_bootstraps must be a positive integer")
  }
  if (CI == "bootstrap" && bootstrap_method != "fixed" && !is.function(bootstrap_refit)) {
    stop("bootstrap_refit must be a function for evaluation/full bootstrap")
  }
  if (match == "control_risk" && tau_hat_method != "tau_treated" ||
      match != "control_risk" && tau_hat_method == "tau_treated") {
    warning(paste0(
      "To obtain valid inference of C-for-benefit, matching on control outcome risk (match = 'control_risk')\n",
      "and concordance evaluation using estimated effect on the treated (tau_hat_method = 'tau_treated')\n",
      "should be used together."
    ), immediate. = TRUE)
  }
  if (match == "treated_risk" && tau_hat_method != "tau_control" ||
      match != "treated_risk" && tau_hat_method == "tau_control") {
    warning(paste0(
      "To obtain valid inference of C-for-benefit, matching on treated outcome risk (match = 'treated_risk')\n",
      "and concordance evaluation using estimated effect on the control (tau_hat_method = 'tau_control')\n",
      "should be used together."
    ), immediate. = TRUE)
  }
  x <- .cfb_validate(object, match)

  # Compute C-for-benefit
  match_args <- list(...)
  point <- .cfb_compute(x, match, match_method, match_distance, tau_hat_method, match_args)
  boot <- NULL

  # If needed, run bootstrap
  if (CI == "bootstrap") {
    raw <- if (is.null(bootstrap_data)) object else bootstrap_data
    n_raw <- if (is.data.frame(raw) || is.matrix(raw)) nrow(raw) else length(raw$Y)
    if (bootstrap_method != "fixed" && (!is.numeric(n_raw) || length(n_raw) != 1L || n_raw < 2L)) {
      stop("bootstrap_data must contain at least two observations (nrow or Y)")
    }
    # bootstrap replication
    boot <- vapply(
      seq_len(n_bootstraps),
      function(b) {
        tryCatch(
          {
            if (bootstrap_method == "fixed") {
              # Assume training sample (and model) fixed and resample C-for-benefit components
              if (match == "none") {
                z <- .cfb_subset(x, sample.int(length(x$tau_hat), replace = TRUE))
                return(.cfb_compute(z, match, match_method, match_distance, tau_hat_method, match_args)$est)
              }
              pair <- point$pairs
              idx <- sample.int(nrow(pair), replace = TRUE)
              z <- rcorr(pair$matched_tau_hat[idx], pair$matched_tau_obs[idx], pair$sample_weight[idx])
              return(as.numeric(z[["C Index"]]))
            }
            # Assume resampling of training sample and rerun full model pipeline
            idx <- sample.int(n_raw, replace = TRUE)
            z <- bootstrap_refit(data = raw, indices = idx, replicate = b, mode = bootstrap_method)
            return(.cfb_compute(z, match, match_method, match_distance, tau_hat_method, match_args)$est)
          },
          error = function(e) {
            if (bootstrap_fail == "error") {
              stop(sprintf("Bootstrap replicate %d: %s", b, conditionMessage(e)), call. = FALSE)
            }
            warning(sprintf("Bootstrap replicate %d: %s", b, conditionMessage(e)), call. = FALSE)
            return(NA_real_)
          }
        )
      },
      numeric(1)
    )
    if (any(!is.finite(boot))) {
      if (bootstrap_fail == "error") stop("Bootstrap produced nonfinite estimates")
      warning("Some bootstrap replicates were invalid; CI uses finite replicates")
    }
  }
  # Compute confidence interval
  interval <- .cfb_ci(point$est, point$se, CI, level, boot)

  # return c_for_benefit object
  return(
    structure(
      list(
        c_for_benefit = point$est,
        c_for_benefit_se = point$se,
        lower_ci = interval[1],
        upper_ci = interval[2],
        matched_patients = point$patients,
        matched_pairs = point$pairs,
        bootstrap_method = if (CI == "bootstrap") bootstrap_method else NULL,
        n_bootstrap_valid = if (is.null(boot)) NULL else sum(is.finite(boot)),
        method = if (match == "none") "model_based" else "matching"
      ),
      class = "c_for_benefit"
    )
  )
}


# causal forest method
#' @rdname CForBenefit
#' @param validation_forest An object of class `causal_forest`.
#' @param n_cores integer, number of workers to use for parallel computation.
#' @export
CForBenefit.causal_forest <- function(object,
                                      validation_forest = NULL,
                                      n_cores = 1L,
                                      CI = c("simple", "bootstrap", "none"),
                                      bootstrap_method = c("fixed", "evaluation", "full"),
                                      bootstrap_refit = NULL,
                                      bootstrap_data = NULL,
                                      ...) {
  if (!is.null(validation_forest) && !inherits(validation_forest, "causal_forest")) {
    stop("validation_forest must be a causal_forest")
  }
  CI <- match.arg(CI)
  bootstrap_method <- match.arg(bootstrap_method)

  x <- .cfb_forest_data(object, validation_forest, n_cores)

  if (CI == "bootstrap" && bootstrap_method != "fixed" && is.null(bootstrap_refit)) {
    bootstrap_refit <- cfb_causal_forest_bootstrap(
      object,
      validation_forest,
      n_cores = n_cores
    )
    bootstrap_data <- list(
      Y = if (bootstrap_method == "full" || is.null(validation_forest)) object$Y.orig else validation_forest$Y.orig
    )
  }

  CForBenefit.default(
    x,
    CI = CI,
    bootstrap_method = bootstrap_method,
    bootstrap_refit = bootstrap_refit,
    bootstrap_data = bootstrap_data,
    ...
  )
}


# causal survival forest method
#' @rdname CForBenefit
#' @param horizon A positive scalar defining the estimand. See \link[grf]{causal_survival_forest}
#' @export
CForBenefit.causal_survival_forest <- function(object,
                                               horizon,
                                               ...) {
  stop("Survival-specific C-for-benefit requires a defined horizon, event-risk scale, ",
       "and handling of censoring. Supply validated horizon-specific risks and ",
       "effects to CForBenefit.default() after choosing an appropriate estimator.",
       call. = FALSE)
}


# print method
#' @rdname CForBenefit
#' @param x An object of class `c_for_benefit`
#' @export
print.c_for_benefit <- function(x,
                                ...) {
  cat("C-for-benefit (", x$method, "): ", format(x$c_for_benefit), "\n", sep = "")
  if (!is.na(x$lower_ci)) cat("Interval: [", x$lower_ci, ", ", x$upper_ci, "]\n", sep = "")
  invisible(x)
}


#' Create a bootstrap refitting callback for a causal forest
#'
#' @param forest Original fitted causal_forest; contains X.orig, Y.orig, W.orig.
#' @param validation_forest Optional independent causal_forest fitted on evaluation data.
#' @param n_cores Threads for grf fits/predictions (not parallel bootstrap workers).
#' @param fit_fun Optional custom function(X, Y, W, sample.weights, clusters) returning a causal_forest object.
#' fit_fun = NULL (the default) refits a grf::causal_forest with default nuisance estimation and tuning parameters;
#' it does NOT reconstruct every original tuning choice.
#' @return Function(data, indices, replicate, mode) returning a complete named list for CForBenefit.default().
#' @details
#' mode='full': resample training observations and refit causal forest;
#'   independently resample and refit the validation forest if provided. Without
#'   validation_forest, evaluate on the resampled training sample using OOB
#'   predictions; this is not an independent validation procedure.
#' mode='evaluation': keep original CATE forest fixed, resample evaluation
#'   observations, refit a separate causal forest to obtain nuisance/risk
#'   predictions on that resampled evaluation sample. Requires validation_forest.
#'   This uses OOB nuisance predictions; ordinary bootstrap duplicates can
#'   affect their properties. Treat resulting CIs as exploratory.
#' The default fit_fun does not reproduce custom SuperLearner nuisance models,
#' cross-fitting, tuning or custom forest arguments. Provide fit_fun for those.
#' If clusters exist, observations are resampled individually, NOT by cluster;
#' clustered inference requires a cluster-aware callback.

cfb_causal_forest_bootstrap <- function(forest,
                                        validation_forest = NULL,
                                        n_cores = 1L,
                                        fit_fun = NULL) {
  if (!inherits(forest, "causal_forest")) stop("forest must be a causal_forest")
  if (!is.null(validation_forest) && !inherits(validation_forest, "causal_forest")) {
    stop("validation_forest must be a causal_forest")
  }
  if (is.null(forest$X.orig) || is.null(forest$Y.orig) || is.null(forest$W.orig)) {
    stop("Original forest must retain X.orig, Y.orig and W.orig")
  }
  if (is.null(fit_fun)) {
    fit_fun <- function(X, Y, W, sample.weights, clusters) {
      args <- list(X = X, Y = Y, W = W, num.threads = n_cores)
      if (!is.null(sample.weights)) args$sample.weights <- sample.weights
      if (!is.null(clusters)) args$clusters <- clusters
      do.call(grf::causal_forest, args)
    }
  }
  if (!is.function(fit_fun)) stop("fit_fun must be a function")
  function(data, indices, replicate, mode) {
    mode <- match.arg(mode, c("evaluation", "full"))
    if (mode == "evaluation" && is.null(validation_forest)) {
      stop("evaluation bootstrap requires independent validation_forest")
    }
    original <- if (mode == "full") forest else validation_forest
    n <- length(original$Y.orig)
    if (anyNA(indices) || !is.numeric(indices) || any(indices < 1 | indices > n) || any(indices != floor(indices))) {
      stop("Invalid bootstrap indices")
    }
    X <- original$X.orig[indices, , drop = FALSE]
    Y <- original$Y.orig[indices]
    W <- original$W.orig[indices]
    if (length(unique(W)) != 2L) stop("Bootstrap sample has only one treatment arm")
    weights <- if (is.null(original$sample.weights)) NULL else original$sample.weights[indices]
    clusters <- if (is.null(original$clusters)) NULL else original$clusters[indices]
    fitted <- fit_fun(X = X, Y = Y, W = W, sample.weights = weights, clusters = clusters)
    if (!inherits(fitted, "causal_forest")) stop("fit_fun must return a causal_forest")
    if (mode == "evaluation") {
      # Predictions from original CATE model on the resampled validation sample
      # nuisance components estimated using the resampled evaluation sample
      z <- .cfb_forest_data(forest, fitted, n_cores)
    } else if (is.null(validation_forest)) {
      # OOB evaluation on bootstrap training sample (not independent validation).
      z <- .cfb_forest_data(fitted, NULL, n_cores)
    } else {
      # Independent two-sample bootstrap: resample validation data separately,
      # refit its nuisance/risk model, then predict CATE using refitted training model.
      nv <- length(validation_forest$Y.orig)
      iv <- sample.int(nv, replace = TRUE)
      Xv <- validation_forest$X.orig[iv, , drop = FALSE]
      Yv <- validation_forest$Y.orig[iv]
      Wv <- validation_forest$W.orig[iv]
      if (length(unique(Wv)) != 2L) stop("Bootstrap validation sample has only one treatment arm")
      wv <- if (is.null(validation_forest$sample.weights)) NULL else validation_forest$sample.weights[iv]
      cv <- if (is.null(validation_forest$clusters)) NULL else validation_forest$clusters[iv]
      fv <- fit_fun(X = Xv, Y = Yv, W = Wv, sample.weights = wv, clusters = cv)
      if (!inherits(fv, "causal_forest"))
        stop("fit_fun must return a causal_forest for validation data")
      z <- .cfb_forest_data(fitted, fv, n_cores)
    }
    z
  }
}
