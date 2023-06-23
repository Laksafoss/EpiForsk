#' c-for-benefit
#'
#' Calculates the c-for-benefit, as proposed by D. van Klaveren et
#' al. (2018), by matching patients based on patient characteristics.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}().
#' @param match character, `"covariates"` to match on covariates or `"CATE"` to
#'   match on estimated CATE.
#' @param tau_hat_method character, `"treatment"` to calculate the expected
#'   treatment effect in matched groups as the risk under treatment for the
#'   treated subject minus the risk under control for the untreated
#'   subject. `"average"` to calculate it as the average treatment effect of
#'   matched subject.
#' @param time_limit numeric, maximum allowed time to compute C-for-benefit. If
#'   limit is reached, execution stops.
#' @param CI character, `"none"` for no confidence interval, `"simple"` to use a
#'   normal approximation, and `"bootstrap"` to use the bootstrap.
#' @param level numeric, confidence level of the confidence interval.
#' @param n_bootstraps numeric, number of bootstraps to use for the bootstrap
#'   confidence interval computation.
#' @param time_limit_CI numeric, maximum time allowed to compute the bootstrap
#'   confidence interval. If limit is reached, the user is asked if execution
#'   should continue or be stopped.
#' @param verbose boolean, TRUE to display progress bar, FALSE to not display
#'   progress bar.
#' @param method see \link[MatchIt]{matchit}.
#' @param distance see \link[MatchIt]{matchit}.
#' @param Y a vector of outcomes. If provided, replaces `forest$Y.orig`.
#' @param W a vector of treatment assignment; 1 for active treatment; 0 for
#'   control If provided, replaces `forest$W.orig`.
#' @param X a matrix of patient characteristics. If provided, replaces
#' `forest$X.orig`.
#' @param p_0 a vector of outcome probabilities under control.
#' @param p_1 a vector of outcome probabilities under active treatment.
#' @param tau_hat a vector of individualized treatment effect predictions. If
#'   provided, replaces forest$predictions.
#' @param ... additional arguments for \link[MatchIt]{matchit}.
#'
#' @returns a list with the following components:
#'
#' - matched_patients: a tibble containing the matched patients.
#' - c_for_benefit: the resulting C-for-benefit value.
#' - lower_CI: the lower bound of the confidence interval (if CI = TRUE).
#' - upper_CI: the upper bound of the confidence interval (if CI = TRUE).
#'
#' @details The c-for-benefit statistic is inspired by the c-statistic used with
#'   prediction models to measure discrimination. The c-statistic takes all
#'   pairs of observations discordant on the outcome, and calculates the
#'   proportion of these where the subject with the higher predicted probability
#'   was the one who observed the outcome. In order to extend this to treatment
#'   effects, van Klaveren et al. suggest matching a treated subject to a
#'   control subject on the predicted treatments effect (or alternatively the
#'   covariates) and defining the observed effect as the difference between the
#'   outcomes of the treated subject and the control subject (either -1, 0, or
#'   1). The c-for-benefit statistic is then defined as the proportion of
#'   matched pairs with unequal observed effect in which the subject pair
#'   receiving greater treatment effect also has the highest expected treatment
#'   effect. \cr
#'   When calculating the expected treatment effect, van Klaveren et al. use the
#'   average CATE from the matched subjects in a pair (tau_hat_method = "mean").
#'   However, this doesn't match the observed effect used, unless the baseline
#'   risks are equal. The observed effect is the difference between the observed
#'   outcome for the subject receiving treatment and the observed outcome for
#'   the subject receiving control. Their outcomes are governed by the exposed
#'   risk and the baseline risk respectively. The baseline risks are ideally
#'   equal when covariate matching, although instability of the forest estimates
#'   can cause significantly different baseline risks due to non-exact matching.
#'   When matching on CATE, we should not expect baseline risks to be equal.
#'   Instead, we can more closely match the observed treatment effect by using
#'   the difference between the exposed risk for the subject receiving treatment
#'   and the baseline risk of the subject receiving control (tau_hat_method =
#'   "treatment").
#'
#' @author KIJA
#'
#' @examples
#' n <- 1000
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' CB_out <- CForBenefit(
#' forest = cf, CI = "bootstrap", n_bootstraps = 20, verbose = TRUE,
#' method = "nearest", distance = "mahalanobis"
#' )
#' CB_out
#'
#' @export

CForBenefit <- function(forest,
                        match = c("covariates", "CATE"),
                        tau_hat_method = c("treatment", "mean"),
                        time_limit = Inf,
                        CI = c("none", "simple", "bootstrap"),
                        level = 0.95,
                        n_bootstraps = 1,
                        time_limit_CI = Inf,
                        verbose = TRUE,
                        method = "nearest",
                        distance = "mahalanobis",
                        Y = NULL,
                        W = NULL,
                        X = NULL,
                        p_0 = NULL,
                        p_1 = NULL,
                        tau_hat = NULL,
                        ...) {
  stopifnot(
    "forest must be a causal_forest object" =
      missing(forest) || is.null(forest) || "causal_forest" %in% class(forest)
  )
  if (is.null(Y)) Y <- forest$Y.orig
  if (is.null(W)) W <- forest$W.orig
  if (is.null(X)) X <- as.matrix(forest$X.orig)
  if (is.null(p_0)) {
    p_0 <- as.numeric(forest$Y.hat - forest$W.hat * forest$predictions)
  }
  if (is.null(p_1)) {
    p_1 <- as.numeric(forest$Y.hat + (1 - forest$W.hat) * forest$predictions)
  }
  if (is.null(tau_hat)) tau_hat <- as.numeric(forest$predictions)
  subclass <- NULL
  # ensure correct data types
  stopifnot("Y must be a numeric vector" = is.vector(Y) && is.numeric(Y))
  stopifnot("W must be a numeric vector" = is.vector(W) && is.numeric(W))
  stopifnot("X must be a numeric matrix" = is.matrix(X) && is.numeric(X))
  stopifnot("p_0 must be a numeric vector" = is.vector(p_0) && is.numeric(p_0))
  stopifnot("p_1 must be a numeric vector" = is.vector(p_1) && is.numeric(p_1))
  stopifnot(
    "tau_hat must be a numeric vector" =
      is.vector(tau_hat) && is.numeric(tau_hat)
  )
  stopifnot(
    "level must be a scalar between 0 and 1" =
      length(level) == 1 && 0 < level && level < 1
  )
  stopifnot(
    "n_bootstraps must be a scalar" =
      is.numeric(n_bootstraps) && length(n_bootstraps) == 1
  )
  stopifnot("method must be a character" = is.character(method))
  stopifnot("distance must be a character" = is.character(distance))
  stopifnot(
    "verbose must be a boolean (TRUE or FALSE)" =
      isTRUE(verbose) | isFALSE(verbose)
  )
  match <- match.arg(match)
  tau_hat_method <- match.arg(tau_hat_method)
  CI <- match.arg(CI)

  # patient can only be matched to one other patient from other treatment arm
  if (sum(W == 1) <= sum(W == 0)){
    # ATT: all treated patients get matched with control patient
    estimand_method <- "ATT"
  } else if (sum(W == 1) > sum(W == 0)){
    # ATC: all control patients get matched with treated patient
    estimand_method <- "ATC"
  }

  # combine all data in one tibble
  data_tbl <- tibble::tibble(
    match_id = 1:length(Y),
    W = W, X = X, Y = Y,
    p_0 = p_0, p_1 = p_1, tau_hat = tau_hat
  )

  # set time limit on calculating C for benefit
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)

  if (match == "covariates") {
    # match on covariates
    matched <- MatchIt::matchit(
      W ~ X,
      data = data_tbl,
      method = method,
      distance = distance,
      estimand = estimand_method,
      ...
    )
  } else if (match == "CATE") {
    matched <- MatchIt::matchit(
      W ~ tau_hat,
      data = data_tbl,
      method = method,
      distance = distance,
      estimand = estimand_method,
      ...
    )
  }
  matched_patients <- MatchIt::match.data(matched)
  matched_patients$subclass <- as.numeric(matched_patients$subclass)
  matched_patients <- tibble::as_tibble(matched_patients)

  # sort on subclass and W
  matched_patients <- matched_patients |>
    dplyr::arrange(subclass, 1 - W)

  # matched observed treatment effect
  observed_te <- matched_patients |>
    dplyr::select(subclass, Y) |>
    dplyr::group_by(subclass) |>
    dplyr::summarise(
      Y = diff(Y),
      .groups = "drop"
    ) |>
    dplyr::pull(Y)
  matched_patients$matched_tau_obs <- rep(observed_te, each = 2)

  if(tau_hat_method == "treatment") {
  # matched p_0 = P[Y = 1| W = 0]
  matched_p_0 <- (1 - matched_patients$W) * matched_patients$p_0
  matched_patients$matched_p_0 <- rep(matched_p_0[matched_p_0 != 0], each = 2)

  # matched p_1 = P[Y = 1| W = 1]
  matched_p_1 <- matched_patients$W * matched_patients$p_1
  matched_patients$matched_p_1 <- rep(matched_p_1[matched_p_1 != 0], each = 2)

  # matched treatment effect (risk of treated - risk of control)
  matched_patients$matched_tau_hat <-
    matched_patients$matched_p_1 -
    matched_patients$matched_p_0
  } else if (tau_hat_method == "mean") {
    # matched treatment effect (average CATE)
    matched_patients <- matched_patients |>
      dplyr::group_by(subclass) |>
      dplyr::mutate(
        matched_tau_hat = mean(tau_hat)
      ) |>
      dplyr::ungroup()
  }

  # C-for-benefit
  cindex <- Hmisc::rcorr.cens(
    matched_patients$matched_tau_hat[seq(1, nrow(matched_patients), 2)],
    matched_patients$matched_tau_obs[seq(1, nrow(matched_patients), 2)]
  )
  c_for_benefit <- cindex["C Index"][[1]]

  if (CI == "simple") {
    lower_CI <- c_for_benefit -
      qnorm(1 - (1 - level) / 2) * cindex["S.D."][[1]] / 2
    upper_CI <- c_for_benefit +
      qnorm(1 - (1 - level) / 2) * cindex["S.D."][[1]] / 2
  } else if (CI == "bootstrap") {
    CB_for_CI <- c()
    B <- 0
    if (verbose) cli::cli_progress_bar("Bootstrapping", total = n_bootstraps)
    setTimeLimit(cpu = time_limit_CI, elapsed = time_limit_CI, transient = TRUE)
    while (B < n_bootstraps) {
      tryCatch(
        {
          # bootstrap matched patient pairs
          subclass_IDs <- unique(matched_patients$subclass)
          sample_subclass <- sample(
            subclass_IDs,
            length(subclass_IDs),
            replace = TRUE
          )
          # matched_patients is ordered by subclass
          duplicated_matched_patients <- matched_patients |>
            dplyr::slice(sample_subclass * 2)
          # calculate C-for-benefit for duplicated matched pairs
          duplicated_cindex <- Hmisc::rcorr.cens(
            duplicated_matched_patients$matched_tau_hat,
            duplicated_matched_patients$matched_tau_obs
          )
          CB_for_CI <- c(CB_for_CI, duplicated_cindex["C Index"][[1]])
          B <- B + 1
          if (verbose) cli::cli_progress_update()
        },
        error = function(e) {
          if (
            grepl(
              "reached elapsed time limit|reached CPU time limit",
              e$message)
          ) {
            input <- readline(
              "Time limit reached. Do you want execution to continue (y/n) "
            )
            if (input %in% c("y", "n")) {
              if(input == "n") stop("Time limit reached, execution stopped")
            } else {
              input <- readline(
                "Please input either 'y' to continue or 'n' to stop execution. "
              )
              if (input %in% c("y", "n")) {
                if(input == "n") stop("Time limit reached, execution stopped")
              } else {
                stop("Answer not 'y' or 'n'. Execution stopped.")
              }
            }
          } else {
            stop(e)
          }
        }
      )
    }
    lower_CI <- as.numeric(quantile(CB_for_CI, (1 - level) / 2))
    upper_CI <- as.numeric(quantile(CB_for_CI, 1 - (1 - level) / 2))
  }
  else if (CI == "none") {
    lower_CI <- NA
    upper_CI <- NA
  }

  return(
    list(
      matched_patients = matched_patients,
      c_for_benefit = c_for_benefit,
      lower_CI = lower_CI,
      upper_CI = upper_CI
    )
  )
}
