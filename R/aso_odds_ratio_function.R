#' Easier to perform logistic and log-linear regressions giving a standardized
#' output table
#'
#' odds_ratio_function analyses specified data given user specifications,
#' including outcome, exposures and possible weights. It can handle survey-data,
#' but not complex sampling schemes (if specified as survey-data, the model will
#' create a simple survey-object from the data, using weights as specified - if
#' not specified, the weights are 1 for each observation) The standard
#' regression is logistic regression (yielding Odds Ratios=OR) but it is
#' possible to perform a log-linear regression (yielding Risk Ratios=RR)
#' instead, if specified and requirements are met.
#'
#' @param normaldata A data frame or data frame extension (e.g. a tibble).
#' @param outcomevar A character string naming of factor variable in normaldata
#'   to use as the outcome.
#' @param expvars A character vector with the names of the exposure variables
#'   (either numeric or factors). Any transformations or interactions to be
#'   included must also be specified, e.g.
#'   `c("Var1", "I(Var1^2)", "Var2", "Var3*Var4")`.
#' @param number_decimals An integer giving the number of decimals to show in
#'   the standardized output (default is two decimals).
#' @param alpha A scalar, between 0 and 1 specifying the desired significance
#'   level of the confidence intervals (default is 0.05 which will yield the
#'   usual 95% confidence interval).
#' @param regtype A character string specifying the analysis method. Can either
#'   be "logistic" for logistic regression (the default) or "log-linear" for
#'   log-linear regression. Log-linear regression can only be used with
#'   binomial, unconditional analysis.
#' @param matchgroup Character string specifying a variable in normaldata to
#'   condition the analysis on. Can only be used in binomial logistic regression
#'   models (default is NULL).
#' @param matchtiemethod Character string specifying the method for ties when
#'   using a matched/conditional analysis. The default options is "exact",
#'   however this option does not take weights into account for the analysis, so
#'   if weights (other than 1) are used, another option should be selected.
#'   Other options are "approximate", "efron", and "breslow" - for further
#'   explanations, see documentation for \link[survival]{clogit}.
#' @param values_to_remove A Character vector specifying values to remove from
#'   ALL variables used in the regression before the analysis (default is NULL).
#'   This is useful if some value(s) are used consistently to encode
#'   missing/irrelevant in the data (e.g. c("888", "987") - normal missing (NA)
#'   don't need to be specified as it will be removed automatically. Do NOT
#'   remove the reference values as this will lead to unexpected results!
#' @param weightvar A character string specifying a numeric variable in
#'   normaldata with pre-calculated weights for observations in the analysis.
#'   The default value NULL corresponds to weight 1 for all observations.
#' @param surveydata A Boolean specifying whether the data comes from a survey
#'   (default is FALSE).
#' @param textvar A character string with text (like a note) to be added to the
#'   output. The default value NULL corresponds to no added note.
#' @param model_object A Boolean. If TRUE, returns the raw output object from
#'   the analysis instead of the standard output. This might be useful to see
#'   information not included in the standardized output (default is FALSE).
#'
#' @return A standardized analysis object with results from a model.
#'
#' @author ASO
#'
#' @seealso [odds_ratio_function_repeated()] to perform several analysis in one
#' go.
#'
#' @examples
#' ### Binomial outcome
#' data(logan, package = "survival")
#'
#' resp <- levels(logan$occupation)
#' n <- nrow(logan)
#' indx <- rep(1:n, length(resp))
#' logan2 <- data.frame(
#'   logan[indx,],
#'   id = indx,
#'   tocc = factor(rep(resp, each=n))
#' )
#' logan2$case <- (logan2$occupation == logan2$tocc)
#' logan2$case <- as.factor(logan2$case)
#' logan2$case <- relevel(logan2$case, ref = "FALSE")
#'
#' # Standard binomial logistic regression but using interaction for exposures:
#' func_est1 <- odds_ratio_function(
#'   logan2,
#'   outcomevar = "case",
#'   expvars = c("tocc", "education", "tocc:education")
#' )
#'
#' \donttest{
#' # Conditional binomial logistic regression with some extra text added:
#' func_est2 <- odds_ratio_function(
#'   logan2,
#'   outcomevar = "case",
#'   expvars = c("tocc", "tocc:education"),
#'   matchgroup = "id",
#'   textvar = "Testing function"
#' )
#' }
#'
#' # Standard binomial logistic regression as survey data with no prepared
#' # weights:
#' func_est3 <- odds_ratio_function(
#'   logan2,
#'   outcomevar = "case",
#'   expvars = c("tocc", "education"),
#'   surveydata = TRUE
#' )
#'
#' # Example changing significance level and the number of decimals in fixed
#' # output and adding some text:
#' func_est4 <- odds_ratio_function(
#'   logan2,
#'   outcomevar = "case",
#'   expvars = c("tocc", "education"),
#'   number_decimals = 5,
#'   alpha = 0.01,
#'   textvar = "Testing function"
#' )
#'
#' # Getting raw output from the regression function:
#' func_est5 <- odds_ratio_function(
#'   logan2,
#'   outcomevar = "case",
#'   expvars = c("tocc", "education"),
#'   model_object = TRUE
#' )
#'
#' ### Polytomous/multinomial outcome
#' data(api, package = "survey")
#'
#' # As normal data, but using weights:
#' func_est6 <- odds_ratio_function(
#'   apiclus2,
#'   outcomevar = "stype",
#'   expvars = c("ell", "meals", "mobility", "sch.wide"),
#'   weightvar = "pw"
#' )
#'
#' # As survey data with weights:
#' func_est7 <- odds_ratio_function(
#'   apiclus2,
#'   outcomevar = "stype",
#'   expvars = c("ell", "meals", "mobility"),
#'   weightvar = "pw", surveydata = TRUE
#' )
#'
#' # Binomial logistic regression with same data (by removing all observations
#' # with a specific value of outcome):
#' func_est8 <- odds_ratio_function(
#'   apiclus2,
#'   outcomevar = "stype",
#'   expvars = c("ell", "meals", "mobility"),
#'   weightvar = "pw",
#'   values_to_remove = c("E")
#' )
#'
#' @export

odds_ratio_function <- function(
    normaldata,
    outcomevar,
    expvars,
    number_decimals = 2,
    alpha = 0.05,
    regtype = c("logistic", "log-linear"),
    matchgroup = NULL,
    matchtiemethod = c("exact", "approximate", "efron", "breslow"),
    values_to_remove = NULL,
    weightvar = NULL,
    surveydata = FALSE,
    textvar = NULL,
    model_object = FALSE
) {
  ### Start of Input checks
  if (missing(normaldata)) {
    stop("'normaldata' must be a data frame.")
  }
  if (!inherits(normaldata, "data.frame")) {
    stop("'normaldata' must be a data frame.")
  }
  if (missing(outcomevar) ||
      !(is.character(outcomevar) && length(outcomevar) == 1)) {
    stop(
      "'outcomevar' must be a length 1 character vector naming ",
      "a factor variable in 'normaldata' to use as outcome."
    )
  }
  if (missing(expvars) || !(is.character(expvars))) {
    stop(
      "'expvars' must be a character vector specifying exposure variables,",
      "including transformations (e.g. I(a^2)) and interactions (e.g. a:b)."
    )
  }
  if (!inherits(number_decimals, "numeric")) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  if (number_decimals < 0) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  if (!inherits(alpha, "numeric")) {
    stop("'alpha' must specify a significance level between 0 and 1.")
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("'alpha' must specify a significance level between 0 and 1.")
  }
  regtype <- match.arg(regtype)
  if (!(is.null(matchgroup) ||
        (is.character(matchgroup) && length(matchgroup) == 1))) {
    stop(
      "'matchgroup' must be NULL or optionally a length 1 character ",
      "vector\nnaming a variable in 'normaldata' to condition the analysis on."
    )
  }
  matchtiemethod <- match.arg(matchtiemethod)
  if (!is.null(values_to_remove)) {
    if (!inherits(values_to_remove, "character")) {
      stop(
        "'values_to_remove' must be NULL or optionally a character vector."
      )
    }
  }
  if (!is.null(weightvar)) {
    if (!inherits(weightvar, "character")) {
      stop(
        "weightvar must be NULL or optionally a character of length 1 ",
        "naming a column\nin `normaldata` with numeric weights ",
        "for each observation."
      )
    } else if (!(length(weightvar == 1) && weightvar %in% names(normaldata))) {
      stop(
        "weightvar must name a single column in 'normaldata' with numeric",
        "weights\nfor each observation."
      )
    } else if (!is.numeric(normaldata[[weightvar]])) {
      stop(
        "The column named in weightvar must contain numeric weights",
        "for each observation."
      )
    }
  }
  if (!(isTRUE(surveydata) || isFALSE(surveydata))) {
    stop("'surveydata' must be a boolean.")
  }
  if (!(is.null(textvar) || inherits(textvar, "character"))) {
    stop("When 'textvar' is specified it must be a character.")
  }
  if (!(isTRUE(model_object) || isFALSE(model_object))) {
    stop("'model_object' must be a boolean.")
  }
  if (isTRUE(surveydata) & !is.null(matchgroup)) {
    stop(
      "The combination of using surveydata and conditioning/matching is ",
      "not supported."
    )
  }
  if (regtype == "log-linear" && !is.null(matchgroup)) {
    stop("When regtype is set to 'log-linear', no conditioning is supported.")
  }
  if (regtype == "log-linear" && surveydata == TRUE) {
    stop("When regtype is set to 'log-linear', surveydata is not supported.")
  }
  ### End of input checks

  # Only keeping relevant variables
  # Make sure interaction variables are also included in the final data
  # (splitting string)
  new_expvars_prp <- expvars
  new_expvars_prp2 <- unlist(strsplit(new_expvars_prp, ":"))
  new_expvars <- unlist(strsplit(new_expvars_prp2, "[*]"))

  # Selecting all relevant variables from data
  # (effectively dropping all other variables)
  func_table1 <- normaldata |>
    dplyr::select(
      "Outc" = dplyr::all_of(outcomevar),
      dplyr::all_of(new_expvars),
      "matchID" = dplyr::all_of(matchgroup),
      "weight_used" = dplyr::all_of(weightvar)
    )

  # Setting weights (unless specified, weight will be 1)
  # Only selecting variables to use in OR estimation
  # (adjusted or not, with weights or not)
  if (is.null(weightvar)) {
    func_table2 <- func_table1 |> dplyr::mutate(weight_used = as.numeric(1))
  } else {
    func_table2 <- dplyr::filter(func_table1, .data$weight_used > 0)
  }

  # Removing observations with missing in ANY of the used variables
  used_var <- ls(func_table2)
  func_table3 <- func_table2 |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(used_var), ~ !is.na(.x)))

  # Removes specified observation values in any of the used variables IF the
  # option "values_to_remove" is given (as vector)
  #   e.g. in Project SEXUS values "888" (="I don't know") and
  #        "987"=("Logically implausible") will usually not be used in analyses
  # In addition, all factor levels NOT existing in the present data are removed
  # (using the droplevels() function)
  if (is.null(values_to_remove)) {
    func_table4 <- droplevels(func_table3)
  } else {
    func_table4_prp <- func_table3 |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(used_var),
          ~ !as.character(.x) %in% values_to_remove
        )
      )
    func_table4 <- droplevels(func_table4_prp)
  }

  # Get number of levels the specified outcome has as well as the specified
  # exposure
  #   This will be used so the results can be treated correctly
  outcome_levels <- length(unique(func_table4$Outc))

  # Stops the function if outcome is not binary (since it's not developed for
  # polytomous/multinomial regressions)
  if (outcome_levels < 2) {
    stop("The outcome was the same for all samples!")
  }
  if (outcome_levels > 2 & !is.null(matchgroup)) {
    stop("Matched polytomous/multinomial models are not supported!")
  }

  # Model calls - depending of specifications in function call
  if (is.null(matchgroup)) {
    Model_call <- as.formula(
      paste("Outc ~", paste(expvars, collapse = "+"), sep = "")
    )
  } else {
    Model_call <- as.formula(
      paste(
        "Outc ~",
        paste(expvars, collapse = "+"),
        "+ strata(matchID)",
        sep = ""
      )
    )
  }

  #Getting N for all levels of the outcome
  func_model_n_prp <- func_table4 |>
    dplyr::summarize(
      Freq = dplyr::n(),
      Freqw = sum(.data$weight_used),
      .by = "Outc"
    ) |>
    (\(tbl) dplyr::mutate(tbl, sortnr = rownames(tbl), .before = 1))() |>
    dplyr::mutate(
      Part_ = dplyr::case_when(
        .data$sortnr == 1 ~ paste0(
          "Non-outcome=",
          .data$Outc,
          " (n=",
          .data$Freq,
          "/weighted n=",
          sprintf("%.2f", .data$Freqw),
          ")"
        ),
        TRUE ~ paste0(
          "Outcome=",
          .data$Outc,
          " (n=",
          .data$Freq,
          "/weighted n=",
          sprintf("%.2f", .data$Freqw),
          ")"
        )
      )
    )
  func_model_n_sum <- dplyr::summarize(
    func_model_n_prp,
    Total = sum(.data$Freq),
    Total_weighted = sum(.data$Freqw)
  )
  func_model_n_prp2 <- tidyr::pivot_wider(
    dplyr::select(func_model_n_prp, "sortnr", "Part_"),
    names_from = "sortnr",
    values_from = c("Part_"),
    names_prefix = "Outcome"
  )
  func_model_n_prp3_1 <- tidyr::pivot_wider(
    dplyr::select(func_model_n_prp, "Outc", "Freq"),
    names_from = "Outc",
    values_from = c("Freq"),
    names_prefix = "Unweighted_n_"
  ) |>
    dplyr::mutate(sortnr = 0)
  func_model_n_prp3_2 <- tidyr::pivot_wider(
    dplyr::select(func_model_n_prp, "Outc", "Freqw"),
    names_from = "Outc",
    values_from = c("Freqw"),
    names_prefix = "Weighted_n_"
  ) |>
    dplyr::mutate(sortnr = 0)
  func_model_n <- as.data.frame(
    paste0(
      paste(as.vector(func_model_n_prp2), collapse = ", "),
      ", Total: n=",
      dplyr::select(func_model_n_sum, "Total"),
      "/ weighted n=",
      sprintf("%.2f", dplyr::select(func_model_n_sum, "Total_weighted"))
    )
  ) |>
    dplyr::rename("N" = 1) |>
    dplyr::mutate(
      term = "(Intercept)",
      sortnr = 0
    ) |> dplyr::select("term", "N", "sortnr") |>
    dplyr::left_join(func_model_n_prp3_1, by = "sortnr") |>
    dplyr::left_join(func_model_n_prp3_2, by = "sortnr")

  # Binomial outcome
  if (outcome_levels == 2) {

    Outcome_type <- c("Binomial") # Used to create information of model used

    # "Normal" binomial logistic regression
    if (regtype == "logistic") {

      if (surveydata == FALSE & is.null(matchgroup)) {

        Regression_type <- c("logistic regression,") #Used to create information of model used
        Model_info <- c("glm(), stats package") #Used to create information of model used

        # Getting the non reference level of the outcome variable in the
        # binomial model
        outcome_level <- as.character(levels(func_table4$Outc)[2])

        # Running regression and getting estimates, standard errors etc. in a
        # table (normal version)
        func_table5_prp <- glm(
          Model_call,
          data = func_table4,
          family = binomial(link = "logit"),
          weights = func_table4$weight_used
        )
        # return raw output if requested
        if (model_object == TRUE) {
          return(func_table5_prp)
        }

        #Extract estimates and standard error etc. from model output
        func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

        #Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
          (\(tbl) dplyr::mutate(tbl, Variable = rownames(tbl), .before = 1))() |>
          dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
          dplyr::filter(!is.na(.data$P_anova)) |>
          dplyr::rename("P_drop1" = "P_anova")

      } else if (surveydata == FALSE) {

        #Conditional logistic regression
        # Matched data, the clogit() (each matched group have a common ID) is
        # used. Only works
        #   when the outcome is binary - it's possible to set how to handle ties
        #   ("exact", "approximate", "efron", "breslow"):
        #    if not set at all "exact" will be used BUT this option ignore
        #    possible weights

        Regression_type <- c("conditional/matched logistic regression,") #Used to create information of model used
        Model_info <- c("clogit(), survival package") #Used to create information of model used

        # Getting the non reference level as well as the reference level of the
        # outcome variable in the binomial model
        outcome_level <- as.character(levels(func_table4$Outc)[2])
        non_outcome_level <- as.character(levels(func_table4$Outc)[1])
        func_table4model <- func_table4
        func_table4model$Outc <- as.numeric(func_table4model$Outc)

        # Running regression and getting estimates, standard errors etc. in a
        # table - depends on if weights are used or not
        if (is.null(weightvar)) {
          func_table5_prp <- survival::clogit(
            Model_call,
            data = func_table4model,
            method = matchtiemethod
          )
        } else {
          func_table5_prp <- survival::clogit(
            Model_call,
            data = func_table4model,
            method = matchtiemethod,
            weights = func_table4model$weight_used
          )
        }
        # return raw output if requested
        if (model_object == TRUE) {
          return(func_table5_prp)
        }

        # Extract estimates and standard error etc. from model output
        func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

        # Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
          (\(tbl) dplyr::mutate(tbl, Variable = rownames(tbl), .before = 1))() |>
          dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
          dplyr::filter(!is.na(.data$P_anova)) |>
          dplyr::rename("P_drop1" = "P_anova")

        # Due to ONLY observations having matches are used, the earlier
        # calculated N can't be used
        # Getting number of observations used as well as for each level
        # (2 levels only!) of the outcome
        func_model_n_prp1 <- as.data.frame(func_table5_prp$n) |>
          dplyr::mutate(matchnum = "1")
        func_model_n_prp2 <- as.data.frame(func_table5_prp$nevent) |>
          dplyr::mutate(matchnum = "1")
        func_model_n <- dplyr::full_join(
          func_model_n_prp1,
          func_model_n_prp2,
          by = "matchnum"
        ) |>
          dplyr::mutate(
            term = "(Intercept)",
            n_nonevent = as.character(
              as.numeric(.data$`func_table5_prp$n`) -
                as.numeric(.data$`func_table5_prp$nevent`)
            ),
            N = paste0(
              "Non-outcome=", non_outcome_level,
              " (n=", .data$n_nonevent,")",
              ", Outcome=", outcome_level,
              " (n=", .data$`func_table5_prp$nevent`,")",
              ", Total n=", .data$`func_table5_prp$n`
            ),
            sortnr = 0
          ) |>
          dplyr::select("term", "N", "sortnr")
      } else if (surveydata == TRUE) {
        #Logistic regression with data from survey

        Regression_type <- c("logistic regression with surveydata,") #Used to create information of model used
        Model_info <- c("svyglm(), survey package") #Used to create information of model used

        # Getting the non reference level of the outcome variable in the
        # binomial model
        outcome_level <- as.character(levels(func_table4$Outc)[2])

        # Creating a survey object (i.e. a data frame with weights etc. that R
        # recognizes as survey data)
        func_table5_prp_prp <- survey::svydesign(
          ~0,
          probs = NULL,
          strata = NULL,
          variables = NULL,
          fpc = NULL,
          data = func_table4,
          weights = ~weight_used
        )

        # Running regression and getting estimates, standard errors etc. in a
        # table (survey data)
        func_table5_prp <- svyglm(
          Model_call,
          design = func_table5_prp_prp,
          family = quasibinomial(link = "logit")
        )

        # return raw output if requested
        if (model_object == TRUE) {
          return(func_table5_prp)
        }

        func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

        #Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
          (\(tbl) dplyr::mutate(tbl, Variable = rownames(tbl), .before = 1))() |>
          dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
          dplyr::filter(!is.na(.data$P_anova)) |>
          dplyr::rename("P_drop1" = "P_anova")
      }
    } else if (regtype == "log-linear") {

      Regression_type <- c("log-linear regression,")
      Model_info <- c("glm(), stats package") #Used to create information of model used

      # Getting the non reference level of the outcome variable in the binomial model
      outcome_level <- as.character(levels(func_table4$Outc)[2])

      #Running regression and getting estimates, standard errors etc. in a table (normal version)
      func_table5_prp <- glm(
        Model_call,
        data = func_table4,
        family = binomial(link = "log"),
        weights = func_table4$weight_used
      )

      # return raw output if requested
      if (model_object == TRUE) {
        return(func_table5_prp)
      }

      func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

      # Getting p-values for included components in model
      func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
        (\(tbl) dplyr::mutate(tbl, Variable = rownames(tbl), .before = 1))() |>
        dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
        dplyr::filter(!is.na(.data$P_anova)) |>
        dplyr::rename("P_drop1" = "P_anova")
    }
  } else if (outcome_levels > 2) {
    #Polytomous/multinomial outcome

    Outcome_type <- c("Polytomous/multinomial") #Used to create information of model used

    #Multinomial/polytomous logistic regression ("normal")
    if (surveydata == FALSE) {

      Regression_type <- c("logistic regression,") #Used to create information of model used
      Model_info <- c("multinom(), nnet package") #Used to create information of model used

      # Running multinomial regression and getting estimates, standard errors
      # etc. in a table (normal version)
      func_table5_prp <- nnet::multinom(
        Model_call,
        data = func_table4,
        weights = func_table4$weight_used
      )

      # return raw output if requested
      if (model_object == TRUE) {
        return(func_table5_prp)
      }

      func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)
    } else if (surveydata == TRUE) {
      #Multionomial/polytomous logistic regression with surveydata

      Regression_type <- c("logistic regression with surveydata,") #Used to create information of model used
      Model_info <- c("svy_vglm(), svyVGAM package") #Used to create information of model used

      # Creating a survey object (i.e. a data frame with weights etc. that R
      # recognizes as survey data)
      func_table5_prp_prp <- survey::svydesign(
        ~0,
        probs = NULL,
        strata = NULL,
        variables = NULL,
        fpc = NULL,
        data = func_table4,
        weights = ~weight_used
      )
      # Running multinomial regression and getting estimates, standard errors
      # etc. in a table (survey data) AND
      #   make sure the variable names are the same as from the other models
      func_table5_prp <- svyVGAM::svy_vglm(
        Model_call,
        family = VGAM::multinomial(refLevel = 1),
        design = func_table5_prp_prp
      )

      # return raw output if requested
      if (model_object == TRUE) {
        return(func_table5_prp)
      }

      #Getting coefficients etc.
      func_table5_prp2 <- as.data.frame(
        summary(func_table5_prp)$coeftable
      ) |>
        (\(tbl) dplyr::mutate(tbl, term_prp = rownames(tbl), .before = 1))() |>
        dplyr::rowwise() |>
        dplyr::mutate(
          cut_point = utils::tail(unlist(gregexpr(":", .data$term_prp)), n = 1),
          y.level = substring(.data$term_prp, first = (.data$cut_point + 1)),
          term = substr(.data$term_prp, start = 1, stop = (.data$cut_point - 1))
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          "y.level",
          "term",
          "estimate" = "Coef",
          "std.error" = "SE",
          "statistic" = "z",
          "p.value" = "p"
        )
      # Getting outcome group names (Not included in standard output)
      func_table5_prp3 <- as.data.frame(func_table5_prp$fit@extra$colnames.y) |>
        (\(tbl) dplyr::mutate(tbl, Outcome_order = rownames(tbl), .before = 1))()
      # Getting hold of outcome reference level number
      func_table5_prp4 <- as.data.frame(func_table5_prp$fit@extra$use.refLevel)
      # Change column/variable names so they are standardized
      colnames(func_table5_prp3)[2] <- "Outcome_levels"
      colnames(func_table5_prp4)[1] <- "refLevel"
      # Final adjustments, so the numeric levels used in function now have the
      # outcome level names instead
      func_table5_prp5 <- func_table5_prp4 |>
        dplyr::mutate(
          ref_indicator = .data$refLevel,
          refLevel = paste0(.data$refLevel)
        )
      func_table5_prp6 <- dplyr::full_join(
        func_table5_prp3,
        func_table5_prp5,
        by = c("Outcome_order" = "refLevel")
      ) |>
        dplyr::mutate(
          Outcome_order_new_prp = as.numeric(.data$Outcome_order),
          ref = as.numeric(.data$ref_indicator),
          ref_level = min(.data$ref, na.rm = TRUE),
          Outcome_order_new = dplyr::case_when(
            .data$Outcome_order_new_prp == .data$ref_level ~ paste0(0),
            .data$Outcome_order_new_prp < .data$ref_level ~
              paste0(.data$Outcome_order_new_prp),
            .data$Outcome_order_new_prp > .data$ref_level ~
              paste0(.data$Outcome_order_new_prp - 1)
          )
        ) |>
        dplyr::select("Outcome_order_new", "Outcome_levels")
      func_table5 <- dplyr::right_join(
        func_table5_prp6,
        func_table5_prp2,
        by = c("Outcome_order_new" = "y.level")
      ) |>
        dplyr::rename("y.level" = "Outcome_levels") |>
        dplyr::select(-"Outcome_order_new")
    }
  }

  # Getting the "correct" value of Z for getting confidence limits (two sided,
  # when alpha=0.05 then z=1.96 (standard))
  z <- round(qnorm((1 - (alpha / 2))), digits = 4)
  #use of the tidy() function of the broom package to standardize the output
  # (gets multiple rows
  #   with variables with variable name+level, log(estimate), log(std.error),
  # statistic and p.value (vs. reference)
  #   + adding calculated OR and CI
  func_table6 <- func_table5 |>
    dplyr::mutate(
      OR = paste0(
        sprintf(paste0("%.",number_decimals,"f"), exp(.data$estimate)),
        " (",
        sprintf(paste0("%.",number_decimals,"f"),
                exp(.data$estimate - .data$std.error * z)),
        "-",
        sprintf(paste0("%.",number_decimals,"f"),
                exp(.data$estimate + .data$std.error * z)),
        ")"
      ),
      Point_estimate = exp(.data$estimate),
      Lower_confidence_limit = exp(.data$estimate - .data$std.error * z),
      Upper_confidence_limit = exp(.data$estimate + .data$std.error * z)
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        "y.level",
        "term",
        "OR",
        "Point_estimate",
        "Lower_confidence_limit",
        "Upper_confidence_limit",
        "P_row"="p.value"
      ))
    )

  #Getting reference groups for factors
  func_table7 <- func_table4 |> dplyr::select(dplyr::all_of(new_expvars))
  func_table7_var_count <- ncol(func_table7)
  func_table7_levels <- lapply(func_table7, levels)

  # Due to unlist() behaves differently if there is only one variable in the
  # data compared to two or more, special steps are taken here to
  #   get the reference groups. Also, if all exposures are numeric (not
  #   factors), there are no rows in file, so it must be added
  func_table7_levels2_prp <- as.data.frame(unlist(func_table7_levels)) |>
    (\(tbl) dplyr::mutate(tbl, Variable_prp = rownames(tbl), .before = 1))() |>
    dplyr::mutate(
      count_digit1 = substr(
        .data$Variable_prp,
        nchar(.data$Variable_prp),
        nchar(.data$Variable_prp)
      ),
      Variable = substr(.data$Variable_prp, 1, (nchar(.data$Variable_prp) - 1))
    ) |>
    dplyr::filter(.data$count_digit1 == "1")
  if (nrow(func_table7_levels2_prp) > 0) {
    func_table7_levels2 <- dplyr::select(
      func_table7_levels2_prp,
      "Variable",
      "Reference" = "unlist(func_table7_levels)"
    )
  }
  else{
    func_table7_levels2 <- func_table7_levels2_prp |>
      dplyr::add_row(Variable = "") |>
      dplyr::mutate(Reference = "")
  }


  func_table7_var_types <- as.data.frame(
    t(as.data.frame(purrr::map(func_table7, class)))
  ) |>
    (\(tbl) dplyr::mutate(tbl, Variable = rownames(tbl), .before = 1))() |>
    dplyr::filter(.data$V1 %in% c("factor")) |>
    dplyr::select("Variable")
  # Making sure reference values shown for all outcomes (except the
  # "none-outcome")
  func_table7_final_prp <- dplyr::inner_join(
    func_table7_levels2,
    func_table7_var_types,
    by = "Variable"
  ) |>
    dplyr::mutate(
      term = paste0(.data$Variable, .data$Reference),
      OR = "1 (Ref)",
      Point_estimate = 1
    ) |>
    dplyr::select("term", "OR", "Point_estimate")

  if (outcome_levels == 2) {
    func_table7_final <- func_table7_final_prp
  }
  else if (outcome_levels > 2) {
    func_table7_final_prp2 <- dplyr::distinct(
      dplyr::select(func_table6, "y.level")
    )
    func_table7_final <- tidyr::crossing(
      func_table7_final_prp2,
      func_table7_final_prp
    )
  }

  # Due to problems withe the anova for survey-data, the p-values from a
  # survey-model is disregarded at the moment
  # if (surveydata == FALSE && outcome_levels == 2) {
  if (outcome_levels == 2) {
    # Adding the reference level of the exposure variable
    # (so all levels can be seen in output)
    func_table8_prp <- dplyr::bind_rows(func_table6, func_table7_final) |>
      dplyr::full_join(func_table5_p, by = c("term" = "Variable"))
    func_table8 <- func_table8_prp |>
      dplyr::mutate(
        sortnr = dplyr::case_when(
          grepl(":", .data$term) == TRUE ~ 2,
          .data$term == "(Intercept)" ~ 0,
          TRUE ~ 1
        )
      ) |>
      dplyr::arrange(.data$sortnr, .data$term)
  } else {
    # Adding the reference level of the exposure variable
    # (so all levels can be seen in output)
    func_table8_prp <- dplyr::bind_rows(func_table6, func_table7_final)
    func_table8 <- func_table8_prp |>
      dplyr::mutate(
        sortnr = dplyr::case_when(
          grepl(":", .data$term) == TRUE ~ 2,
          .data$term == "(Intercept)" ~ 0,
          TRUE ~ 1
        )
      ) |>
      dplyr::arrange(.data$sortnr, .data$term)
  }

  Full_model_info <- as.data.frame(
    paste(Outcome_type, Regression_type, Model_info, sep = " ")
  ) |>
    dplyr::mutate(term = "(Intercept)", sortnr = 0) |>
    dplyr::select("term", "Model_info" = 1, "sortnr")

  #Adding number of observations and model information
  func_table9 <-
    dplyr::full_join(
      func_table8,
      func_model_n,
      by = c("term", "sortnr")
    ) |>
    dplyr::full_join(Full_model_info, by = c("term", "sortnr")) |>
    dplyr::relocate("Model_info", .after = "N") |>
    dplyr::rename(dplyr::any_of(c("Outcome_level"="y.level"))) |>
    dplyr::arrange(
      dplyr::across(dplyr::any_of(c("Outcome_level", "sortnr", "term")))
    ) |>
    dplyr::select(-"sortnr")

  # Due to log-linear regression don't give Odds Ratios but Risk Ratios, result
  # variable OR is renamed RR in those cases
  if (regtype == "log-linear") {
    func_table9 <- func_table9 |> dplyr::rename("RR" = "OR")
  }

  # Check if there should be some text/description added and if so,
  # the text-variable is added
  if (is.null(textvar)) {
    return(func_table9)
  } else {
    #Adding text-variable
    func_table10 <- func_table9 |>
      dplyr::mutate(Description = textvar) |>
      dplyr::relocate("Description")
    return(func_table10)}
}


#' Summary function for svy_vglm objects
#'
#' Internal summary function for svy_vglm objects
#'
#' @param object An svy_vglm object
#' @param ... additional arguments. Not used.
#'
#' @return
#' A "summary.svy_vglm" object is returned.

summary.svy_vglm <- function(object, ...) {
  object$coeftable <- cbind(
    Coef = object$coef,
    SE = sqrt(diag(object$var)),
    z = object$coef / sqrt(diag(object$var)),
    p = pnorm(- abs(object$coef / sqrt(diag(object$var)))) * 2
  )
  class(object) <- "summary.svy_vglm"
  object
}
