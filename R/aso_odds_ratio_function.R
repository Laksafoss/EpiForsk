#The function ASO_OR_function_v3 is intended to make it easier to perform logistic and log-linear regressions
#   and giving a standardized output table using several different packages and functions depending on chosen options.

#The function uses data and tries to analyse it given user specifications, including outcome, exposures and possible weights.
#The function can also handle survey-data, but not complex sampling schemes (if specified as survey-data, the model will create
#   a simple survey-object from the data, using weights as specified - if not specified, the weights are 1 for each observation)
#The standard regression is logistic regression (yielding Odds Ratios=OR) but it is possible to perform a log-linear regression
#   (yielding Risk Ratios=RR) instead, if specified and requirements are met

#The options and specifications are as follows:
# 1)  normaldata = data.frame to use with function (must be specified with no default)
# 2)  outcomevar = name of factor variable in data that will be used as the outcome. (must be specified with no default AND
#       there must be citation marks around the name): outcomevar = "Var"
# 3)  expvars = a vector with the names of the exposure variables (numeric or factors) and may also include interactions.
#       (must be specified and have citation marks around all elements): c("Var1", "Var2", "Var1:Var2", "Var3*Var4")
# 4)  number_decimals = an integer telling how many decimals to show in the standardized output. (optional, standard is two):
#       number_decimals=2
# 5)  alpha = a value between 0 and 1 witch will influence the width of the confidence limits. (optional, standard is 0.05
#       which will yield the usual 95% confidence limits): alpha=0.05
# 6)  regtype = a vector option to select if the analysis should be logistic regression or log-linear regression. Log-linear
#       regression can only be used when having binomial, unconditional analysis. (optional, standard is logistic regression):
#       regtype = c("logistic") or regtype = c("log-linear")
# 7)  matchgroup = an option to condition the analysis on a specific variable. can only be used in binomial logistic
#       regression models (optional, standard is NULL but if given, the variable needs to be given within citation marks):
#       matchgroup="condition_var"
# 8)  matchtiemethod = an option to change method for ties when using a matched/conditional analysis. "exact" is used
#       as a standard BUT this option does not take weights into account for the analysis, so if weights (other than 1) is
#       to be used, another option should be selected (possible: "exact", "approximate", "efron", "breslow" - for further
#       explanations, see documentation for clogit() from the survival package). (optional, standard is "exact"):
#       matchtiemethod = c("exact") or matchtiemethod = c("approximate") or matchtiemethod = c("efron") or
#       matchtiemethod = c("breslow")
# 9)  values_to_remove = a vector option to remove one or several values from ALL variables used in the regression before
#       the analysis. This might be useful if some value(s) are used consistently for missing/irrelevant in the data -
#       normal missing (NA) don't need to be specified here as it will be removed automatically anyway from the analysis.
#       (optional, standard is NULL, but if used should be given as a vector with each value within citation marks):
#       values_to_remove = c("888","987") Do NOT remove the reference values due to this might give unexpected results!
# 10) weightvar = an option to use pre-calculated weights for observations in the analysis. (optional, standard is NULL and hence
#       uses weight=1 for all observations - if specified a variable name should be given within citation marks):
#       weightvar = "weight_variable"
# 11) surveydata = an option to specify if the data comes from a survey or not. (optional, standard is FALSE):
#       surveydata = FALSE or surveydata = TRUE
# 12) textvar = an option to add some extra text (like a note) to the output. (optional, standard is null, but if given it should
#       be as a text surrounded by citation marks): textvar = "This is some text"
# 13) model_object = an option to get the raw output object from the analysis instead of the standard output. This might be useful
#       if wanting to see something else than the information included in the standardized output (optional, the standard is FALSE):
#       model_object = FALSE or model_object = TRUE


#' Title
#'
#' @param normaldata
#' @param outcomevar
#' @param expvars
#' @param number_decimals
#' @param alpha
#' @param regtype
#' @param matchgroup
#' @param matchtiemethod
#' @param values_to_remove
#' @param weightvar
#' @param surveydata
#' @param textvar
#' @param model_object
#'
#' @return
#' @export
#'
#' @examples
odds_ratio_function <- function(
    normaldata,
    outcomevar,
    expvars,
    number_decimals = 2,
    alpha = 0.05,
    regtype = c("logistic"),
    matchgroup = NULL,
    matchtiemethod = c("exact"),
    values_to_remove = NULL,
    weightvar = NULL,
    surveydata = FALSE,
    textvar = NULL,
    model_object = FALSE
){
  # Some logic checks to see if some of the specifications are supported -
  # else an error with a message is shown
  if (regtype != "logistic" & regtype != "log-linear") {
    stop(
      "The regtype must be specified as 'logistic' (standard) or 'log-linear'"
    )
  } else if (surveydata != FALSE & !is.null(matchgroup)){
    stop(
      paste0(
        "The combination of using surveydata and conditioning/matching is",
        " not supported"
      )
    )
  }
  if (regtype == "log-linear" & !is.null(matchgroup)) {
    stop("When regtype is set to 'log-linear', no conditioning is supported")
  }
  if (regtype == "log-linear" & surveydata == TRUE) {
    stop("When regtype is set to 'log-linear', surveydata is not supported")
  }

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
      "Outc" = tidyselect::all_of(outcomevar),
      tidyselect::all_of(new_expvars),
      "matchID" = tidyselect::all_of(matchgroup),
      "weight_used" = tidyselect::all_of(weightvar)
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
    dplyr::filter(dplyr::if_all(tidyselect::all_of(used_var), ~ !is.na(.x)))

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
          tidyselect::all_of(used_var),
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
  if (outcome_levels < 2){
    stop("The function was stopped due to the outcome is the same for all!")
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
    dplyr::group_by(.data$Outc) |>
    dplyr::summarize(
      Freq = dplyr::n(),
      Freqw = sum(.data$weight_used),
      .groups = "drop"
    ) |>
    tibble::rownames_to_column(var = "sortnr") |>
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
    dplyr::left_join(func_model_n_prp3_1, by = c("sortnr")) |>
    dplyr::left_join(func_model_n_prp3_2, by = c("sortnr"))

  # Binomial outcome
  if (outcome_levels == 2){

    Outcome_type <- c("Binomial") # Used to create information of model used

    # "Normal" binomial logistic regression
    if (regtype == "logistic"){

      if (surveydata == FALSE & is.null(matchgroup)){

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

        #Extract estimates and standard error etc. from model output
        func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

        #Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
          tibble::rownames_to_column(var = "Variable") |>
          dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
          dplyr::filter(!is.na(.data$P_anova)) |>
          dplyr::rename("P_drop1" = "P_anova")

      } else if (surveydata == FALSE){

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

        # Extract estimates and standard error etc. from model output
        func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

        # Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
          tibble::rownames_to_column(var = "Variable") |>
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
          by = c("matchnum")
        ) |>
          dplyr::mutate(
            term = "(Intercept)",
            n_nonevent = as.character(
              as.numeric(.data$`func_table5_prp$n`) -
                as.numeric(.data$`func_table5_prp$nevent`)
            ),
            N = paste0(
              "Non-outcome=", .data$non_outcome_level,
              " (n=", .data$n_nonevent,")",
              ", Outcome=", .data$outcome_level,
              " (n=", .data$`func_table5_prp$nevent`,")",
              ", Total n=", .data$`func_table5_prp$n`
            ),
            sortnr = 0
          ) |>
          dplyr::select("term", "N", "sortnr")
      } else if (surveydata == TRUE){
        #Logistic regression with data from survey

        Regression_type <- c("logistic regression with surveydata,") #Used to create information of model used
        Model_info <- c("svyglm(), survey package") #Used to create information of model used

        # Getting the non reference level of the outcome variable in the
        # binomial model
        outcome_level <- as.character(levels(func_table4$Outc)[2])

        # Creating a survey object (i.e. a data frame with weights etc. that R
        # recognizes as survey data)
        func_table5_prp <- survey::svydesign(
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
        func_table5_prp2 <- survey::svyglm(
          Model_call,
          design = func_table5_prp,
          family = quasibinomial(link="logit")
        )
        func_table5 <- broom::tidy(func_table5_prp2, exponentiate = FALSE)

        #Getting p-values for included components in model
        func_table5_p <- drop1(func_table5_prp2, test = "Chisq") |>
          tibble::rownames_to_column(var = "Variable") |>
          dplyr::select("Variable", "P_anova" = "Pr(>Chi)") |>
          dplyr::filter(!is.na(.data$P_anova)) |>
          dplyr::rename("P_drop1" = "P_anova")
      }
    } else if (regtype == "log-linear"){

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
      func_table5 <- broom::tidy(func_table5_prp, exponentiate = FALSE)

      # Getting p-values for included components in model
      func_table5_p <- drop1(func_table5_prp, test = "Chisq") |>
        tibble::rownames_to_column(var = "Variable") |>
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

      #Getting coefficients etc.
      func_table5_prp2 <- as.data.frame(
        summary(func_table5_prp)$coeftable
      ) |>
        tibble::rownames_to_column(var = "term_prp") |>
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
        tibble::rownames_to_column(var = "Outcome_order")
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
                exp(.data$estimate - .data$std.error * .data$z)),
        "-",
        sprintf(paste0("%.",number_decimals,"f"),
                exp(.data$estimate + .data$std.error * .data$z)),
        ")"
      ),
      Point_estimate = exp(.data$estimate),
      Lower_confidence_limit = exp(.data$estimate - .data$std.error * .data$z),
      Upper_confidence_limit = exp(.data$estimate + .data$std.error * .data$z)
    ) |>
    dplyr::select(
      tidyselect::any_of(c(
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
  func_table7 <- func_table4 |> dplyr::select(tidyselect::all_of(new_expvars))
  func_table7_var_count <- ncol(func_table7)
  func_table7_levels <- lapply(func_table7, levels)

  # Due to unlist() behaves differently if there is only one variable in the
  # data compared to two or more, special steps are taken here to
  #   get the reference groups. Also, if all exposures are numeric (not
  #   factors), there are no rows in file, so it must be added
  func_table7_levels2_prp <- as.data.frame(unlist(func_table7_levels)) |>
    tibble::rownames_to_column(var = "Variable_prp") |>
    dplyr::mutate(
      count_digit1 = substr(
        .data$Variable_prp,
        nchar(.data$Variable_prp),
        nchar(.data$Variable_prp)
      ),
      Variable = substr(.data$Variable_prp, 1, (nchar(.data$Variable_prp) - 1))
    ) |>
    dplyr::filter(.data$count_digit1 == "1")
  if(nrow(func_table7_levels2_prp) > 0) {
    func_table7_levels2 <- dplyr::select(
      "func_table7_levels2_prp",
      "Variable",
      "Reference" = "unlist(func_table7_levels)"
    )
  }
  else{
    func_table7_levels2 <- tibble::add_row(func_table7_levels2_prp, Variable = "") |>
      dplyr::mutate(Reference = "")
  }


  func_table7_var_types <- as.data.frame(
    t(as.data.frame(purrr::map(func_table7, class)))
  ) |>
    tibble::rownames_to_column(var = "Variable") |>
    dplyr::filter(.data$V1 %in% c("factor")) |>
    dplyr::select("Variable")
  # Making sure reference values shown for all outcomes (except the
  # "none-outcome")
  func_table7_final_prp <- dplyr::inner_join(
    func_table7_levels2,
    func_table7_var_types,
    by = c("Variable")
  ) |>
    dplyr::mutate(
      term = paste0(.data$Variable, .data$Reference),
      OR = "1 (Ref)",
      Point_estimate = 1
    ) |>
    dplyr::select("term", "OR", "Point_estimate")

  if (outcome_levels == 2){
    func_table7_final <- func_table7_final_prp
  }
  else if (outcome_levels > 2){
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
  # if(surveydata==FALSE & outcome_levels==2){
  if (outcome_levels == 2){
    # Adding the reference level of the exposure variable
    # (so all levels can be seen in output)
    func_table8_prp <- dplyr::bind_rows(func_table6, func_table7_final) |>
      dplyr::full_join(func_table5_p, by = c("term" = "Variable"))
    func_table8 <- func_table8_prp |>
      dplyr::mutate(
        sortnr = dplyr::case_when(
          str_detect(.data$term, ":") == TRUE ~ 2,
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
          str_detect(.data$term,":") == TRUE ~ 2,
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
  func_table9 <- dplyr::full_join(
    func_table8,
    func_model_n,
    by = c("term", "sortnr")
  ) |>
    dplyr::full_join(Full_model_info, by = c("term", "sortnr")) |>
    dplyr::relocate("Model_info", .after = "N") |>
    dplyr::rename(tidyselect::any_of(c("Outcome_level"="y.level"))) |>
    dplyr::arrange(
      dplyr::across(tidyselect::any_of(c("Outcome_level", "sortnr", "term")))
    ) |>
    dplyr::select(-"sortnr")

  # Due to log-linear regression don't give Odds Ratios but Risk Ratios, result
  # variable OR is renamed RR in those cases
  if (regtype == "log-linear"){
    func_table9 <- func_table9 |> dplyr::rename("RR" = "OR")
  }

  # Check type of output requested and if it's the "normal" output, if there
  # should be some text/description added and if so,
  #   the text-variable is added
  if (model_object == TRUE){
    return(func_table5_prp)
  } else if (is.null(textvar)) {
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

# #Examples
# #Binomial outcome
# data(logan)
#
# resp <- levels(logan$occupation)
# n <- nrow(logan)
# indx <- rep(1:n, length(resp))
# logan2 <- data.frame(logan[indx,],
#                      id = indx,
#                      tocc = factor(rep(resp, each=n)))
# logan2$case <- (logan2$occupation == logan2$tocc)
# logan2$case <- as.factor(logan2$case)
# logan2$case <- relevel(logan2$case, ref="FALSE")
#
# #clogit(case ~ tocc + tocc:education + strata(id), logan2)
#
# #Standard binomial logistic regression but using interaction for exposures
# func_est1 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "education", "tocc:education"))
# #Conditional binomial logistic regression (with som extra text added)
# func_est2 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "tocc:education"), matchgroup = "id",
#                             textvar = "Testing function")
# #Standard binomial logistic regression as surveydata with no prepared weights
# func_est3 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "education"), surveydata = TRUE)
#
# #Example changing alpha (in practice changing confidence interval) and the number of decimals in fixed output and adding some text:
# #   those options will not change the models in any way but will change what is in the output
# func_est4 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "education"), number_decimals = 5, alpha = 0.01,
#                             textvar = "Testing function")
#
# #Getting RAW output from regression
# func_est5 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "education"), model_object = TRUE)
#
# #Polytomous/multinomial outcome
# data(api)
# #As normal data, but using weights
# func_est6 <- OR_function_v3(apiclus2, outcomevar = "stype", expvars = c("ell", "meals", "mobility", "sch.wide"),
#                             weightvar = "pw")
# #As survey data with weights
# func_est7 <- OR_function_v3(apiclus2, outcomevar = "stype", expvars = c("ell", "meals", "mobility"),
#                             weightvar = "pw", surveydata = TRUE)
#
# #Binomial logistic regression with same data (by removing all observations with a specific value of outcome)
# func_est8 <- OR_function_v3(apiclus2, outcomevar = "stype", expvars = c("ell", "meals", "mobility"),
#                             weightvar = "pw", values_to_remove = c("E"))
#
# #Example changing alpha (in practice changing confidence interval) and the number of decimals in fixed output and adding some text:
# #   those options will not change the models in any way but will change what is in the output
# func_est9 <- OR_function_v3(logan2, outcomevar = "case", expvars = c("tocc", "education"), number_decimals = 5, alpha = 0.01,
#                             textvar = "Testing function")


