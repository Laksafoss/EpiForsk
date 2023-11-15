#Function intended for making it possible to get OR for several similar models in one go,
#   where either same analysis is made except for one variable or the same analysis done but
#   by a variable (each level of the variable is analysed separately).
#The function is a kind of wrapper for the OR_function() with similar syntax but
#   some additional arguments have been added and the MEANING of one of the arguments
#   have changed compared to the original function:
#   1)  outcomevar is here a vector of outcome-variables of interest that will each be run,
#         with separate models. Note that all outcomes should be factors when calling the
#         function. (at least one variable needed) (example: c("height_grp","weight_grp","bmi_grp"))
#   2)  expvars is here a vector with variable names that will be run in separate models
#         (at least one variable needed)
#         (example: c("var1", "var2") will be run in two models, one including "var1" and
#         another including "var2")
#   3)  adjustment_fixed is a vector for all variables that will not change between models,
#         usually some fixed adjustment for models (optional) (example: c("age","sex") will give
#         models adjusted for age and sex)
#   4)  by_var (optional) is the possibility to run the analyses completely separate for all
#         levels of the specified variable. It only works with one variable (example: by_var="Grouping")
#         NOTE: NA and "" levels will not be used but all other levels will have separate models
#   5)  It's possible to have same variable in expvars() and adjustment_fixed()
#   6)  When a model results in an error, the function will not stop - it continues with other
#         models until done BUT in the output the error text can be seen.
#Other restrictions from the OR_function() is still relevant here as well as all other options

#' Title
#'
#' @param normaldata
#' @param outcomevar
#' @param expvars
#' @param adjustment_fixed
#' @param by_var
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
#'
#' @author ASO
#'
#' @examples
#'
#' @export

odds_ratio_function_repeated <- function(
    normaldata,
    outcomevar,
    expvars,
    adjustment_fixed = NULL,
    by_var = NULL,
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
  new_expvars_prp <- expvars
  new_expvars_prp2 <- unlist(strsplit(new_expvars_prp, ":"))
  new_expvars <- unlist(strsplit(new_expvars_prp2, "[*]"))
  func_var_names <- unique(
    c(outcomevar, new_expvars, adjustment_fixed, by_var, weightvar)
  )

  func_table1 <- normaldata |>
    dplyr::select(dplyr::all_of(func_var_names))

  if (is.null(by_var)){
    by_var_level_count <- 1
  } else {
    by_var2 <- dplyr::pull(
      unique(dplyr::distinct(dplyr::select(func_table1, {{ by_var }})))
    )
    if (is.numeric(by_var2)){
      by_var3 <- by_var2[!is.na(by_var2)]
    }
    if (is.factor(by_var2) | is.character(by_var2)){
      by_var3 <- as.character(by_var2[!is.na(by_var2) & by_var2 != ""])
    }
    by_var_level_count <- length(by_var3)
  }

  outcome_var_count <- length(outcomevar)
  expvars_var_count <- length(expvars)

  k <- 1
  while (k <= by_var_level_count){
    i <- 1
    if (is.null(by_var)){
      By_var_name <- c("None")
      func_table1_2 <- func_table1
    } else {
      by_var_level <- dplyr::nth(by_var3, n = k)
      By_var_name <- paste(by_var, "=", by_var_level)
      print(paste0("By_var: ", By_var_name))
      func_table1_2 <- dplyr::filter(
        func_table1,
        as.character(.data[[by_var]]) == by_var_level
      )
    }

    while (i <= outcome_var_count){
      j <- 1
      Outcome_var_name <- dplyr::nth(outcomevar, n = i)
      print(paste0("Outcome: ", Outcome_var_name))

      while (j <= expvars_var_count){
        Expvar_var_name <- dplyr::nth(expvars, n = j)
        print(paste0("Expvar: ", Expvar_var_name))
        new_expvar <- unique(c(Expvar_var_name, adjustment_fixed))
        func_res1 <- try_catch_warnings(
          odds_ratio_function(
            normaldata = func_table1_2,
            outcomevar = Outcome_var_name,
            expvars = new_expvar,
            number_decimals = number_decimals,
            alpha = alpha,
            regtype = regtype,
            matchgroup = matchgroup,
            matchtiemethod = matchtiemethod,
            values_to_remove = values_to_remove,
            weightvar = weightvar,
            surveydata = surveydata,
            textvar = textvar,
            model_object = model_object
          ),
          character = TRUE
        )
        if (model_object == FALSE & func_res1$error == ''){
          func_res2_prp <- func_res1$value |>
            dplyr::mutate(
              By_name = By_var_name,
              Outcome_name = Outcome_var_name,
              Expvar_name = Expvar_var_name
            )
          if (func_res1$warning != ''){
            func_res2 <- func_res2_prp |>
              dplyr::mutate(
                Warning = dplyr::case_when(
                  .data$term == "(Intercept)" ~ paste0(func_res1$warning),
                  TRUE ~ ""
                )
              )
          } else {
            func_res2 <- func_res2_prp
          }
          if (k==1 & i==1 & j==1){
            func_table2 <- func_res2
          } else {
            func_table2 <- dplyr::bind_rows(func_table2, func_res2)
          }
        } else if (model_object == TRUE & func_res1$error == ''){
          if (k==1 & i==1 & j==1){
            func_table2 <- c(
              "By" = By_var_name,
              "Outcome" = Outcome_var_name,
              "Expvar" = Expvar_var_name,
              "Warning" = func_res1$warning,
              func_res1$value
            )
          } else {
            func_table2 <- c(
              func_table2,
              "By" = By_var_name,
              "Outcome" = Outcome_var_name,
              "Expvar" = Expvar_var_name,
              "Warning" = func_res1$warning,
              func_res1$value
            )
          }
        } else if (model_object == FALSE & func_res1$error != ''){
          func_res2 <- as.data.frame(func_res1$error) |>
            dplyr::rename(Error = 1) |>
            dplyr::mutate(
              By_name = By_var_name,
              Outcome_name = Outcome_var_name,
              Expvar_name = Expvar_var_name
            ) |>
            dplyr::relocate("Error", .after = dplyr::last_col())
          if (k == 1 & i == 1 & j == 1){
            func_table2 <- func_res2
          } else {
            func_table2 <- dplyr::bind_rows(func_table2, func_res2)
          }
        } else if (model_object == TRUE & func_res1$error != ''){
          if(k == 1 & i == 1 & j == 1){
            func_table2 <- c(
              "By" = By_var_name,
              "Outcome" = Outcome_var_name,
              "Expvar" = Expvar_var_name,
              "Error" = func_res1$error
            )
          } else {
            func_table2 <- c(
              func_table2,
              "By" = By_var_name,
              "Outcome" = Outcome_var_name,
              "Expvar" = Expvar_var_name,
              "Error" = func_res1$error
            )
          }
        }
        j <- (j + 1)
      }
      i <- (i + 1)
    }
    k <- (k+1)
  }

  if (model_object == FALSE) {
    func_table3 <- func_table2 |>
      dplyr::relocate("Expvar_name", .before = 1) |>
      dplyr::relocate("Outcome_name", .before = 1) |>
      dplyr::relocate("By_name", .before = 1)
  } else{
    func_table3 <- func_table2
  }
  return(func_table3)
}

# #Examples
# #Data to use
# data("infert")
# infert2 <- infert |>
#   mutate(Age_grp=case_when(age<25 ~ "<25", 25<=age & age<35 ~ "25-<35", age>=35 ~ "35+"),
#          Parity_grp=case_when(parity==1 ~ "1", parity>=2 & parity<=3 ~ "2-3", parity>3 ~ "4+"))
# infert2$Age_grp <- relevel(as.factor(infert2$Age_grp), ref="25-<35")
# infert2$Parity_grp <- relevel(as.factor(infert2$Parity_grp), ref="2-3")
# infert2$induced <- relevel(as.factor(infert2$induced), ref="0")
# infert2$case <- relevel(as.factor(infert2$case), ref="0")
# infert2$spontaneous <- relevel(as.factor(infert2$spontaneous), ref="0")
#
# #Two outcomes (Parity_grp, case) with their own set of models,
# #three variables included in separate models (spontaneous,induced and education) and
# #one variable that is included in all models (Age_grp)
# test <- OR_function_repeated(normaldata = infert2, outcomevar = c("Parity_grp","case"),
#                              expvars = c("spontaneous","induced","education"),
#                              adjustment_fixed = c("Age_grp"))

# #One outcome (case), two variables included in separate models (spontaneous and induced),
# #one variable included in all models (Age_grp) and all analyses made for each
# #level of another variable (Parity_grp)
# test2 <- OR_function_repeated(normaldata = infert2, outcomevar = c("case"),
#                               expvars = c("spontaneous","induced"),
#                               adjustment_fixed = c("Age_grp"),
#                               by_var = "Parity_grp")



