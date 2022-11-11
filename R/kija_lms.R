#' Wrapper around lm for sibling design
#'
#' Fits a linear model using demeaned data. Useful for sibling design.
#'
#' @param formula A formula, used to create a model matrix with demeaned columns.
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param grp_id <[`data-masking`][dplyr_data_masking]> One unquoted expression
#' naming the id variable in data defining the groups to demean,
#' e.g. sibling groups.
#' @param obs_id <[`data-masking`][dplyr_data_masking]> Optional, One unquoted
#' expression naming an id variable to keep track of the input data order.
#' @param ... Additional arguments to be passed to \link[stats]{lm}().
#'
#' @returns
#' A list with class (lms, lm). Contains the output from lm applied to
#' demeaned data according to formula, the original data, and the provided
#' formula.
#'
#' @details
#' TODO
#'
#' # Author(s)
#' KIJA
#'
#' @examples
#'
#' sib_id <- sample(2000, 10000, replace = TRUE)
#' sib_out <- rnorm(2000)
#' x1 <- rnorm(10000)
#' x2 <- rnorm(10000) + sib_out[sib_id] + x1
#' y <- rnorm(10000, 1, 0.5) + 2 * sib_out[sib_id] - x1 + 2 * x2
#' data <- data.frame(
#' x1 = x1,
#' x2 = x2,
#' y = y,
#' sib_id = sib_id,
#' obs_id = 1:10000
#' )
#' mod_lm <- lm(y ~ x1 + x2, data)
#' mod_lms <- lms(y ~ x1 + x2 - 1, data, sib_id, obs_id)
#' summary(mod_lm)
#' summary(mod_lms)
#' print(mod_lms)
#'
#' @export

lms <- function (formula, data, grp_id, obs_id = NULL, ...)
{
  grp_id <- rlang::ensym(grp_id)
  tryCatch(obs_id <- rlang::ensym(obs_id), error = function(e) e)
  if (length(formula) < 3) rlang::abort("formula must have a LHS")
  # create model matrix from formula:
  model_matrix <- model.matrix(
    formula[-2],
    data = data
  ) %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(
      data %>% dplyr::select({{ grp_id }})
    )
  # add obs_id variable to model_matrix:
  if (!is.null(obs_id)) {
    model_matrix <- model_matrix %>%
      dplyr::bind_cols(
        data %>% dplyr::select({{ obs_id }})
      )
  }
  # demean data by grp_id
  model_matrix_trans <- data.table::as.data.table(model_matrix)
  model_matrix_trans <- eval(
    rlang::parse_expr(
      paste0(
        "model_matrix_trans[, list(",
        if (!is.null(obs_id)) {
          paste0(
            obs_id,
            " = ",
            obs_id,
            ", "
          )
        },
        paste0(
          "`",
          names(model_matrix_trans)[
            seq_len(length(model_matrix_trans) - 1 - !is.null(obs_id))
          ],
          "` = (function(x) x - mean(x))(`",
          names(model_matrix_trans)[
            seq_len(length(model_matrix_trans) - 1 - !is.null(obs_id))
          ],
          collapse = "`), "
        ),
        "`)), keyby = list(",
        grp_id,
        ")]"
      )
    )
  )
  model_matrix_trans <- tibble::as_tibble(model_matrix_trans)

  # demean outcome by grp_id
  outcome_trans <- data.table::as.data.table(
    data %>%
      dplyr::select(
        !!formula[[2]],
        tidyselect::all_of(grp_id),
        tidyselect::all_of(obs_id)
      )
  )
  outcome_trans <- eval(
    rlang::parse_expr(
      paste0(
        "outcome_trans[, list(",
        if (!is.null(obs_id)) {
          paste0(
            names(outcome_trans)[length(outcome_trans)],
            " = ",
            names(outcome_trans)[length(outcome_trans)],
            ", "
          )
        },
        formula[[2]],
        " = ",
        formula[[2]],
        " - mean(",
        formula[[2]],
        ")), keyby = list(",
        grp_id,
        ")]"
      )
    )
  )
  outcome_trans <- tibble::as_tibble(outcome_trans)
  # combine data
  if (!is.null(obs_id)) {
    mod_data <- dplyr::left_join(
      model_matrix_trans,
      outcome_trans,
      by = c(paste0(obs_id), paste0(grp_id))
    )
  } else {
    mod_data <- dplyr::bind_cols(
      model_matrix_trans,
      outcome_trans %>% dplyr::select(-tidyselect::all_of(grp_id))
    )
  }
  # OLS model fitting demeaned data
  mod <- lm(
    formula = formula(paste0(formula[[2]], "~ . - 1")),
    data = mod_data %>%
      dplyr::select(-c(tidyselect::all_of(obs_id), tidyselect::all_of(grp_id))),
    ...
  )

  # return enriched OLS model
  out <- c(
    unclass(mod),
    list(
      formula = formula,
      data = data,
      grp_id = model_matrix_trans[grp_id], # follows modified order in model
      obs_id = if (!is.null(obs_id)) model_matrix_trans[obs_id]
    )
  )
  class(out) <- c("lms", "lm")
  return(out)
}
