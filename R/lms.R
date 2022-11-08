#' Linear model with sibling design
#'
#' Fit a linear model using demeaned data. Useful for sibling design.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param formula A numeric, giving the number of days allowed between time
#' intervals that should be collapsed into one.
#' @param grp_id <[`data-masking`][dplyr_data_masking]> One unquoted expression
#' naming the id variable in data defining the groups to demean,
#' e.g. sibling groups.
#' @param obs_id <[`data-masking`][dplyr_data_masking]> Optional, One unquoted
#' expression naming an id variable to keep track of the input data order.
#' @param trans_out Whether to include the transformed data in the output. If
#' TRUE, output willinclude the transformed model matrix and the transformed
#' outcome.
#'
#' @returns
#' A list with class "lms".
#'
#' @details
#' TODO
#'
#' # Author(s)
#' KIJA
#'
#' @examples
#'
#' TODO
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export

lms <- function(data, formula, grp_id, obs_id = NULL, trans_out = TRUE) {
  library(data.table)
  grp_id <- rlang::ensym(grp_id)
  tryCatch(obs_id <- rlang::ensym(obs_id), error = function(e) e)
  if(length(formula) < 3) {rlang::abort("formula must have a LHS")}
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
  if(!is.null(obs_id)) {
    model_matrix <- model_matrix %>%
      dplyr::bind_cols(
        data %>% dplyr::select({{ obs_id }})
      )
  }
  # demean data by grp_id
  if(is.null(obs_id)) {
    glo_len <- .GlobalEnv$len
    .GlobalEnv$len <- length(model_matrix) - 1
    model_matrix_trans <- model_matrix %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ grp_id }}) %>%
      dplyr::transmute(
        dplyr::across(
          seq_len(len),
          function(x) x - mean(x)
        )
      ) %>%
      tibble::as_tibble()
    if(is.null(glo_len)) {
      rm(len, pos = .GlobalEnv)
    } else {
      .GlobalEnv$len <- glo_len
    }
  } else {
    glo_len <- .GlobalEnv$len
    .GlobalEnv$len <- length(model_matrix) - 2
    model_matrix_trans <- model_matrix %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ grp_id }}) %>%
      dplyr::transmute(
        obs_id = .data[[!!obs_id]],
        dplyr::across(
          seq_len(len),
          function(x) x - mean(x)
        )
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        {{ obs_id }} := obs_id
      )
    if(is.null(glo_len)) {
      rm(len, pos = .GlobalEnv)
    } else {
      .GlobalEnv$len <- glo_len
      }
  }
  # demean outcome by grp_id
  glo_form <- .GlobalEnv$form
  .GlobalEnv$form <- formula[[2]]
  if(is.null(obs_id)) {
    outcome_trans <- data %>%
      dtplyr::lazy_dt() %>%
      dplyr::group_by({{ grp_id }}) %>%
      dplyr::transmute(
        outcome := .data[[form]] - mean(.data[[form]])
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        !!sym(formula[[2]]) := outcome
      )
  } else {
  outcome_trans <- data %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by({{ grp_id }}) %>%
    dplyr::transmute(
      obs_id = .data[[!!obs_id]],
      outcome := .data[[form]] - mean(.data[[form]])
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      {{ obs_id }} := obs_id,
      !!sym(formula[[2]]) := outcome
    )
  }
  if(is.null(glo_form)) {
    rm(form, pos = .GlobalEnv)
  } else {
    .GlobalEnv$form <- glo_form
  }
  # combine data
  if(!is.null(obs_id)) {
    mod_data <- dplyr::left_join(
      model_matrix_trans,
      outcome_trans,
      by = c(str_c(obs_id), str_c(grp_id))
    )
  } else {
    mod_data <- dplyr::bind_cols(
      model_matrix_trans,
      outcome_trans %>% dplyr::select(-grp_id)
    )
  }
  # OLS model fitting demeaned data
  mod <- lm(
    formula = formula(str_c(formula[[2]], "~ . - ", formula[[2]], " - 1")),
    data = mod_data %>%
      dplyr::select(-c(tidyselect::all_of(obs_id), tidyselect::all_of(grp_id)))
  )

  # return OLS model, model matrix used, and outcome data used
  if(trans_out) {
  out <- list(
    mod = mod,
    model_matrix_trans = model_matrix_trans,
    outcome_trans = outcome_trans,
    trans_out = trans_out
  )
  } else {
    out <- list(
      mod = mod,
      trans_out = trans_out
    )
  }
  class(out) <- "lms"
  return(out)
}
