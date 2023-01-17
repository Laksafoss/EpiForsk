#' Flatten Date Intervals
#'
#' A tidyverse compatible function for simplifying time interval data
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param id <[`data-masking`][dplyr_data_masking]> One unquoted expression
#' naming the id variable in data.
#' @param in_date <[`data-masking`][dplyr_data_masking]> One unquoted expression
#' naming the start date variable in data.
#' @param out_date <[`data-masking`][dplyr_data_masking]> One unquoted
#' expression naming the end date variable in data.
#' @param lag A numeric, giving the number of days allowed between time
#' intervals that should be collapsed into one.
#' @param status 123
#'
#' @returns
#' A data frame with the 'id' and simplified 'in_date' and 'out_date'.
#'
#' @details
#' This functions identifies overlapping time intervals within individual and
#' collapses them into distinct and disjoint intervals. If 'lag' is specified
#' then intervals must be more then 'lag' time units apart to be considered
#' distinct.
#'
#' @author
#' ADLS, EMTH & ASO
#'
#' @examples
#'
#' # The flatten function works with both dates and numeric
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3, 4),
#'    START = c(1, 2, 5, 3, 6, 2, 3, 6),
#'    END   = c(3, 3, 7, 4, 9, 3, 5, 8))
#' dat |> flatten_date_intervals(ID, START, END)
#'
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3),
#'    START = as.Date(c("2012-02-15", "2005-12-13", "2006-01-24",
#'                      "2002-03-14", "1997-02-27",
#'                      "2008-08-13", "1998-09-23")),
#'    END   = as.Date(c("2012-06-03", "2007-02-05", "2006-08-22",
#'                      "2005-02-26", "1999-04-16",
#'                      "2008-08-22", "2015-01-29")))
#' #at |> flatten_date_intervals(ID, START, END)
#'
#' @export
flatten_date_intervals <- function(data, id, in_date, out_date, status = NULL, lag = 0) {
  flat <- data |>
    dplyr::mutate(.numeric_in_date = as.numeric({{ in_date }}),
                  .numeric_out_date = as.numeric({{ out_date }})) |>
    dplyr::arrange(.data$.numeric_in_date, .data$.numeric_out_date) |>
    dplyr::group_by({{ id }}, {{ status }}) |>
    dplyr::mutate(.internal_running_index = c(
      0,
      cumsum(dplyr::lead(.data$.numeric_in_date) >
               cummax(.data$.numeric_out_date + lag))[-dplyr::n()])) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ id }}, {{ status }}, .data$.internal_running_index) |>
    dplyr::summarize(.interval_IN_DATE = min({{ in_date }}),
                     .interval_OUT_DATE = max({{ out_date }}),
                     .groups = "drop") |>
    dplyr::select(-.data$.internal_running_index) |>
    dplyr::rename({{ in_date }} := .data$.interval_IN_DATE,
                  {{ out_date }} := .data$.interval_OUT_DATE)
  if (lag == 0 | data |> dplyr::select({{ status }}) |> (\(x) ncol(x) == 0)()) {
    return(flat)
  }
  flat |>
    dplyr::mutate(
      .short = ({{ out_date }} - {{ in_date }} < lag),
      .first = {{ in_date }} == stats::ave({{ in_date }}, {{ id }}, FUN = min),
      .last = {{ out_date }} == stats::ave({{ out_date }}, {{ id }}, FUN = max)) |>
    dplyr::filter(!.data$.short | .data$.first | .data$.last) |>
    dplyr::select(-.data$.short, -.data$.first, -.data$.last)
}
