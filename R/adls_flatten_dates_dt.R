#' Flatten Date Intervals
#'
#' A data.table compatible function for simplifying time interval data
#'
#' @param data A list, A data frame, or a data frame extension (e.g. a tibble) coercible to data.table.
#' @param id  One or more unquoted expression naming the id variables in data.
#' @param in_date One unquoted expressions naming the start date variable in data.
#' @param out_date One unquoted expression naming the end date variable in data.
#' @param status One or more unquoted expressions naming a status variable in data, such as region or
#'   hospitalization reason.
#' @param lag A numeric, giving the number of days allowed between time
#'   intervals that should be collapsed into one.
#'
#' @returns
#' A data.table with the `id`, `status` if specified and simplified `in_date`
#' and `out_date`. The returned data is sorted by `id` and `in_date`.
#'
#' @details
#' This functions identifies overlapping time intervals within individual and
#' collapses them into distinct and disjoint intervals. When `status` is
#' specified these intervals are both individual and status specific.
#'
#' If `lag` is specified then intervals must be more then `lag` time units apart
#' to be considered distinct.
#'
#'
#' @author
#' ADLS, EMTH & ASO
#'
#' @examples
#'
#' ### The flatten function works with both dates and numeric
#'
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3, 4),
#'    START = c(1, 2, 5, 3, 6, 2, 3, 6),
#'    END   = c(3, 3, 7, 4, 9, 3, 5, 8))
#' dat |> FlattenDatesDT(ID, START, END)
#'
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3, 4, 4),
#'    START = as.Date(c("2012-02-15", "2005-12-13", "2006-01-24",
#'                      "2002-03-14", "1997-02-27",
#'                      "2008-08-13", "1998-09-23",
#'                      "2005-01-12", "2007-05-10")),
#'    END   = as.Date(c("2012-06-03", "2007-02-05", "2006-08-22",
#'                      "2005-02-26", "1999-04-16",
#'                      "2008-08-22", "2015-01-29",
#'                      "2007-05-07", "2008-12-12")))
#' dat |> FlattenDatesDT(ID, START, END)
#'
#'
#'
#' ###  Allow for a 5 days lag between
#'
#' dat |> FlattenDatesDT(ID, START, END, lag = 5)
#'
#'
#'
#' ### Adding status information
#'
#' dat <- data.frame(
#'    ID     = c(1, 1, 1, 2, 2, 3, 3, 4, 4),
#'    START  = as.Date(c("2012-02-15", "2005-12-13", "2006-01-24",
#'                       "2002-03-14", "1997-02-27",
#'                       "2008-08-13", "1998-09-23",
#'                       "2005-01-12", "2007-05-10")),
#'    END    = as.Date(c("2012-06-03", "2007-02-05", "2006-08-22",
#'                       "2005-02-26", "1999-04-16",
#'                       "2008-08-22", "2015-01-29",
#'                      "2007-05-07", "2008-12-12")),
#'    REGION = c("H", "H", "N", "S", "S", "M", "N", "S", "S"))
#'
#' dat |> FlattenDatesDT(ID, START, END, REGION)
#'
#' @export
FlattenDatesDT <- function(
    data,
    id,
    in_date,
    out_date,
    status = NULL,
    lag = 0
) {
  # Copy data
  data <- data.table::copy(data)

  # Coerce to data.table
  data.table::setDT(data)

  # Figure out column names
  id <- deparse(substitute(id))
  in_date <- deparse(substitute(in_date))
  out_date <- deparse(substitute(out_date))

  # Build grouping keys
  if(!missing(status)) {
    status <- deparse(substitute(status))
    id <- c(id, status)
  }

  # Order within each group
  data.table::setorderv(data, c(id, in_date, out_date))

  # Mark merge groups by cumsum of gaps > lag
  data[
    ,
    merge_id := {
      s <- get(in_date)
      e <- get(out_date)
      gaps <- c(FALSE, head(e, -1) + lag < tail(s, -1))
      cumsum(gaps)
    },
    by = id
  ]

  # Collapse each merge_id to [min(start), max(end)]
  result <- data[
    ,
    .(
      tmp_start = min(get(in_date)),
      tmp_end = max(get(out_date))
    ),
    by = c(id, "merge_id")
  ]

  # Update names by reference
  data.table::setnames(result,
           old = c("tmp_start", "tmp_end"),
           new = c(in_date, out_date))

  # Remove merge_id column from result
  result[, merge_id := NULL]

  # return
  return(result)
}
