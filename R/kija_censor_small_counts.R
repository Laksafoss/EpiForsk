#' Check and optionally censor small counts in a table
#'
#' @description
#' Identifies cells containing nonnegative integer counts strictly smaller than a specified threshold. Counts may be
#' stored as numeric values or as strings representing numbers. The function can issue a warning, replace small counts,
#' or do both. By default, zero counts are also considered small.
#'
#' @param data A data frame (including a tibble or data.table) or a matrix.
#' @param threshold A single positive whole number. Counts strictly below this value are considered small.
#'   The default is `5`.
#' @param columns Columns to inspect, given by names, positive integer positions, or a logical vector of length
#' `ncol(data)`. `NULL` (default) selects all columns. Character column names must identify columns unambiguously.
#' @param rows Rows to inspect, given by positive integer positions (for example, `c(1:5, 10:20)`) or a logical vector
#'   of length `nrow(data)`. `NULL` (default) selects all rows.
#' @param cells Optional cell coordinates. Supply a data frame with columns `row` and `column`, where `row` contains
#'   row positions and `column` contains column names or positions. Alternatively, supply a two-column numeric matrix
#'   of row and column positions. When specified, only these cells are inspected, additionally restricted by `rows`
#'   and `columns`. `NULL` (default) inspects all intersections of selected rows and columns.
#' @param allow_zero A single logical value. If `FALSE` (default), zero counts are eligible for censoring and
#'   included in small-count warnings. If `TRUE`, zeros remain unchanged and are excluded from warnings, even
#'   when they are below `threshold`.
#' @param warn A single logical value. If `TRUE` (default), issue a warning when small counts are found, reporting
#'   their number and up to ten locations, but not their values.
#' @param censor A single logical value. If `TRUE`, replace the identified small counts.
#'   If `FALSE` (default), return the input unchanged.
#' @param replacement Either `"NA"` (default), to replace small counts with missing values, or `"label"`, to replace
#'   them with a string such as `"<5"` for `threshold = 5`.
#'
#' @details
#' A count must be finite, nonnegative, and integer-valued. For example, `2`, `"2"`, `" 2 "`, and `"2.0"` are counts,
#' whereas `-1`, `2.5`, `"2 people"`, and missing values are not. By default, zero is checked and eligible for
#' censoring, unless `allow_zero = TRUE`. Columns with unsupported types (including factors and dates) are skipped.
#'
#' Selection is the intersection of `columns`, `rows`, and (if provided) `cells`. If all three are `NULL`,
#' the entire table is checked. Selecting an empty set of columns, rows, or cells leaves the input unchanged.
#'
#' When `replacement = "label"`, affected numeric data-frame columns are converted to character. For numeric matrices,
#' the entire matrix is converted to character because matrices have a single storage type.
#'
#' @return The input table, with its original dimensions and row/column names.
#'   Values are unchanged if `censor = FALSE` or no small counts are found.
#'   Otherwise, only selected small-count cells are replaced, subject to the type conversion described above.
#'
#' @section Disclosure-control limitation:
#' This function performs *primary suppression only*. Suppressed cell counts may still be recoverable from displayed
#' totals or other cells. It does not perform complementary (secondary) suppression or certify that a table is
#' safe to release.
#'
#' @examples
#' df <- data.frame(
#'   group = c("A", "B", "C"),
#'   n = c(2L, 0L, 7L),
#'   events = c("1", "5", "4")
#' )
#'
#' # Inspect count columns without modifying the data.
#' CensorSmallCounts(
#'   df,
#'   columns = c("n", "events")
#' )
#'
#' # Censor small counts in the first two rows using a threshold label.
#' CensorSmallCounts(
#'   df,
#'   columns = c("n", "events"),
#'   rows = 1:2,
#'   censor = TRUE,
#'   replacement = "label",
#'   warn = FALSE
#' )
#'
#' # Leave zero counts visible while censoring other small counts.
#' CensorSmallCounts(
#'   df,
#'   columns = "n",
#'   censor = TRUE,
#'   allow_zero = TRUE,
#'   warn = FALSE
#' )
#'
#' # Inspect two specified cells only.
#' CensorSmallCounts(
#'   df,
#'   cells = data.frame(row = c(1, 3), column = c("n", "events")),
#'   censor = TRUE,
#'   warn = FALSE
#' )
#'
#' @export

CensorSmallCounts <- function(data,
                              threshold = 5L,
                              columns = NULL,
                              rows = NULL,
                              cells = NULL,
                              allow_zero = FALSE,
                              warn = TRUE,
                              censor = FALSE,
                              replacement = c("NA", "label")) {
  ### Check input
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data frame or matrix.", call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold) || threshold <= 0 || threshold != floor(threshold)) {
    stop("`threshold` must be a single non-negative integer.", call. = FALSE)
  }

  for (arg in c("allow_zero", "warn", "censor")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }

  replacement <- match.arg(replacement)

  ### Resolve indices without dropping duplicates
  # cell coordinates must remain paired after their row and column components are validated.
  resolve_positions <- function(x, size, labels = NULL, argument) {
    if (is.null(x)) return(seq_len(size))
    if (is.logical(x)) {
      if (length(x) != size || anyNA(x)) {
        stop(sprintf("`%s` must be a non-missing logical vector of length %d.", argument, size), call. = FALSE)
      }
      return(which(x))
    }
    if (is.numeric(x)) {
      if (anyNA(x) || any(!is.finite(x)) || any(x < 1 | x > size | x != floor(x))) {
        stop(sprintf("`%s` contains invalid positions.", argument), call. = FALSE)
      }
      return(as.integer(x))
    }
    if (is.character(x) && !is.null(labels)) {
      if (anyNA(x) || anyNA(match(x, labels)) ||
          any(vapply(x, function(value) sum(labels == value, na.rm = TRUE) != 1L, logical(1)))) {
        stop(sprintf("`%s` contains unknown or ambiguous column names.", argument), call. = FALSE)
      }
      return(match(x, labels))
    }
    stop(sprintf("`%s` has an unsupported selector type.", argument), call. = FALSE)
  }

  # Resolve for columns and rows
  nc <- ncol(data)
  nr <- nrow(data)
  col_labels <- colnames(data)
  selected_cols <- unique(resolve_positions(columns, nc, col_labels, "columns"))
  selected_rows <- unique(resolve_positions(rows, nr, argument = "rows"))

  # resolve for cells
  cells_by_col <- NULL
  if (!is.null(cells)) {
    if (is.data.frame(cells) && all(c("row", "column") %in% names(cells))) {
      cell_rows <- cells[["row"]]
      cell_cols <- cells[["column"]]
    } else if (is.matrix(cells) && ncol(cells) == 2L && is.numeric(cells)) {
      cell_rows <- cells[, 1L]
      cell_cols <- cells[, 2L]
    } else {
      stop("`cells` must be a data frame with `row` and `column`, or a two-column numeric matrix.", call. = FALSE)
    }
    cell_cols <- resolve_positions(cell_cols, nc, col_labels, "cells$column")
    cell_rows <- resolve_positions(cell_rows, nr, argument = "cells$row")
    keep <- (cell_cols %in% selected_cols) & (cell_rows %in% selected_rows)
    cells_by_col <- split(cell_rows[keep], as.character(cell_cols[keep]))
  }

  ### Determine which entries in data are small
  hit_rows <- vector("list", nc)
  for (j in selected_cols) {
    candidate_rows <- if (is.null(cells)) selected_rows else unique(cells_by_col[[as.character(j)]])
    if (!length(candidate_rows)) next

    values <- if (is.matrix(data)) {
      data[candidate_rows, j]
    } else {
      column <- data[[j]]
      if (!is.atomic(column) || !is.null(dim(column))) next
      column[candidate_rows]
    }

    if (is.numeric(values) && is.null(attr(values, "class"))) {
      numeric_values <- values
    } else if (is.character(values)) {
      numeric_values <- suppressWarnings(as.numeric(trimws(values)))
    } else {
      next
    }

    small <-
      is.finite(numeric_values) &
      numeric_values >= 0 &
      (!allow_zero | numeric_values != 0) &
      numeric_values == floor(numeric_values) &
      numeric_values < threshold

    hit_rows[[j]] <- candidate_rows[which(small)]
  }

  affected_cols <- which(lengths(hit_rows) > 0L)
  n_hits <- sum(lengths(hit_rows))

  # Print warning
  if (warn && n_hits > 0L) {
    preview <- character(0)
    for (j in affected_cols) {
      col_name <- if (!is.null(col_labels) && !is.na(col_labels[j]) && nzchar(col_labels[j])) {
        sprintf("%s [column %d]", col_labels[j], j)
      } else {
        sprintf("column %d", j)
      }
      for (i in head(hit_rows[[j]], 10L - length(preview))) {
        preview <- c(preview, sprintf("(%d, %s)", i, col_name))
      }
      if (length(preview) == 10L) break
    }
    more <- if (n_hits > length(preview)) ", ..." else ""
    warning(
      sprintf("Found %s count(s) below %s in %d column(s). First %s location(s) (row, column):\n %s%s",
              format(n_hits, scientific = FALSE),
              format(threshold, scientific = FALSE),
              length(affected_cols),
              format(n_hits, scientific = FALSE),
              paste(preview, collapse = ", "),
              more),
      call. = FALSE
    )
  }

  # Return original data if censoring not chosen or no censoring found
  if (!censor || n_hits == 0L) {
    return(data)
  }

  # Avoid changing data.table inputs by reference.
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  }

  label <- paste0("<", format(threshold, scientific = FALSE, trim = TRUE))
  if (is.matrix(data)) {
    if (replacement == "label" && is.numeric(data)) {
      data <- matrix(as.character(data), nrow = nr, ncol = nc, dimnames = dimnames(data))
    }
    for (j in affected_cols) {
      data[hit_rows[[j]], j] <- if (replacement == "NA") NA else label
    }
  } else {
    for (j in affected_cols) {
      column <- data[[j]]
      if (replacement == "label") column <- as.character(column)
      column[hit_rows[[j]]] <- if (replacement == "NA") NA else label
      data[[j]] <- column
    }
  }

  return(data)
}
