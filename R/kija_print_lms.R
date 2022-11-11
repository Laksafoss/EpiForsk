#' Print method for lms objects
#'
#' Prints information about an lms object.
#'
#' @param x The S3 object with class lms.
#' @param digits The number of significant digits to be passed to
#' \link[base]{format}(\link[stats]{coef}(x), .) when \link[base]{print}()ing.
#' @param ... Any additional arguments are ignored without warning.
#'
#' @returns
#' Silently returns the input lms object.
#'
#' @details
#' TODO
#'
#' # Author(s)
#' KIJA
#'
#' @examples
#'
#' 1+1
#'
#' @export

print.lms <- function (x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nNumber of observations: ", nrow(x$data),
      "\nNumber of groups: ", nrow(x$grp_id %>% dplyr::distinct()),
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
