#' Handle errors returned by ci_fct
#'
#' @param e error returned by ci_fct
#'
#' @return returns NULL if no stop command is executed.
#'
#' @examples 1+1

ci_fct_error_handler <- function(e) {
  if (
    grepl(
      'task 1 failed - "Problem does not follow DCP rules."',
      e$message
    ) &
    interactive()
  ) {
    # ask user if they want to continue with a grid search.
    input <- readline(
      "f does not follow DCP rules. Cannot find confidence limits using convex optimizer.\nDo you want to continue with a grid search? (y/n)"
    )
    for (i in seq_len(3)) {
      if (input %in% c("y", "n")) {
        if (input == "n") stop("f does not follow DCP rules.")
        break
      } else {
        if (i == 3) stop("Failed to answer 'y' or 'n' to many times")
        input <- readline(
          "Please input either 'y' to continue or 'n' to stop execution: "
        )
      }
    }
    for (i in seq_len(3)) {
      input <- readline("Please provide either 'n_grid' or 'k': ")
      if (input == "n_grid") {
        n_grid <<- c()
        for (l in seq_len(sum(which_parm))) {
          for (j in seq_len(3)) {
            input <- readline(
              glue::glue("Please provide a positive integer (as 1, not 1L) for entry {l} of n_grid: ")
            )
            input <- suppressWarnings(as.integer(input))
            if (!is.na(input) && is.integer(input) && as.integer(input) > 0L) {
              n_grid[l] <<- as.integer(input)
              break
            }
            if (j == 3) stop("Failed to answer with a positive integer to many times")
          }
          if (l == 1) {
            input <- readline(
              glue::glue("n_grid must have length 1 or {sum(which_parm)}. Should n_grid have length {sum(which_parm)}? (y/n): ")
            )
            if (input %in% c("y", "n")) {
              if (input == "n") break
            } else {
              input <- readline(
                "Please input either 'y' to add to n_grid or 'n' to continue: "
              )
              if (input %in% c("y", "n")) {
                if (input == "n") break
              } else {
                stop("Answer not 'y' or 'n'. Execution stopped.")
              }
            }
          }
        }
        break
      } else if (input == "k") {
        for (j in 1:3) {
          input <- readline("Please provide a positive integer (as 1, not 1L) for k: ")
          input <- suppressWarnings(as.integer(input))
          if (!is.na(input) && is.integer(input) && as.integer(input) > 0L) {
            k <<- as.integer(input)
            break
          }
          if (j == 3) stop("Failed to answer with a positive integer to many times")
        }
        break
      }
      if (i == 3) stop("Failed to answer 'n_grid' or 'k' to many times")
    }
  } else {
    stop(e)
  }
  return(invisible(NULL))
}
