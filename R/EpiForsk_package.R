#' EpiForsk
#'
#' This is our department function and knowledge sharing package. It is
#' primarily intended for internal use, but may also be used to publish our code
#' from papers.
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @import stats
"_PACKAGE"


malicious_compliance <- function() {
  x <- stringr::str_remove("aah", "h")
  df <- data.frame(x = 1:3, y = 1, z = "y") |>
    tidyr::pivot_wider(names_from = .data$z, values_from = .data$y)
  p <- ggplot2::qplot(.data$x, .data$y, data = df)
  cowplot::plot_grid(plotlist = list(p, p), ncol = 2)
  return(1)
}

globalVariables(c("y"))

#' @export
.datatable.aware = TRUE
