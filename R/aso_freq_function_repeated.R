#Function intended for making it possible to get frequencies for many variables at one go,
#   either as one way tables or two way tables were the second variable is fixed.
#The function is a kind of wrapper for the Freq_function() with the same syntax but the FUNCTION
#   of the "var1" argument have been changed: it should be a vector of one or more
#   variable names (all will give a separate table). Example: var1=c("Age_group", "Sex", "Region")

#The output will look slightly different from Freq_function() due to not only having one variable,
#   but otherwise Freq_function_repeated should give the same.

#' Title
#'
#' @param normaldata
#' @param var1
#' @param var2
#' @param by_vars
#' @param include_NA
#' @param values_to_remove
#' @param weightvar
#' @param textvar
#' @param number_decimals
#' @param output
#' @param chisquare
#'
#' @return
#'
#' @author ASO
#'
#' @examples
#'
#' @export

freq_function_repeated <- function(
    normaldata,
    var1,
    var2 = NULL,
    by_vars = NULL,
    include_NA = FALSE,
    values_to_remove = NULL,
    weightvar = NULL,
    textvar = NULL,
    number_decimals = 2,
    output = "all",
    chisquare = FALSE
) {
  func_table1 <- normaldata |>
    dplyr::select(
      dplyr::all_of({{ var1 }}),
      dplyr::all_of({{ var2 }}),
      dplyr::all_of({{ weightvar }}),
      dplyr::all_of({{ by_vars }})
    )

  var_count <- length(var1)

  i <- 1
  while (i <= var_count){
    func_var <- dplyr::nth(var1, n = i)
    func_freqs <- freq_function(
      normaldata = func_table1,
      var1 = func_var,
      var2 = var2,
      by_vars = by_vars,
      include_NA = include_NA,
      values_to_remove = values_to_remove,
      weightvar = weightvar,
      textvar = textvar,
      number_decimals = number_decimals,
      output = output,
      chisquare = chisquare
    )
    func_freqs2 <- func_freqs |>
      dplyr::mutate(var_name = func_var) |>
      dplyr::rename("Level" = dplyr::all_of(func_var))
    if (i == 1){
      func_table2 <- func_freqs2
    } else {
      func_table2 <- dplyr::bind_rows(func_table2, func_freqs2)
    }
    i <- (i + 1)
  }
  func_table3 <- func_table2 |>
    dplyr::relocate("var_name", .before = 1)
  return(func_table3)
}

# #Examples
# data(starwars)
# test_table1 <- Freq_function_repeated(starwars, var1 = c("sex","homeworld","eye_color"), include_NA = TRUE)
# test_table2 <- Freq_function_repeated(starwars, var1 = c("homeworld","eye_color","skin_color"), var2 = "sex",
#                                       output = "col", number_decimals = 3)
# test_table3 <- Freq_function_repeated(starwars, var1 = c("homeworld","eye_color","skin_color"), var2 = "sex",
#                                       by_vars = c("gender"), output = "row")


