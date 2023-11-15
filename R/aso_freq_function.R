#' Frequency Tables with Percentage and Odds Ratios
#'
#' A method for making 1 and 2 way frequency tables with percentages and odds
#' ratios.
#'
#' @param normaldata A data frame.
#' @param var1 A character naming the first variable to get frequencies.
#' @param var2 An optional character naming the second variable to get
#'   frequencies. If `NULL` (standard) a 1-way frequency table of only `var1` is
#'   created, and if `var2` is specified a 2-way table is returned.
#' @param by_vars An optional character vector naming variables in `normal_data`
#'   to stratify the calculations and output by. That is, ALL calculations will
#'   be made within the combinations of variables in the vector, hence it's
#'   possible to get N and % for many groups in one go.
#' @param include_NA A logical. If `FALSE` (standard) missing variables (`NA`'s)
#'   will be removed from `var1` and `var2`. Any missing values in `by_vars`
#'   will not be removed. If `TRUE` all missing values will be included in
#'   calculations and the output.
#' @param values_to_remove An optional character vector. When specified all
#'   values from `var1` and `var2` found in `values_to_remove` will be removed
#'   from the calculations and output.
#' @param weightvar An optional character naming a column in `normaldata` with
#'   numeric weights for each observation. If `NULL` (standard) all observations
#'   have weight 1.
#' @param textvar An optional character. When specified `textvar` is added to
#'   the resulting table as a comment. When `NULL` (standard) no such text
#'   addition is made.
#' @param number_decimals A numeric indicating the number of decimals to show on
#'   percentages and weighted frequencies in the combined frequency and percent
#'   variables.
#' @param output A character indicating the output type wanted:
#'   * `"all"` - will give ALL output from tables. In many cases unnecessary and
#'   hard to get an overview of. This is set as the standard.
#'   * `"numeric"` - will give frequencies and percents as numeric variables
#'   only, thus the number_decimals option is not in effect. This option might
#'   be useful when making figures/graphs.
#'   * "col" - will only give unweighted number of observations and weighted
#'   column percent (if weights are used, otherwise unweighted)
#'   * `"colw"` - will only give weighted number of observations and weighted
#'   column percent (if weights are used, otherwise unweighted)
#'   * `"row"`- will only give unweighted number of observations and weighted
#'   row percent (if weights are used, otherwise unweighted). Only works in
#'   two-way tables (`var2` is specified)
#'   * `"roww"` - will only give weighted number of oberservations and weighted
#'   column percent (if weights are used, otherwise unweighted). Only works in
#'   two-way tables (`var2` is specified)
#'   * `"total"` - will only give unweighted number of observations and
#'   weighted percent of the total (if weights are used, otherwise unweighted).
#'   Only works in two-way tables (`var2` is specified)
#'   * `"totalw"` - will only give weighted number of observations and
#'   weighted percent of the total (if weights are used, otherwise unweighted).
#'   Only works in two-way tables (`var2` is specified)
#'   * Any other text will give the default ("all")
#' @param chisquare A logical. `FALSE` (standard) will not calculate p-value for
#'   the chi-square test for two-way tables (`var2` is specified). If `TRUE`,
#'   the table will include the chi-square p-value as well as the chi-square
#'   statistic and the corresponding degrees of freedom. It will be included in
#'   the output whichever output option have been specified. No chi-square test
#'   is performed or included in one-way tables (`var2` is unspecified)
#'
#' @return A data frame.
#'
#' @author ASO
#'
#' @examples
#' data("starwars", package = "dplyr")
#'
#' test_table1 <- freq_function(
#'   starwars,
#'   var1 = "homeworld"
#' )
#'
#' test_table2 <- freq_function(
#'   starwars,
#'   var1 = "sex",
#'   var2 = "eye_color",
#'   output = "total"
#' )
#'
#' test_table3 <- freq_function(
#'   starwars,
#'   var1 = "hair_color",
#'   var2 = "skin_color",
#'   by_vars = c("gender"),
#'   output = "col",
#'   number_decimals = 5
#' )
#'
#' @export

freq_function <- function(
    normaldata,
    var1,
    var2 = NULL,
    by_vars = NULL,
    include_NA = FALSE,
    values_to_remove = NULL,
    weightvar = NULL,
    textvar = NULL,
    number_decimals = 2,
    output = c("all", "numeric", "colw", "row", "roww", "total", "totalW"),
    chisquare = FALSE
) {

  # ADLS: I have added a few checks
  if (missing(normaldata)) {
    stop("'normaldata' must be a data frame.")
  }
  if (!inherits(normaldata, "data.frame")) {
    stop("'normaldata' must be a data frame.")
  }
  if (!(is.character(var1) && length(var1) == 1)) {
    stop(
      "'var1' must be a length 1 character vector naming ",
      "the first variable to get frequencies."
    )
  }
  if (!(is.null(var2) || (is.character(var2) && length(var2) == 1))) {
    stop(
      "'var2' must be NULL or optionally a length 1 character ",
      "vector naming the second\nvariable to get frequencies."
    )
  }
  if (!is.null(by_vars)) {
    if (!inherits(by_vars, "character")) {
      stop(
        "by_vars must be NULL or optionally a character vector ",
        "naming variables in normal_data\n to stratify the ",
        "calculations and output by."
      )
    } else if (!all(by_vars %in% names(normaldata))) {
      stop(
        glue::glue(
          "by_vars must name variables in normaldata.\n",
          "The following are not names of normaldata columns:\n",
          "{paste(by_vars[!(by_vars %in% names(starwars))], collapse = ', ')}"
        )
      )
    }
  }
  if (!(isTRUE(include_NA) || isFALSE(include_NA))) {
    stop("'include_NA' must be a boolean.")
  }
  if (!is.null(values_to_remove)) {
    if (!inherits(values_to_remove, "character")) {
      stop(
        "'values_to_remove' must be NULL or optionally a character vector."
      )
    }
  }
  if (!is.null(weightvar)) {
    if (!inherits(weightvar, "character")) {
      stop(
        "weightvar must be NULL or optionally a character of length 1 ",
        "naming a column\nin `normaldata` with numeric weights ",
        "for each observation."
      )
    } else if (!(length(weightvar == 1 && weightvar %in% names(normaldata)))) {
      stop(
        "weightvar must name a single column in 'normaldata' with numeric",
        "weights\nfor each observation."
      )
    } else if (!is.numeric(normaldata[[weightvar]])) {
      stop(
        "The column named in weightvar must contain numeric weights",
        "for each observation."
      )
    }
  }
  if (!is.null(textvar)) {
    if (!inherits(textvar, "character")) {
      stop("When 'textvar' is specified it must be a character.")
    }
  }
  if (!inherits(number_decimals, "numeric")) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  if (number_decimals < 0) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  output <- match.arg(output)
  if (!(isTRUE(chisquare) || isFALSE(chisquare))) {
    stop("'chisquare' must be a boolean.")
  }
  # End a added checks

  # Getting name of original Variable1 as a string (to be used below)
  Orig_var1_name <- var1

  # Select only mentioned variables from called data (not using specifications)
  func_table1 <- normaldata |>
    dplyr::select(
      dplyr::all_of({{ var1 }}),
      dplyr::all_of({{ var2 }}),
      dplyr::all_of({{ weightvar }}),
      dplyr::all_of({{ by_vars }})
    )

  # Encoding for weight variable - unless a precalculated weight should be used,
  # the weight will be 1 for all observations
  if (!is.null(weightvar)){
    func_table2 <- func_table1 |>
      dplyr::mutate("weight_used" = .data[[weightvar]]) |>
      dplyr::filter(.data$weight_used > 0)
  }
  else {
    func_table2 <- func_table1 |> dplyr::mutate(weight_used = as.numeric(1))
  }

  # Rename the variable of interest to Variable1 so it can be used below (name
  # changed back in the end)
  func_table3 <- func_table2 |>
    dplyr::mutate("Variable1" = as.character(.data[[var1]]))

  # If there should be a two-way table, the second variable is called Variable2
  # below (name changed back in the end)
  if (!is.null(var2)){
    func_table4 <- func_table3 |>
      dplyr::mutate("Variable2" = as.character(.data[[var2]]))
  }
  else {
    func_table4 <- func_table3
  }

  # Keeping/removing missing (=NA) in the data depending on user specification
  if (include_NA  ==  TRUE) {
    func_table5 <- func_table4
  } else if (include_NA == FALSE & is.null(var2)) {
    func_table5 <- func_table4 |> dplyr::filter(!is.na(.data$Variable1))
  } else if (include_NA == FALSE & !is.null(var2)) {
    func_table5 <- func_table4 |>
      dplyr::filter(!is.na(.data$Variable1) & !is.na(.data$Variable2))
  }

  # Remove specified values (if any) from the values used in tables before
  # calculations (by-variables NOT influenced)
  if (is.null(values_to_remove)) {
    func_table6 <- func_table5
  } else if (is.null(var2)) {
    func_table6 <- func_table5 |>
      dplyr::filter(!.data$Variable1 %in% values_to_remove)
  } else if (!is.null(var2)) {
    func_table6 <- func_table5 |>
      dplyr::filter(
        !.data$Variable1 %in% values_to_remove &
          !.data$Variable2 %in% values_to_remove
      )
  }


  # Change all factor variables in data to character variables
  # (basically it will be by-variables who are changed)
  func_table6 <- func_table6 |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

  # Making summations and calculations (n, % etc.)
  if (is.null(by_vars) & is.null(var2)) {
    # Getting unweighted and weighted n for each group
    func_table7 <- func_table6 |>
      dplyr::summarize(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = "Variable1"
      ) |>
      dplyr::mutate(
        Func_var = var1,
        Variable = .data$Variable1
      ) |>
      dplyr::select("Func_var", "Variable", "n", "n_weighted")

    # Summation within variables
    func_table8 <- func_table7 |>
      dplyr::summarize(
        n_total = sum(.data$n),
        n_weighted_total = sum(.data$n_weighted, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        Func_var = var1,
        Variable2 = "Total"
      ) |>
      dplyr::select(
        "Func_var",
        "Variable" = "Variable2",
        "n" = "n_total",
        "n_weighted" = "n_weighted_total"
      )

    # Combining func_table8 (total numbers) with func_table7 (grouped numbers)
    func_table9 <- dplyr::bind_rows(func_table7, func_table8)

    # Calculating percent of total
    func_table10 <- multi_join(
      func_table9,
      func_table8,
      .by = "Func_var"
    ) |>
      dplyr::mutate(
        Column_pct = ((.data$n_weighted.1 / .data$n_weighted.2) * 100),
        Freq_col_pct = paste0(
          .data$n.1,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$n_weighted.1),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Level = .data$Variable.1,
        N = .data$n.1,
        N_weighted = .data$n_weighted.1,
      ) |>
      dplyr::select(
        "Level",
        "N",
        "N_weighted",
        "Column_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )

    # Setting the name of the first variable correctly
    colnames(func_table10)[1] <- Orig_var1_name

    if(output == "all") {
      func_table11 <- func_table10
    } else if (output == "numeric") {
      func_table11 <- dplyr::select(
        func_table10,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col") {
      func_table11 <- dplyr::select(
        func_table10,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freqw_col_pct"
      )
    } else if (output == "colw") {
      func_table11 <- dplyr::select(
        func_table10,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freq_col_pct"
      )
    } else {
      func_table11 <- func_table10
    }

    # Adding text-variable IF text have been added to textvar at function call +
    # return final table
    if (is.null(textvar)){
      return(func_table11)
    } else {
      # Adding text-variable
      func_table12 <- func_table11 |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
      return(func_table12)
    }
  }
  else if (is.null(var2)) {
    # Getting unweighted and weighted n for each group
    func_table7 <- func_table6 |>
      dplyr::summarize(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c({{ by_vars }}, "Variable1")
      ) |>
      dplyr::mutate(
        Func_var = var1,
        Variable = .data$Variable1
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var",
        "Variable",
        "n",
        "n_weighted"
      )

    # Summation within variables
    func_table8 <- func_table7 |>
      dplyr::summarize(
        n_total = sum(.data$n),
        n_weighted_total = sum(.data$n_weighted, na.rm = TRUE),
        .by = {{ by_vars }}
      ) |>
      dplyr::mutate(
        Func_var = var1,
        Variable2 = "Total",
        Variable = .data$Variable2,
        n = .data$n_total,
        n_weighted = .data$n_weighted_total
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var",
        "Variable",
        "n",
        "n_weighted"
      )

    # Combining func_table8 (total numbers) with func_table9 (grouped numbers)
    func_table9 <- dplyr::bind_rows(func_table7, func_table8)

    # Calculating percent of total
    func_table10 <- multi_join(
      func_table9,
      func_table8,
      .by = c({{ by_vars }}, "Func_var")
    ) |>
      dplyr::mutate(
        Column_pct = ((.data$n_weighted.1 / .data$n_weighted.2) * 100),
        Freq_col_pct = paste0(
          .data$n.1,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$n_weighted.1),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Level = .data$Variable.1,
        N = .data$n.1,
        N_weighted = .data$n_weighted.1
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Level",
        "N",
        "N_weighted",
        "Column_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c({{ by_vars }}, "Level"))))

    # Getting number of by-vars
    # (needed to be able to change the correct name below)
    num_by_vars <- length(by_vars)

    # Setting the name of the first variable correctly
    colnames(func_table10)[(num_by_vars + 1)] <- Orig_var1_name

    if(output == "all"){
      func_table11 <- func_table10
    } else if (output == "numeric"){
      func_table11 <- dplyr::select(
        func_table10,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col"){
      func_table11 <- dplyr::select(
        func_table10,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freqw_col_pct"
      )
    } else if (output == "colw"){
      func_table11 <- dplyr::select(
        func_table10,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freq_col_pct"
      )
    } else {
      func_table11 <- func_table10
    }

    # Adding text if textvar is used + return final table
    if (is.null(textvar)){
      return(func_table11)
    } else {
      #Adding text-variable
      func_table12 <- func_table11 |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
      return(func_table12)
    }
  }
  else if (is.null(by_vars)) {
    # Getting unweighted and weighted n for each combination
    func_table7 <- func_table6 |>
      dplyr::summarize(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c("Variable1", "Variable2")
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2",
        "n",
        "n_weighted"
      )

    # Summation within variable1
    func_table8 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE),
        .by = "Variable1"
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        "Func_var1",
        "Variable1",
        "Variable2" = "Variable3",
        "n",
        "n_weighted"
      )

    # Summation within variable2
    func_table9 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE),
        .by = "Variable2"
      ) |>
      dplyr::mutate(
        Func_var2 = var2,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2",
        "n",
        "n_weighted"
      )

    # Summation totals
    func_table10 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable" = "Variable3",
        "Variable2" = "Variable3",
        "n",
        "n_weighted"
      )

    # Getting N and weighted percent for Variable2:
    # N within variable1, percent = on that level of variable1
    func_table11 <- multi_join(
      func_table8,
      func_table7,
      .by = c("Func_var1", "Variable1")
    ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable2.2",
        "Variable1_level_total" = "n.1",
        "Variable1_level_total_weighted" = "n_weighted.1",
        "n" = "n.2",
        "n_weighted" = "n_weighted.2"
      )

    # Combining all numbers so percent etc. can be calculated in
    # several directions
    func_table12 <- multi_join(
      func_table11,
      func_table10,
      .by = c("Func_var1","Func_var2")
    ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable2.1",
        "Total_n" = "n.2",
        "Total_n_weighted" = "n_weighted.2",
        "Variable1_level_total",
        "Variable1_level_total_weighted",
        "n" = "n.1",
        "n_weighted" = "n_weighted.1")

    func_table13 <- multi_join(
      func_table12,
      func_table9,
      .by = c("Func_var2","Variable2")
    ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable1.1",
        "Variable2",
        "Total_n",
        "Total_n_weighted",
        "Variable1_level_total",
        "Variable1_level_total_weighted",
        "Variable2_level_total" = "n.2",
        "Variable2_level_total_weighted" = "n_weighted.2",
        "n" = "n.1",
        "n_weighted" = "n_weighted.1"
      )

    # Percent calculations for each Var1 and Var2 combination
    func_table14 <- func_table13 |>
      dplyr::mutate(
        Total_pct = (
          (.data$n_weighted / .data$Total_n_weighted) * 100
        ),
        Row_pct = (
          (.data$n_weighted / .data$Variable1_level_total_weighted) * 100
        ),
        Column_pct = (
          (.data$n_weighted / .data$Variable2_level_total_weighted) * 100
        )
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2",
        "N" = "n",
        "Weighted_N" = "n_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for each Var1 row totals (sum over var2)
    func_table15 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Variable1_level_total,
        Total_pct = (
          (.data$Variable1_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Column_pct = (
          (.data$Variable1_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Row_pct = (
          (.data$Variable1_level_total_weighted /
             .data$Variable1_level_total_weighted) * 100
        )
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable3",
        "N" = "n",
        "Weighted_N" = "Variable1_level_total_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for each Var2 row totals (sum over var1)
    func_table16 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Variable2_level_total,
        Total_pct = (
          (.data$Variable2_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Column_pct = (
          (.data$Variable2_level_total_weighted /
             .data$Variable2_level_total_weighted) * 100
        ),
        Row_pct = (
          (.data$Variable2_level_total_weighted / .data$Total_n_weighted) * 100
        )
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2",
        "N" = "n",
        "Weighted_N" = "Variable2_level_total_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for total (complete total)
    func_table17 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Total_n,
        Total_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
        Column_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
        Row_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100)
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2" = "Variable3",
        "N" = "n",
        "Weighted_N" = "Total_n_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Remove duplicate lines for totals
    func_table18 <- dplyr::distinct(func_table15)
    func_table19 <- dplyr::distinct(func_table16)
    func_table20 <- dplyr::distinct(func_table17)

    # Combine all n/percentages in one table
    func_table21 <- dplyr::bind_rows(
      func_table14,
      func_table18,
      func_table19,
      func_table20
    ) |>
      dplyr::mutate(
        Freq_col_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freq_row_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freqw_row_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freq_total_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        ),
        Freqw_total_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        )
      ) |>
      dplyr::select(
        "Func_var2",
        "Variable1",
        "Variable2",
        "N",
        "Weighted_N",
        "Row_pct",
        "Column_pct",
        "Total_pct",
        "Freq_total_pct",
        "Freqw_total_pct",
        "Freq_row_pct",
        "Freqw_row_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )
    colnames(func_table21)[2] <- Orig_var1_name

    # Change rotation on table, so the rows and columns are easier to follow
    func_table22_1 <- dplyr::select(
      func_table21,
      -"Freq_total_pct",
      -"Freqw_total_pct",
      -"Freq_row_pct",
      -"Freqw_row_pct",
      -"Freq_col_pct",
      -"Freqw_col_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", "Variable2"),
        values_from = c(
          "N",
          "Weighted_N",
          "Row_pct",
          "Column_pct",
          "Total_pct"
        ),
        values_fill = 0
      )
    func_table22_2 <- dplyr::select(
      func_table21,
      -"N",
      -"Weighted_N",
      -"Row_pct",
      -"Column_pct",
      -"Total_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", "Variable2"),
        values_from = c(
          "Freq_total_pct",
          "Freqw_total_pct",
          "Freq_row_pct",
          "Freqw_row_pct",
          "Freq_col_pct",
          "Freqw_col_pct"
        ),
        values_fill = "0 (0%)"
      )

    func_table22 <- dplyr::full_join(
        func_table22_1,
        func_table22_2,
        by = {{ var1 }}
        )

    if (chisquare == TRUE){
      # Calculating expected numbers for each cell
      func_table23 <- func_table13 |>
        dplyr::mutate(
          expected = (
            (.data$Variable1_level_total_weighted / .data$Total_n_weighted) *
              .data$Variable2_level_total_weighted
          ),
          observed = .data$n_weighted,
          cell_chi = (((.data$observed - .data$expected)^2) / .data$expected)
        )

      # Summing up the total chi-square test statistic
      func_table24 <- func_table23 |>
        dplyr::summarize(cell_chi_total = sum(.data$cell_chi, na.rm = TRUE))

      # Degrees of freedom, chi-square test
      chi_degree1 <- length(unique(func_table6$Variable1))
      chi_degree2 <- length(unique(func_table6$Variable2))
      chi_degree_total <- (chi_degree1 - 1) * (chi_degree2 - 1)
      chi_degree_freedom <- as.data.frame(chi_degree_total)

      chi_p_prp <- dplyr::bind_cols(func_table24, chi_degree_freedom)

      chi_p <- chi_p_prp |>
        dplyr::rowwise() |>
        dplyr::mutate(
          p_value = pchisq(
            .data$cell_chi_total,
            df = .data$chi_degree_total,
            lower.tail = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(var1 = "Total")
      names(chi_p)[length(names(chi_p))] <- Orig_var1_name

      func_table25 <- dplyr::full_join(func_table22, chi_p, by = Orig_var1_name)
    } else {
      func_table25 <- func_table22
    }

    if(output == "all") {
      func_table26 <- func_table25
    } else if (output == "numeric") {
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "colw"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_")
      )
    } else if (output == "row"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "roww"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "total"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "totalw"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else {
      func_table26 <- func_table25
    }

    if (is.null(textvar)){
      return(func_table26)
    } else {
      # Adding text-variable
      func_table27 <- func_table26 |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
      return(func_table27)
    }
  }
  else {
    # Getting unweighted and weighted n for each combination
    func_table7 <- func_table6 |>
      dplyr::summarize(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c({{ by_vars }}, "Variable1", "Variable2")
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2",
        "n",
        "n_weighted"
      )

    # Summation within variable1
    func_table8 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE),
        .by = c({{ by_vars }}, "Variable1")
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Variable1",
        "Variable2" = "Variable3",
        "n",
        "n_weighted"
      )

    # Summation within variable2
    func_table9 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE),
        .by = c({{ by_vars }}, "Variable2")
      ) |>
      dplyr::mutate(
        Func_var2 = var2,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2",
        "n",
        "n_weighted"
      )

    # Summation totals
    func_table10 <- func_table7 |>
      dplyr::summarize(
        n = sum(.data$n),
        n_weighted = sum(.data$n_weighted, na.rm = TRUE),
        .by = {{ by_vars }}
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2,
        Variable3 = "Total"
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable" = "Variable3",
        "Variable2" = "Variable3",
        "n",
        "n_weighted"
      )

    # Getting N and weighted percent for Variable2:
    # N within variable1, percent = on that level of variable1
    func_table11 <- multi_join(
      func_table8,
      func_table7,
      .by = c({{ by_vars }}, "Func_var1", "Variable1")
    ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable2.2",
        "Variable1_level_total" = "n.1",
        "Variable1_level_total_weighted" = "n_weighted.1",
        "n" = "n.2",
        "n_weighted" = "n_weighted.2"
      )

    # Combining all numbers so percent etc. can be calculated in several
    # directions
    func_table12 <- multi_join(
      func_table11,
      func_table10,
      .by = c({{ by_vars }}, "Func_var1","Func_var2")
    ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable2.1",
        "Total_n" = "n.2",
        "Total_n_weighted" = "n_weighted.2",
        "Variable1_level_total",
        "Variable1_level_total_weighted",
        "n" = "n.1",
        "n_weighted" = "n_weighted.1"
      )

    func_table13 <- multi_join(
      func_table12,
      func_table9,
      .by = c({{ by_vars }}, "Func_var2","Variable2")
    ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable1.1",
        "Variable2",
        "Total_n",
        "Total_n_weighted",
        "Variable1_level_total",
        "Variable1_level_total_weighted",
        "Variable2_level_total" = "n.2",
        "Variable2_level_total_weighted" = "n_weighted.2",
        "n" = "n.1",
        "n_weighted" = "n_weighted.1"
      )

    # Percent calculations for each Var1 and Var2 combination
    func_table14 <- func_table13 |>
      dplyr::mutate(
        Total_pct = (
          (.data$n_weighted / .data$Total_n_weighted) * 100
        ),
        Row_pct = (
          (.data$n_weighted / .data$Variable1_level_total_weighted) * 100
        ),
        Column_pct = (
          (.data$n_weighted / .data$Variable2_level_total_weighted) * 100
        )
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2",
        "N" = "n",
        "Weighted_N" = "n_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for each Var1 row totals (sum over var2)
    func_table15 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Variable1_level_total,
        Total_pct = (
          (.data$Variable1_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Column_pct = (
          (.data$Variable1_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Row_pct = (
          (.data$Variable1_level_total_weighted /
             .data$Variable1_level_total_weighted) * 100
        )
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1",
        "Variable2" = "Variable3",
        "N" = "n",
        "Weighted_N" = "Variable1_level_total_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for each Var2 row totals (sum over var1)
    func_table16 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Variable2_level_total,
        Total_pct = (
          (.data$Variable2_level_total_weighted / .data$Total_n_weighted) * 100
        ),
        Column_pct = (
          (.data$Variable2_level_total_weighted /
             .data$Variable2_level_total_weighted) * 100
        ),
        Row_pct = (
          (.data$Variable2_level_total_weighted / .data$Total_n_weighted) * 100
        )
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2",
        "N" = "n",
        "Weighted_N" = "Variable2_level_total_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Percent calculations for total (complete total)
    func_table17 <- func_table13 |>
      dplyr::mutate(
        Variable3 = "Total",
        n = .data$Total_n,
        Total_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
        Column_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
        Row_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100)
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        "Variable1" = "Variable3",
        "Variable2" = "Variable3",
        "N" = "n",
        "Weighted_N" = "Total_n_weighted",
        "Total_pct",
        "Row_pct",
        "Column_pct"
      )

    # Remove duplicate lines for totals
    func_table18 <- dplyr::distinct(func_table15)
    func_table19 <- dplyr::distinct(func_table16)
    func_table20 <- dplyr::distinct(func_table17)

    # Combine all n/percentages in one table
    func_table21 <- dplyr::bind_rows(
      func_table14,
      func_table18,
      func_table19,
      func_table20
    ) |>
      dplyr::mutate(
        Freq_col_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freq_row_pct = paste0(
          .data$N,
          " (",sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freqw_row_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freq_total_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        ),
        Freqw_total_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        )
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var2",
        "Variable1",
        "Variable2",
        "N",
        "Weighted_N",
        "Row_pct",
        "Column_pct",
        "Total_pct",
        "Freq_total_pct",
        "Freqw_total_pct",
        "Freq_row_pct",
        "Freqw_row_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )

    # Getting number of by-vars
    # (needed to be able to change the correct name below)
    num_by_vars <- length(by_vars)
    colnames(func_table21)[(num_by_vars+2)] <- Orig_var1_name

    # Change rotation on table, so the rows and columns are easier to follow
    func_table22_1 <- dplyr::select(
      func_table21,
      -"Freq_total_pct",
      -"Freqw_total_pct",
      -"Freq_row_pct",
      -"Freqw_row_pct",
      -"Freq_col_pct",
      -"Freqw_col_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", "Variable2"),
        values_from = c(
          "N",
          "Weighted_N",
          "Row_pct",
          "Column_pct",
          "Total_pct"
        ),
        values_fill = 0
      )
    func_table22_2 <- dplyr::select(
      func_table21,
      -"N",
      -"Weighted_N",
      -"Row_pct",
      -"Column_pct",
      -"Total_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", "Variable2"),
        values_from = c(
          "Freq_total_pct",
          "Freqw_total_pct",
          "Freq_row_pct",
          "Freqw_row_pct",
          "Freq_col_pct",
          "Freqw_col_pct"
        ),
        values_fill = "0 (0%)"
      )
    func_table22 <- dplyr::full_join(
      func_table22_1,
      func_table22_2,
      by = c({{ by_vars }} , {{ var1 }})
    ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of({{ by_vars }})))

    if (chisquare == TRUE) {
      # Calculating expected numbers for each cell
      func_table23 <- func_table13 |>
        dplyr::mutate(
          expected = (
            (.data$Variable1_level_total_weighted / .data$Total_n_weighted) *
              .data$Variable2_level_total_weighted
          ),
          observed = .data$n_weighted,
          cell_chi = (((.data$observed - .data$expected)^2) / .data$expected)
        )

      # Summing up the total chi-square test statistic
      func_table24 <- func_table23 |>
        dplyr::summarize(
          cell_chi_total = sum(.data$cell_chi, na.rm = TRUE),
          .by = {{ by_vars }}
        )

      # Degrees of freedom, chi-square test
      chi_degree1 <- func_table6 |>
        dplyr::summarize(
          chi_degree1 = dplyr::n_distinct(.data$Variable1),
          .by = {{ by_vars }}
        )
      chi_degree2 <- func_table6 |>
        dplyr::summarize(
          chi_degree2 = dplyr::n_distinct(.data$Variable2),
          .by = {{ by_vars }}
        )
      chi_degree_freedom <- dplyr::full_join(
        chi_degree1,
        chi_degree2,
        by = {{ by_vars }}
      ) |>
        dplyr::mutate(
          chi_degree_total = ((.data$chi_degree1 - 1) * (.data$chi_degree2 - 1))
        ) |>
        dplyr::select(-"chi_degree1", -"chi_degree2")

      chi_p <- dplyr::full_join(
        func_table24,
        chi_degree_freedom,
        by = c({{ by_vars }})
      ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          p_value = pchisq(
            .data$cell_chi_total,
            df = .data$chi_degree_total,
            lower.tail = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(var1 = "Total")
      names(chi_p)[length(names(chi_p))] <- Orig_var1_name

      func_table25 <- dplyr::full_join(
        func_table22,
        chi_p,
        by = c({{ by_vars }}, Orig_var1_name)
      )
    } else {
      func_table25 <- func_table22
    }

    if(output == "all"){
      func_table26 <- func_table25
    } else if (output == "numeric"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "colw"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_")
      )
    } else if (output == "row"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "roww"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "total"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "totalw"){
      func_table26 <- dplyr::select(
        func_table25,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else {
      func_table26 <- func_table25
    }

    if (is.null(textvar)){
      return(func_table26)
    }else {
      # Adding text-variable
      func_table27 <- func_table26 |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
      return(func_table27)
    }
  }
}
