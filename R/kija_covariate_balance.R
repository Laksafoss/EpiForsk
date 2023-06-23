#' Plots for checking covariate balance in causal forest
#'
#' Generate plots showing balance in the covariates before and after propensity
#' score weighting with a causal forest object.
#'
#' @param cf An object of class causal_forest (and inheriting from class grf).
#' @param covariates <[`tidy-select`][dplyr_tidy_select]> An unquoted expression
#'   selecting covariates to include in the balance plots.
#' @param names A named character vector. The vector itself should contain
#'   covariate names from the causal_forest object, while the names attribute
#'   should contain the names to use when plotting. If the vector is unnamed,
#'   the provided vector will act as the new covariate names, given in the order
#'   of `cf$X_orig`. If `NULL` (the default), the original names are used.
#' @param treatment_name Character, name of treatment.
#' @param asmd_breaks Numeric, breaks used in the plot of absolute standardized
#'   mean differences.
#' @param asmd_xlim Numeric, `x`-limits used in the plot of absolute
#'   standardized mean differences.
#' @param asmd_scale_color Function, `scale_color_.` function to use in the plot
#'   of absolute standardized mean differences.
#' @param cd_nrow,cd_ncol Numeric, the dimensions of the grid to create in
#'   covariate distribution plots. If both are `NULL` it will use the same logic
#'   as \link[ggplot2]{facet_wrap} to set the dimensions.
#' @param cd_x_scale_width Numeric, the distance between major `x`-axis tics in
#'   the covariate distribution plots. If `NULL`, a width is chosen to display
#'   approximately six major tics. If length 1, the same width is used for all
#'   covariate plots. If the same length as the number of covariates included,
#'   each number is used as the width for different covariates, in the order of
#'   the covariates after selection with the tidy-select expression in
#'   `covariates`.
#' @param cd_bar_width Numeric, the width of the bars in the covariate
#'   distribution plots (barplots for categorical variables, histograms for
#'   continuous variables). If `NULL`, a width is chosen to display
#'   approximately 50 bars in histograms, while 0.9 times the resolution of the
#'   data is used in bar plots. If length 1, the same width is used for all
#'   covariate plots. This is not recommended if there are both categorical and
#'   continuous covariates. If the same length as the number of covariates
#'   included, each number is used as the bar width for different covariates, in
#'   the order of the covariates after selection with the tidy-select expression
#'   in `covariates`.
#' @param cd_scale_fill Function, `scale_fill_.` function to use in covariate
#'   distribution plots.
#'
#' @return A list with five elements:
#' - asmd_data: data used to plot the absolute standardized mean differences.
#' - asmd: plot object for absolute standardized mean differences.
#' - cd_data: data used to plot covariate distributions.
#' - cd_unadjusted: plot of unadjusted covariate distributions in the exposure
#'   groups.
#' - cd_adjusted: plot of adjusted covariate distributions in the exposure
#'   groups.
#'
#' @details The plot theme can be adjusted using ggplot2 active theme modifiers,
#'   see \link[ggplot2]{theme_get}.
#'
#' @author KIJA
#'
#' @examples
#' n <- 1500
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p) |> as.data.frame()
#' expo_prob <- 1 / (1 + exp(0.2 * X[, 1] + 0.1 * X[, 2] - 0.3 * X[, 3]))
#' W <- rbinom(n, 1, expo_prob)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' cb1 <- CovariateBalance(cf)
#' cb2 <- CovariateBalance(
#' cf,
#' covariates = 1:4,
#' names = c(
#' "medium imbalance" = "V1",
#' "low imbalance" = "V2",
#' "high imbalance" = "V3",
#' "no imbalance" = "V4"
#' ),
#' treatment_name = "Treatment",
#' asmd_breaks = seq(0, 0.5, 0.1),
#' asmd_xlim = c(0, 0.5),
#' cd_nrow = 2,
#' cd_x_scale_width = 1,
#' cd_bar_width = 0.3
#' )
#'
#' @export

CovariateBalance <- function(cf,
                             covariates = dplyr::everything(),
                             names = NULL,
                             treatment_name = "W",
                             asmd_breaks = NULL,
                             asmd_xlim = NULL,
                             asmd_scale_color = NULL,
                             cd_nrow = NULL,
                             cd_ncol = NULL,
                             cd_x_scale_width = NULL,
                             cd_bar_width = NULL,
                             cd_scale_fill = NULL) {
  # check input
  stopifnot(
    "cf must be a 'causal_forest' object" = inherits(cf, "causal_forest")
  )
  if (!inherits(cf, "grf")) {
    warning(
      paste0(
        "cf doesn't inherit from class 'grf'. Make sure your 'causal_forest'",
        "object complies with the class structure used by grf::causal_forest()"
      )
    )
  }
  if (is.null(names)) {
    X_orig <- tibble::as_tibble(cf$X.orig, .name_repair = "universal")
  } else if (is.character(names)) {
    if (is.null(names(names))) {
      X_orig <- tibble::as_tibble(cf$X.orig, .name_repair = "universal") |>
        rlang::set_names(names)
    } else {
      X_orig <- tibble::as_tibble(cf$X.orig, .name_repair = "universal") |>
        dplyr::rename(dplyr::all_of(names))
    }
  } else {
    stop(paste0(
      "names must be NULL to use the original names from cf$X.orig, ",
      "a\ncharacter vector with covariate names that replace names(cf$X.orig),\n",
      "or a named character vector with the original names and the replacement\n",
      "names in the names attribute."
    ))
  }
  X_orig <- dplyr::select(X_orig, {{ covariates }})
  if (!(is.null(cd_x_scale_width) | is.numeric(cd_x_scale_width))) {
    stop (
      glue::glue(
        "cd_x_scale_width must be NULL or a numeric vector of length 1 or ",
        "{ncol(X_orig)}.")
    )
  } else if (!(length(cd_x_scale_width) %in% c(0, 1, ncol(X_orig)))) {
    stop (
      glue::glue(
        "cd_x_scale_width has the wrong length. Must be NULL or have ",
        "length 1 or {ncol(X_orig)}."
      )
    )
  }
  if (!(is.character(treatment_name) && length(treatment_name) == 1)) {
    stop (
      paste0(
        "'treatment_name' must be a single string with the treatment name",
        "to display in the covariate distribution plots"
      )
    )
  }
  if (!(is.null(cd_bar_width) || is.numeric(cd_bar_width))) {
    stop (
      glue::glue(
        "cd_bar_width must be NULL or a numeric vector of length 1 or ",
        "{ncol(X_orig)}.")
    )
  } else if (!(length(cd_bar_width) %in% c(0, 1, ncol(X_orig)))) {
    stop (
      glue::glue(
        "cd_bar_width has the wrong length. Must be NULL or have ",
        "length 1 or {ncol(X_orig)}."
      )
    )
  }
  if (!(is.null(cd_nrow) || (is.numeric(cd_nrow) && cd_nrow > 0.5))) {
    stop ("cd_nrow must be NULL or a positive integer.")
  }
  if (!(is.null(cd_ncol) || (is.numeric(cd_ncol) && cd_ncol > 0.5))) {
    stop ("cd_ncol must be NULL or a positive integer.")
  }
  if (is.null(asmd_scale_color)) {
    if (requireNamespace("ggsci", quietly = TRUE)) {
      asmd_scale_color <- ggsci::scale_color_jama()
    } else {
      asmd_scale_color <- ggplot2::scale_color_discrete()
    }
  }
  if (is.null(cd_scale_fill)) {
    if (requireNamespace("ggsci", quietly = TRUE)) {
      cd_scale_fill <- ggsci::scale_fill_jama()
    } else {
      cd_scale_fill <- ggplot2::scale_fill_discrete()
    }
  }
  # create absolute standardised mean difference plot
  funs <- c(mean = mean, var = var)
  df_0 <- X_orig
  df_0[cf$W.orig == 1,] <- NA
  df_1 <- X_orig
  df_1[cf$W.orig == 0,] <- NA
  df_ori <- tibble::as_tibble(
    cbind(df_0, df_1),
    .name_repair = "minimal"
  ) |>
    purrr::map(
      \(x) tibble::as_tibble(purrr::map(funs, rlang::exec, x, na.rm = TRUE))
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(name = rep(names(X_orig), 2)) |>
    dplyr::summarise(
      std_abs_mean_diff = abs(diff(mean)) / sqrt(sum(var)),
      .by = "name"
    ) |>
    dplyr::arrange(std_abs_mean_diff)
  df_adj <- tibble::as_tibble(
    cbind(X_orig * cf$W.orig / cf$W.hat,
          X_orig * (1 - cf$W.orig) / (1 - cf$W.hat)),
    .name_repair = "minimal"
  ) |>
    purrr::map(
      \(x) tibble::as_tibble(purrr::map(funs, rlang::exec, x, na.rm = TRUE))
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(name = rep(names(X_orig), 2)) |>
    dplyr::summarise(
      std_abs_mean_diff_adj = abs(diff(mean)) / sqrt(sum(var)),
      .by = "name"
    ) |>
    dplyr::arrange(.data$std_abs_mean_diff_adj)
  asmd_plot_data <-
    dplyr::inner_join(
      df_ori,
      df_adj,
      by = "name",
      relationship = "one-to-one"
    ) |>
    dplyr::rename(
      covariate_name = "name",
      `Before adjustment` = "std_abs_mean_diff",
      `After adjustment` = "std_abs_mean_diff_adj"
    ) |>
    tidyr::pivot_longer(
      cols = c("Before adjustment", "After adjustment"),
      names_to = "Cohort",
      values_to = "value"
    )
  covariate_name_ordered <- asmd_plot_data |>
    dplyr::filter(.data$Cohort == "Before adjustment") |>
    dplyr::arrange(.data$value)
  asmd_plot_data <- asmd_plot_data |>
    dplyr::mutate(
      covariate_name = factor(
        .data$covariate_name,
        levels = covariate_name_ordered$covariate_name
      )
    )
  std_abs_mean_diff <- asmd_plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$value,
        y = .data$covariate_name,
        colour = .data$Cohort
      )
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$Cohort),
      orientation = "y"
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 1) +
    ggplot2::geom_vline(xintercept = 0.1, linetype = 2) +
    ggplot2::ylab("") +
    ggplot2::xlab("Absolute standardized mean difference") +
    asmd_scale_color
  if (!(is.null(asmd_breaks) && is.null(asmd_xlim))) {
    std_abs_mean_diff <- std_abs_mean_diff +
      ggplot2::scale_x_continuous(
        breaks = asmd_breaks,
        limits = asmd_xlim
      )
  } else if (!is.null(asmd_breaks)) {
    std_abs_mean_diff <- std_abs_mean_diff +
      ggplot2::scale_x_continuous(
        breaks = asmd_breaks
      )
  } else if (!is.null(asmd_xlim)) {
    std_abs_mean_diff <- std_abs_mean_diff +
      ggplot2::scale_x_continuous(
        limits = asmd_xlim
      )
  }
  # create covariate distribution plots
  cd_plot_data <- tibble::tibble(
    X_orig,
    "{treatment_name}" := as.factor(cf$W.orig),
    IPW = ifelse(
      !!rlang::sym(treatment_name) == 1,
      1 / cf$W.hat,
      1 / (1 - cf$W.hat)
    )
  ) |>
    tidyr::pivot_longer(
      cols = names(X_orig),
      names_to = "covariate_name",
      values_to = "covariate_values"
    )

  plot_type <- X_orig |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(), \(x) ifelse(is.factor(x), "bar", "hist")
      )
    )

  if (is.null(cd_x_scale_width)) {
    cd_x_scale_width <- purrr::map_dbl(
      names(X_orig),
      \(names) {
        cd_plot_data |>
          dplyr::filter(.data$covariate_name == names) |>
          dplyr::pull(.data$covariate_values) |>
          (\(x) signif(diff(range(x)), 1) / 5)()
      }
    )
  } else if (length(cd_x_scale_width) == 1) {
    cd_x_scale_width <- rep(
      cd_x_scale_width,
      ncol(X_orig)
    )
  }

  if (is.null(cd_bar_width)) {
    cd_bar_width <- purrr::map2_dbl(
      names(X_orig),
      plot_type,
      \(names, type) {
        cd_plot_data |>
          dplyr::filter(.data$covariate_name == names) |>
          dplyr::pull(.data$covariate_values) |>
          (\(x) {
            if (type == "bar") {
              0.9 * ggplot2::resolution(x)
            } else {
              diff(range(x)) / 50
            }
          })()
      }
    )
  } else if(length(cd_bar_width) == 1) {
    cd_bar_width <- rep(
      cd_bar_width,
      ncol(X_orig)
    )
  }

  cov_plots_unadjusted <- purrr::pmap(
    list(
      names = names(X_orig),
      type = plot_type,
      cd_x_scale_width = cd_x_scale_width,
      cd_bar_width = cd_bar_width
    ),
    \(names, type, cd_x_scale_width, cd_bar_width) {
      p <- cd_plot_data |>
        dplyr::filter(.data$covariate_name == names) |>
        ggplot2::ggplot() +
        ggplot2::facet_grid(~ .data$covariate_name)
      if(type == "bar") {
        p <- p +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = .data$covariate_values,
              fill = .data[[treatment_name]]
            ),
            position = "dodge",
            width = cd_bar_width
          )
      } else {
        p <- p +
          ggplot2::geom_histogram(
            ggplot2::aes(
              x = .data$covariate_values,
              fill = .data[[treatment_name]]
            ),
            alpha = 0.5,
            position = "identity",
            binwidth = cd_bar_width
          )
      }
      p <- p +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::scale_x_continuous(
          breaks = seq(
            cd_plot_data |>
              dplyr::filter(.data$covariate_name == names)|>
              dplyr::pull(.data$covariate_values) |>
              min() |>
              floor_dec(digits = decimalplaces(cd_x_scale_width)),
            cd_plot_data |>
              dplyr::filter(.data$covariate_name == names)|>
              dplyr::pull(.data$covariate_values) |>
              max() |>
              ceiling_dec(digits = decimalplaces(cd_x_scale_width)),
            cd_x_scale_width
          )
        ) +
        cd_scale_fill
    }
  )
  cov_plots_adjusted <- purrr::pmap(
    list(
      names = names(X_orig),
      type = plot_type,
      cd_x_scale_width = cd_x_scale_width,
      cd_bar_width = cd_bar_width
    ),
    \(names, type, cd_x_scale_width, cd_bar_width) {
      p <- cd_plot_data |>
        dplyr::filter(.data$covariate_name == names) |>
        ggplot2::ggplot() +
        ggplot2::facet_grid(~ .data$covariate_name)
      if(type == "bar") {
        p <- p +
          ggplot2::geom_bar(
            ggplot2::aes(
              x = .data$covariate_values,
              weight = .data$IPW,
              fill = .data[[treatment_name]]
            ),
            position = "dodge",
            width = cd_bar_width
          )
      } else {
        p <- p +
          ggplot2::geom_histogram(
            ggplot2::aes(
              x = .data$covariate_values,
              weight = .data$IPW,
              fill = .data[[treatment_name]]
            ),
            alpha = 0.5,
            position = "identity",
            binwidth = cd_bar_width
          )
      }
      p <- p +
        ggplot2::xlab("") +
        ggplot2::ylab("") +
        ggplot2::scale_x_continuous(
          breaks = seq(
            cd_plot_data |>
              dplyr::filter(.data$covariate_name == names)|>
              dplyr::pull(.data$covariate_values) |>
              min() |>
              floor_dec(digits = decimalplaces(cd_x_scale_width)),
            cd_plot_data |>
              dplyr::filter(.data$covariate_name == names)|>
              dplyr::pull(.data$covariate_values) |>
              max() |>
              ceiling_dec(digits = decimalplaces(cd_x_scale_width)),
            cd_x_scale_width
          )
        ) +
        cd_scale_fill
    }
  )
  cd_plot_unadjusted <- cowplot::ggdraw(
    arrangeGrob(
      patchwork::patchworkGrob(
        (
          Reduce("+", cov_plots_unadjusted) +
            patchwork::plot_layout(
              nrow = cd_nrow,
              ncol = cd_ncol,
              guides = "collect"
            ) &
            ggplot2::theme(legend.position = "top")
        ) +
          patchwork::plot_annotation(
            title = "Covariate plots (before adjustment)"
          )
      ),
      left = "count", bottom = "covariate values"
    )
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
  cd_plot_adjusted <- cowplot::ggdraw(
    arrangeGrob(
      patchwork::patchworkGrob(
        (
          Reduce("+", cov_plots_adjusted) +
            patchwork::plot_layout(
              nrow = cd_nrow,
              ncol = cd_ncol,
              guides = "collect"
            ) &
            ggplot2::theme(legend.position = "top")
        ) +
          patchwork::plot_annotation(
            title = "Covariate plots (after adjustment)"
          )
      ),
      left = "count", bottom = "covariate values"
    )
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  return(
    list(
      asmd_data = asmd_plot_data,
      asmd = std_abs_mean_diff,
      cd_data = cd_plot_data,
      cd_unadjusted = cd_plot_unadjusted,
      cd_adjusted = cd_plot_adjusted
    )
  )
}


#' Round numbers down to a given number of decimal places
#'
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places
#'
#' @return The rounded down numeric vector

floor_dec <- function(x, digits = 1) {
  round(x - 5 * 10^(-digits - 1), digits)
}

#' Round numbers up to a given number of decimal places
#'
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places
#'
#' @return The rounded up numeric vector

ceiling_dec <- function(x, digits = 1) {
  round(x + 5 * 10^(-digits - 1), digits)
}

#' Determine number of decimal places
#'
#' @param x Numeric, a single decimal number
#'
#' @return The number of decimal places in x

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    0
  }
}
