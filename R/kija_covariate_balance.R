#' Plots for checking covariate balance in causal forest
#'
#' Generate plots showing balance in the covariates before and after propensity
#' score weighting with a causal forest object.
#'
#' @param cf An object of class causal_forest (and inheriting from class grf).
#' @param plots Character, `"all"` returns both Love plots and density plots,
#'   `"Love"` returns only Love plots, `"density"` returns only density plots.
#' @param covariates A vector to select covariates to show in balance plots. If
#'   `cf$X.orig` is an unnamed matrix, use a numeric vector to select variables.
#'   Otherwise use a character vector. Names provided in the `names` argument
#'   takes priority over existing names in `cf$X.orig`. If discrete covariates
#'   have been one-hot encoded using \link[EpiForsk]{DiscreteCovariatesToOneHot}
#'   the name of these discrete covariates can be provided in `covariates` to
#'   select it and to collect all levels into a bar plot to show the
#'   distribution.
#' @param names A named character vector. The vector itself should contain
#'   covariate names from the causal_forest object, while the names attribute
#'   should contain the names to use when plotting. If discrete covariates have
#'   been one-hot encoded using \link[EpiForsk]{DiscreteCovariatesToOneHot},
#'   providing just the name of a discrete covariate will modify the name of all
#'   levels for plotting. If the vector is unnamed, the provided vector will act
#'   as the new covariate names, given in the order of `cf$X_orig`. If `NULL`
#'   (the default), the original names are used.
#' @param treatment_name Character, name of treatment.
#' @param love_breaks Numeric, breaks used in the plot of absolute standardized
#'   mean differences.
#' @param love_xlim Numeric, `x`-limits used in the plot of absolute
#'   standardized mean differences.
#' @param love_scale_color Function, `scale_color_.` function to use in the plot
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
#' @return A list with up to five elements:
#' - love_data: data used to plot the absolute standardized mean differences.
#' - love: plot object for absolute standardized mean differences.
#' - cd_data: data used to plot covariate distributions.
#' - cd_unadjusted: plot of unadjusted covariate distributions in the exposure
#'   groups.
#' - cd_adjusted: plot of adjusted covariate distributions in the exposure
#'   groups.
#'
#' @details
#' If an unnamed character vector is provided in `names`, it must have length
#' `ncol(cf$X.orig)`. Names of covarates not selected by `covariates` can be set
#' to `NA`. If a named character vector is provided in `names`, all renamed
#' covariates will be keept regardless if they are selected in `covariates`.
#' Thus to select only renamed covariates, `character(0)` can be used in
#' `covariates`. The plot theme can be adjusted using ggplot2 active theme
#' modifiers, see \link[ggplot2]{theme_get}.
#'
#' @author KIJA
#'
#' @examples
#' n <- 1000
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p) |>
#' as.data.frame() |>
#' dplyr::bind_cols(
#'   DiscreteCovariatesToOneHot(
#'     tibble::tibble(
#'       D1 = factor(
#'         sample(1:3, n, replace = TRUE, prob = c(0.2, 0.3, 0.5)),
#'         labels = c("first", "second", "third")
#'       ),
#'       D2 = factor(
#'         sample(1:2, n, replace = TRUE, prob = c(0.2, 0.8)),
#'         labels = c("a", "b")
#'       )
#'     )
#'   )
#' ) |>
#' dplyr::select(
#'   V1,
#'   V2,
#'   dplyr::starts_with("D1"),
#'   V3,
#'   V4,
#'   dplyr::starts_with("D2"),
#'   V5
#' )
#' expo_prob <- 1 / (1 + exp(0.4 * X[, 1] + 0.2 * X[, 2] - 0.6 * X[, 3] +
#'                           0.4 * X[, 6] + 0.6 * X[, 8] - 0.2 * X[, 9]))
#' W <- rbinom(n, 1, expo_prob)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2] +
#'                            X[, 6] + 3 * X[, 9])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' cb1 <- CovariateBalance(cf)
#' cb2 <- CovariateBalance(
#'   cf,
#'   covariates = character(0),
#'   names = c(
#'   "medium imbalance" = "V1",
#'   "low imbalance" = "V2",
#'   "high imbalance" = "V3",
#'   "no imbalance" = "V4",
#'   "discrete 1" = "D1",
#'   "discrete 2" = "D2"
#'   )
#' )
#' cb3 <- CovariateBalance(
#'   cf,
#'   covariates = character(0),
#'   names = c(
#'     "medium imbalance" = "V1",
#'     "low imbalance" = "V2",
#'     "high imbalance" = "V3",
#'     "no imbalance" = "V4"
#'   ),
#'   treatment_name = "Treatment",
#'   love_breaks = seq(0, 0.5, 0.1),
#'   love_xlim = c(0, 0.5),
#'   cd_nrow = 2,
#'   cd_x_scale_width = 1,
#'   cd_bar_width = 0.3
#' )
#'
#' @export

CovariateBalance <- function(cf,
                             plots = c("all", "Love", "density"),
                             covariates = NULL,
                             names = NULL,
                             treatment_name = "W",
                             love_breaks = NULL,
                             love_xlim = NULL,
                             love_scale_color = NULL,
                             cd_nrow = NULL,
                             cd_ncol = NULL,
                             cd_x_scale_width = NULL,
                             cd_bar_width = NULL,
                             cd_scale_fill = NULL) {
  # check input
  plots <- match.arg(plots)
  love <- plots == "all" | plots == "Love"
  density <- plots == "all" | plots == "density"
  stopifnot(
    "cf must be a 'causal_forest' object" =
      inherits(cf, c("causal_forest", "causal_survival_forest"))
  )
  if (!inherits(cf, "grf")) {
    warning(
      "cf doesn't inherit from class 'grf'. Make sure your 'causal_forest'
object complies with the class structure used by grf::causal_forest()."
    )
  }
  if (is.null(names)) {
    if (!tibble::is_tibble(cf$X.orig)) {
      X_orig <- tibble::as_tibble(cf$X.orig, .name_repair = "unique")
    } else {
      X_orig <- cf$X.orig
    }
    if (!is.null(covariates)) {
      if (is.null(colnames(cf$X.orig))) {
        X_orig <- dplyr::select(X_orig, {{ covariates }})
      } else {
        X_orig <- dplyr::select(X_orig, dplyr::all_of(covariates))
      }
    }
  } else if (is.character(names)) {
    if (!tibble::is_tibble(cf$X.orig)) {
      X_orig <- tibble::as_tibble(cf$X.orig, .name_repair = "minimal")
    } else {
      X_orig <- cf$X.orig
    }
    if (is.null(names(names))) {
      X_orig <- purrr::set_names(X_orig, names)
      if (!is.null(covariates)) {
        X_orig <- dplyr::select(X_orig, dplyr::all_of(covariates))
      }
    } else {
      which_fct <- purrr::map_lgl(
        names,
        function(names) {
          sum(grepl(paste0("^", names), names(X_orig))) > 1
        }
      )
      if (sum(which_fct) > 0) {
        X_orig_fct <- X_orig
        for (i in which(which_fct)) {
          which_var <- which(grepl(paste0("^", names[i]), names(X_orig_fct)))
          tmp <- vector("character", nrow(X_orig_fct))
          for (j in which_var) {
            tmp[dplyr::pull(X_orig_fct, j) == 1] <-
              sub(paste0("^", names[i], "_"), "", names(X_orig_fct)[j])
          }
          X_orig_fct <- X_orig_fct |>
            dplyr::select(!dplyr::all_of(which_var)) |>
            dplyr::mutate(
              "{names(names)[i]}" := factor(tmp)
            )
        }
        X_orig_fct <- dplyr::rename(X_orig_fct, dplyr::all_of(names[!which_fct]))
        X_orig <- dplyr::rename(X_orig, dplyr::all_of(names[!which_fct]))
        for (i in which(which_fct)) {
          names(X_orig)[grepl(paste0("^", names[i]), names(X_orig))] <-
            sub(
              paste0(names[i], "_"),
              paste0(names(names)[i], " - "),
              names(X_orig)[grepl(paste0("^", names[i]), names(X_orig))]
            )
        }
        if (!is.null(covariates)) {
          covariates <- unique(c(covariates, names(names)))
          X_orig_fct <- dplyr::select(X_orig_fct, dplyr::all_of(covariates))
          X_orig <- dplyr::select(X_orig, dplyr::starts_with(covariates))
        }
      } else {
        X_orig <- dplyr::rename(X_orig, dplyr::all_of(names))
        if (!is.null(covariates)) {
          covariates <- unique(c(covariates, names(names)))
          X_orig <- dplyr::select(X_orig, dplyr::all_of(covariates))
        }
      }
    }
  } else {
    stop(
      "'names' must either be NULL to use the original names from cf$X.orig,
a character vector with covariate names that replace names(cf$X.orig), or
a named character vector with the original names and the replacement names
in the names attribute."
    )
  }
  if (!(is.null(cd_x_scale_width) || is.numeric(cd_x_scale_width))) {
    stop (
      glue::glue(
        "'cd_x_scale_width' must be NULL or a numeric vector of length 1 or ",
        "{ncol(X_orig)}."
      )
    )
  }
  if (!(length(cd_x_scale_width) %in% c(0, 1, ncol(X_orig)))) {
    stop (
      glue::glue(
        "'cd_x_scale_width' has the wrong length. Must be NULL or have ",
        "length 1 or {ncol(X_orig)}."
      )
    )
  }
  if (!(is.character(treatment_name) && length(treatment_name) == 1)) {
    stop (
      "'treatment_name' must be a single string with the treatment name to
display in the covariate distribution plots."
    )
  }
  if (!(is.null(cd_bar_width) || is.numeric(cd_bar_width))) {
    stop (
      glue::glue(
        "'cd_bar_width' must be NULL or a numeric vector of length 1 or ",
        "{ncol(X_orig)}.")
    )
  }
  if (!(length(cd_bar_width) %in% c(0, 1, ncol(X_orig)))) {
    stop (
      glue::glue(
        "'cd_bar_width' has the wrong length. Must be NULL or have ",
        "length 1 or {ncol(X_orig)}."
      )
    )
  }
  if (!(is.null(cd_nrow) || (is.numeric(cd_nrow) && cd_nrow > 0.5))) {
    stop ("'cd_nrow' must be NULL or a positive integer.")
  }
  if (!(is.null(cd_ncol) || (is.numeric(cd_ncol) && cd_ncol > 0.5))) {
    stop ("'cd_ncol' must be NULL or a positive integer.")
  }

  # create Love plot
  if (love) {
    if (is.null(love_scale_color)) {
      if (requireNamespace("ggsci", quietly = TRUE)) {
        love_scale_color <- ggsci::scale_color_jama()
      } else {
        love_scale_color <- ggplot2::scale_color_discrete()
      }
    }
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
        \(x) tibble::as_tibble(purrr::map(funs, purrr::exec, x, na.rm = TRUE))
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(name = rep(names(X_orig), 2)) |>
      dplyr::summarise(
        std_abs_mean_diff = abs(diff(mean)) / sqrt(sum(var)),
        .by = "name"
      ) |>
      dplyr::arrange(.data$std_abs_mean_diff)
    df_adj <- tibble::as_tibble(
      cbind(X_orig * cf$W.orig / cf$W.hat,
            X_orig * (1 - cf$W.orig) / (1 - cf$W.hat)),
      .name_repair = "minimal"
    ) |>
      purrr::map(
        \(x) tibble::as_tibble(purrr::map(funs, purrr::exec, x, na.rm = TRUE))
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(name = rep(names(X_orig), 2)) |>
      dplyr::summarise(
        std_abs_mean_diff_adj = abs(diff(mean)) / sqrt(sum(var)),
        .by = "name"
      ) |>
      dplyr::arrange(.data$std_abs_mean_diff_adj)
    love_plot_data <-
      dplyr::inner_join(
        df_ori,
        df_adj,
        by = "name",
        relationship = "one-to-one"
      ) |>
      dplyr::rename(
        "covariate_name" = "name",
        "Before adjustment" = "std_abs_mean_diff",
        "After adjustment" = "std_abs_mean_diff_adj"
      ) |>
      tidyr::pivot_longer(
        cols = c("Before adjustment", "After adjustment"),
        names_to = "Cohort",
        values_to = "value"
      )
    covariate_name_ordered <- love_plot_data |>
      dplyr::filter(.data$Cohort == "Before adjustment") |>
      dplyr::arrange(.data$value)
    love_plot_data <- love_plot_data |>
      dplyr::mutate(
        covariate_name = factor(
          .data$covariate_name,
          levels = covariate_name_ordered$covariate_name
        )
      )
    love_plot <- love_plot_data |>
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
      ggplot2::xlab("Absolute standardized mean difference") +
      ggplot2::ylab("") +
      love_scale_color
    if (!(is.null(love_breaks) && is.null(love_xlim))) {
      love_plot <- love_plot +
        ggplot2::scale_x_continuous(
          breaks = love_breaks,
          limits = love_xlim
        )
    } else if (!is.null(love_breaks)) {
      love_plot <- love_plot +
        ggplot2::scale_x_continuous(
          breaks = love_breaks
        )
    } else if (!is.null(love_xlim)) {
      love_plot <- love_plot +
        ggplot2::scale_x_continuous(
          limits = love_xlim
        )
    }
  }

  # create covariate distribution plots
  if (density) {
    if (exists("X_orig_fct")) {
      X_orig <- X_orig_fct
    }
    if (is.null(cd_scale_fill)) {
      if (requireNamespace("ggsci", quietly = TRUE)) {
        cd_scale_fill <- ggsci::scale_fill_jama()
      } else {
        cd_scale_fill <- ggplot2::scale_fill_discrete()
      }
    }

    cd_plot_data <- tibble::tibble(
      X_orig,
      "{treatment_name}" := as.factor(cf$W.orig),
      IPW = ifelse(
        .data[[treatment_name]] == 1,
        1 / cf$W.hat,
        1 / (1 - cf$W.hat)
      )
    )

    plot_type <- X_orig |>
      dplyr::summarise(
        dplyr::across(
          dplyr::everything(), \(x) ifelse(is.factor(x), "bar", "hist")
        )
      )

    if (is.null(cd_x_scale_width)) {
      nm_num <- names(X_orig)
      if (exists("which_fct")) {
        nm_num <- nm_num[!(names(X_orig) %in% names(names)[which_fct])]
      }
      if (length(nm_num) == 0) {
        cd_x_scale_width <- vector("numeric", ncol(X_orig))
      } else {
        scale_width_dt <- data.table::as.data.table(cd_plot_data)
        scale_width_dt <- scale_width_dt[
          ,
          (nm_num) := lapply(.SD, \(x) signif(diff(range(x)), 1) / 5),
          .SDcols = nm_num
        ]
        cd_x_scale_width <- suppressWarnings(
          as.numeric(scale_width_dt[1])[seq_len(ncol(X_orig))]
        )
      }
    } else if (length(cd_x_scale_width) == 1) {
      cd_x_scale_width <- rep(
        cd_x_scale_width,
        ncol(X_orig)
      )
    }

    if (is.null(cd_bar_width)) {
      bar_width_dt <- cd_plot_data |>
        dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric)) |>
        data.table::as.data.table()
      bar_width_dt <- bar_width_dt[
        ,
        (names(X_orig)) := purrr::map2(
          .SD,
          plot_type,
          \(x, y) {
            if (any(y == "bar")) {
              0.9 * ggplot2::resolution(x)
            } else {
              diff(range(x)) / 50
            }
          }
        ),
        .SDcols = names(X_orig)
      ]
      cd_bar_width <- as.numeric(bar_width_dt[1])[seq_len(ncol(X_orig))]
    } else if (length(cd_bar_width) == 1) {
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
        plot_data <- cd_plot_data |>
          dplyr::select(
            dplyr::all_of(treatment_name),
            "IPW",
            "covariate_values" = dplyr::all_of(names)
          ) |>
          dplyr::mutate(
            "covariate_name" = names
          )
        covariate_values <- plot_data[["covariate_values"]]
        p <- plot_data |>
          ggplot2::ggplot() +
          ggplot2::facet_grid(~ .data$covariate_name)
        if (type == "bar") {
          p <- p +
            ggplot2::geom_bar(
              ggplot2::aes(
                x = .data$covariate_values,
                fill = .data[[treatment_name]]
              ),
              alpha = 0.5,
              position = "dodge",
              width = cd_bar_width
            )
        } else {
          p <- p +
            ggplot2::geom_histogram(
              ggplot2::aes(
                x = .data$covariate_values,
                y = ggplot2::after_stat(density),
                fill = .data[[treatment_name]]
              ),
              alpha = 0.5,
              position = "identity",
              binwidth = cd_bar_width
            ) +
            ggplot2::scale_x_continuous(
              breaks = seq(
                floor_dec(
                  min(covariate_values),
                  digits = decimalplaces(cd_x_scale_width)
                ),
                ceiling_dec(
                  max(covariate_values),
                  digits = decimalplaces(cd_x_scale_width)
                ),
                cd_x_scale_width
              )
            )
        }
        p <- p +
          ggplot2::xlab("") +
          ggplot2::ylab("") +
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
        plot_data <- cd_plot_data |>
          dplyr::select(
            dplyr::all_of(treatment_name),
            "IPW",
            "covariate_values" = dplyr::all_of(names)
          ) |>
          dplyr::mutate(
            "covariate_name" = dplyr::all_of(names)
          )
        covariate_values <- plot_data[["covariate_values"]]
        p <- plot_data |>
          ggplot2::ggplot() +
          ggplot2::facet_grid(~ .data$covariate_name)
        if (type == "bar") {
          p <- p +
            ggplot2::geom_bar(
              ggplot2::aes(
                x = .data$covariate_values,
                weight = .data$IPW,
                fill = .data[[treatment_name]]
              ),
              alpha = 0.5,
              position = "dodge",
              width = cd_bar_width
            )
        } else {
          p <- p +
            ggplot2::geom_histogram(
              ggplot2::aes(
                x = .data$covariate_values,
                y = ggplot2::after_stat(density),
                weight = .data$IPW,
                fill = .data[[treatment_name]]
              ),
              alpha = 0.5,
              position = "identity",
              binwidth = cd_bar_width
            ) +
            ggplot2::scale_x_continuous(
              breaks = seq(
                floor_dec(
                  min(covariate_values),
                  digits = decimalplaces(cd_x_scale_width)
                ),
                ceiling_dec(
                  max(covariate_values),
                  digits = decimalplaces(cd_x_scale_width)
                ),
                cd_x_scale_width
              )
            )
        }
        p <- p +
          ggplot2::xlab("") +
          ggplot2::ylab("") +
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
        left = "density/count", bottom = "covariate values"
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
        left = "density/count", bottom = "covariate values"
      )
    ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  }

  # return list with plots
  out <- c()
  if (love) {
    out <- list(
      love_data = love_plot_data,
      love = love_plot
    )
  }
  if (density) {
    out <- c(
      out,
      list(
        cd_data = cd_plot_data,
        cd_unadjusted = cd_plot_unadjusted,
        cd_adjusted = cd_plot_adjusted
      )
    )
  }
  return(out)
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
