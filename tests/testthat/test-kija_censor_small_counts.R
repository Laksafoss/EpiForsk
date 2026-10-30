test_that(
  "inspection warns without changing data",
  {
    x <- data.frame(n = c(0L, 2L, 5L), text = c("1", "4", "7"))
    expect_warning(
      out <- CensorSmallCounts(x, columns = "n", censor = FALSE),
      "Found 2 count\\(s\\) below 5"
    )
    expect_identical(out, x)
  }
)

test_that(
  "allow_zero leaves zero counts visible when TRUE",
  {
    x <- data.frame(n = c(0L, 2L, 5L), text = c("0", "2", "5"))

    out_default <- CensorSmallCounts(x, censor = TRUE, warn = FALSE)
    expect_identical(out_default$n, c(NA_integer_, NA_integer_, 5L))
    expect_identical(out_default$text, c(NA_character_, NA_character_, "5"))

    out_include <- CensorSmallCounts(x, allow_zero = TRUE, censor = TRUE, warn = FALSE)
    expect_identical(out_include$n, c(0L, NA_integer_, 5L))
    expect_identical(out_include$text, c("0", NA_character_, "5"))

    only_zeros <- data.frame(n = 0L, text = "0")
    expect_silent(CensorSmallCounts(only_zeros, allow_zero = TRUE))
    expect_warning(CensorSmallCounts(only_zeros), "Found 2 count")

    out_label <- CensorSmallCounts(only_zeros, allow_zero = TRUE, censor = TRUE, replacement = "label", warn = FALSE)
    expect_identical(out_label, only_zeros)
  }
)

test_that(
  "numeric and character counts are censored selectively",
  {
    x <- data.frame(
      n = c(0L, 2L, 4L, 5L),
      text = c("1", " 2 ", "4.0", "not a count"),
      rate = c(0.25, 0.50, 0.75, 1.25)
    )
    out <- CensorSmallCounts(x, columns = c("n", "text"), rows = 2:3, censor = TRUE, warn = FALSE)
    expect_identical(out$n, c(0L, NA_integer_, NA_integer_, 5L))
    expect_identical(out$text, c("1", NA_character_, NA_character_, "not a count"))
    expect_identical(out$rate, x$rate)
    expect_identical(x$n, c(0L, 2L, 4L, 5L))
  }
)

test_that(
  "explicit cells intersect with row and column selections",
  {
    x <- data.frame(n = c(1L, 2L, 3L), m = c(1L, 2L, 3L))
    coords <- data.frame(row = c(1L, 2L, 3L), column = c("n", "m", "m"))
    out <- CensorSmallCounts(
      x, columns = "m",
      rows = 2:3,
      cells = coords,
      warn = FALSE,
      censor = TRUE,
      replacement = "label"
    )
    expect_identical(out$n, x$n)
    expect_identical(out$m, c("1", "<5", "<5"))
  }
)

test_that(
  "numeric matrices and dimnames are preserved where possible",
  {
    x <- matrix(c(1, 5, 2, 7), nrow = 2, dimnames = list(c("a", "b"), c("n", "m")))
    out_na <- CensorSmallCounts(x, cells = matrix(c(1, 2), nrow = 1), warn = FALSE, censor = TRUE)
    expect_identical(dimnames(out_na), dimnames(x))
    expect_true(is.na(out_na[1, 2]))
    expect_type(out_na, "double")

    out_label <- CensorSmallCounts(x, columns = "n", warn = FALSE, censor = TRUE, replacement = "label")
    expect_type(out_label, "character")
    expect_identical(dimnames(out_label), dimnames(x))
    expect_identical(out_label[1, 1], "<5")
    expect_identical(out_label[2, 1], "5")
    expect_identical(x[1, 1], 1)
  }
)

test_that(
  "non-count values, factors and dates are not censored",
  {
    x <- data.frame(
      count = c(-1, 2.5, 0, Inf, NA_real_),
      factor = factor(c("1", "2", "3", "4", "5")),
      date = as.Date("2020-01-01") + 0:4,
      text = c("1%", "no", NA_character_, "3", "<5")
    )
    out <- CensorSmallCounts(x, censor = TRUE, warn = FALSE)
    expect_identical(out$count, c(-1, 2.5, NA_real_, Inf, NA_real_))
    expect_identical(out$factor, x$factor)
    expect_identical(out$date, x$date)
    expect_identical(out$text, c("1%", "no", NA_character_, NA_character_, "<5"))
  }
)

test_that(
  "invalid selection and options fail loudly",
  {
    x <- data.frame(n = 1:3)
    expect_error(CensorSmallCounts(x, threshold = 1.5), "non-negative integer")
    expect_error(CensorSmallCounts(x, columns = "missing"), "unknown")
    expect_error(CensorSmallCounts(x, rows = 4), "invalid positions")
    expect_error(CensorSmallCounts(x, cells = data.frame(row = 1)), "cells")
    expect_error(CensorSmallCounts(x, warn = NA), "TRUE or FALSE")
    expect_error(CensorSmallCounts(x, allow_zero = NA), "TRUE or FALSE")
    expect_error(CensorSmallCounts(x, allow_zero = 1), "TRUE or FALSE")
    expect_error(CensorSmallCounts(x, replacement = "bad"))
  }
)

test_that(
  "data.table is returned without modifying the input",
  {
    x <- data.table::data.table(n = c(1L, 6L))
    out <- CensorSmallCounts(x, censor = TRUE, warn = FALSE)
    expect_true(data.table::is.data.table(out))
    expect_identical(x$n, c(1L, 6L))
    expect_identical(out$n, c(NA_integer_, 6L))
  }
)

