# Tests for function of indicator define funs  ----
context("Tests for function of f indicator define funs")

# prepare test datasets

series_length <- 10

test_that("Lag", {

  x <- sample(1:series_length, series_length)
  y <- sample(1:series_length, series_length)

  # Lag with default arguments ====
  actual_lag_x <- Lag(x)
  expect_lag_x <- c(rep(NA, 1), x[1:series_length - 1])
  expect_equal(actual_lag_x, expect_lag_x)

  # Lag with various arguments ====
  for (i in -series_length:series_length) {
    actual_x <- Lag(x, k = i)

    expect_x <- if (i > 0) {
      na_series <- rep(NA_integer_, i)
      if (length(na_series) < length(x)) {
        c(na_series, x[1:(series_length - i)])
      } else {
        na_series
      }
    } else if (i < 0) {
      na_series <- rep(NA_integer_, abs(i))
      if (length(na_series) < length(x)) {
        c(x[(abs(i) + 1):series_length], na_series)
      } else {
        na_series
      }
    } else {
      x
    }
    expect_identical(actual_x, expect_x)
  }
})

test_that("GrowthRate", {

  # GrowthRate with default arguments ====
  expect_growth <- 0.5
  x <- 1:series_length
  x <- purrr::accumulate(x, ~ .x * (1 + expect_growth))

  actual_growth <- GrowthRate(x)
  expect_equal(mean(actual_growth, na.rm = TRUE), expect_growth)

})

test_that("Ratio", {

  # Ratio with default arguments ====
  expect_ratio <- 0.5
  x <- 1:series_length
  y <- x / expect_ratio

  actual_Ratio <- Ratio(x, y)
  expect_equal(mean(actual_Ratio, na.rm = TRUE), expect_ratio)

})

test_that("Demean", {

  # Ratio with default arguments ====
  x <- sample(1:series_length, series_length)
  x_demean <- Demean(x)

  expect_true(mean(x_demean, na.rm = TRUE) == 0)

})

test_that("Beta", {

  # Beta with default arguments ====
  expect_beta <- 2
  x <- sample(1:series_length, series_length)
  y <- x * expect_beta
  actual_beta <- Beta(y*2, x*2)
  expect_equivalent(actual_beta, expect_beta)

})


