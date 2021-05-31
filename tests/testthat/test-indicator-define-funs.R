# Tests for function of indicator define funs  ----
context("Tests for function of indicator define funs")

# prepare test datasets
# set up testing context
dsn <- "GTA_SQLData"
DB_PROFILE_FILE <- "gta_profile.xlsx"

stock_db <- stock_db(gta_db, dsn)
suppressMessages(db_ready <- open_stock_db(stock_db))
# skip tests if test dsn is not ready
skip_if_not(db_ready,
  message = sprintf("DSN(%s) is not ready, skip all tests for stock_db", dsn)
)
suppressMessages(init_stock_db(stock_db))

# Functions for defining indicator expr ----

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

  actual_ratio <- Ratio(x, y)
  expect_equal(mean(actual_ratio, na.rm = TRUE), expect_ratio)
})

test_that("Sum", {

  # Sum with default arguments ====
  # >> Sum non NA Series ----
  x <- rep(1, series_length)
  y <- x
  z <- x
  expect_sum <- x + y + z

  actual_sum <- Sum(x, y, z)
  expect_equal(actual_sum, expect_sum)

  # >> Sum on NA Series ----
  na_length <- series_length %/% 2
  x <- 1:series_length
  y <- c(x[1:(series_length - na_length)], rep(NA, na_length))
  z <- c(x[1:(series_length - na_length)], rep(NA, na_length))
  expect_sum <- c(
    x[1:(series_length - na_length)] * 3,
    x[(na_length + 1):series_length] +
      rep(0, na_length) +
      rep(0, na_length)
  )
  # compute sum of series
  actual_sum <- Sum(x, y, z)
  expect_equal(actual_sum, expect_sum)

  # Sum with various arguments ====
  # >> argument: substitue_NA ----
  x <- 1:series_length
  y <- c(x[1:(series_length - na_length)], rep(NA, na_length))
  z <- c(x[1:(series_length - na_length)], rep(NA, na_length))
  substitute_NA_values <- c("zero", "mean", "median", "keep")

  for (i in seq_along(substitute_NA_values)) {

    # compute replace_na_value
    substitute_NA <- substitute_NA_values[i]
    replace_na_value <- switch(substitute_NA,
      "zero" = 0,
      "mean" = mean(x[1:(series_length - na_length)]),
      "median" = median(x[1:(series_length - na_length)]),
      "keep" = NA
    )
    expect_sum <- c(
      x[1:(series_length - na_length)] * 3,
      x[(na_length + 1):series_length] +
        rep(replace_na_value, na_length) +
        rep(replace_na_value, na_length)
    )

    # compute sum of series
    actual_sum <- Sum(x, y, z, substitute_NA = substitute_NA)

    expect_equal(actual_sum, expect_sum)
  }
})

test_that("Demean", {

  # Demean with default arguments ====
  x <- sample(1:series_length, series_length)
  x_demean <- Demean(x)

  expect_true(mean(x_demean, na.rm = TRUE) == 0)
})

test_that("Quarter_TTM", {

  # function to create time series
  create_periodic_ts <- function(start_date,
                                   end_date,
                                   accumulated = TRUE,
                                   period = c("day", "month", "quarter")) {
    match.arg(period)
    accumulated_periodic_data <- switch(period,
      "day" = lubridate::yday,
      "month" = lubridate::month,
      "quarter" = lubridate::quarter
    )

    date <- seq(start_date, end_date, by = period)
    date <- lubridate::ceiling_date(date, unit = period) - 1

    if (accumulated) {
      # accumulated data with increament of 1
      x <- accumulated_periodic_data(date)
    } else {
      # non-accumulated data with no-increament
      x <- rep(1.0, length(date))
    }

    ts <- tibble::tibble(date = date, x = x, y = x * 2)
    return(ts)
  }

  # Quarter_TTM on valid date series ====
  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_quarter_regular <- create_periodic_ts(start_date, end_date,
    accumulated = TRUE,
    period = "quarter"
  )

  ts_trail <- Quarter_TTM(ts_quarter_regular$date, ts_quarter_regular$x)

  expect_equivalent(mean(ts_trail, na.rm = TRUE), 4)
  expect_equal(sum(is.na(ts_trail)), 3)
})

test_that("Beta", {

  # Beta with default arguments ====
  expect_beta <- 2
  x <- sample(1:series_length, series_length)
  y <- x * expect_beta
  actual_beta <- Beta(y * 2, x * 2)
  expect_equivalent(actual_beta, expect_beta)
})

# Functions for defining dynamic indicator expr ----

test_that("RiskFreeRate", {

  # RiskFreeRate with default arguments ====
  rf_return <- RiskFreeRate(stock_db, "d_risk_rate")

  expect_is(rf_return, "data.frame")
  expect_fields <- c("date", "period", "d_risk_rate")
  actual_fields <- names(rf_return)
  expect_true(all(actual_fields %in% expect_fields))
  expect_true(is_periodic_dates(rf_return$date, freq_rule = "day"))
  expect_true(all(rf_return$period %in% "day"))

  # RiskFreeRate with various arguments ====
  ds_indicators_info <- tibble::tibble(
    indicator_code = c(
      "d_riskfree_rate",
      "m_riskfree_rate",
      "q_riskfree_rate",
      "y_riskfree_rate"
    ),
    period = c("day", "month", "quarter", "year")
  )

  for (i in seq_len(NROW(ds_indicators_info))) {
    indicator_code <- ds_indicators_info$indicator_code[[i]]
    period <- ds_indicators_info$period[[i]]

    rf_return <- RiskFreeRate(stock_db,
      indicator_code = indicator_code,
      period = period
    )

    expect_is(rf_return, "data.frame")
    expect_fields <- c("date", "period", indicator_code)
    actual_fields <- names(rf_return)
    expect_true(all(actual_fields %in% expect_fields))
    expect_true(is_periodic_dates(rf_return$date, freq_rule = period))
    expect_true(all(rf_return$period %in% period))
  }
})

# clear up testing conext
suppressMessages(close_stock_db(stock_db))
