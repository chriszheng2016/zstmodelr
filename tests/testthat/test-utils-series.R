# Tests for utility functions of series ----

context("Tests for utitlity functions of series")

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

test_that("rollify_series, with various arguments", {

  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_month <- create_periodic_ts(start_date, end_date,
    accumulated = FALSE,
    period = "month"
  )

  # rollify_series with default arguments ====
  # >>rollify_series on a series ----
  result_series <- rollify_series(ts_month$x, fun = mean)
  expect_equal(length(result_series), length(ts_month$x))
  expect_equal(
    mean(result_series, na.rm = TRUE),
    mean(ts_month$x, na.rm = TRUE)
  )

  # >>rollify_series on two series ----
  result_series <- rollify_series(ts_month,
    fun = purrr::as_mapper(~(max(.x$x + .x$y)))
  )
  expect_equal(length(result_series), NROW(ts_month))
  expect_equal(
    mean(result_series, na.rm = TRUE),
    max(mean(ts_month$x) + mean(ts_month$y))
  )


  # rollify_series on various arguments ====
  # >>argument: window ----
  # valid window
  for (i in 1:NROW(ts_month)) {
    window <- as.integer(i)
    result_series <- rollify_series(ts_month$x, fun = mean, window = window)
    expect_equal(length(result_series), length(ts_month$x))
    expect_true(sum(is.na(result_series)) == (window - 1))
  }
  # invalid window
  expect_error(rollify_series(ts_month$x, fun = mean, window = -1L))
  expect_error(rollify_series(ts_month$x, fun = mean, window = 0L))
  expect_error(rollify_series(ts_month$x, fun = mean, window = NROW(ts_month) + 1))


  # >>argument: unlist ----
  result_series <- rollify_series(ts_month$x,
    fun = mean, window = 12L,
    unlist = FALSE
  )
  expect_equal(length(result_series), length(ts_month$x))
  expect_is(result_series, "list")

  # >>argument: na_value ----
  result_series <- rollify_series(ts_month$x,
    fun = mean, window = 12L,
    na_value = NA_integer_
  )
  expect_equal(length(result_series), length(ts_month$x))
  expect_true(any(is.na(result_series)))
})

test_that("trail_periodic_series, with various arguments", {

  # trail_periodic_series on default arguments ====
  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_day <- create_periodic_ts(start_date, end_date,
    accumulated = TRUE,
    period = "day"
  )

  ts_trail <- trail_periodic_series(ts_day$date, ts_day[, c("x", "y")])
  expect_equivalent(colMeans(ts_trail, na.rm = TRUE), c(365, 730))
  expect_equivalent(sum(is.na(ts_trail$x)), 365 - 1)
  expect_equivalent(sum(is.na(ts_trail$y)), 365 - 1)

  # trail_periodic_series on various arguments ====

  # >> argument: period ----
  # create dataset of periodic timeseries
  ds_periodic_accumlated_ts <- tibble::tibble(
    period = c("day", "month", "quarter"),
    ts = list(NULL),
    expect_result = c(list(c(365, 730)), list(c(12, 24)), list(c(4, 8)))
  )

  ds_periodic_accumlated_ts <- ds_periodic_accumlated_ts %>%
    dplyr::mutate(ts = purrr::map(
      period,
      ~create_periodic_ts(start_date,
        end_date,
        accumulated = TRUE,
        period = .x
      )
    ))

  # test for each ts with deferent period
  for (i in seq_len(NROW(ds_periodic_accumlated_ts))) {
    period <- ds_periodic_accumlated_ts$period[[i]]
    ts <- ds_periodic_accumlated_ts$ts[[i]]
    expect_result <- ds_periodic_accumlated_ts$expect_result[[i]]

    ts_trail <- trail_periodic_series(ts$date,
      data_series = ts[, c("x", "y")],
      period = period,
      accumulated = TRUE,
      trailing_month = 12L,
      agg_fun = sum
    )
    expect_equivalent(colMeans(ts_trail, na.rm = TRUE), expect_result)
  }

  # >> argument: accumulated ----
  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")

  for (accumulated in c(TRUE, FALSE)) {
    ts_month <- create_periodic_ts(start_date, end_date,
      accumulated = accumulated,
      period = "month"
    )
    ts_trail <- trail_periodic_series(ts_month$date, ts_month[, c("x", "y")],
      period = "month",
      accumulated = accumulated
    )
    expect_equivalent(colMeans(ts_trail, na.rm = TRUE), c(12, 24))
    expect_equivalent(sum(is.na(ts_trail$x)), 12 - 1)
    expect_equivalent(sum(is.na(ts_trail$y)), 12 - 1)
  }


  # >> argument: trailing_month ----
  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_month <- create_periodic_ts(start_date, end_date,
    accumulated = FALSE,
    period = "month"
  )

  for (i in 1:NROW(ts_month)) {
    trailing_month <- as.integer(i)
    ts_trail <- trail_periodic_series(ts_month$date, ts_month[, c("x", "y")],
      period = "month",
      accumulated = FALSE,
      trailing_month = trailing_month,
      agg_fun = sum
    )
    expect_equivalent(colMeans(ts_trail, na.rm = TRUE), c(i, i * 2))
    expect_equivalent(sum(is.na(ts_trail$x)), i - 1)
    expect_equivalent(sum(is.na(ts_trail$y)), i - 1)
  }

  # >> argument: agg_fun ----
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_month <- create_periodic_ts(start_date, end_date,
    accumulated = TRUE,
    period = "month"
  )

  ts_trail <- trail_periodic_series(ts_month$date, ts_month[, c("x", "y")],
    period = "month",
    agg_fun = mean
  )
  expect_equivalent(colMeans(ts_trail, na.rm = TRUE), c(1, 2))
  expect_equivalent(sum(is.na(ts_trail$x)), 12 - 1)
  expect_equivalent(sum(is.na(ts_trail$y)), 12 - 1)
})
