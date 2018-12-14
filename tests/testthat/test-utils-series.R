# Tests for utility functions of series ----

context("Tests for utitlity functions of series")

# function to create time series
create_periodic_accumlated_ts <- function(start_date, end_date,
                                          period = c("day", "month", "quarter")) {
  match.arg(period)
  generate_periodic_data <- switch(period,
    "day" = lubridate::yday,
    "month" = lubridate::month,
    "quarter" = lubridate::quarter
  )

  date <- seq(start_date, end_date, by = period)
  date <- lubridate::ceiling_date(date, unit = period) - 1
  x <- generate_periodic_data(date)
  ts <- tibble::tibble(date = date, x = x, y = x * 2)
  return(ts)
}

test_that("rollify_series, with various arguments", {

  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_month <- create_periodic_accumlated_ts(start_date, end_date, period = "month")

  # rollify_series on a series ====
  result_series <- rollify_series(ts_month$x, fun = max, window = 12)
  expect_equal(length(result_series), length(ts_month$x))
  expect_equal(mean(result_series, na.rm = TRUE), max(ts_month$x))

  # rollify_series on two series ====
  result_series <- rollify_series(ts_month,
    fun = purrr::as_mapper(~max(.x$x, .x$y)),
    window = 12
  )

  expect_equal(length(result_series), NROW(ts_month))
  expect_equal(mean(result_series, na.rm = TRUE), max(max(ts_month$x), max(ts_month$y)))
})

test_that("trail_periodic_series, with various arguments", {

  # trail_periodic_series on default arguments ====

  # create periodic timeseries
  start_date <- as.Date("2018/1/1")
  end_date <- as.Date("2019/12/31")
  ts_day <- create_periodic_accumlated_ts(start_date, end_date, period = "day")

  ts_trail <- trail_periodic_series(ts_day$date, ts_day[, c("x", "y")])
  expect_equivalent(colMeans(ts_trail, na.rm = TRUE), c(365, 730))

  # trail_periodic_series on default arguments ====

  # create dataset of periodic timeseries
  ds_periodic_accumlated_ts <- tibble::tibble(
    period = c("day", "month", "quarter"),
    ts = list(NULL),
    expect_result = c(list(c(365, 730)), list(c(12, 24)), list(c(4, 8)))
  )

  ds_periodic_accumlated_ts <- ds_periodic_accumlated_ts %>%
    dplyr::mutate(ts = purrr::map(
      period,
      ~create_periodic_accumlated_ts(start_date,
        end_date,
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
      fun = sum
    )

    expect_equivalent(colMeans(ts_trail, na.rm = TRUE), expect_result)
  }
})
