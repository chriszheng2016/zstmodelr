# Tests for utility functions of dates ----

context("Tests for utility functions of dates")

# prepare test datasets

# regular date series
regular_daily_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "day")
regular_monthly_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "month")
regular_quarterly_dates <- seq(as.Date("2018/1/1"), as.Date("2019/12/31"), by = "quarters")
regular_yearly_dates <- seq(as.Date("2001/1/1"), as.Date("2018/1/1"), by = "years")

# irregular date series

# function to convert regular dates to irregular dates
dates_reg2irreg <- function(regular_dates, n_bad_dates = 5) {
  origin_idx <- 2:(length(regular_dates) - 1)
  rnd_idx <- sample(origin_idx, size = n_bad_dates)
  irregular_dates <- regular_dates[!(origin_idx %in% rnd_idx)]
  return(irregular_dates)
}

irregular_daily_dates <- dates_reg2irreg(regular_daily_dates, 10)
irregular_monthly_dates <- dates_reg2irreg(regular_monthly_dates, 2)
irregular_quarterly_dates <- dates_reg2irreg(regular_quarterly_dates, 1)
irregular_yearly_dates <- dates_reg2irreg(regular_yearly_dates, 2)


test_that("is_periodic_dates, with various arguments", {

  # is_periodic_dates on default arguments  ====

  # >>is_periodic_dates on regular dates series ----
  expect_true(is_periodic_dates(regular_daily_dates,
    freq_rule = "day"
  ))
  expect_true(is_periodic_dates(regular_monthly_dates,
    freq_rule = "month"
  ))
  expect_true(is_periodic_dates(regular_quarterly_dates,
    freq_rule = "quarter"
  ))
  expect_true(is_periodic_dates(regular_yearly_dates,
    freq_rule = "year"
  ))

  # >>is_periodic_dates on irregular dates series  ----
  expect_true(is_periodic_dates(irregular_daily_dates,
    freq_rule = "day"
  ))
  expect_true(is_periodic_dates(irregular_monthly_dates,
    freq_rule = "month"
  ))
  expect_true(is_periodic_dates(irregular_quarterly_dates,
    freq_rule = "quarter"
  ))
  expect_true(is_periodic_dates(irregular_yearly_dates,
    freq_rule = "year"
  ))

  # is_periodic_dates on various arguments  ====

  # >>is_periodic_dates on regular dates series ----
  expect_true(is_periodic_dates(regular_daily_dates,
    freq_rule = "day", regular = TRUE
  ))
  expect_true(is_periodic_dates(regular_monthly_dates,
    freq_rule = "month", regular = TRUE
  ))
  expect_true(is_periodic_dates(regular_quarterly_dates,
    freq_rule = "quarter", regular = TRUE
  ))
  expect_true(is_periodic_dates(regular_yearly_dates,
    freq_rule = "year", regular = TRUE
  ))

  # >>is_periodic_dates on irregular dates series  ----
  expect_true(is_periodic_dates(irregular_daily_dates,
    freq_rule = "day", regular = FALSE
  ))
  expect_true(is_periodic_dates(irregular_monthly_dates,
    freq_rule = "month", regular = FALSE
  ))
  expect_true(is_periodic_dates(irregular_quarterly_dates,
    freq_rule = "quarter", regular = FALSE
  ))
  expect_true(is_periodic_dates(irregular_yearly_dates,
    freq_rule = "year", regular = FALSE
  ))
})

test_that("guess_dates_period, with various arguments", {

  # guess_dates_period on default arguments  ====

  # >>guess_dates_period on regular dates series  ----
  expect_true(guess_dates_period(regular_daily_dates) == "day")
  expect_true(guess_dates_period(regular_monthly_dates) == "month")
  expect_true(guess_dates_period(regular_quarterly_dates) == "quarter")
  expect_true(guess_dates_period(regular_yearly_dates) == "year")

  # >>guess_dates_period on irregular dates series  ----
  expect_true(guess_dates_period(irregular_daily_dates) == "day")
  expect_true(guess_dates_period(irregular_monthly_dates) == "month")
  expect_true(guess_dates_period(irregular_quarterly_dates) == "quarter")
  expect_true(guess_dates_period(irregular_yearly_dates) == "year")

  # guess_dates_period on various arguments  ====

  # >>guess_dates_period on regular dates series  ----
  expect_true(guess_dates_period(regular_daily_dates, regular = TRUE) == "day")
  expect_true(guess_dates_period(regular_monthly_dates, regular = TRUE) == "month")
  expect_true(guess_dates_period(regular_quarterly_dates, regular = TRUE) == "quarter")
  expect_true(guess_dates_period(regular_yearly_dates, regular = TRUE) == "year")

  # >>guess_dates_period on irregular dates series  ----
  expect_true(guess_dates_period(irregular_daily_dates, regular = FALSE) == "day")
  expect_true(guess_dates_period(irregular_monthly_dates, regular = FALSE) == "month")
  expect_true(guess_dates_period(irregular_quarterly_dates, regular = FALSE) == "quarter")
  expect_true(guess_dates_period(irregular_yearly_dates, regular = FALSE) == "year")
})
