# Tests for utility functions of dates ----

context("Tests for utitlity functions of dates")

# prepare test datasets

# regular date series
regular_daily_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "day")
regular_monthly_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "month")
regular_quarterly_dates <- seq(as.Date("2018/1/1"), as.Date("2018/12/31"), by = "quarters")
regular_yearly_dates <- seq(as.Date("2001/1/1"), as.Date("2018/1/1"), by = "years")

# irregular date series
irregular_daily_dates <- regular_daily_dates +
  sample(c(-1, 0, 1), size = length(regular_daily_dates), replace = TRUE)
irregular_monthly_dates <- regular_monthly_dates +
  sample(c(-2, 0, 2), size = length(regular_monthly_dates), replace = TRUE)
irregular_quarterly_dates <- regular_quarterly_dates +
  sample(c(-3, 0, 3), size = length(regular_quarterly_dates), replace = TRUE)
irregular_yearly_dates <- regular_yearly_dates +
  sample(c(-3, 0, 3), size = length(regular_yearly_dates), replace = TRUE)



test_that("is_dates_periodic, with various arguments", {

  # is_dates_periodic on regular dates series  ====
  expect_true(is_dates_periodic(regular_daily_dates, freq_rule = "day"))
  expect_true(is_dates_periodic(regular_monthly_dates, freq_rule = "month"))
  expect_true(is_dates_periodic(regular_quarterly_dates, freq_rule = "quarter"))
  expect_true(is_dates_periodic(regular_yearly_dates, freq_rule = "year"))

  # is_dates_periodic on irregular dates series  ====
  expect_true(is_dates_periodic(irregular_daily_dates, freq_rule = "day"))
  expect_true(is_dates_periodic(irregular_monthly_dates, freq_rule = "month"))
  expect_true(is_dates_periodic(irregular_quarterly_dates, freq_rule = "quarter"))
  expect_true(is_dates_periodic(irregular_yearly_dates, freq_rule = "year"))

})

test_that("guess_dates_period, with various arguments", {

  # guess_dates_period on regular dates series  ====
  expect_true(guess_dates_period(regular_daily_dates) == "D")
  expect_true(guess_dates_period(regular_monthly_dates) == "M")
  expect_true(guess_dates_period(regular_quarterly_dates) == "Q")
  expect_true(guess_dates_period(regular_yearly_dates) == "Y")

  # guess_dates_period on irregular dates series  ====
  expect_true(guess_dates_period(irregular_daily_dates) == "D")
  expect_true(guess_dates_period(irregular_monthly_dates) == "M")
  expect_true(guess_dates_period(irregular_quarterly_dates) == "Q")
  expect_true(guess_dates_period(irregular_yearly_dates) == "Y")

})
