
# Build test dataset
ds_test_timeseries <- function(from = "2015-01-01",
                               to = "2016-12-31",
                               by = c("day", "week", "month", "quarter", "year"),
                               output = c("tibble", "timeSeries")) {

  by = match.arg(by)
  date = seq(as.Date(from), as.Date(to), by)
  value = seq(1, length(date))

  output = match.arg(output)
  switch(output,
         "tibble" = {
           ds_test <- tibble::tibble(date = date, value = value)
           },
         "timeSeries" = {
           ds_test <- timeSeries::timeSeries(data = value,
                                               charvec = date,
                                               units = "value")
           }
         )

  return(ds_test)
}


# Generate test datasets ----

start_date <- "2015-01-01"
end_date <- "2018-12-31"

# test datasets of tibble format
ds_test_daily_df <- ds_test_timeseries(start_date,
                                       to = end_date,
                                       by = "day",
                                       output = "tibble")
ds_test_weekly_df <- ds_test_timeseries(start_date,
                                        to = end_date,
                                        by = "week",
                                        output = "tibble")
ds_test_monthly_df <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "month",
                                         output = "tibble")
ds_test_quarterly_df <- ds_test_timeseries(start_date,
                                           to = end_date,
                                           by = "quarter",
                                           output = "tibble")
ds_test_yearly_df <- ds_test_timeseries(start_date,
                                        to = end_date,
                                        by = "year",
                                        output = "tibble")

# test datasets of timeSeries format
#
ds_test_daily_fts <- ds_test_timeseries(start_date,
                                       to = end_date,
                                       by = "day",
                                       output = "timeSeries")
ds_test_weekly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "week",
                                         output = "timeSeries")
ds_test_monthly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "month",
                                         output = "timeSeries")
ds_test_quarterly_fts <- ds_test_timeseries(start_date,
                                           to = end_date,
                                           by = "quarter",
                                           output = "timeSeries")
ds_test_yearly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "year",
                                         output = "timeSeries")

# Tests for utility functions of timeseries ----
context("Tests for utility functions of timeseries")

test_that("ts_resample, with various arguments", {

  # ts_resample on tibble datasets ====
  # load test dataset
  #
  #


  # ts_resample on timeSeries datasets ====


})


test_that("ts_asfreq, with various arguments", {

  .valide_asfreq_result <- function(ds_test_origin,
                                    ds_test_result,
                                    freq_rule = c("day", "month", "quarter"),
                                    fillna_method = c("nfill", "ffill", "bfill"),
                                    ds_type = c("tibble", "timeSeries")) {

    ds_type = match.arg(ds_type)
    switch(ds_type,
           "tibble" = {
             origin_date = ds_test_origin$date
             origin_value = ds_test_origin$value
             result_date = ds_test_result$date
             result_value = ds_test_result$value
           },
           "timeSeries" = {
             origin_date = time(ds_test_origin)
             origin_value = ds_test_origin$value
             result_date = time(ds_test_result)
             result_value = ds_test_result$value
           })

    # validate result

    # check time frequency of result ds by freq_rule
    freq_rule = match.arg(freq_rule)
    switch(freq_rule,
           "day" = {
             expect_true(timeDate::isDaily(
               timeDate::as.timeDate(result_date)))
           },
           "month" = {
             expect_true(timeDate::isMonthly(
               timeDate::as.timeDate(result_date)))
           },
           "quarter" = {
             expect_true(timeDate::isQuarterly(
               timeDate::as.timeDate(result_date)))
           })

    # check value of result ds by fill_method

    if (length(unique(origin_date)) <= length(unique(result_date))) {
      # upsampling with Interpolation : from low frequency to high frequency
      expect <- origin_value[origin_date %in% result_date]
    } else {
      # downsampling with aggregation : from high frequency to low frequency
      expect <- origin_value[origin_date %in% result_date]
      actual <- result_value
    }

    expect_equal(expect, actual)
  }

  # ts_asfreq on tibble dataset ====

  # downsampling with aggregation : from high frequency to low frequency
  ds_test_origin <- ds_test_daily_df
  freq_rule = "quarter"
  fillna_method = "nfill"
  ds_test_asfreq <- ts_asfreq(ds_test_origin,
                              freq_rule = freq_rule,
                              fillna_method = fillna_method)
  .valide_asfreq_result(ds_test_origin, ds_test_asfreq,
                        freq_rule = freq_rule,
                        fillna_method = fillna_method,
                        ds_type = "tibble")

  # upsampling with Interpolation : from low frequency to high frequency
  ds_test_origin <- ds_test_yearly_df
  freq_rule = "quarter"
  fillna_method = "nfill"
  ds_test_asfreq <- ts_asfreq(ds_test_origin,
                              freq_rule = freq_rule,
                              fillna_method = fillna_method)
  .valide_asfreq_result(ds_test_origin, ds_test_asfreq,
                        freq_rule = freq_rule,
                        fillna_method = fillna_method,
                        ds_type = "tibble")


  # ts_asfreq on timeSeries dataset ====

})

test_that("ts_lag, with various arguments", {

  # valiate lag results
  .validate_lag_result <- function(ds_test_origin, ds_test_result,
                                   ds_type = c("tibble", "timeSeries"), k = 1) {

    ds_type = match.arg(ds_type)
    switch(ds_type,
           "tibble" = {
              origin_date = ds_test_origin$date
              origin_value = ds_test_origin$value
              result_date = ds_test_result$date
              result_value = ds_test_result$value
           },
           "timeSeries" = {
              origin_date = time(ds_test_origin)
              origin_value = ds_test_origin$value
              result_date = time(ds_test_result)
              result_value = ds_test_result$value
           })

    # valiate the result
    if (k >= 0 ) {

      expect_date <- origin_date[(1 + k):length(origin_date)]
      expect_value <- origin_value[1:(length(origin_value) - k)]

      actual_date <- result_date
      actual_value <- result_value

      expect_equal(expect_date, actual_date)
      expect_equal(expect_value, actual_value)

    }else{
      expect_date <- origin_date[1:(length(origin_value) + k)]
      expect_value <- origin_value[(1 + (-k)):length(origin_value)]

      actual_date <- result_date
      actual_value <- result_value
      expect_equal(expect_date, actual_date)
      expect_equal(expect_value, actual_value)
    }
  }



  # ts_lag on tibble dataset ====
  k <- c(-2, -1, 0, 1, 2)
  ds_type = "tibble"
  ds_test_origin <- ds_test_yearly_df
  for (i in k) {
    ds_test_lag <- na.omit(ts_lag(ds_test_origin, k = i,trim = FALSE))
    .validate_lag_result(ds_test_origin, ds_test_lag, ds_type = ds_type, k = i)

    ds_test_lag <- na.omit(ts_lag(ds_test_origin, k = i,trim = TRUE))
    .validate_lag_result(ds_test_origin, ds_test_lag, ds_type = ds_type, k = i)
  }


  # ts_lag on timeSeries dataset ====
  k <- c(-2, -1, 0, 1, 2)
  ds_type = "timeSeries"
  ds_test_origin <- ds_test_yearly_fts
  for (i in k) {

    ds_test_lag <- na.omit(ts_lag(ds_test_origin, k = i,trim = FALSE))
    .validate_lag_result(ds_test_origin, ds_test_lag, ds_type = ds_type, k = i)

    ds_test_lag <- na.omit(ts_lag(ds_test_origin, k = i,trim = TRUE))
    .validate_lag_result(ds_test_origin, ds_test_lag, ds_type = ds_type, k = i)
  }


})
