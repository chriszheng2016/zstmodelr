
# Build test dataset
ds_test_timeseries <- function(from = "2015-01-01",
                               to = "2016-12-31",
                               by = c("day", "week", "month", "quarter", "year"),
                               side = c("start", "end"),
                               output = c("tibble", "timeSeries")) {

  by = match.arg(by)
  side = match.arg(side)
  date = seq(as.Date(from), as.Date(to), by)
  switch(by,
         "day" = {
           value <- lubridate::year(date) + lubridate::month(date)/100 + lubridate::day(date)/10000
           # value <- format(value, nsmall = 4)
         },
         "week" = {
           if (side == "end")
              date <- lubridate::ceiling_date(date, "7 days") - 1
           value <- lubridate::year(date) + lubridate::week(date)/100
           # value <- format(value, nsmall = 2)
          },
         "month" = {
           if (side == "end")
              date <- lubridate::ceiling_date(date, "month") - 1
           value <- lubridate::year(date) + lubridate::month(date)/100
           # value <- format(value, nsmall = 2)
          },
         "quarter" = {
           if (side == "end")
              date <- lubridate::ceiling_date(date, "quarter") - 1
           value <- lubridate::year(date) + lubridate::quarter(date)/100
           # value <- format(value, nsmall = 1)
          },
         "year" = {
           if (side == "end")
               date <- lubridate::ceiling_date(date, "year") - 1
           value <- lubridate::year(date)
           # value <- format(value)
          }
         )

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
                                       side = "end",
                                       output = "tibble")
ds_test_weekly_df <- ds_test_timeseries(start_date,
                                        to = end_date,
                                        by = "week",
                                        side = "end",
                                        output = "tibble")
ds_test_monthly_df <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "month",
                                         side = "end",
                                         output = "tibble")
ds_test_quarterly_df <- ds_test_timeseries(start_date,
                                           to = end_date,
                                           by = "quarter",
                                           side = "end",
                                           output = "tibble")
ds_test_yearly_df <- ds_test_timeseries(start_date,
                                        to = end_date,
                                        by = "year",
                                        side = "end",
                                        output = "tibble")

# test datasets of timeSeries format
#
ds_test_daily_fts <- ds_test_timeseries(start_date,
                                       to = end_date,
                                       by = "day",
                                       side = "end",
                                       output = "timeSeries")
ds_test_weekly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "week",
                                         side = "end",
                                         output = "timeSeries")
ds_test_monthly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "month",
                                         side = "end",
                                         output = "timeSeries")
ds_test_quarterly_fts <- ds_test_timeseries(start_date,
                                           to = end_date,
                                           by = "quarter",
                                           side = "end",
                                           output = "timeSeries")
ds_test_yearly_fts <- ds_test_timeseries(start_date,
                                         to = end_date,
                                         by = "year",
                                         side = "end",
                                         output = "timeSeries")

# Tests for utility functions of timeseries ----
context("Tests for utility functions of timeseries")

validate_engine_resample_refreq <- function(ds_test_origin,
                                            ds_test_result,
                                            freq_rule = c("day", "month", "quarter"),
                                            fillna_method = c("nfill", "ffill", "bfill"),
                                            test_fun = c("ts_asfreq", "ts_resample"),
                                            agg_fun = mean,
                                            ...,
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
           origin_date = as.Date(time(ds_test_origin))
           origin_value = ds_test_origin$value
           result_date = as.Date(time(ds_test_result))
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
  # 1. check whether original data is in result date
  fillna_method <- match.arg(fillna_method)
  if (length(unique(origin_date)) <= length(unique(result_date))) {
    # upsampling : from low frequency to high frequency
    expect <- origin_value
    actual <- result_value[result_date %in% origin_date]
    expect_equal(expect, actual)
  } else {
    # downsampling : from high frequency to low frequency
    switch(test_fun,
           "ts_asfreq" = {
             expect <- origin_value[origin_date %in% result_date]
             actual <- result_value
             expect_equal(expect, actual)
             },
           "ts_resample" = {
             cut_breaks <- switch(freq_rule,
                                 "day" = "day",
                                 "month" = "month",
                                 "quarter" = "quarter"
                                 )
             date_group <- as.Date(as.character(cut(origin_date,
                                      breaks = cut_breaks, right = FALSE)))

             expect <- aggregate(origin_value,
                                       list(date_group = date_group),
                                       agg_fun, ...)

             # expect <- rlang::eval_tidy(rlang::quo(aggregate(
             #                                             origin_value,
             #                                             list(date_group = date_group),
             #                                             agg_fun,
             #                                             !!!list2(...)
             #                                           )))
             expect <- expect$x

             actual <- result_value
             expect_equal(expect, actual)
             }
           )
  }

  # 2. check whether pre/next data value is ok in result data
  if (length(unique(origin_date)) < length(unique(result_date))) {
    # upsampling : from low frequency to high frequency
    switch( fillna_method,
            "nfill" = {
              next_index <- match(origin_date, result_date) + 1
              expect_next <- result_value[next_index]
              expect_true(all(is.na(expect_next)))

            },
            "ffill" = {
              # forward fill: current row == next row in result datase
              actual <- result_value[result_date %in% origin_date]
              actual <- actual[1:(length(actual) - 1)]

              next_index <- match(origin_date, result_date) + 1
              expect <- result_value[next_index]
              expect <- expect[1:(length(expect) - 1)]

              expect_equal(expect, actual)
            },
            "bfill" = {
              # backward fill: current row == pre row in result datase
              actual <- result_value[result_date %in% origin_date]
              actual <- actual[2:length(actual)]

              pre_index <- match(origin_date, result_date) - 1
              expect <- result_value[pre_index]
              # expect <- expect[2:length(expect)]

              expect_equal(expect, actual)
            }
    )

  }
}

test_that("ts_resample, with various arguments", {

  # conduct one test
  .validate_resample_result <- function(ds_test_origin,
                                        ds_test_result,
                                        freq_rule = c("day", "month", "quarter"),
                                        fillna_method = c("nfill", "ffill", "bfill"),
                                        agg_fun = mean,
                                        ...,
                                        ds_type = c("tibble", "timeSeries")) {
    # call test engine to work
    validate_engine_resample_refreq(ds_test_origin = ds_test_origin,
                                    ds_test_result = ds_test_result,
                                    freq_rule = freq_rule,
                                    fillna_method = fillna_method,
                                    test_fun = "ts_resample",
                                    agg_fun = agg_fun,
                                    ...,
                                    ds_type = ds_type)
  }

  # conduct multi-tests in a btach mode
  .batch_valiate_resample_result <- function(params_tbl){

    for (i in 1:nrow(params_tbl))
    {
      # get test params for one test
      ds_test_origin <- get(params_tbl$ds_test_origin[i])
      freq_rules <- params_tbl$freq_rules[i][[1]]
      fillna_methods <- params_tbl$fillna_methods[i][[1]]
      agg_fun <- params_tbl$agg_fun[i][[1]]
      extra_agg_fun_params <- params_tbl$extra_agg_fun_params[i]
      ds_type <- params_tbl$ds_type[i]

      # conduct a test and vilidate result
      for (freq_rule in freq_rules) {
        for (fillna_method in fillna_methods) {


          # compute the result
          ds_test_resample <- rlang::eval_tidy(rlang::quo(
            ts_resample(
              ds_test_origin,
              freq_rule = freq_rule,
              fillna_method = fillna_method,
              agg_fun = agg_fun,
              !!!extra_agg_fun_params )
          ))

          # validate the result
          rlang::eval_tidy(rlang::quo(
            .validate_resample_result(
              ds_test_origin,
              ds_test_resample,
              freq_rule = freq_rule,
              fillna_method = fillna_method,
              agg_fun = agg_fun,
              !!!extra_agg_fun_params,
              ds_type = ds_type )
          ))
        }
      }
    }
  }

  # ts_resample on tibble dataset ====

  # downsampling resamle: from high frequency to low frequency

  # -- standard agg_fun
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_df",
                                                  "ds_test_monthly_df"),
                               freq_rules = list(c("day", "month", "quarter"),
                                                 c("month","quarter")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(mean),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "tibble")

  .batch_valiate_resample_result(params_tbl)

  # -- customized agg-fun
  agg_method <- function(x, ...) { prod(1 + x, ...) - 1 }
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_df",
                                                  "ds_test_monthly_df"),
                               freq_rules = list(c("day", "month", "quarter"),
                                                 c("month","quarter")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(agg_method),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "tibble")
  .batch_valiate_resample_result(params_tbl)


  # upsampling resample : from low frequency to high frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_yearly_df",
                                                  "ds_test_quarterly_df",
                                                  "ds_test_monthly_df"),
                               freq_rules = list(c("quarter", "month", "day"),
                                                 c("month", "day"),
                                                 c("day")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(mean),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "tibble")
  .batch_valiate_resample_result(params_tbl)



  # ts_resample on timeSeries dataset ====

  # downsampling resample: from high frequency to low frequency

  # -- standard agg_fun
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_fts",
                                                  "ds_test_monthly_fts"),
                               freq_rules = list(c("day", "month", "quarter"),
                                                 c("month","quarter")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(mean),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "timeSeries")
  .batch_valiate_resample_result(params_tbl)


  # --- customized agg-fun
  agg_method <- function(x, ...) { prod(1 + x, ...) - 1 }
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_fts",
                                                  "ds_test_monthly_fts"),
                               freq_rules = list(c("day", "month", "quarter"),
                                                 c("month","quarter")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(agg_method),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "timeSeries")
  .batch_valiate_resample_result(params_tbl)



  # upsampling resample : from low frequency to high frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_yearly_fts",
                                                  "ds_test_quarterly_fts",
                                                  "ds_test_monthly_fts"),
                               freq_rules = list(c("quarter", "month", "day"),
                                                 c("month", "day"),
                                                 c("day")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               agg_fun = list(mean),
                               extra_agg_fun_params = list(na.rm = TRUE),
                               ds_type = "timeSeries")
  .batch_valiate_resample_result(params_tbl)

})


test_that("ts_asfreq, with various arguments", {

  # conduct one test
  .validate_asfreq_result <- function(ds_test_origin,
                                    ds_test_result,
                                    freq_rule = c("day", "month", "quarter"),
                                    fillna_method = c("nfill", "ffill", "bfill"),
                                    ds_type = c("tibble", "timeSeries")) {

  # call test engine to work
    validate_engine_resample_refreq(ds_test_origin = ds_test_origin,
                                    ds_test_result = ds_test_result,
                                    freq_rule = freq_rule,
                                    fillna_method = fillna_method,
                                    test_fun = "ts_asfreq",
                                    agg_fun = NULL,
                                    ds_type = ds_type)

  }


  # conduct multi-tests in a btach mode
  .batch_valiate_asfreq_result <- function(params_tbl){

    for (i in 1:nrow(params_tbl))
    {
      # get test params for one test
      ds_test_origin <- get(params_tbl$ds_test_origin[i])
      freq_rules = params_tbl$freq_rules[i][[1]]
      fillna_methods = params_tbl$fillna_methods[i][[1]]
      ds_type = params_tbl$ds_type[i]

      # conduct a test and vilidate result
      for (freq_rule in freq_rules) {
        for (fillna_method in fillna_methods) {

          # compute the result
          ds_test_asfreq <- ts_asfreq(ds_test_origin,
                                      freq_rule = freq_rule,
                                      fillna_method = fillna_method)

          # validate the result
          .validate_asfreq_result(ds_test_origin, ds_test_asfreq,
                                freq_rule = freq_rule,
                                fillna_method = fillna_method,
                                ds_type = ds_type)
        }
      }
    }
  }


  # ts_asfreq on tibble dataset ====

  # downsampling refreq: from high frequency to low frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_df",
                                                  "ds_test_monthly_df"),
                                freq_rules = list(c("day", "month", "quarter"),
                                                     c("month","quarter")),
                                fillna_methods = list(c("nfill", "ffill", "bfill")),
                                ds_type = "tibble" )
  .batch_valiate_asfreq_result(params_tbl)





  # upsampling refreq : from low frequency to high frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_yearly_df",
                                                  "ds_test_quarterly_df",
                                                  "ds_test_monthly_df"),
                               freq_rules = list(c("quarter", "month", "day"),
                                                 c("month", "day"),
                                                 c("day")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               ds_type = "tibble" )
  .batch_valiate_asfreq_result(params_tbl)


  # ts_asfreq on timeSeries dataset ====

  # downsampling refreq: from high frequency to low frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_daily_fts",
                                                  "ds_test_monthly_fts"),
                               freq_rules = list(c("day", "month", "quarter"),
                                                 c("month","quarter")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               ds_type = "timeSeries" )
  .batch_valiate_asfreq_result(params_tbl)


  # upsampling refreq : from low frequency to high frequency
  params_tbl <- tibble::tibble(ds_test_origin = c("ds_test_yearly_fts",
                                                  "ds_test_quarterly_fts",
                                                  "ds_test_monthly_fts"),
                               freq_rules = list(c("quarter", "month", "day"),
                                                 c("month", "day"),
                                                 c("day")),
                               fillna_methods = list(c("nfill", "ffill", "bfill")),
                               ds_type = "timeSeries" )
  .batch_valiate_asfreq_result(params_tbl)

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
              origin_date = as.Date(time(ds_test_origin))
              origin_value = ds_test_origin$value
              result_date = as.Date(time(ds_test_result))
              result_value = ds_test_result$value
           })

    # valiate the result
    if (k >= 0 ) {

      expect_date <- origin_date[(1 + k):length(origin_date)]
      expect_value <- origin_value[1:(length(origin_value) - k)]

      actual_date <- result_date
      actual_value <- result_value

      expect_equivalent(expect_date, actual_date)
      expect_equal(expect_value, actual_value)

    }else{
      expect_date <- origin_date[1:(length(origin_value) + k)]
      expect_value <- origin_value[(1 + (-k)):length(origin_value)]

      actual_date <- result_date
      actual_value <- result_value
      expect_equivalent(expect_date, actual_date)
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
