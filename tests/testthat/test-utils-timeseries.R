
# Build test dataset
ds_test_timeseries <- function(from = "2015-01-01",
                               to = "2016-12-31",
                               by = c("day", "week", "month", "quarter", "year"),
                               side = c("start", "end"),
                               output = c("tibble", "timeSeries")) {
  by <- match.arg(by)
  side <- match.arg(side)
  date <- seq(as.Date(from), as.Date(to), by)
  switch(by,
    "day" = {
      value <- lubridate::year(date) + lubridate::month(date) / 100 + lubridate::day(date) / 10000
      # value <- format(value, nsmall = 4)
    },
    "week" = {
      if (side == "end") {
        date <- lubridate::ceiling_date(date, "7 days") - 1
      }
      value <- lubridate::year(date) + lubridate::week(date) / 100
      # value <- format(value, nsmall = 2)
    },
    "month" = {
      if (side == "end") {
        date <- lubridate::ceiling_date(date, "month") - 1
      }
      value <- lubridate::year(date) + lubridate::month(date) / 100
      # value <- format(value, nsmall = 2)
    },
    "quarter" = {
      if (side == "end") {
        date <- lubridate::ceiling_date(date, "quarter") - 1
      }
      value <- lubridate::year(date) + lubridate::quarter(date) / 100
      # value <- format(value, nsmall = 1)
    },
    "year" = {
      if (side == "end") {
        date <- lubridate::ceiling_date(date, "year") - 1
      }
      value <- lubridate::year(date)
      # value <- format(value)
    }
  )

  output <- match.arg(output)
  switch(output,
    "tibble" = {
      ds_test <- tibble::tibble(date = date, key = "000001", value = value)
    },
    "timeSeries" = {
      ds_test <- timeSeries::timeSeries(
        data = value,
        charvec = date,
        units = "value"
      )
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
  output = "tibble"
)
ds_test_weekly_df <- ds_test_timeseries(start_date,
  to = end_date,
  by = "week",
  side = "end",
  output = "tibble"
)
ds_test_monthly_df <- ds_test_timeseries(start_date,
  to = end_date,
  by = "month",
  side = "end",
  output = "tibble"
)
ds_test_quarterly_df <- ds_test_timeseries(start_date,
  to = end_date,
  by = "quarter",
  side = "end",
  output = "tibble"
)
ds_test_yearly_df <- ds_test_timeseries(start_date,
  to = end_date,
  by = "year",
  side = "end",
  output = "tibble"
)

# test datasets of timeSeries format
#
ds_test_daily_fts <- ds_test_timeseries(start_date,
  to = end_date,
  by = "day",
  side = "end",
  output = "timeSeries"
)
ds_test_weekly_fts <- ds_test_timeseries(start_date,
  to = end_date,
  by = "week",
  side = "end",
  output = "timeSeries"
)
ds_test_monthly_fts <- ds_test_timeseries(start_date,
  to = end_date,
  by = "month",
  side = "end",
  output = "timeSeries"
)
ds_test_quarterly_fts <- ds_test_timeseries(start_date,
  to = end_date,
  by = "quarter",
  side = "end",
  output = "timeSeries"
)
ds_test_yearly_fts <- ds_test_timeseries(start_date,
  to = end_date,
  by = "year",
  side = "end",
  output = "timeSeries"
)

# Tests for utility functions of timeseries ----
context("Tests for utility functions of timeseries")

# working horse of validating results of resample/refreq
validate_engine_resample_refreq <- function(ds_test_origin,
                                            ds_test_result,
                                            freq_rule = c("day", "month", "quarter", "year"),
                                            fillna_method = c("nfill", "ffill", "bfill"),
                                            test_fun = c("ts_asfreq", "ts_resample"),
                                            agg_fun = mean,
                                            ...,
                                            ds_type = c("tibble", "timeSeries")) {
  ds_type <- match.arg(ds_type)
  switch(ds_type,
    "tibble" = {
      origin_date <- ds_test_origin$date
      origin_value <- ds_test_origin$value
      origin_key <- ds_test_origin$key
      result_date <- ds_test_result$date
      result_value <- ds_test_result$value
      result_key <- ds_test_result$key
    },
    "timeSeries" = {
      origin_date <- as.Date(time(ds_test_origin))
      origin_value <- ds_test_origin$value
      result_date <- as.Date(time(ds_test_result))
      result_value <- ds_test_result$value
    }
  )

  # Validate result

  # Validate time frequency of result ds by freq_rule
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
    "day" = {
      expect_true(is_periodic_dates(result_date,
        freq_rule = "day",
        regular = TRUE
      ))
    },
    "month" = {
      expect_true(is_periodic_dates(result_date,
        freq_rule = "month",
        regular = TRUE
      ))
    },
    "quarter" = {
      expect_true(is_periodic_dates(result_date,
        freq_rule = "quarter",
        regular = TRUE
      ))
    },
    "year" = {
      expect_true(is_periodic_dates(result_date,
        freq_rule = "year",
        regular = TRUE
      ))
    }
  )

  # Validate value of result ds by fill_method
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
          "quarter" = "quarter",
          "year" = "year"
        )
        date_group <- as.Date(as.character(cut(origin_date,
          breaks = cut_breaks, right = FALSE
        )))

        expect <- aggregate(
          origin_value,
          list(date_group = date_group),
          agg_fun, ...
        )

        expect <- expect$x

        actual <- result_value
        expect_equal(expect, actual)
      }
    )
  }

  # 2. check whether pre/next data value is ok in result data
  if (length(unique(origin_date)) < length(unique(result_date))) {
    # upsampling : from low frequency to high frequency
    switch(fillna_method,
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

  # Validate key value for tibble
  if (ds_type == "tibble") {
    # No NAs and has value same as key value of origin
    expect_true(all(!is.na(result_key)))
    expect_true(all(result_key %in% origin_key))
  }
}

test_that("ts_resample, with various arguments", {

  # conduct one test
  .validate_resample_result <- function(ds_test_origin,
                                          ds_test_result,
                                          freq_rule = c("day", "month", "quarter", "year"),
                                          fillna_method = c("nfill", "ffill", "bfill"),
                                          agg_fun = mean,
                                          ...,
                                          ds_type = c("tibble", "timeSeries")) {
    # call test engine to work
    validate_engine_resample_refreq(
      ds_test_origin = ds_test_origin,
      ds_test_result = ds_test_result,
      freq_rule = freq_rule,
      fillna_method = fillna_method,
      test_fun = "ts_resample",
      agg_fun = agg_fun,
      ...,
      ds_type = ds_type
    )
  }

  # conduct multi-tests in a btach mode
  .batch_valiate_resample_result <- function(params_tbl, parallel = TRUE) {
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
              key_fields = "key",
              parallel = parallel,
              !!!extra_agg_fun_params
            )
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
              ds_type = ds_type
            )
          ))
        }
      }
    }
  }

  # ts_resample on default arguments ====
  ds_test_origin <- ds_test_monthly_df
  # compute the result
  ds_test_resample <- ts_resample(ds_test_origin,
    date_index_field = "date",
    key_fields = "key"
  )
  # validate the result
  .validate_resample_result(
    ds_test_origin,
    ds_test_resample,
    freq_rule = "day",
    fillna_method = "nfill",
    agg_fun = mean,
    ds_type = "tibble"
  )

  # ts_resample on tibble dataset ====

  # >>downsampling resamle: from high frequency to low frequency ----

  # -- standard agg_fun
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_df",
      "ds_test_monthly_df",
      "ds_test_quarterly_df",
      "ds_test_yearly_df"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(mean),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "tibble"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_resample_result(params_tbl, parallel = FALSE)

  # -- customized agg-fun
  agg_method <- function(x, ...) {
    prod(1 + x, ...) - 1
  }
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_df",
      "ds_test_monthly_df",
      "ds_test_quarterly_df",
      "ds_test_yearly_df"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(agg_method),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "tibble"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_resample_result(params_tbl, parallel = FALSE)

  # >>upsampling resample : from low frequency to high frequency ----
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_yearly_df",
      "ds_test_quarterly_df",
      "ds_test_monthly_df",
      "ds_test_daily_df"
    ),
    freq_rules = list(
      c("year", "quarter", "month", "day"),
      c("quarter", "month", "day"),
      c("month", "day"),
      c("day")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(mean),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "tibble"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_resample_result(params_tbl, parallel = FALSE)

  # ts_resample on timeSeries dataset ====

  # >>downsampling resample: from high frequency to low frequency ----

  # -- standard agg_fun
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_fts",
      "ds_test_monthly_fts",
      "ds_test_quarterly_fts",
      "ds_test_yearly_fts"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(mean),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "timeSeries"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl)

  # --- customized agg-fun
  agg_method <- function(x, ...) {
    prod(1 + x, ...) - 1
  }
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_fts",
      "ds_test_monthly_fts",
      "ds_test_quarterly_fts",
      "ds_test_yearly_fts"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(agg_method),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "timeSeries"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl)

  # >>upsampling resample : from low frequency to high frequency ----

  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_yearly_fts",
      "ds_test_quarterly_fts",
      "ds_test_monthly_fts",
      "ds_test_daily_fts"
    ),
    freq_rules = list(
      c("year", "quarter", "month", "day"),
      c("quarter", "month", "day"),
      c("month", "day"),
      c("day")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    agg_fun = list(mean),
    extra_agg_fun_params = list(na.rm = TRUE),
    ds_type = "timeSeries"
  )

  # test on parallel process
  .batch_valiate_resample_result(params_tbl)
})


test_that("ts_asfreq, with various arguments", {

  # Conduct one test
  .validate_asfreq_result <- function(ds_test_origin,
                                        ds_test_result,
                                        freq_rule = c("day", "month", "quarter"),
                                        fillna_method = c("nfill", "ffill", "bfill"),
                                        ds_type = c("tibble", "timeSeries")) {

    # call test engine to work
    validate_engine_resample_refreq(
      ds_test_origin = ds_test_origin,
      ds_test_result = ds_test_result,
      freq_rule = freq_rule,
      fillna_method = fillna_method,
      test_fun = "ts_asfreq",
      agg_fun = NULL,
      ds_type = ds_type
    )
  }


  # Conduct multi-tests in a btach mode
  .batch_valiate_asfreq_result <- function(params_tbl, parallel = TRUE) {
    for (i in 1:nrow(params_tbl))
    {
      # get test params for one test
      ds_test_origin <- get(params_tbl$ds_test_origin[i])
      freq_rules <- params_tbl$freq_rules[i][[1]]
      fillna_methods <- params_tbl$fillna_methods[i][[1]]
      ds_type <- params_tbl$ds_type[i]

      # conduct a test and vilidate result
      for (freq_rule in freq_rules) {
        for (fillna_method in fillna_methods) {

          # compute the result
          ds_test_asfreq <- ts_asfreq(ds_test_origin,
            freq_rule = freq_rule,
            fillna_method = fillna_method,
            key_fields = "key",
            parallel = parallel
          )

          # validate the result
          .validate_asfreq_result(ds_test_origin, ds_test_asfreq,
            freq_rule = freq_rule,
            fillna_method = fillna_method,
            ds_type = ds_type
          )
        }
      }
    }
  }

  # ts_asfreq on default arguments ====
  ds_test_origin <- ds_test_monthly_df
  # compute the result
  ds_test_asfreq <- ts_asfreq(ds_test_origin,
    date_index_field = "date",
    key_fields = "key"
  )
  # validate the result
  .validate_asfreq_result(
    ds_test_origin,
    ds_test_asfreq,
    freq_rule = "day",
    fillna_method = "nfill",
    ds_type = "tibble"
  )

  # ts_asfreq on tibble dataset ====

  # >>downsampling refreq: from high frequency to low frequency ----
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_df",
      "ds_test_monthly_df",
      "ds_test_quarterly_df",
      "ds_test_yearly_df"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    ds_type = "tibble"
  )

  # test on parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = FALSE)


  # >>upsampling refreq : from low frequency to high frequency ----
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_yearly_df",
      "ds_test_quarterly_df",
      "ds_test_monthly_df",
      "ds_test_daily_df"
    ),
    freq_rules = list(
      c("year", "quarter", "month", "day"),
      c("quarter", "month", "day"),
      c("month", "day"),
      c("day")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    ds_type = "tibble"
  )

  # test on parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = FALSE)


  # ts_asfreq on timeSeries dataset ====

  # >>downsampling refreq: from high frequency to low frequency ----
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_daily_fts",
      "ds_test_monthly_fts",
      "ds_test_quarterly_fts",
      "ds_test_yearly_fts"
    ),
    freq_rules = list(
      c("day", "month", "quarter", "year"),
      c("month", "quarter", "year"),
      c("quarter", "year"),
      c("year")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    ds_type = "timeSeries"
  )

  # test on parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = FALSE)


  # >>upsampling refreq : from low frequency to high frequency ----
  params_tbl <- tibble::tibble(
    ds_test_origin = c(
      "ds_test_yearly_fts",
      "ds_test_quarterly_fts",
      "ds_test_monthly_fts",
      "ds_test_daily_fts"
    ),
    freq_rules = list(
      c("year", "quarter", "month", "day"),
      c("quarter", "month", "day"),
      c("month", "day"),
      c("day")
    ),
    fillna_methods = list(c("nfill", "ffill", "bfill")),
    ds_type = "timeSeries"
  )

  # test on parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = TRUE)
  # test on non-parallel process
  .batch_valiate_asfreq_result(params_tbl, parallel = FALSE)
})

test_that("ts_lag, with various normal arguments", {

  # Valiate lag results
  .validate_lag_result <- function(ds_test_origin, ds_test_result,
                                     ds_type = c("tibble", "timeSeries"),
                                     k = 1,
                                     trim = TRUE) {
    ds_type <- match.arg(ds_type)
    switch(ds_type,
      "tibble" = {
        origin_date <- ds_test_origin$date
        origin_value <- ds_test_origin$value
        origin_key <- ds_test_origin$key
        result_date <- ds_test_result$date
        result_value <- ds_test_result$value
        result_key <- ds_test_result$key
      },
      "timeSeries" = {
        origin_date <- as.Date(time(ds_test_origin))
        origin_value <- ds_test_origin$value

        result_date <- if (length(ds_test_result) != 0) {
          as.Date(time(ds_test_result))
        } else {
          lubridate::as_date(numeric(0))
        }

        result_value <- as.vector(timeSeries::series(ds_test_result))
      }
    )

    origin_length <- length(origin_value)
    result_length <- length(result_value)

    # Validate clss of result
    expect_equal(class(ds_test_origin), class(ds_test_result))

    # Validate value of result
    # Case: normal k setting (abs(k) < length of ds_test_origin)
    if (abs(k) < origin_length) {
      # validate value of result
      if (k >= 0) {
        if (trim == TRUE) {
          # trim: no NA is allowed
          expect_date <- origin_date[(1 + k):length(origin_date)]
          expect_value <- origin_value[1:(length(origin_value) - k)]
        } else {
          # not trim: pad NAs in the head
          expect_date <- origin_date
          expect_value <- origin_value[1:(length(origin_value) - k)]
          expect_value <- c(rep(NA, k), expect_value)
        }

        actual_date <- result_date
        actual_value <- result_value

        expect_equivalent(expect_date, actual_date)
        expect_equal(expect_value, actual_value)
      } else {
        if (trim == TRUE) {
          # trim: no NA is allowed
          expect_date <- origin_date[1:(length(origin_value) + k)]
          expect_value <- origin_value[(1 + (-k)):length(origin_value)]
        } else {
          # not trim: pad NA in then end
          expect_date <- origin_date
          expect_value <- origin_value[(1 + (-k)):length(origin_value)]
          expect_value <- c(expect_value, rep(NA, (-k)))
        }

        actual_date <- result_date
        actual_value <- result_value
        expect_equivalent(expect_date, actual_date)
        expect_equal(expect_value, actual_value)
      }

      # validate length of result
      if (trim == TRUE) {
        expect_gte(origin_length, result_length)
      } else {
        expect_equal(origin_length, result_length)
      }

      # validate key value for tibble
      if (ds_type == "tibble") {
        if (trim == TRUE) {
          # trim: key field of result is shorter than origin,
          # but key value of that of origin
          expect_true(all(result_key %in% origin_key))
        } else {
          # not trim: key field of result is same as that of origin
          expect_equal(result_key, origin_key)
        }
      }
    }

    # Case: abnormal k setting (abs(k) < length of ds_test_origin)
    if (abs(k) >= origin_length) {
      # Validate value of result
      if (trim == TRUE) {
        # result must be null object
        expect_equal(result_length, 0)
      } else {
        # result must be NA with same length of origin dataset
        expect_equal(origin_length, result_length)
        expect_true(all(is.na(result_value)))
      }

      # Validate key value for tibble
      if (ds_type == "tibble") {
        if (trim != TRUE) {
          # not trim: key field of result is same as that of origin
          expect_equal(result_key, origin_key)
        }
      }
    }
  }

  # ts_lag on default arguments ====
  ds_test_origin <- ds_test_yearly_df
  ds_test_lag <- ts_lag(ds_test_origin)
  .validate_lag_result(ds_test_origin, ds_test_lag,
    ds_type = "tibble",
    k = 1, trim = TRUE
  )

  # ts_lag on tibble dataset ====
  ds_type <- "tibble"
  ds_test_origin <- ds_test_yearly_df
  length_ds <- nrow(ds_test_origin)
  k <- seq(from = -(length_ds + 1), to = (length_ds + 1), by = 1)
  for (i in k) {

    # When trim is FALSE
    trim <- FALSE
    for (parallel in c(TRUE, FALSE)) {
      ds_test_lag <- ts_lag(ds_test_origin,
        k = i, trim = trim,
        date_index_field = c("date"),
        key_fields = "key",
        parallel = parallel
      )
      .validate_lag_result(ds_test_origin, ds_test_lag,
        ds_type = ds_type,
        k = i, trim = trim
      )
    }

    # When trim is TRUE
    trim <- TRUE
    for (parallel in c(TRUE, FALSE)) {
      ds_test_lag <- ts_lag(ds_test_origin,
        k = i, trim = trim,
        date_index_field = c("date"),
        key_fields = "key",
        parallel = parallel
      )
      .validate_lag_result(ds_test_origin, ds_test_lag,
        ds_type = ds_type,
        k = i, trim = trim
      )
    }
  }

  # ts_lag on timeSeries dataset ====
  ds_type <- "timeSeries"
  ds_test_origin <- ds_test_yearly_fts
  length_ds <- nrow(ds_test_origin)
  k <- seq(from = -(length_ds + 1), to = (length_ds + 1), by = 1)
  for (i in k) {

    # When trim is FALSE
    trim <- FALSE
    for (parallel in c(TRUE, FALSE)) {
      ds_test_lag <- ts_lag(ds_test_origin,
        k = i, trim = trim,
        parallel = parallel
      )
      .validate_lag_result(ds_test_origin, ds_test_lag,
        ds_type = ds_type,
        k = i, trim = trim
      )
    }

    # When trim is TRUE
    trim <- TRUE
    for (parallel in c(TRUE, FALSE)) {
      ds_test_lag <- ts_lag(ds_test_origin,
        k = i, trim = trim,
        parallel = parallel
      )
      .validate_lag_result(ds_test_origin, ds_test_lag,
        ds_type = ds_type,
        k = i, trim = trim
      )
    }
  }
})

test_that("refreq_dateindex, with various normal arguments", {

  # refreq_dateindex, with various normal arguments ====

  daily_dates <- ds_test_daily_df$date
  monthly_dates <- ds_test_monthly_df$date
  quarterly_dates <- ds_test_quarterly_df$date
  yearly_dates <- ds_test_yearly_df$date

  test_dates_list <- list(
    daily_dates = daily_dates,
    monthly_dates = monthly_dates,
    quarterly_dates = quarterly_dates,
    yearly_dates = quarterly_dates
  )

  freq_rule_list <- c("day", "month", "quarter", "year")

  for (freq_rule in freq_rule_list) {
    for (test_dates in test_dates_list) {
      new_dates <- refreq_dateindex(test_dates,
        freq_rule = freq_rule
      )
      expect_true(is_periodic_dates(new_dates,
        freq_rule = freq_rule,
        regular = TRUE
      ))
    }
  }
})
