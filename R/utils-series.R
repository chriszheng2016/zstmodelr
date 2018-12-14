
# Roll to apply a function on data series to output a series
rollify_series <- function(data_series, fun, window, ...,
                           unlist = TRUE, na_value = NA) {

  # validate params
  assertive::assert_is_not_null(data_series)
  assertive::assert_is_function(fun)
  assertive::assert_is_not_null(na_value)

  roll_length <- NROW(data_series)

  # initialize `output` vector
  output <- rlang::rep_along(1:roll_length, list(na_value))

  # get rolling result
  if (window <= roll_length) {
    for (i in window:roll_length) {
      if (is.null(dim(data_series))) {
        # 1-d series
        f_data <- data_series[(i - window + 1):i]
      } else {
        # multi-d series
        f_data <- data_series[(i - window + 1):i, ]
      }

      output[[i]] <- fun(f_data, ...)
    }
  }

  # unlist result if request, except foratomic scalar
  if (unlist) {
    is_scalar_atomic <- purrr::map_lgl(output, ~(rlang::is_scalar_atomic(.x)))
    if (all(is_scalar_atomic)) {
      output <- unlist(output)
    }
  }

  return(output)
}

# Tail a periodic time series
trail_periodic_series <- function(date, data_series,
                                  period = c("day", "month", "quarter"),
                                  fun = sum) {

  # function to calculate value in each period
  .period_value <- function(a_series, period_index) {

    # value of period(except 1st period) is difference
    # between adjacent element of x
    period_value <- a_series - dplyr::lag(a_series)

    # value of 1st period is value of 1st element of x
    period_value[1] <- a_series[1]

    return(period_value)
  }

  # body of main function
  date_expr <- rlang::enexpr(date)
  data_series_expr <- rlang::enexpr(data_series)

  # valiate params
  assertive::assert_is_date(date)
  assertive::assert_is_not_null(data_series)
  assertive::assert_all_are_equal_to(NROW(date), NROW(data_series))
  assertive::assert_is_function(fun)

  period <- match.arg(period)
  if (is_periodic_dates(date, freq_rule = period, regular = TRUE)) {
    # trail regular peridic data
    switch(period,
      "day" = {
        period_index_fun <- lubridate::yday
        rolly_window <- 365
      },
      "month" = {
        period_index_fun <- lubridate::month
        rolly_window <- 12
      },
      "quarter" = {
        period_index_fun <- lubridate::quarter
        rolly_window <- 4
      }
    )

    # convert series into dataframe
    ds_date <- tibble::tibble(date = date)
    ds_series <- tibble::as_tibble(data_series)
    ds_origin <- dplyr::bind_cols(ds_date, ds_series)

    # convert into period data
    ds_period <- ds_origin %>%
      dplyr::mutate(
        year = lubridate::year(date),
        period_index = period_index_fun(date)
      )

    ds_period <- ds_period %>%
      dplyr::group_by(year) %>%
      dplyr::mutate_at(
        .vars = vars(-date, -year, -period_index),
        .funs = purrr::as_mapper(~.period_value(.x, period_index))
      )

    # trail data from period data
    ds_trial <- ds_period %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(
        .vars = vars(-date, -year, -period_index),
        .funs = purrr::as_mapper(~rollify_series(.x,
          fun = fun,
          window = rolly_window
        ))
      )

    # output only series data
    ds_trial <- ds_trial %>%
      dplyr::select_at(.vars = vars(-date, -year, -period_index))


  } else {
    # can't trail irregluar series
    msg <- sprintf(
      "Can't trail series(%s) with irregular date(%s) by period(%s).",
      rlang::expr_text(data_series_expr),
      rlang::expr_text(date_expr),
      period
    )
    rlang::abort(msg)
  }

  return(ds_trial)
}
