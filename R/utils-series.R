
#' Roll to apply a function on data series to output a series
#'
#' Apply fun on some data series in rolling window to get a result series
#' with same length of original data series
#'
#' @param data_series  A dataframe or matrix of numeric series to trail.#'
#' @param fun          A function to apply on sereis data.
#' @param ...          Params passed to fun.
#' @param window       A integer of periods in rolling window which must be
#'   in range of [1L, length of data_series], default 1L.
#' @param unlist       A logical flag to deterimine whether unlist result or not.
#'   Default TRUE means unlist result into a vector of numeric. The argument
#'   don't work if result can't be convert into a atomic vector, e.g., a list of
#'   object or a list of list, etc.
#'
#' @param na_value     A NA value to fill non-avaliable data in results,
#'   default NA.
#'
#'
#' @family utils_series
#'
#' @return A vector or list of result with same length of original series
#'   if succeed, otherwise a vector of NAs with same length of orginal series.
#' @noRd
rollify_series <- function(data_series, fun, ..., window = 1L,
                           unlist = TRUE, na_value = NA) {

  # validate params
  assertive::assert_is_not_null(data_series)
  assertive::assert_is_function(fun)
  assertive::assert_is_integer(window)
  assertive::assert_is_not_null(na_value)

  roll_length <- NROW(data_series)

  # initialize `output` vector
  output <- rlang::rep_along(1:roll_length, list(na_value))

  # get rolling result
  if ((window >= 1L) && (window <= roll_length)) {
    # window must in [1L, roll_length]
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
  } else {
    msg <- sprintf(
      "window(%d) isn't in range of [1L, %dL].",
      window,
      roll_length
    )
    rlang::abort(msg)
  }

  # unlist result if request, except for atomic scalar
  if (unlist) {
    is_scalar_atomic <- purrr::map_lgl(output, ~ (rlang::is_scalar_atomic(.x)))
    if (all(is_scalar_atomic)) {
      output <- unlist(output)
    } else {
      msg <- sprintf("can't unlist non-atomic scalar, return list as rollified result")
      rlang::warn(msg)
    }
  }

  return(output)
}

#' Trail periodic time series
#'
#' Trailing periodic time sereis( accumaulated or not accumaulated) means to
#'  apply aggregating function in specified months windows.
#'
#' @param dates         A vector of data.
#' @param datas_series  A dataframe or matrix of numeric series to trail.
#' @param period        A period string of dates, i.e., "day", "month",
#'   "quarter". Default day.
#' @param accumulated   A logic flag of whether spcified data sereis is
#'   accumluated or not.
#' @param trailling_month  A integer of months of data to trail. Default is 12,
#'   which means 12 months, i.e., TTT(Trail Twelve Month).
#' @param agg_fun       A function to aggrate data sereis in trailling month.
#' @param ...    Params to agg_fun.
#'
#'
#'
#'
#' @family utils_series
#'
#' @return A dataframe of trailed data if succeed, otherwise a dataframe with
#'  zero length.
#' @noRd
trail_periodic_series <- function(dates, data_series,
                                  period = c("day", "month", "quarter"),
                                  accumulated = TRUE,
                                  trailing_month = 12L,
                                  agg_fun = sum,
                                  ...) {

  # function to calculate value in each period
  .period_value <- function(a_series) {

    # remove the NA at index of 1 if need
    a_series <- tidyr::replace_na(a_series, replace = 0)

    # value of period(except 1st period) is difference
    # between adjacent element of x
    period_value <- a_series - dplyr::lag(a_series)

    # value of 1st period is value of 1st element of x
    period_value[1] <- a_series[1]

    return(period_value)
  }

  # body of main function
  date_expr <- rlang::enexpr(dates)
  data_series_expr <- rlang::enexpr(data_series)

  # valiate params
  assertive::assert_is_date(dates)
  assertive::assert_is_not_null(data_series)
  assertive::assert_all_are_equal_to(NROW(dates), NROW(data_series))
  assertive::assert_is_logical(accumulated)
  assertive::assert_is_integer(trailing_month)
  assertive::assert_is_function(agg_fun)

  period <- match.arg(period)
  if (is_periodic_dates(dates, freq_rule = period, regular = TRUE)) {
    # trail regular peridic data
    switch(period,
      "day" = {
        rolly_window <- as.integer((365 / 12) * trailing_month)
      },
      "month" = {
        rolly_window <- as.integer((12 / 12) * trailing_month)
      },
      "quarter" = {
        rolly_window <- as.integer((4 / 12) * trailing_month)
      }
    )

    # convert series into dataframe
    ds_date <- tibble::tibble(date = dates)
    ds_series <- tibble::as_tibble(data_series)
    ds_origin <- dplyr::bind_cols(ds_date, ds_series)

    # convert into period data
    if (accumulated) {

      # original data is accumalated

      # add year field to indicate period
      ds_period <- ds_origin %>%
        dplyr::mutate(
          year = lubridate::year(date)
        )

      # fill NAs with value before NAs
      ds_period <- ds_period %>%
        dplyr::group_by(year) %>%
        tidyr::fill(names(ds_series), .direction = "down") %>%
        dplyr::ungroup()

      # compute value of each period
      ds_period <- ds_period %>%
        dplyr::group_by(year) %>%
        dplyr::mutate_at(
          .vars = vars(-date, -year),
          .funs = .period_value
        )

      # get period dataset
      ds_period <- ds_period %>%
        dplyr::ungroup() %>%
        dplyr::select(-date, -year)
    } else {

      # original data is not accumated(already contain period data)

      # use origin data as period data directly
      ds_period <- ds_origin %>%
        dplyr::select(-date)

      # replace NA in series with 0
      ds_period <- ds_period %>%
        dplyr::mutate_all(.funs = tidyr::replace_na, replace = 0)
    }

    # trail data from period data
    ds_trial <- ds_period %>%
      dplyr::mutate_all(
        .funs = rollify_series,
        fun = agg_fun,
        ...,
        window = rolly_window
      )
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
