# Utility functions - dates

#' Check date series is periodic or not
#'
#' Check whether date series is regular/irregular periodic or not.
#'
#' @details
#'   Periodic dates includes two kinds:
#' \itemize{
#'    \item \strong{regular periodic series}:
#'         a date/time vector is regular periodic, if the vector has not more
#'     one date/time stamp on each date of peroid and doesn't miss any date of period.
#'     For example, c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01",
#'     "2018-05-01", "2018-06-01") is a regular monthly periodic dates.
#'
#'    \item \strong{irregular periodic series}:
#'        a date/time vector is regular periodic, if the vector has more one
#'     date/time stamp on each date of peroid or miss some date of period.
#'     For example, c("2018-01-01", "2018-02-01", "2018-04-01",
#'     "2018-05-01", "2018-06-01") is is irregular monthly periodic dates,
#'     which miss date of "2018-03-01" in dates of months.
#' }
#
#' @param dates_series   A vector of dates/timestamps.
#' @param freq_rule    Frequency rule of period to test,  e.g. "day", "month", "quarter",
#'  "year", default "Day".
#' @param regular    Whether to test date series as a regular periodic series
#'  or not. Since irregular test is looser than regular test, irregular test
#'  on regular series will always return true. Default is FALSE, which means we will
#'  use looser test(irregular).
#'
#' @family utils_dates
#' @return   return TRUE if dates is periodic, otherwise FALSE.
#' @noRd
is_periodic_dates <- function(dates_series,
                              freq_rule = c("day", "month", "quarter", "year"),
                              regular = FALSE) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))

  is_regular_fun <- NULL
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
    "day" = {
      # A date/time vector is defined as daily, if the vector
      # has not more than one date/timestamp per day.
      is_regular_fun <- timeDate::isDaily

      # params for test irregular dates
      interval_min <- 0
      interval_max <- 1
    },
    "month" = {

      # A date/time vector is defined as daily if the vector
      # has not more than one date/timestamp per day.
      is_regular_fun <- timeDate::isMonthly

      # params for test irregular dates
      interval_min <- 30 - 2
      interval_max <- 30 + 2
    },
    "quarter" = {

      # A date/time vector is defined as quarterly if the vector
      # has not more than one date/timestamp per quarter.
      is_regular_fun <- timeDate::isQuarterly

      # params for test irregular dates
      interval_min <- 30 * 3 - 2
      interval_max <- 30 * 3 + 2
    },
    "year" = {
      # A date/time vector is defined as daily if the vector
      # has not more than one date/time stamp per year,
      # or missing date/time stamp per year.
      is_regular_fun <- isRegularYearly

      # params for test irregular dates
      interval_min <- 365 - 3
      interval_max <- 365 + 3
    }
  )

  # check whether it is periodic dates(regular/irregular)
  is_periodic <- FALSE
  if (regular) {
    # check whether it is a regular daily dates
    if (is_regular_fun(timeDate::as.timeDate(dates_series))) {
      is_periodic <- TRUE
    }
  } else {
    # check whether it is an irregular periodic dates
    dates_series <- sort(unique(dates_series))
    intervals <- median(dates_series - dplyr::lag(dates_series), na.rm = TRUE)
    if ((intervals >= interval_min) & (intervals <= interval_max)) {
      is_periodic <- TRUE
    }
  }

  return(is_periodic)
}

# Function to test regular yearly dates, a implmentation
# similar to timeDate::isQuearterly,etc.
isRegularYearly <- function(x) {
  dates <- lubridate::as_date(x)
  dates <- sort(dates)

  origin_years <- lubridate::year(dates)
  min_year <- min(origin_years)
  max_year <- max(origin_years)
  regular_years <- seq(from = min_year, to = max_year, by = 1)

  # A date/time vector is defined as daily if the vector
  # has not more than one date/time stamp per year,
  # or missing date/time stamp per year.
  if (length(origin_years) == length(regular_years)) {
    is_regular <- TRUE
  } else {
    is_regular <- FALSE
  }

  return(is_regular)
}

#' Guess periodicity of date series
#
#' @param dates_series     A vector of date/timestamps.
#'
#' @param regular    A logic flag of whether to test date series as a regular periodic series
#'  or not. Since irregular test is looser than regular test, irregular test
#'  on regular series will return true. Default is FALSE, which means we will
#'  use looser test(irregular).
#'
#' @family utils_dates
#' @return  return "day" for daily, "month" for monthly, "quarter" for quarterly,
#'  "year" for yearly, "unknown" for unknown date period.
#'
#' @noRd
guess_dates_period <- function(dates_series, regular = FALSE) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))

  dates_period <- "unknown"

  # it is a monthly date series?
  if (dates_period == "unknown") {
    if (is_periodic_dates(dates_series,
      freq_rule = "day",
      regular = regular
    )) {
      dates_period <- "day"
    }
  }

  # it is a monthly date series?
  if (dates_period == "unknown") {
    if (is_periodic_dates(dates_series,
      freq_rule = "month",
      regular = regular
    )) {
      dates_period <- "month"
    }
  }

  # it is a quarterly date series?
  if (dates_period == "unknown") {
    if (is_periodic_dates(dates_series,
      freq_rule = "quarter",
      regular = regular
    )) {
      dates_period <- "quarter"
    }
  }

  # it is a yearly date series?
  if (dates_period == "unknown") {
    if (is_periodic_dates(dates_series,
      freq_rule = "year",
      regular = regular
    )) {
      dates_period <- "year"
    }
  }

  return(dates_period)
}

#' Convert dates series into periodic dates series
#
#' @param dates_series     A vector of date/timestamps.
#'
#' @param period           A character of period, e.g. "day", "month",
#'   "quarter", "year". Default "day".
#' @param period_date      A character of period_date format, e.g. "start",
#'   "end", "start" format date as start of the period, "end" format date as end
#'   of period. Default "start".
#'
#' @family utils_dates
#' @return  A date_series with period format.
#'
#' @noRd
as_period_date <- function(dates_series,
                           period = c("day", "month", "quarter", "year"),
                           period_date = c("start", "end")) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))

  # set date of timeseries
  period <- match.arg(period)
  switch(
    period,
    "day" = {
      period_unit <- "day"
    },
    "month" = {
      period_unit <- "month"
    },
    "quarter" = {
      period_unit <- "quarter"
    },
    "year" = {
      period_unit <- "year"
    }
  )

  period_date <- match.arg(period_date)
  switch(period_date,
    "start" = {
      # first day of period = floor_date
      floor_date <- lubridate::floor_date(dates_series,
        unit = period_unit
      )
      period_dates_series <- floor_date
    },
    "end" = {
      # last day of period = ceiling_date -1
      ceiling_date <- lubridate::ceiling_date(dates_series,
        unit = period_unit,
        change_on_boundary = TRUE
      )
      period_dates_series <- ceiling_date - 1
    }
  )

  return(period_dates_series)
}
