#' Check date series periodic or not
#'
#' Check whether date series is regular/irregular periodic.
#'
#' @details
#' periodic dates includes two kinds:
#' \itemize{
#'    \item regular periodic series:
#'         a date/time vector is regular periodic, if the vector has not more
#'     one date/time stamp on each date of peroid and dosen't miss any date of period.
#'     For example, c("2018-01-01", "2018-02-01", "2018-03-01", "2018-04-01",
#'     "2018-05-01", "2018-06-01") is a regular monthly periodic dates.
#'
#'    \item irrgular periodic series:
#'        a date/time vector is regular periodic, if the vector has more one
#'     date/time stamp on each date of peroid or miss some date of period.
#'     For example, c("2018-01-01", "2018-02-01", "2018-04-01",
#'     "2018-05-01", "2018-06-01") is is irregular monthly periodic dates,
#'     which miss date of "2018-03-01" in dates of months
#' }
#
#' @param dates_series   A vector of dates/timestamps.
#' @param freq_rule    Frequency rule of period to test,  e.g. "day", "month", "quarter",
#'  "year", default "Day".
#' @param regular    Whether to test date series as a regular periodic series
#'  or not. Since irregular test is looser than regluar test, irregluar test
#'  on regluar series will return true. Default is FALSE, which means we will
#'  use looser test(irregular).
#'
#' @family utility functions of dates
#' @return   return TRUE if dates is periodic.
#' @export
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

      # params for test irregluar dates
      interval_min <- 0
      interval_max <- 1
    },
    "month" = {

      # A date/time vector is defined as daily if the vector
      # has not more than one date/timestamp per day.
      is_regular_fun <- timeDate::isMonthly

      # params for test irregluar dates
      interval_min <- 30 - 2
      interval_max <- 30 + 2
    },
    "quarter" = {

      #A date/time vector is defined as quarterly if the vector
      # has not more than one date/timestamp per quarter.
      is_regular_fun <- timeDate::isQuarterly

      # params for test irregluar dates
      interval_min <- 30 * 3 - 2
      interval_max <- 30 * 3 + 2
    },
    "year" = {
      # A date/time vector is defined as daily if the vector
      # has not more than one date/time stamp per year,
      # or missing date/time stamp per year.
      is_regular_fun <- isRegularYearly

      # params for test irregluar dates
      interval_min <- 365 - 3
      interval_max <- 365 + 3
    }
  )

  # check whether it is periodic dates(regluar/irregular)
  is_periodic <- FALSE
  if ( regular) {
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
isRegularYearly <- function(x){

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
#' @param regular    Whether to test date series as a regular periodic series
#'  or not. Since irregular test is looser than regluar test, irregluar test
#'  on regluar series will return true. Default is FALSE, which means we will
#'  use looser test(irregular).
#'
#' @family utility functions of dates
#' @return  return "D" for daily, "M" for monthly, "Q" for quarterly, "Y" for yearly,
#'  "U" for unkown.
#'
#' @export
guess_dates_period <- function(dates_series, regular = FALSE) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))

  dates_period <- "U"

  # it is a monthly date series?
  if (dates_period == "U") {
    if (is_periodic_dates(dates_series, freq_rule = "day",
                          regular = regular)) {
      dates_period <- "D"
    }
  }

  # it is a monthly date series?
  if (dates_period == "U") {
    if (is_periodic_dates(dates_series, freq_rule = "month",
                          regular = regular)) {
      dates_period <- "M"
    }
  }

  # it is a quarterly date series?
  if (dates_period == "U") {
    if (is_periodic_dates(dates_series, freq_rule = "quarter",
                          regular = regular)) {
      dates_period <- "Q"
    }
  }

  # it is a yearly date series?
  if (dates_period == "U") {
    if (is_periodic_dates(dates_series, freq_rule = "year",
                          regular = regular)) {
      dates_period <- "Y"
    }
  }

  return(dates_period)
}
