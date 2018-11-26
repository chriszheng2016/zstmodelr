# Check whether date series is periodic
# return TRUE if dates is periodic like dayly, monthly, quarterly, and yearly.
is_dates_periodic <- function(dates_series,
                              freq_rule = c("day", "month", "quarter", "year")) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))
  dates_series <- sort(unique(dates_series))

  is_regular_fun <- NULL
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
    "day" = {
      is_regular_fun <- timeDate::isDaily
      interval_min <- 0
      interval_max <- 1
    },
    "month" = {
      is_regular_fun <- timeDate::isMonthly
      interval_min <- 30 - 2
      interval_max <- 30 + 2
    },
    "quarter" = {
      is_regular_fun <- timeDate::isQuarterly
      interval_min <- 30 * 3 - 3
      interval_max <- 30 * 3 + 3
    },
    "year" = {
      is_regular_fun <- NULL
      interval_min <- 365 - 3
      interval_max <- 365 + 3
    }
  )

  # check whether is periodic
  is_periodic <- FALSE
  # test for regular daily dates
  if (!is.null(is_regular_fun)) {
    if (is_regular_fun(timeDate::as.timeDate(dates_series))) {
      is_periodic <- TRUE
    }
  }

  # irregular daily dates
  if (!is_periodic) {
    intervals <- median(dates_series - dplyr::lag(dates_series), na.rm = TRUE)
    if ((intervals >= interval_min) & (intervals <= interval_max)) {
      is_periodic <- TRUE
    }
  }

  return(is_periodic)
}

# Guess periodicty of date series
# return "D" for daily, "M" for monthly, "Q" for quarterly, "Y" for yearly,
#  "U" for unkown.
guess_dates_period <- function(dates_series) {

  # validate params
  stopifnot(!is.null(dates_series), lubridate::is.Date(dates_series))

  dates_period <- "U"

  # it is a monthly date series?
  if (dates_period == "U") {
    if (is_dates_periodic(dates_series, freq_rule = "day")) {
      dates_period <- "D"
    }
  }

  # it is a monthly date series?
  if (dates_period == "U") {
    if (is_dates_periodic(dates_series, freq_rule = "month")) {
      dates_period <- "M"
    }
  }

  # it is a quarterly date series?
  if (dates_period == "U") {
    if (is_dates_periodic(dates_series, freq_rule = "quarter")) {
      dates_period <- "Q"
    }
  }

  # it is a yearly date series?
  if (dates_period == "U") {
    if (is_dates_periodic(dates_series, freq_rule = "year")) {
      dates_period <- "Y"
    }
  }

  return(dates_period)
}
