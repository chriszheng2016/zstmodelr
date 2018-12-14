# Utils functions for indicator definition
# Notice: in order to be different form normal R function, we use CamelCase
# stlye for functions name

# Functions for defining indicator expr ----

# Lag a series by k periods
Lag <- function(x, k = 1) {

  # Shift data at current timeline
  if (k > 0) {
    # shift backward
    lag_x <- dplyr::lag(x, n = k)
  } else if (k < 0) {
    # shift forward
    lag_x <- dplyr::lead(x, n = abs(k))
  } else {
    # don't shift
    lag_x <- x
  }
  return(lag_x)
}

# Growth rate of a series
GrowthRate <- function(x) {

  # compute growth
  lag_x <- Lag(x)
  growth_x <- (x - lag_x) / lag_x

  # convert Inf as NA
  growth_x[is.infinite(growth_x)] <- NA

  return(growth_x)
}

# Ratio of two series
Ratio <- function(x_numerator, y_denominator) {

  # compute ratio
  ratio <- x_numerator / y_denominator

  # convert Inf as NA
  ratio[is.infinite(ratio)] <- NA

  return(ratio)
}

# Substract mean from a series
Demean <- function(x) {

  # compute demean
  demean_x <- x - mean(x, na.rm = TRUE)

  return(demean_x)
}

# Trail a periodic series
TrailSeries <- function(date, x) {

  date_expr <- rlang::enexpr(date)
  x_expr <- rlang::enexpr(x)

  #validate params
  assertive::assert_is_date(date)
  assertive::assert_is_vector(x)

  # predicate period of dates
  period <- guess_dates_period(date, regular = TRUE)

  # trail data by periodic dates
  if (period != "unknown") {
    # trail regular periodic series
    trail_x <- trail_periodic_series(date,
                                     data_series = x,
                                     period = period,
                                     fun = sum)
  } else {
    msg <- sprintf("Can't trail series(%s) with irregular periodic date(%s), return NAs ",
                   rlang::expr_text(x_expr),
                   rlang::expr_text(date_expr)
                   )
    rlang::abort(msg)
  }

  #only ouput a vector of data
  trail_x <- trail_x[[1]]

  return(trail_x)
}

# Beta between two vars
Beta <- function(y, x) {
  model <- lm(y ~ x)
  beta <- coef(model)["x"]

  return(beta)
}

# Functions for defining dynamic indicator expr ----

# provide dyamic indicator of risfree rate
RiskFreeRate <- function(stock_db, indicator_code,
                         period = c("day", "month", "quarter", "year")) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_character(indicator_code)

  rf_return <- get_riskfree_rate(stock_db, period = period)
  rf_return <- dplyr::rename(rf_return, !!indicator_code := riskfree_return)

  return(rf_return)
}

