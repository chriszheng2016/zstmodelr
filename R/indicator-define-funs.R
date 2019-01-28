# Utils functions for indicator definition
# Notice: in order to be different form normal R function, we use CamelCase
# stlye for functions name

# Functions for defining indicator expr ----

#' Utils functions to facilitate defining indicator expr
#'
#' Functions are only used in defining indicator expr.
#'
#' @note In order to be different form normal R function, we use CamelCase
#'  stlye for functions name.
#'
#' @details
#'  In order to facilitate defining indicator expr, there are two kinds of
#'  expr functions:
#'  \itemize{
#'     \item \strong{expr_funs to define customized indicator}:
#'       calling function to generate a result series, like
#'        \code{Lag}, \code{GrowthRate}, \code{Ratio},
#'        \code{Demean}, \code{Quarter_TTM}, \code{Beta}, etc.
#'     \item \strong{expr_funs to define dynamic indicator}:
#'       provide dynamic time series from database.
#'
#'  }
#'
#'
#' @name indicator_expr_funs
NULL

# Lag a series by k periods
#' @describeIn indicator_expr_funs  lag a sereis at k period.
#' (k>0 lag behind, k<0 lag ahead)
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
#' @describeIn indicator_expr_funs  compute growth rate of a sereis.
GrowthRate <- function(x) {

  # compute growth
  lag_x <- Lag(x)
  growth_x <- (x - lag_x) / lag_x

  # convert Inf as NA
  growth_x[is.infinite(growth_x)] <- NA

  return(growth_x)
}

# Ratio of two series
#' @describeIn indicator_expr_funs  compute ratio of two sereis.
Ratio <- function(x_numerator, y_denominator) {

  # compute ratio
  ratio <- x_numerator / y_denominator

  # convert Inf as NA
  ratio[is.infinite(ratio)] <- NA

  return(ratio)
}

# Sum multiple series
Sum <- function(..., substitute_NA = c("zero", "mean", "median", "keep")) {
  series_list <- list(...)

  # substitute NA in sereis
  substitute_NA <- match.arg(substitute_NA)
  replace_na_value <- switch(substitute_NA,
    "zero" = function(x) 0,
    "mean" = purrr::partial(mean, na.rm = TRUE),
    "median" = purrr::partial(median, na.rm = TRUE),
    "keep" = function(x) x
  )
  series_list_no_na <- purrr::map(series_list, ~ ifelse(is.na(.x),
    replace_na_value(.x),
    .x
  ))

  # sum series without NA
  result_series <- purrr::reduce(series_list_no_na, .f = `+`)

  return(result_series)
}

# Substract mean from a series
#' @describeIn indicator_expr_funs  substract mean from a series.
Demean <- function(x) {

  # compute demean
  demean_x <- x - mean(x, na.rm = TRUE)

  return(demean_x)
}

# Make a quarterly TTM(Trial Twelve Month) series
#' @describeIn indicator_expr_funs  make a quarterly TTM(Trial Twelve Month)
#'  series.
Quarter_TTM <- function(date, x, accumulated = TRUE) {
  date_expr <- rlang::enexpr(date)
  x_expr <- rlang::enexpr(x)

  # validate params
  assertive::assert_is_date(date)
  assertive::assert_is_vector(x)
  assertive::assert_is_logical(accumulated)

  # get quarter dataset
  ds_origin <- tibble::tibble(date = date, x = x)
  ds_origin <- ds_origin %>%
    dplyr::filter(lubridate::month(date) %in% c(3, 6, 9, 12))

  # make regular quarter dataset
  ds_origin_quarter <- ts_asfreq(ds_origin,
    freq_rule = "quarter",
    fillna_method = "nfill",
    date_index_field = "date",
    key_fields = NULL,
    parallel = FALSE
  )

  # predicate period of dates
  period <- guess_dates_period(ds_origin_quarter$date, regular = TRUE)

  # trail data by periodic dates
  if (period != "unknown") {

    # trail dataset of value_fields
    trail_x <- trail_periodic_series(ds_origin_quarter$date,
      data_series = ds_origin_quarter$x,
      period = period,
      accumulated = accumulated,
      trailing_month = 12L,
      agg_fun = sum,
      na.rm = TRUE
    )
  } else {
    msg <- sprintf(
      "Can't trail series(%s) with irregular periodic date(%s).",
      rlang::expr_text(x_expr),
      rlang::expr_text(date_expr)
    )
    rlang::abort(msg)
  }

  # only ouput a vector of data
  trail_x <- trail_x[[1]]

  return(trail_x)
}

# Beta between two varable series
#' @describeIn indicator_expr_funs  compute beta between two varable series
Beta <- function(y, x) {
  model <- lm(y ~ x)
  beta <- coef(model)["x"]

  return(beta)
}

# Functions for defining dynamic indicator expr ----

# Provide timeseries of dynamic indicator of risfree rate
#' @describeIn indicator_expr_funs  provide timeseries of dynamic indicator of
#' risfree rate.
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
