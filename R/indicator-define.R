
# Generic functions for indicator defining  ------------------------------


#' Get definitions of customzied indicators from stock_db
#'
#' Generic function to get definition of customized indicators from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of definitions of customized indicators if succeed,
#' otherwise NULL.
#'
#' @export
#'
#' @examples
# S3 generic definition
# get_indicator_defs <- function(stock_db, ...){
#   UseMethod("get_indicator_defs")
# }
# S4 generic definition
setGeneric(
  name = "get_indicator_defs",
  signature = c("stock_db"),
  def = get_indicator_defs <- function(stock_db, ...) {
    standardGeneric("get_indicator_defs")
  }
)

#' Get input vars for computing customzied indicators from stock_db
#'
#' Generic function to get vars for computing customized indicators from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param indicator_defs   A dataframe of indicator definitions.
#'
#' @family indicator build functions
#'
#' @return A dataframe of input vars for computing customized indicators if succeed,
#' otherwise NULL.
#'
#' @export
#'
#' @examples
# S3 generic definition
# get_indicator_vars <- function(stock_db, indicator_defs, ...){
#   UseMethod("get_indicator_vars")
# }
# S4 generic definition
setGeneric(
  name = "get_indicator_vars",
  signature = c("stock_db"),
  def = get_indicator_vars <- function(stock_db, indicator_defs, ...) {
    standardGeneric("get_indicator_vars")
  }
)

#' Parse vars in indicator expr from stock_db
#'
#' Generic function to parse vars in indicator expr from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#' @param indicator_expr   A expr of indicator to parase.
#'
#' @family indicator build functions
#'
#' @return A character vector of name of vars if succeed, otherwise NULL.
#'
#' @export
#'
#' @examples
# S3 generic definition
# get_indicator_vars <- function(stock_db, indicator_expr, ...){
#   UseMethod("get_indicator_vars")
# }
# S4 generic definition
setGeneric(
  name = "parse_indicator_vars",
  signature = c("stock_db"),
  def = parse_indicator_vars <- function(stock_db, indicator_expr, ...) {
    standardGeneric("parse_indicator_vars")
  }
)


# Non-generic functions for indicator defining  ---------------------------------

#' Translate indicator params into definiton function
#'
#' Combine indicator params into a definition function for computing indicators.
#'
#' @param indicator_expr   A expr as a formula to compute indicator.
#' @param indicator_name   A character for indicator name.
#' @param rolly_window   A integer as rolly computing window.
#' @param period   A periodicity of indicator, i.e. "day", "month",
#'    "quarter", "yearly".
#'
#'
#' @family indicator build functions
#'
#' @return A function of indicator definition to compute indicator if succeed,
#' otherwise NULL.
#'
#' @export
#'
#' @examples
create_indicator_def <- function(indicator_expr,
                                 indicator_name,
                                 rolly_window = 0,
                                 period = c(
                                   "day", "month",
                                   "quarter", "year"
                                 )) {

  # validate params
  assertive::assert_is_call(indicator_expr)
  assertive::assert_is_character(indicator_name)
  assertive::assert_all_are_greater_than_or_equal_to(rolly_window, 0)

  # avoid side-effect of lazy-eval
  compute_expr <- indicator_expr
  indicator_name <- force(indicator_name)
  rolly_window <- force(rolly_window)
  period <- match.arg(period)
  eval_env <- rlang::current_env()

  # define method of evaluting exprs
  .eval_expr <- function(ds_vars) {

    # validate params
    assertive::assert_is_data.frame(ds_vars)

    result <- NULL

    # redefine eval_tidy to return default value and disply error
    # if an error occured in evaluating expr
    eval_tidy_with_dfault <- purrr::possibly(rlang::eval_tidy,
      otherwise = NULL,
      quiet = FALSE
    )

    # evaluate expr to compute indicator
    result <- eval_tidy_with_dfault(compute_expr,
                                    data = ds_vars,
                                    env = eval_env)

    return(result)
  }

  # define rollify methods for evaluation
  .rollify <- function(.data, window, .f, ...,
                         unlist = TRUE, na_value = NULL) {

    # validate params
    assertive::assert_is_data.frame(.data)

    roll_length <- NROW(.data)

    # initialize `output` vector
    output <- rlang::rep_along(1:roll_length, list(na_value))

    # get rolling result
    if (window <= roll_length) {
      for (i in window:roll_length) {
        f_data <- .data[(i - window + 1):i, ]
        output[[i]] <- .f(f_data, ...)
      }
    }

    # unlist result if request, except foratomic scalar
    if (unlist) {
      is_scalar_atomic <- purrr::map_lgl(output, rlang::is_scalar_atomic)
      if (all(is_scalar_atomic)) {
        output <- unlist(output)
      }
    }

    return(output)
  }

  # define ds_var process
  .process_vars <- function(ds_vars,
                              date_index_field = c("date"),
                              re_freq = c("day", "month",
                                          "quarter", "year")) {

    # validate params
    assertive::assert_is_data.frame(ds_vars)

    # ensure all import fields are existed
    check_fields(ds_vars, c("ind_name", "ind_value"))


    # re-group vars by period
    ds_vars_by_period <- ds_vars %>%
      tidyr::spread(key = "ind_name", value = "ind_value") %>%
      dplyr::group_by(period) %>%
      tidyr::nest()

    # re-freq vars in different priods
    re_freq <- match.arg(re_freq)
    ds_vars_by_period <- ds_vars_by_period %>%
      dplyr::mutate(
        refreq_data =
          purrr::map(data,
            ts_resample,
            freq_rule = re_freq,
            fillna_method = "ffill",
            agg_method = mean,
            date_index_field = c("date"),
            key_fields = NULL
          )
      )

    # remove columns which are all NA in refreq_data
    ds_vars_by_period <- ds_vars_by_period %>%
      dplyr::mutate(refreq_data = purrr::map(
        refreq_data,
        dplyr::select_if,
        function(x) !all(is.na(x))
      ))

    # combine refreq_data into final result
    suppressMessages({
      ds_vars_output <- purrr::reduce(
        ds_vars_by_period$refreq_data,
        dplyr::full_join
      )
    })


    return(ds_vars_output)
  }

  # define indicator definition function
  ind_def_fun <- function(ds_vars, date_index_field = c("date"),
                            key_fields = NULL, ....) {

    # validate params
    assertive::assert_is_data.frame(ds_vars)

    success <- TRUE

    # ensure all import fields are existed
    check_fields(ds_vars, c(date_index_field, key_fields))

    # process ds_vars
    ds_vars <- .process_vars(ds_vars, re_freq = period)

    # Evaluate exprs in ds_vars
    if (rolly_window > 0) {
      # rolling evalattion
      ds_indicator <- .rollify(ds_vars,
        .eval_expr,
        window = rolly_window,
        unlist = TRUE,
        na_value = NA
      )
    } else {
      # normal evaluation
      ds_indicator <- .eval_expr(ds_vars)
    }

    # validate result
    if (NROW(ds_vars) != NROW(ds_indicator)) {
      msg <- sprintf(
        "error in computing indicator(%s):
        length of indicator(%d) isn't equal to that of vars(%d)",
        indicator_name,
        NROW(ds_indicator),
        NROW(ds_vars)
      )
      rlang::abort(msg)
    }

    # create timeseries for indicators
    ts_indicator <- tibble::tibble(
      date = ds_vars$date,
      period = !!period,
      !!indicator_name := ds_indicator
    )

    # combine key fields and indicators
    ts_indicator <- ds_vars %>%
      dplyr::select(
        !!date_index_field,
        !!key_fields
      ) %>%
      dplyr::left_join(ts_indicator, by = date_index_field)

    return(ts_indicator)
  }

  # return function of defining indicator
  return(ind_def_fun)
}
