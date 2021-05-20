#' Add attribute to indicator timeseries of stocks
#'
#' Compute attribute values of stocks and add them to indicator timeseries.
#'
#' @details
#'
#' \code{\link{attr_indicators}} is a working horse behind
#' \code{\link{attr_indicators_indcd}},\code{\link{attr_indicators_trdstat}},
#' which is mainly used to develop customized attribute function to
#' add attribute to indicator timeseries.
#'
#' \code{\link{attr_indicators_indcd}}, \code{\link{attr_indicators_trdstat}}, etc.
#' are used to add predefined attribute to indicator timeseries by using data from
#' stock_db.
#'
#' There are two way to add attribute to timeseries of indicators of stocks:
#' \itemize{
#' \item \strong{add attribute directly}:
#'  use \code{\link{attr_indicators_indcd}}, \code{\link{attr_indicators_trdstat}}
#'  to add attribute directly. The pros is easy to use, the cons is slow since
#'  it have to load data from stock database each calling. The method is
#'  mainly used to add one attribute to one indicator timeseries.
#'
#' \item \strong{add attribute indirectly}:
#' use \code{\link{ind_attr_def_indcd}}, \code{\link{ind_attr_def_trdstat}} and
#' \code{\link{modify_indicator}}. The pros is that it can be faster by dividing
#' defining attribute(load data from database once) and modifying indicators
#' (can modify repeatedly by using same attribute definition). The cons is that it is more complex than direct method. The method is mainly
#' used to add same attribute to multi-indicators timeseries.
#'
#' }
#'
#'
#' @family indicator attribute functions
#'
#' @return A dataframe of indicator timeseries with new attribute if succeed,
#'  otherwise NULL.
#' @name indicator_attribute
#' @examples
#'
#' \dontrun{
#'
#' # create a indicator from vars dataset.
#' ts_indicator <- create_indicator(ds_def_vars,
#'   ind_def_fun = ind_def_fun,
#'   debug = debug,
#'   date_index_field = "date",
#'   key_fields = key_fields,
#'   parallel = parallel
#' )
#'
#'
#' # --- add attribute directly ---
#'
#' # add attribute of indcd
#' ts_indicators_with_indcd <- attr_indicators_indcd(stock_db,
#'   ts_indicators = ts_indicators
#' )
#'
#' # add attribute of trdstats
#' ts_indicators_wide_with_attr <- attr_indicators_trdstat(stock_db,
#'   ts_indicators = ts_indicators
#' )
#'
#' # --- add attribute indirectly ---
#'
#' # create attr defs by using data from stock
#' new_attr_indcd <- ind_attr_def_indcd(stock_db)
#'
#' # use attribute def to modify multi-indicators
#' for (i in seq_len(NROW(ds_ts_indicators))) {
#'   ts_indicator <- ds_ts_indicators$data[[i]]
#'
#'   ts_indicator <- modify_indicator(
#'     ts_indicator = ts_indicator,
#'     modify_fun = new_attr_indcd,
#'     replace_exist = FALSE,
#'     date_index_field = "date",
#'     key_fields = "stkcd",
#'     parallel = parallel
#'   )
#' }
#' }
NULL


#' Add customized attribute to indicator timeseries of stocks
#' @param ts_indicators   A dataframe of indicator timeseries.
#' @param new_attr_def   A function to compute attribute value for stock
#'   indicator.
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#'
#' @describeIn indicator_attribute  add customized attribute to indicators
#'   timeseries of stocks.
#' @export
attr_indicators <- function(ts_indicators,
                            new_attr_def,
                            parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # validate params
  assertive::assert_is_data.frame(ts_indicators)
  assertive::assert_is_function(new_attr_def)

  # transform indicators from long format into wide format
  is_long_format <- "ind_code" %in% names(ts_indicators)
  if (is_long_format) {
    ind_codes <- unique(ts_indicators$ind_code)
    ts_indicator_wide <- ts_indicators %>%
      tidyr::pivot_wider(names_from = ind_code, values_from = ind_value)
  } else {
    ts_indicator_wide <- ts_indicators
  }

  # add attributes into indicators
  ts_indicator_with_attr <- modify_indicator(
    ts_indicator = ts_indicator_wide,
    modify_fun = new_attr_def,
    replace_exist = FALSE,
    date_index_field = "date",
    key_fields = "stkcd",
    parallel = parallel
  )

  # transform back to long format if need
  if (is_long_format) {
    ts_indicator_with_attr <- ts_indicator_with_attr %>%
      tidyr::pivot_longer(names_to = "ind_code", values_to = "ind_value", !!ind_codes)
  }

  return(ts_indicator_with_attr)
}

#' Add industry code attribute to indicator timeseries of stocks
#' @param stock_db A stock database object to get data.
#' @describeIn indicator_attribute  add attribute of industry code to indicator
#'   timeseries of stocks.
#' @export
attr_indicators_indcd <- function(stock_db,
                                  ts_indicators,
                                  parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ts_indicators)

  # create indcd attribute def
  new_attr_indcd <- ind_attr_def_indcd(stock_db)

  # add indcd attribute to indicators
  ts_indicator_with_indcd <- attr_indicators(ts_indicators,
    new_attr_def = new_attr_indcd,
    parallel = parallel
  )

  return(ts_indicator_with_indcd)
}


#' Add trading status attribute to indicator timeseries of stocks
#' @describeIn indicator_attribute  add attribute of trading status to indicator
#'   timeseries of stocks.
#' @export
attr_indicators_trdstat <- function(stock_db,
                                    ts_indicators,
                                    parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ts_indicators)

  # create trdstat attribute def
  new_attr_trdstat <- ind_attr_def_trdstat(stock_db)

  # add trdstat attribute to indicators
  ts_indicator_with_trdstat <- attr_indicators(ts_indicators,
    new_attr_def = new_attr_trdstat,
    parallel = parallel
  )

  return(ts_indicator_with_trdstat)
}
