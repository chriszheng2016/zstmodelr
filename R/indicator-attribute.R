
# Add attribute to indicators
attr_indictors <- function(ts_indicators,
                           new_attr_def,
                           parallel = TRUE){

  # validate params
  assertive::assert_is_data.frame(ts_indicators)
  assertive::assert_is_function(new_attr_def)

  #transform indicators from long format into wide format
  is_long_format <- "ind_code" %in% names(ts_indicators)
  if (is_long_format) {
    ind_codes <- unique(ts_indicators$ind_code)
    ts_indicator_wide <- ts_indicators %>%
      tidyr::spread(key = ind_code, value = ind_value)

  } else {
    ts_indicator_wide <- ts_indicators
  }

  # add attributes into indicatros
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
      tidyr::gather(key = "ind_code", value = "ind_value", !!ind_codes)
  }

  return(ts_indicator_with_attr)
}

# Add industry code attribute to indicators
attr_indictors_indcd <- function(stock_db,
                                 ts_indicators,
                                 parallel = TRUE) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ts_indicators)

  # create indcd attribute def
  new_attr_indcd <- ind_attr_def_indcd(stock_db)

  # add indcd attribute to indicators
  ts_indicator_with_indcd <- attr_indictors(ts_indicators,
                                            new_attr_def = new_attr_indcd,
                                            parallel = parallel)

  return(ts_indicator_with_indcd)
}


# Add trading status attribute to indicators
attr_indictors_trdstat <- function(stock_db,
                               ts_indicators,
                               parallel = TRUE) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ts_indicators)

  # create trdstat attribute def
  new_attr_trdstat <- ind_attr_def_trdstat(stock_db)

  # add trdstat attribute to indicators
  ts_indicator_with_trdstat <- attr_indictors(ts_indicators,
                                            new_attr_def = new_attr_trdstat,
                                            parallel = parallel)

  return(ts_indicator_with_trdstat)

}

