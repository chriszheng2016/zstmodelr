#' @include indicator-define.R
#' @include stock-db-gta.R

# Generic functions implemetation by gta_db class ------------------------


# Get definitiona of customzied indicators from gta_db
#  Method definition for s3 generic
# @describeIn get_indicator_defs get indicator defs from database
#'  of gta_db class.
#' @export
get_indicator_defs.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # get profile of stock_db
  gta_profile_name <- get_profile(stock_db)

  # get info of customized indicators from profile
  customized_indictors_info <- profile_get_customized_indicators(gta_profile_name)

  # create indicator_defs
  indicator_defs <- NULL
  if (!is.null(customized_indictors_info)) {
    indicator_defs <- build_indicator_defs.gta_db(stock_db, customized_indictors_info)
  }

  return(indicator_defs)
}

# Method definition for s4 generic
#' @describeIn get_indicator_defs get indicator defs from database
#'  of gta_db class.
#' @export
setMethod(
  "get_indicator_defs",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    get_indicator_defs.gta_db(stock_db)
  }
)


# Get input vars for computing customzied indicators from gta_db
# Method definition for s3 generic
# @describeIn get_indicator_vars get input vars for computing customzied
#'  indicators from database of gta_db class.
#' @export
get_indicator_vars.gta_db <- function(stock_db, indicator_defs) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_data.frame(indicator_defs)

  # get indicators of vars
  ds_vars <- NULL
  gta_profile_name <- get_profile(stock_db)
  all_indictors_info <- profile_get_indicators(gta_profile_name)
  if (!is.null(all_indictors_info)) {

    # get ind_codes for vars
    all_indictors_codes <- all_indictors_info$ind_code
    vars_syms <- unique(purrr::reduce(indicator_defs$ind_vars, c))
    vars_are_existed <- tolower(all_indictors_codes) %in% tolower(vars_syms)
    vars_ind_code <- all_indictors_codes[vars_are_existed]

    # get indicators for vars
    ds_vars <- get_indicators(stock_db,
      indicator_codes = vars_ind_code
    )
  }

  return(ds_vars)
}

# Method definition for s4 generic
#' @describeIn get_indicator_vars  get input vars for computing customzied
#'  indicators from database of gta_db class.
#' @export
setMethod(
  "get_indicator_vars",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_defs, ...) {
    get_indicator_vars.gta_db(stock_db, indicator_defs)
  }
)

# Parse vars in indicator expr from gta_db
# Method definition for s3 generic
# @describeIn parse_indicator_vars parse vars in indicator expr from
#'  database of gta_db class.
#' @export
parse_indicator_vars.gta_db <- function(stock_db, indicator_expr) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_call(indicator_expr)

  # get syms from expr
  syms <- find_syms(!!indicator_expr,
    pattern = "[^\\{\\+\\-\\*\\/\\(\\^\\<-]"
  )

  # get indicator_vars codes matched with symbols in expr
  indicator_vars <- NULL
  gta_profile_name <- get_profile(stock_db)
  all_indictors_info <- profile_get_indicators(gta_profile_name)
  if (!is.null(all_indictors_info)) {
    all_indictors_codes <- all_indictors_info$ind_code
    all_indictors_names <- all_indictors_info$ind_name
    names(all_indictors_names) <- all_indictors_codes

    syms_are_indicators <- syms %in% all_indictors_codes
    indicator_vars <- syms[syms_are_indicators]
    indicator_vars_names <- all_indictors_names[indicator_vars]
    non_indicator_vars <- syms[!syms_are_indicators]

    msg <- sprintf(
      "\nIndicator vars in parsed expr: %s;\nNon_indicator vars in parsed expr: %s.",
      paste0(indicator_vars, "(", indicator_vars_names, ")",
        collapse = ","
      ),
      paste0(non_indicator_vars, collapse = ",")
    )
    rlang::inform(msg)
  }

  return(indicator_vars)
}
# Method definition for s4 generic
#' @describeIn parse_indicator_vars parse vars in indicator expr from
#'  database of gta_db class.
#' @export
setMethod(
  "parse_indicator_vars",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_expr, ...) {
    parse_indicator_vars.gta_db(stock_db, indicator_expr)
  }
)

# Get attribute definition function of industry code from gta_db
# Method definition for s3 generic
# @describeIn ind_attr_def_indcd  get attribute definition function of
#' industry code from database of gta_db class.
#' @export
ind_attr_def_indcd.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # get stock industry info from stock_db
  ds_stock_industry <- get_stock_industry(stock_db)

  # create attrubtue def
  ind_attr_indcd <- NULL
  if (!is.null(ds_stock_industry)) {
    # build fun for computing att_value
    attr_value_fun <- purrr::partial(compute_attr_value.gta_db,
      find_stock_attr_fun = find_stock_indcd.gta_db,
      ds_attr_source = ds_stock_industry
    )
    # create attr def
    ind_attr_indcd <- create_attribute_def_fun("indcd",
      attr_fun = attr_value_fun
    )
  }

  return(ind_attr_indcd)
}
# Method definition for s4 generic
#' @describeIn ind_attr_def_indcd  get attribute definition function of
#' industry code from database of gta_db class.
#' @export
setMethod(
  "ind_attr_def_indcd",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    ind_attr_def_indcd.gta_db(stock_db)
  }
)

# Get attribute definition function of trading status from gta_db
# Method definition for s3 generic
# @describeIn ind_attr_def_tradstat  get attribute definition function of
#' trading status from database of gta_db class.
#' @export
ind_attr_def_trdstat.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # get spt stocks info from stock_db
  ds_spt_stocks <- get_spt_stocks(stock_db)

  # create attrubtue def
  ind_attr_trdstat <- NULL
  if (!is.null(ds_spt_stocks)) {
    # build fun for computing att_value
    attr_value_fun <- purrr::partial(compute_attr_value.gta_db,
      find_stock_attr_fun = find_stock_trdstat.gta_db,
      ds_attr_source = ds_spt_stocks
    )
    # create attr def
    ind_attr_trdstat <- create_attribute_def_fun("trdstat",
      attr_fun = attr_value_fun
    )
  }

  return(ind_attr_trdstat)
}
# Method definition for s4 generic
#' @describeIn ind_attr_def_trdstat  get attribute definition function of
#' trading status from database of gta_db class.
##' @export
setMethod(
  "ind_attr_def_trdstat",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    ind_attr_def_trdstat.gta_db(stock_db)
  }
)


# Non-generic internal functions for gta_db operation -----------------------

#' Build indicator defs from customeized indicator info
#'
#' @param stock_db         A stock database object to operate.
#' @param customized_indictors_info A dataframe of customized indicator info.
#'
#' @return A dataframe of inicator defs.
#' @noRd
build_indicator_defs.gta_db <- function(stock_db, customized_indictors_info) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(customized_indictors_info)

  indicator_defs <- customized_indictors_info %>%
    dplyr::mutate(
      ind_expr = list(NULL),
      ind_def_fun = list(NULL),
      ind_vars = list(NULL)
    )

  # create indicator_def
  for (i in seq_len(NROW(indicator_defs))) {

    # Print debug msg
    msg <- sprintf(
      "\nCreate indicator definition for %s:",
      indicator_defs$ind_code[i]
    )
    rlang::inform(msg)

    # convert ind_keys
    ind_keys <- stringr::str_split(indicator_defs$ind_keys,
      pattern = ","
    )
    ind_keys <- purrr::map(ind_keys, stringr::str_trim)
    indicator_defs$ind_keys <- ind_keys

    # create indicator_def for each cutomized indicator
    indicator_def <- indicator_defs[i, ]
    indicator_formula <- indicator_def$ind_formula

    if (!is.na(indicator_formula)) {

      # create ind_expr
      indicator_expr <- create_expr(!!indicator_formula)
      indicator_defs$ind_expr[i] <- list(indicator_expr)

      # create def_fun for indicator
      indicator_def_fun <- create_indicator_def_fun(
        indicator_code = indicator_def$ind_code,
        indicator_expr = indicator_expr,
        rolly_window = indicator_def$rolling_window,
        period = indicator_def$period,
        fillna_method = indicator_def$fillna_method
      )
      indicator_defs$ind_def_fun[i] <- list(indicator_def_fun)

      # parse vars for computing indicator
      indicaotr_vars <- parse_indicator_vars(stock_db, indicator_expr)
      indicator_defs$ind_vars[i] <- list(indicaotr_vars)
    }
  }

  # select output fields
  output_fields <- c(
    "ind_code", "ind_type", "ind_name", "ind_category",
    "ind_source", "ind_description", "ind_formula",
    "ind_keys", "rolling_window", "period", "output_format",
    "is_active", "ind_expr", "ind_def_fun", "ind_vars"
  )
  indicator_defs <- indicator_defs %>%
    dplyr::select(output_fields)


  return(indicator_defs)
}

#' Compute attr values for stocks at various dates
#'
#' @param dates  A vector of dates to compute.
#' @param stkcds A vector of stkcds to compute.
#' @param find_stock_attr_fun A function to find value of stock attribute in
#'   `ds_attr_source`.
#' @param ds_attr_source A dataframe of attribute data.
#' @param ...  Other param to `find_stock_attr_fun`.
#'
#' @return A vector of attribute values with same length of `dates`/`stkcds`.`
#' @noRd
compute_attr_value.gta_db <- function(dates,
                                      stkcds,
                                      find_stock_attr_fun,
                                      ds_attr_source,
                                      ...) {
  # validate params
  assertive::assert_is_date(dates)
  assertive::assert_is_character(stkcds)
  assertive::assert_is_function(find_stock_attr_fun)
  assertive::assert_is_data.frame(ds_attr_source)

  # limit ds_attr_source to target stocks
  ds_attr_source <- ds_attr_source %>%
    dplyr::filter(stkcd %in% unique(stkcds))

  # get attribute values by matching dates/stkcds
  attr_values <- purrr::map2_chr(
    .x = dates, .y = stkcds,
    .f = find_stock_attr_fun,
    ds_attr_source,
    ...
  )

  return(attr_values)
}

#' Find industry code for a stock at specified date in target dataset
#'
#' @param date   A date to search for.
#' @param stkcd  A stkcd to search for.
#' @param ds_stock_industry A dataframe of stock industry info to search in.
#'
#' @return A industry code of matched date/stkcd if successed, otherwise NA.
#' @noRd
find_stock_indcd.gta_db <- function(date, stkcd, ds_stock_industry) {

  # validate params
  assertive::assert_is_character(stkcd)
  assertive::assert_is_date(date)
  assertive::assert_is_data.frame(ds_stock_industry)

  # find indcd by date and stkcd
  indcd <- NA
  ds_match_indcd <- ds_stock_industry %>%
    dplyr::arrange(stkcd, date) %>%
    dplyr::filter(stkcd == !!stkcd, date <= !!date)
  result_rows <- NROW(ds_match_indcd)
  if (result_rows > 0) {
    indcd <- ds_match_indcd$indcd[[result_rows]]
  }

  return(indcd)
}

#' Find trading status for a stock at specified date in target dataset.
#'
#' @param date   A date to search for.
#' @param stkcd  A stkcd to search for.
#' @param ds_stock_industry A dataframe of stock special trade info to search in.
#'
#' @return A list ststaus code of matched date/stkcd if successed, otherwise NA.
#' @noRd
find_stock_trdstat.gta_db <- function(date, stkcd, ds_spt_stocks) {

  # validate params
  assertive::assert_is_character(stkcd)
  assertive::assert_is_date(date)
  assertive::assert_is_data.frame(ds_spt_stocks)

  # find trdstat by date and stkcd
  trdstat <- "list"
  ds_match_trdstat <- ds_spt_stocks %>%
    dplyr::arrange(stkcd, date) %>%
    dplyr::filter(stkcd == !!stkcd, date <= !!date)
  result_rows <- NROW(ds_match_trdstat)
  if (result_rows > 0) {
    trdstat <- ds_match_trdstat$trade_status[[result_rows]]
  }

  return(trdstat)
}
