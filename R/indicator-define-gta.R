#' @include indicator-define.R
#' @include stock-db-gta.R

# Generic functions implemetation by gta_db class ------------------------


# Get definitiona of customzied indicators from gta_db
#  Method definition for s3 generic
#' @describeIn get_indicator_defs get indicator defs from database
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
  if (!is.null(customized_indictors_info))  {
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
#' @describeIn get_indicator_vars get input vars for computing customzied
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
#' @describeIn parse_indicator_vars parse vars in indicator expr from
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

    syms_in_indicators <- all_indictors_codes %in% tolower(syms)
    indicator_vars <- all_indictors_codes[syms_in_indicators]
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
#' @describeIn ind_attr_def_indcd  get attribute definition function of
#' industry code from databaseof gta_db class.
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
    ind_attr_indcd <- create_attribute_def_fun("indcd",
      attr_fun = match_indcds.gta_db,
      ds_stock_industry = ds_stock_industry
    )
  }

  return(ind_attr_indcd)
}
# Method definition for s4 generic
#' @describeIn ind_attr_def_indcd  get attribute definition function of
#' industry code from databaseof gta_db class.
##' @export
setMethod(
  "ind_attr_def_indcd",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    ind_attr_def_indcd.gta_db(stock_db)
  }
)


# Non-generic internal functions for gta_db operation -----------------------
# build indicator defs from customeized indicator info
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
        period = indicator_def$period
      )
      indicator_defs$ind_def_fun[i] <- list(indicator_def_fun)

      # parse vars for computing indicator
      indicaotr_vars <- parse_indicator_vars(stock_db, indicator_expr)
      indicator_defs$ind_vars[i] <- list(indicaotr_vars)
    }
  }

  return(indicator_defs)
}

# Get indcd for a stock at specified date
find_indcd.gta_db <- function(date, stkcd, ds_stock_industry) {

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

# Get indcds by matching dates and stkcds
match_indcds.gta_db <- function(dates, stkcds, ds_stock_industry, ...) {

  # validate params
  assertive::assert_is_date(dates)
  assertive::assert_is_character(stkcds)
  assertive::assert_is_data.frame(ds_stock_industry)

  ds_stock_industry <- ds_stock_industry %>%
    dplyr::filter(stkcd %in% unique(stkcds))

  matched_indcds <- purrr::map2_chr(
    .x = dates, .y = stkcds,
    .f = find_indcd.gta_db,
    ds_stock_industry = ds_stock_industry
  )
}