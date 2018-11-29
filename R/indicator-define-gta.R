#' @include indicator-define.R
#' @include stock-db-gta.R

# get list of indicator definition
#
# Get data source info for importing raw data
# Method definition for s3 generic
#' @describeIn get_indicator_defs get a list of indicator defs  from
#'  database of gta_db class
#' @export
get_indicator_defs.gta_db <- function(stock_db) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get profile of stock_db
  gta_profile_name <- get_profile(stock_db)

  # get info of customized indicators from profile
  customized_indictors_info <- profile_get_customized_indicators(gta_profile_name)
  if (!is.null(customized_indictors_info)) {
    customized_indictors_info <- customized_indictors_info %>%
      dplyr::mutate(ind_expr = list(NULL),
                    ind_def_fun = list(NULL),
                    ind_vars = list(NULL))
  } else {
    success <- FALSE
  }

  # create indicator_def
  if (success) {
    for (i in seq_len(NROW(customized_indictors_info))) {

      # create indicator_def for each cutomized indicator
      customized_indicator <- customized_indictors_info[i, ]
      indicator_formula <- customized_indicator$ind_formula

      if (!is.na(indicator_formula)) {

        # create ind_expr
        indicator_expr <- create_expr(indicator_formula)
        customized_indictors_info$ind_expr[i] <- list(indicator_expr)

        # create def_fun for indicator
        indicator_def <- create_indicator_def(
          indicator_expr = indicator_expr,
          indicator_name = customized_indicator$ind_code,
          rolly_window = customized_indicator$rolling_window,
          period = customized_indicator$period
        )
        customized_indictors_info$ind_def_fun[i] <- list(indicator_def)

        # parse vars for computing indicator
        indicaotr_vars <- parse_indicator_vars(indicator_expr)
        customized_indictors_info$ind_vars[i] <- list(indicaotr_vars)

      }

    }
  }

  return(customized_indictors_info)
}

# Method definition for s4 generic
#' @describeIn get_indicator_defs get_indicator_defs get a list of indicator defs  from
#'  database of gta_db class
#' @export
setMethod(
  "get_indicator_defs",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    get_indicator_defs.gta_db(stock_db)
  }
)

# get list of indicator definition
#
# Get data source info for importing raw data
# Method definition for s3 generic
#' @describeIn get_indicator_vars get a list of indicator defs  from
#'  database of gta_db class
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
                                  indicator_codes = vars_ind_code)
  }

  return(ds_vars)
}

# Method definition for s4 generic
#' @describeIn get_indicator_vars  get a list of indicator defs  from
#'  database of gta_db class
#' @export
setMethod(
  "get_indicator_vars",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_defs, ...) {
    get_indicator_vars.gta_db(stock_db, indicator_defs)
  }
)



