
#' Compute indicator by customized function and variables timeseries
#'
#' Use customized function and variable timeseries to compute indicator.
#' Its is a working horse behind \code{\link{create_indicator}},
#' \code{\link{modify_indicator}}.
#'
#' @param ts_compute_vars   A dataframe of variable timeseries to compute.
#' @param compute_fun   A function of computing indicator.
#' @param ...       Params to compute_fun.
#' @param date_index_field  Name of date index field of ts_vars, default 'date'.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date. Default NULL means to not divide data into
#'   groups.
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of result timeseries if succeed, otherwise NULL.
#'
#' @export
compute_indicator <- function(ts_compute_vars,
                              compute_fun,
                              ...,
                              date_index_field = c("date"),
                              key_fields = NULL,
                              parallel = TRUE) {

  # Define method to compute single group
  .compute_indicator_single_group <- function(ts_compute_vars,
                                                compute_fun,
                                                ...,
                                                date_index_field = c("date"),
                                                key_fields = NULL) {

    # validate params
    assertive::assert_is_data.frame(ts_compute_vars)
    assertive::assert_is_function(compute_fun)

    # compute indicator
    ts_indicator <- NULL
    result <- tryCatch({
      compute_fun(ts_compute_vars,
        date_index_field = date_index_field,
        key_fields = key_fields,
        ...
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      # inform user of failure and return NULL
      key_id <- ""
      if (!is.null(key_fields) && NROW(ts_compute_vars) > 0) {
        key_id <- paste0(ts_compute_vars[1, key_fields], collapse = "-")
      }
      error_msg <- conditionMessage(result)
      msg <- sprintf(
        "Fail to compute indicator for %s:\n  %s",
        key_id,
        error_msg
      )
      rlang::inform(msg)
      ts_indicator <- NULL
    } else {
      ts_indicator <- result
    }

    return(ts_indicator)
  }


  # -- Main Function --

  # validate params
  assertive::assert_is_data.frame(ts_compute_vars)
  assertive::assert_is_function(compute_fun)
  # can't process data.frame with zero data
  assertive::assert_all_are_true(NROW(ts_compute_vars) > 0)

  # pre-process ts_input_vars
  ts_compute_vars <- tibble::as_tibble(ts_compute_vars)
  arrange_expr <-
    rlang::parse_exprs(c(key_fields, date_index_field))
  ts_compute_vars <- ts_compute_vars %>%
    dplyr::arrange(!!!arrange_expr)

  # Work for single/multi group dataset
  if (is.null(key_fields)) {
    # For single group
    ds_indicator <- .compute_indicator_single_group(
      ts_compute_vars,
      compute_fun = compute_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {
    # For mutlti groups by key_fields
    progress_display <- if (exists("winProgressBar")) {
      plyr::progress_win(title = "Computing...")
    } else {
      plyr::progress_text()
    }
    suppress_warnings({
      ds_indicator <- plyr::ddply(
        ts_compute_vars,
        .variables = key_fields,
        .fun = .compute_indicator_single_group,
        compute_fun = compute_fun,
        ...,
        date_index_field = date_index_field,
        key_fields = key_fields,
        .parallel = parallel,
        .progress = progress_display
      )
    },
    # suppress warnings due to parallel process
    warn_pattern = "<anonymous>: ...")

    # If there are no results, dplyr::ddply will return a data frame
    # with zero rows and columns
    if (NROW(ds_indicator) == 0)
      ds_indicator <- NULL
  }

  if (!is.null(ds_indicator)) {
    ds_indicator <- tibble::as_tibble(ds_indicator)
  }

  return(ds_indicator)
}


#' Create indicator by definition function and variables timeseries
#'
#' Use definition function and variable timeseries to create indicator.
#'
#' @param ts_def_vars   A dataframe of variable timeseries to create indicator.
#' @param ind_def_fun   A function of defining indicator.
#' @param ...       Params to ind_def_fun.
#' @param debug     A logic to determine whether to turn on debug in creating
#'  indicator. Default FALSE means not to use debug.
#' @param date_index_field  Name of date index field of ts_def_vars,
#'  default 'date'.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date. Default NULL means to not divide data into
#'   groups.
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of new indicator timeseries if succeed, otherwise NULL.
#'
#' @export
#' @examples
#' \dontrun{
#'
#'   # load vars dataset for generating indicators
#'   ds_all_vars <- get_indicator_vars(stock_db,
#'                                  indicator_defs = ds_indicator_defs)
#'
#'   # create ind_expr
#'   indicator_formula <- c("stock_return <- mretwd
#'                           market_return <- cmretwdtl
#'                           model <- lm(stock_return ~ market_return)
#'                           beta <- coef(model)['market_return']")
#'   indicator_expr <- create_expr(!!indicator_formula)
#'
#'   # create def_fun for indicator
#'   indicator_def_fun <- create_indicator_def_fun(
#'       indicator_code = "m_stock_beta1",
#'       indicator_expr = indicator_expr,
#'       rolly_window = 12,
#'       period = "month"
#'       )
#'
#'   # create a indicator from vars dataset.
#'   ts_indicator <- create_indicator(
#'         ds_def_vars,
#'         ind_def_fun = ind_def_fun,
#'         debug = FALSE,
#'         date_index_field = "date",
#'         key_fields = "stkcd",
#'         parallel = TRUE
#'         )
#'
#' }
#'
create_indicator <- function(ts_def_vars,
                             ind_def_fun,
                             ...,
                             debug = FALSE,
                             date_index_field = c("date"),
                             key_fields = NULL,
                             parallel = TRUE) {

  # validate params
  assertive::assert_is_data.frame(ts_def_vars)
  assertive::assert_is_function(ind_def_fun)

  # process input vars to fix problem when the value of keys are na
  ts_def_vars <- ts_def_vars %>%
    dplyr::filter(!is.na(ind_value))

  # get dataset of value of keys are not na
  keys_are_ok_expr <- key_fields %>%
    purrr::map_chr(~sprintf("!is.na(%s)", .x)) %>%
    paste(collapse = " || ") %>%
    rlang::parse_expr()
  ds_keys_are_ok <- ts_def_vars %>%
    dplyr::filter(!!keys_are_ok_expr)

  # get dataset of value of keys are na
  keys_are_na_expr <- key_fields %>%
    purrr::map_chr(~sprintf("is.na(%s)", .x)) %>%
    paste(collapse = " && ") %>%
    rlang::parse_expr()
  ds_keys_are_na <- ts_def_vars %>%
    dplyr::filter(!!keys_are_na_expr)

  # fix value of keys when the value of keys are na
  if (NROW(ds_keys_are_na) > 0) {

    # remove columns which are all NA in dataset
    ds_keys_are_na <- ds_keys_are_na %>%
      dplyr::select_if(.predicate = function(x) !all(is.na(x)))

    # fix value of keys with na value
    ds_keys <- tibble::as_tibble(ds_keys_are_ok[key_fields])
    ds_keys <- ds_keys[!duplicated(ds_keys), ]
    ds_keys_fixed <- ds_keys %>%
      dplyr::mutate(data = list(ds_keys_are_na)) %>%
      tidyr::unnest(cols = c(data))

    # rebuild def vars
    ts_def_vars <- ds_keys_are_ok %>%
      dplyr::bind_rows(ds_keys_fixed)
  }


  # compute result
  ds_new_indicator <- compute_indicator(ts_def_vars,
    compute_fun = ind_def_fun,
    debug = debug,
    ...,
    date_index_field = date_index_field,
    key_fields = key_fields,
    parallel = parallel
  )

  return(ds_new_indicator)
}


#' Modify indicator to add new attribute
#'
#' Use modifying function to modify indicator timeseries,
#'  e.g. add new attribute to existed indicator timeseries.
#'
#' @param ts_indicator A dataframe of indicator timeseries to modify.
#' @param modify_fun   A function of modify indicator.
#' @param ...          Params to modify_fun.
#' @param replace_exist      A logical to determine whether to replace existed
#'  attribute fields. Default FALSE means to not replace existed field.
#' @param date_index_field  Name of date index field of ts_indicator,
#'  default 'date'.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date. Default NULL means to not divide data into
#'   groups.
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of modified indicator timeseries if succeed, otherwise NULL.
#'
#' @examples
#'
#' \dontrun{
#'
#'   # modify ts_indicator with customized ind_attr_def_fun
#'
#'   # create attribute definition function of customized attribute
#'   attr_fun <- function(date, stkcd, ...) {
#'                  "attr_value"
#'                }
#'   ind_attr_def_fun <- create_attribute_def_fun(
#'         attr_name,
#'         attr_fun = attr_fun
#'         )
#'   # modify existed ts_indicators
#'   ts_modify_indicator <- modify_indicator(ts_modify_indicator,
#'        modify_fun = ind_attr_def_fun,
#'        date_index_field = "date",
#'        key_fields = "stkcd",
#'        parallel = FALSE
#'       )
#'
#'
#'   # modify ts_indicator with pre-defined ind_attr_def_fun
#'
#'   # create definition function of pre-defined attribute of indcd
#'   new_attr_indcd <- ind_attr_def_indcd(stock_db)
#'
#'   # modify existed ts_indicators
#'   ts_indicator <- modify_indicator(
#'       ts_indicator = ts_indicator,
#'       modify_fun = new_attr_indcd,
#'       replace_exist = FALSE,
#'       date_index_field = "date",
#'       key_fields = "stkcd",
#'       parallel = FALSE
#    )
#'
#' }
#'
#' @export
modify_indicator <- function(ts_indicator,
                             modify_fun,
                             ...,
                             replace_exist = FALSE,
                             date_index_field = c("date"),
                             key_fields = NULL,
                             parallel = TRUE) {



  # validate params
  assertive::assert_is_data.frame(ts_indicator)
  assertive::assert_is_function(modify_fun)

  # compute result
  ts_ind_attribute <- compute_indicator(ts_indicator,
    compute_fun = modify_fun,
    ...,
    date_index_field = date_index_field,
    key_fields = key_fields,
    parallel = parallel
  )

  # combine attribute into indicator
  ds_modify_indicator <- NULL
  if (!is.null(ts_ind_attribute)) {

    # Whether new attribute has existed
    attr_field_names <- names(ts_ind_attribute)
    attr_name <- attr_field_names[!(attr_field_names %in% c(
      date_index_field,
      key_fields
    ))]
    attr_is_existed <- attr_name %in% names(ts_indicator)

    # replace old attribute or add new attribute
    if (attr_is_existed) {
      # replace existed attribute
      if (replace_exist) {
        # force replace existed attribute

        ds_modify_indicator <- ts_indicator %>%
          dplyr::left_join(ts_ind_attribute,
            by = c(date_index_field, key_fields),
            suffix = c(".x", "")
          ) %>%
          dplyr::select(-dplyr::ends_with(".x")) %>%
          dplyr::select(
            !!date_index_field,
            !!key_fields, !!attr_name, dplyr::everything()
          )

        msg <- sprintf(
          "attribute(%s) has existed, and replace by new value.",
          attr_name
        )
        rlang::warn(msg)
      } else {
        # not replace exsisted attributee
        ds_modify_indicator <- ts_indicator

        msg <- sprintf(
          "Attribute(%s) has existed, and old attribute will remains.",
          attr_name
        )
        rlang::warn(msg)
      }
    } else {
      # add new attribute
      ds_modify_indicator <- ts_indicator %>%
        dplyr::left_join(ts_ind_attribute,
          by = c(date_index_field, key_fields)
        ) %>%
        dplyr::select(
          !!date_index_field,
          !!key_fields, !!attr_name, dplyr::everything()
        )
    }
  }

  return(ds_modify_indicator)
}
