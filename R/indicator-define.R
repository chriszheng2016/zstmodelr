
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


#' Get attribute definition function of industry code from stock_db
#'
#' Generic function to get attribute definition function of industry code
#'  from stock_db.
#'
#' @param stock_db         A stock database object to operate.
#'
#' @family indicator build functions
#'
#' @return A function of attribute definition of industry code if succeed,
#'  otherwise NULL.
#'
#' @export
#'
#' @examples
# S3 generic definition
# ind_attr_def_indcd <- function(stock_db, ...){
#   UseMethod("ind_attr_def_indcd")
# }
# S4 generic definition
setGeneric(
  name = "ind_attr_def_indcd",
  signature = c("stock_db"),
  def = ind_attr_def_indcd <- function(stock_db, ...) {
    standardGeneric("ind_attr_def_indcd")
  }
)


# Non-generic functions for indicator defining  ---------------------------------

#' Create definition function of indicator for computing indicator
#'
#' Combine indicator params to crate a definition function for computing indicator.
#'
#' @param indicator_code   A character for indicator code.
#' @param indicator_expr   A expr as a formula to compute indicator.
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
create_indicator_def_fun <- function(indicator_code,
                                     indicator_expr,
                                     rolly_window = 0,
                                     period = c(
                                       "day", "month",
                                       "quarter", "year"
                                     )) {

  # validate params
  assertive::assert_is_character(indicator_code)
  assertive::assert_is_call(indicator_expr)
  assertive::assert_all_are_greater_than_or_equal_to(rolly_window, 0)

  # avoid side-effect of lazy-eval
  compute_expr <- indicator_expr
  indicator_code <- force(indicator_code)
  rolly_window <- force(rolly_window)
  period <- match.arg(period)
  eval_env <- rlang::current_env()

  # define method of evaluting exprs
  .eval_expr <- function(ds_vars) {

    # validate params
    assertive::assert_is_data.frame(ds_vars)

    result <- NULL

    # redefine eval_tidy to return default value and display error,
    # if an error occured in evaluating expr
    eval_tidy_with_dfault <- purrr::possibly(rlang::eval_tidy,
      otherwise = NULL,
      quiet = FALSE
    )

    # evaluate expr to compute indicator
    result <- eval_tidy_with_dfault(compute_expr,
      data = ds_vars,
      env = eval_env
    )

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
                              re_freq = c(
                                "day", "month",
                                "quarter", "year"
                              )) {

    # validate params
    assertive::assert_is_data.frame(ds_vars)

    # ensure all import fields are existed
    check_fields(ds_vars, c("ind_code", "ind_value"))


    # re-group vars by period
    ds_vars_by_period <- ds_vars %>%
      tidyr::spread(key = "ind_code", value = "ind_value") %>%
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
                            key_fields = NULL,
                            debug = FALSE,
                            ....) {

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
        indicator_code,
        NROW(ds_indicator),
        NROW(ds_vars)
      )
      rlang::abort(msg)
    }

    # create timeseries for indicators
    ts_indicator <- tibble::tibble(
      date = ds_vars$date,
      period = !!period,
      !!indicator_code := ds_indicator
    )

    # get value fields of ds_vars need to be removed.
    # it assume value fields of vars are double and
    # attr fields are not double
    value_fields_vars <- expect_type_fields(ds_vars,
      .expect_type = "double"
    )

    # combine key fields and indicators
    if (debug) {
      # keep all fields of ds_vars in final result
      ts_indicator <- ds_vars %>%
        dplyr::left_join(ts_indicator, by = date_index_field)
    } else {
      # remove value indicators of ds_vars in final result
      ts_indicator <- ds_vars %>%
        dplyr::select(-!!value_fields_vars) %>%
        dplyr::left_join(ts_indicator, by = date_index_field)
    }

    # filter non-na result
    # ts_indicator <- ts_indicator %>%
    #   dplyr::filter(!is.na(!!rlang::parse_expr(indicator_code)))


    # arrange result fields : non-ind fields, ind fields
    # it assumes indicator fields are double fields, non-double
    # fields are non_indicator fields
    result_indicator_fields <- expect_type_fields(ts_indicator,
      .expect_type = "double"
    )
    result_non_indicator_fields <- expect_type_fields(ts_indicator,
      .expect_type = "double",
      .negate = TRUE
    )
    ts_indicator <- ts_indicator %>%
      dplyr::select(!!result_non_indicator_fields, !!result_indicator_fields)

    return(ts_indicator)
  }

  # return function of defining indicator
  return(ind_def_fun)
}

#' Create definition function of new attribute for modifying indicator
#'
#' Create a definition function of new attribute for modifying indicator.
#'
#' @param attr_name   A character for attribute name.
#' @param attr_fun   A function to generate attribute.
#' @param ...   Other arguments to attr_fun.
#'
#'
#' @family indicator build functions
#'
#' @return A function of attribute definition to modify indicator if succeed,
#' otherwise NULL.
#'
#' @export
create_attribute_def_fun <- function(attr_name,
                                     attr_fun,
                                     ...) {

  # validate params
  assertive::assert_is_function(attr_fun)
  assertive::assert_is_character(attr_name)

  # define new attributes definition functiion
  ind_attr_def <- function(ts_indicator, date_index_field = c("date"),
                             key_fields = NULL, ....) {

    # validate params
    assertive::assert_is_data.frame(ts_indicator)

    # generate attribute
    date_index_expr <- rlang::parse_expr(date_index_field)
    key_fields_exprs <- rlang::parse_exprs(key_fields)
    names(key_fields_exprs) <- key_fields

    ts_attribute <- ts_indicator %>%
      dplyr::mutate(!!attr_name := attr_fun(
        date = !!date_index_expr, !!!key_fields_exprs, ...
      ))

    # create timeseries for attribute:
    # date_index, key fields and attribute
    ts_ind_attribute <- ts_attribute %>%
      dplyr::select(!!date_index_field, !!key_fields, !!attr_name)

    return(ts_ind_attribute)
  }

  # return indicator attr_def function
  return(ind_attr_def)
}


#' Prioritize indicators defs by dependency  indicators
#'
#' By analysing dependency among indicators, indicator defs will re-ordered by
#' priority(1 to n) which means 1 is highest and n is lowest. Indicator with
#' higher priority should be generated before indicator with lower priority.
#'
#' Create a definition function of new attribute for modifying indicator.
#'
#' @param ds_indicator_defs   A dataframe of indicators definition info.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of definitions of prioritized indicators if succeed,
#'  otherwise NULL.
#' @export
prioritize_indicator_defs <- function(ds_indicator_defs) {

  # function to find indpendent indicators recursively
  .find_independ_indcators <- function(ind_code, ind_defs_trees) {

    # validate params
    assertive::assert_is_character(ind_code)
    assertive::assert_is_data.frame(ind_defs_trees)

    independ_indicators <- character(0)

    # find ind_def node matched by ind_code
    ind_def <- ind_defs_trees %>%
      dplyr::filter(ind_code == !!ind_code)
    if (NROW(ind_def) == 1) {

      # find current indicator def
      depend_indicators <- unique(ind_def$depend_ind_codes[[1]])

      # judge whether ind_def is independent or not
      ind_def_is_independent <- is.null(depend_indicators) ||
        (length(depend_indicators) == 0)

      # process by its dependency
      if (ind_def_is_independent) {
        # independent indicator:
        # get to a indepent indicator and output
        independ_indicators <- ind_def$ind_code
      } else {
        # dependent indicator:
        # use recursion to find independent indicators of
        # dependent indicators of current indicator

        # get dependent indicators defs of current indicator
        depend_indicators_def_trees <- ind_defs_trees %>%
          dplyr::filter(ind_code %in% depend_indicators)

        # find independent indicators of dependent indicators
        independ_indicators_list <- purrr::map(depend_indicators_def_trees$ind_code,
          .find_independ_indcators,
          ind_defs_trees = ind_defs_trees
        )
        independ_indicators <- purrr::flatten_chr(independ_indicators_list)
      }
    } else {
      # find zero or more than one ind_code in ind_defs_trees
      msg <- sprintf(
        "There is %d of %s in ind_defs_trees, not unique one",
        NROW(ind_def),
        ind_code
      )
      rlang::abort(msg)
    }

    # only return unqiue indicators
    independ_indicators <- unique(independ_indicators)

    return(independ_indicators)
  }

  # function to clean ind_defs_trees to ensure indicators are ok
  .clean_ind_defs_trees <- function(ind_defs_trees) {

    # validate params
    assertive::assert_is_data.frame(ind_defs_trees)

    # validate indicators in defs_trees
    clean_defs_trees <- validate_indicators(ind_defs_trees)

    # check duplicated indicators in defs_trees
    clean_defs_trees <- check_duplicated_indicators(clean_defs_trees)

    # check loop-dependency among indicators
    clean_defs_trees <- check_loop_depdency(clean_defs_trees)

    return(clean_defs_trees)
  }

  # main body of function ---

  # validate params
  assertive::assert_is_data.frame(ds_indicator_defs)

  # build defs_trees to parase dependency among indicators
  ind_defs_trees <- create_ind_defs_trees(ds_indicator_defs)

  # set priority for indicators according to dependency
  ds_indicator_defs_priority <- ds_indicator_defs %>%
    dplyr::mutate(priority = NA)
  priority <- 1
  while (NROW(ind_defs_trees) > 0) {

    # clean ind_defs trees
    ind_defs_trees <- .clean_ind_defs_trees(ind_defs_trees)

    # find indenpendent indicators of head tree of def_trees
    root_defs_tree <- ind_defs_trees[1, ]
    root_ind_code <- root_defs_tree$ind_code
    independ_indicators <- .find_independ_indcators(root_ind_code,
      ind_defs_trees = ind_defs_trees
    )

    # set priority for independent indicator
    ds_indicator_defs_priority$priority[ds_indicator_defs_priority$ind_code
      %in% independ_indicators] <- priority

    # reset priority for next indicator
    if (root_ind_code %in% independ_indicators) {
      # when indicator of head_defs_tree is independent indicator,
      # it means analyzing head_defs_tree has finished, we should
      # move to next head_defs_tree and reset priority to 1
      priority <- 1
    } else {
      # otherwise, we still in analyzing sub-tree of head_defs_tree,
      # move priority to next level
      priority <- priority + 1
    }

    # get new trees by removing found independent indicators
    ind_defs_trees <- ind_defs_trees %>%
      dplyr::filter(!(ind_code %in% independ_indicators))

    ind_defs_trees <- as_ind_defs_trees(ind_defs_trees)

  }

  # re-order and re-group indicator defs by priority
  ds_indicator_defs_priority <- ds_indicator_defs_priority %>%
    dplyr::filter(!is.na(priority)) %>%
    dplyr::arrange(priority) %>%
    dplyr::group_by(priority) %>%
    tidyr::nest(.key = "ds_indicator_defs")

  return(ds_indicator_defs_priority)

}

# Internal functions for indicator defining  -------------------------------

# Create tress of indicator defs
create_ind_defs_trees <- function(ds_indicator_defs) {

  # validate params
  assertive::assert_is_data.frame(ds_indicator_defs)

  # build defs_trees to parase dependency among indicators
  if (all(c("ind_code", "ind_vars") %in% names(ds_indicator_defs))) {
    defs_trees_info <- ds_indicator_defs %>%
      dplyr::select(ind_code, depend_ind_codes = ind_vars)
  } else if (all(c("ind_code", "depend_ind_codes") %in% names(ds_indicator_defs))) {
    defs_trees_info <- ds_indicator_defs
  } else {
    msg <- "Invalid data.frame of indicators_defs."
    rlang::abort(msg)
  }

  # creaet new class
  defs_trees <- tibble::new_tibble(
    defs_trees_info,
    subclass = "ind_defs_trees"
  )

  return(defs_trees)

}

# wrapper converter for create_ind_defs_tree
as_ind_defs_trees <- function(x) {

  assertive::assert_is_data.frame(x)

  if (inherits(x, "ind_defs_trees")) {
    ind_defs_trees <- x
  } else {
    ind_defs_trees <- create_ind_defs_trees(x)
  }

  return(ind_defs_trees)

}

# Validate indicators to ensure every indicator depends on
# indicators defined in defs trees
validate_indicators <- function(ind_defs_trees) {

  # validate params
  assertive::assert_is_data.frame(ind_defs_trees)
  stopifnot(!is.null(ind_defs_trees),
            inherits(ind_defs_trees, "ind_defs_trees"))

  # filter out any ind_code of depend_ind_codes, which never exists
  # in ind_code of defs_trees
  all_valid_ind_codes <- unique(ind_defs_trees$ind_code)
  valid_defs_trees <- ind_defs_trees %>%
    dplyr::mutate(depend_ind_codes = purrr::map(
      ind_defs_trees$depend_ind_codes,
      function(.x) {
        # get valid ind code in depend_ind_codes
        valid_ind_codes <- .x[.x %in% all_valid_ind_codes]
        # replace character(0) in depend_ind_code as NULL
        if (length(valid_ind_codes) == 0) {
          valid_ind_codes <- NULL
        }
        return(valid_ind_codes)
      }
    ))

  # fix problem of losing class of ind_defs_trees after dplyr process
  valid_defs_trees <- as_ind_defs_trees(valid_defs_trees)

  return(valid_defs_trees)

}

# Check duplicated indicators.
# If there is any duplicated indicator, raise error,
# otherwise output original ind_def_trees.
check_duplicated_indicators <- function(ind_defs_trees) {

  # validate params
  assertive::assert_is_data.frame(ind_defs_trees)
  stopifnot(!is.null(ind_defs_trees),
            inherits(ind_defs_trees, "ind_defs_trees"))

  # check duplicated indicators in defs_tree
  duplicated_ind_codes <- ind_defs_trees$ind_code[duplicated(ind_defs_trees$ind_code)]
  if (NROW(duplicated_ind_codes) == 0) {
    # No duplicated indicators
    pass_defs_trees <- ind_defs_trees
  } else {
    # found duplicated indicators
    msg <- sprintf(
      "Found duplicated indicators(%s) in defs tree, please check!!",
      paste0(duplicated_ind_codes, collapse = ",")
    )
    rlang::abort(msg)
  }

  # fix problem of losing class of ind_defs_trees after dplyr process
  pass_defs_trees <- as_ind_defs_trees(pass_defs_trees)

  return(pass_defs_trees)
}

# Check loop-dependency among indicators.
# If there is any loop dependeny, raise error,
# otherwise oupput ind_def_trees by putting root indicators
# in front of ind_def_trees.
check_loop_depdency <- function(ind_defs_trees) {

  # validate params
  assertive::assert_is_data.frame(ind_defs_trees)
  stopifnot(!is.null(ind_defs_trees),
            inherits(ind_defs_trees, "ind_defs_trees"))

  # if no root_indicator existed in defs_trees,
  # there must be loop dependency among trees,
  # because every indicator depend on each others.
  all_indicators <- unique(ind_defs_trees$ind_code)
  all_depend_indicators <- unique(purrr::flatten_chr(ind_defs_trees$depend_ind_codes))
  root_indicators <- all_indicators[!(all_indicators %in% all_depend_indicators)]
  found_loop_dependency <- ifelse(length(root_indicators) == 0, TRUE, FALSE)

  # reorder def-trees by putting root indicators in front of def_trees
  if (!found_loop_dependency) {
    # No loop dependency among indicators
    # put root indicators in the front of defs_tree
    pass_defs_trees <- ind_defs_trees %>%
      dplyr::mutate(is_root_indicator = (ind_code %in% root_indicators)) %>%
      dplyr::arrange(desc(is_root_indicator)) %>%
      dplyr::select(-is_root_indicator)
  } else {
    # found loop dependency
    msg <- sprintf(
      "Found loop dependency among indicators in defs tree, please check!!"
    )
    rlang::abort(msg)

  }

  # fix problem of losing class of ind_defs_trees after dplyr process
  pass_defs_trees <- as_ind_defs_trees(pass_defs_trees)

  return(pass_defs_trees)

}







