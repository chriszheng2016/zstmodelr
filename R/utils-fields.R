
#' Check whether fields exsit in data or not.
#' @param  data   a dataframe or matrix to check.
#' @param  fields a character vector of field names to check.
#'
#' @family utils_fields
#' @return  return invisble NULL if succeed, otherwise raise error.
#'
#' @export
check_fields <- function(data, fields) {

  # validate params
  data_sym <- rlang::ensym(data)

  assertive::assert_is_not_null(data)
  assertive::assert_is_character(fields)

  all_fields <- names(data)

  # check whether the fields are exist in data
  fields_are_existed <- fields %in% all_fields
  if (any(!fields_are_existed)) {
    msg <- sprintf(
      "Some fields(%s) don't exist in %s",
      fields[!fields_are_existed],
      as.character(data_sym)
    )
    rlang::abort(msg)
  }
}

#' Check whether fields are specified type
#'
#' Predicate fields of dataframe are specifed type or not.
#' @note
#'  numeric means intger or double; double don't include date.
#'
#'
#' @param data     A vector of date/timestamps.
#' @param expect_type A character of type to test, e.g. "numeric", "integer",
#' "double", "character", "date", "factor", "list", "NA"
#' @param negate  A logical to negate expect_type of not, Default FALSE means
#'  not to negate the expect_type.
#' @param predicate_fun  A function used as testing, return TRUE if matched.
#'  Default NULL means not to use predicate_fun for testing.
#' @param ... Params to predicate_fun.
#'
#'
#' @family utils_fields
#' @return  return a vectors of logical with same length of names of dataframe.
#'
#' @export
is_type_field <- function(data, expect_type = c(
                                 "numeric",
                                 "integer",
                                 "double",
                                 "character",
                                 "date",
                                 "factor",
                                 "list",
                                 "NA"
                               ),
                               negate = FALSE,
                               predicate_fun = NULL,
                               ...) {
  # validate params
  assertive::assert_is_not_null(data)
  assertive::assert_is_logical(negate)

  # build fun of expecation
  if (is.null(predicate_fun)) {
    expect_type <- match.arg(expect_type)
    predicate_fun <- switch(expect_type,
      "numeric" = {
        # numeric means an object of typeof intger or double
        # purrr::as_mapper(~(inherits(., what = "numeric")))
        purrr::as_mapper(~(is.numeric(.)))
      },
      "integer" = {
        purrr::as_mapper(~(typeof(.) == "integer"))
      },
      "double" = {
        # need to exclude Date type whose tyepof also is double
        purrr::as_mapper(~(typeof(.) == "double" &&
          (!inherits(., what = "Date"))))
      },
      "character" = {
        purrr::as_mapper(~(typeof(.) == "character"))
      },
      "date" = {
        purrr::as_mapper(~(inherits(., what = "Date")))
      },
      "factor" = {
        purrr::as_mapper(~(is.factor(.)))
      },
      "list" = {
        # list means to coloum-list
        purrr::as_mapper(~(typeof(.) == "list"))
      },
      "NA" = {
        # NA means all data in a coloum are NA
        purrr::as_mapper(~(all(is.na(.))))
      }
    )
  } else {
    assertive::assert_is_function(predicate_fun)
    predicate_fun <- predicate_fun
  }

  # need to get negative expected fields
  if (negate) {
    predicate_fun <- purrr::negate(predicate_fun)
  }

  # use fun of expection to get result
  are_expect_fields <- purrr::map_lgl(data, predicate_fun, ...)

  return(are_expect_fields)
}


#' Identify fields with specified type
#'
#' Get the field names of dataframe with specified type.
#'
#' @inheritParams is_type_field
#'
#' @return return a vectors of field names with specified type. If no field is
#' expect type , return a character(0).
#'
#' @family utils_fields
#'
#' @export
expect_type_fields <- function(data, expect_type = c(
                                 "numeric",
                                 "integer",
                                 "double",
                                 "character",
                                 "date",
                                 "factor",
                                 "list",
                                 "NA"
                               ),
                               negate = FALSE,
                               predicate_fun = NULL,
                               ...) {
  # validate params
  assertive::assert_is_not_null(data)
  assertive::assert_is_logical(negate)

  # find out whether field is specified type
  are_expect_fields <- is_type_field(data,
                                     expect_type = expect_type,
                                     negate = negate,
                                     predicate_fun = predicate_fun,
                                     ...)
  all_fields <- names(data)
  expect_fields <- all_fields[are_expect_fields]

  return(expect_fields)
}
