
# Check whether fields exsit in data
check_fields <- function(.data, .fields) {

  # validate params
  .data_sym <- rlang::ensym(.data)

  assertive::assert_is_not_null(.data)
  assertive::assert_is_character(.fields)

  all_fields <- names(.data)

  # check whether the fields are exist in .data
  fields_are_existed <- .fields %in% all_fields
  if (any(!fields_are_existed)) {
    msg <- sprintf(
      "Some fields(%s) don't exist in %s",
      .fields[!fields_are_existed],
      as.character(.data_sym)
    )
    rlang::abort(msg)
  }
}

# Identify fields with specified type
# numeric means an object of typeof intger or double
expect_type_fields <- function(.data, .expect_type = c(
                                 "numeric",
                                 "integer",
                                 "double",
                                 "character",
                                 "date",
                                 "factor",
                                 "list",
                                 "NA"
                               ),
                               .negate = FALSE,
                               .predicate = NULL,
                               ...) {
  # validate params
  assertive::assert_is_not_null(.data)
  assertive::assert_is_logical(.negate)

  all_fields <- names(.data)

  # build fun of expecation
  if (is.null(.predicate)) {
    .expect_type <- match.arg(.expect_type)
    predicate_fun <- switch(.expect_type,
      "numeric" = {
        # numeric means an object of typeof intger or double
        # purrr::as_mapper(~(inherits(., what = "numeric")))
        purrr::as_mapper(~(is.numeric(.)))
      },
      "integer" = {
        purrr::as_mapper(~(typeof(.) == "integer"))
      },
      "double" = {
        purrr::as_mapper(~(typeof(.) == "double"))
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
    assertive::assert_is_function(.predicate)
    predicate_fun <- .predicate
  }

  # need to get negative expected fields
  if (.negate) {
    predicate_fun <- purrr::negate(predicate_fun)
  }

  # use fun of expection to get result
  are_expect_fields <- purrr::map_lgl(.data, predicate_fun, ...)
  expect_fields <- all_fields[are_expect_fields]

  return(expect_fields)
}