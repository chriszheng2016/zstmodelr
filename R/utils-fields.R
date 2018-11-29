
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
expect_type_fields <- function(.data, .expect_type = c(
                          "numeric",
                          "integer",
                          "character",
                          "date",
                          "factor",
                          "NA",
                          "NULL"
                        ),
                        .negate = FALSE,
                        .expect_true_fun = NULL,
                        ...) {
  # validate params
  assertive::assert_is_not_null(.data)
  assertive::assert_is_logical(.negate)

  all_fields <- names(.data)

  # build fun of expecation
  if (is.null(.expect_true_fun)) {
    .expect_type <- match.arg(.expect_type)
    expect_fun <- switch(.expect_type,
      "numeric" = {
        purrr::as_mapper(~(inherits(., what = "numeric")))
      },
      "integer" = {
        purrr::as_mapper(~(typeof(.) == "integer"))
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
      "NA" = {
        purrr::as_mapper(~(all(is.na(.))))
      },
      "NULL" = {
        purrr::as_mapper(~(all(is.null(.))))
      }
    )
  } else {
    assertive::assert_is_function(.expect_true_fun)
    expect_fun <- .expect_true_fun
  }

  # need to get negative expected fields
  if (.negate) {
    expect_fun <- purrr::negate(expect_fun)
  }

  # use fun of expection to get result
  are_expect_fields <- purrr::map_lgl(.data, expect_fun, ...)
  expect_fields <- all_fields[are_expect_fields]

  return(expect_fields)
}
