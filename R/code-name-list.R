#' @include stock-db.R



# Class definition of code_name_list class -----------------------------------

# Class definition of code_name_list class
setRefClass("code_name_list",
  fields = c(
    code = "ANY",
    name = "ANY"
  )
)

setClassUnion("code_name_listOrNull", c("code_name_list", "NULL"))
# setClassUnion("code_name_listOrNull", c("code_name_list"))


#' Class creator of code_name_list class
#'
#' code_name_list class creator
#'
#' @param codes a vector of code
#' @param names a vector of name
#'
#' @return a object of code_name_list
#'
#' @export
code_name_list <- function(codes, names) {
  stopifnot(!is.null(codes), !is.null(names))
  stopifnot(length(codes) == length(names))

  # Creat object of s3 class
  # code_name_list <- structure(list(code = codes, name = names),
  #                             class = "code_name_list")

  # create object of S4 class
  code_name_list <- new("code_name_list",
    code = as.character(codes),
    name = as.character(names)
  )
  return(code_name_list)
}


# Generic functions implemetation by code_name_list class ------------------------

# Translate code into name in code_name_list
# @describeIn code2name Translate code into name in a object of code_name_list
# @export
code2name.code_name_list <- function(x, code, exact_match = TRUE, ...) {
  stopifnot(inherits(x, "code_name_list"), !is.null(code))

  # # translate number code into charater code if stored code is character
  # if (is.character(x$code) && is.numeric(code)) {
  #   code_length <- mean(nchar(x$code), na.rm = TRUE)
  #   code <- stringr::str_pad(code, width = code_length, pad = "0")
  #   msg <- "Coerce code to character with padding with 0 on the left as the same
  #           length of codes in code_name_list"
  #   warnings(msg)
  # }
  #
  # # match code into name
  # match_index <- match(code, x$code)
  # name <- x$name[match_index]

  name <- match_var(x,
    search = code,
    exact_match = exact_match,
    search_var = "code",
    result_var = "name"
  )

  return(name)
}
# Method definition for s4 generic
#' @describeIn code2name Translate code into name in a object of code_name_list
#' @export
setMethod(
  "code2name",
  signature(x = "code_name_list"),
  function(x, code, exact_match, ...) {
    code2name.code_name_list(x, code, exact_match, ...)
  }
)

# Translate name into code in code_name_list
# @describeIn name2code Translate name into code in a object of code_name_list
# @export
name2code.code_name_list <- function(x, name, exact_match = TRUE, ...) {
  stopifnot(inherits(x, "code_name_list"), !is.null(name), is.character(name))

  # match_index <- match(name, x$name)
  # code <- x$code[match_index]

  code <- match_var(x,
    search = name,
    exact_match = exact_match,
    search_var = "name",
    result_var = "code"
  )

  return(code)
}
# Method definition for s4 generic
#' @describeIn name2code Translate name into code in a object of code_name_list
#' @export
setMethod(
  "name2code",
  signature(x = "code_name_list"),
  function(x, name, exact_match, ...) {
    name2code.code_name_list(x, name, exact_match, ...)
  }
)


# Non-generic internal functions ---------------------------------

#' Matched var by searching words
#'
#' @param x A code_name_list object to operate...
#' @param search A character or vector of character to search.
#' @param exact_match A logic to use exact matching nor not,
#'  default TRUE means to match whole word in search_var.
#' @param search_var A character of name of searching field.
#' @param result_var A character of name of result field.
#'
#' @return A character or vector of matched result,
#' if not matched, it return NA.
#' @noRd
match_var <- function(x, search, exact_match = TRUE,
                      search_var = "code", result_var = "name") {
  match_var_single_value <- function(x, exact_match = TRUE,
                                     search, search_var, result_var) {

    # Validate parameters
    assertive::assert_is_inherited_from(x, c("code_name_list"))
    assertive::assert_is_character(search)
    assertive::assert_is_character(search_var)
    assertive::assert_is_character(result_var)
    assertive::assert_all_are_true(search_var %in% names(x))
    assertive::assert_all_are_true(result_var %in% names(x))


    if (exact_match) {
      match_index <- match(search, x[[search_var]])
    } else {
      match_index <- grep(x[[search_var]], pattern = search)
    }
    result <- x[[result_var]][match_index]

    return(result)
  }

  if (length(search) == 1) {
    result <- match_var_single_value(
      x,
      search = search,
      exact_match = exact_match,
      search_var = search_var,
      result_var = result_var
    )
  } else {
    result <- purrr::map(search,
      .f = ~ match_var_single_value(
        x,
        search = .x,
        exact_match = exact_match,
        search_var = search_var,
        result_var = result_var
      )
    )

    result <- purrr::reduce(result, .f = c)
  }
}
