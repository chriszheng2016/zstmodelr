# Utility functions - expr

#' Utility functions of expr
#'
#' Utility functions to handle expr basing on rlang, which is used to facilitate
#' defining indicator.
#'
#'
#' @name utils_expr
#' @noRd
NULL

# Create a expr for evaluation from expr, list of exprs, or strings
#' @param expr  A expr or list of exprs or string to parse as expr.
#' @describeIn utils_expr  create a expr for evaluation from expr,
#' list of exprs, strings.
#' @noRd
create_expr <- function(expr) {
  expr <- rlang::enexpr(expr)

  # covert exprs to support mutli-types of exprs
  if (is.call(expr)) {
    # just expr
    result_expr <- expr
  } else if (is.list(expr)) {
    # list of expr
    result_expr <- rlang::expr({
      !!!expr
    })
  } else if (is.character(expr)) {
    # expr string
    exprs_string <- expr
    exprs_string <- stringr::str_remove_all(exprs_string, "\\r")
    expr <- rlang::parse_exprs(exprs_string)
    result_expr <- rlang::expr({
      !!!expr
    })
  } else {
    # invalid exprs
    msg <- sprintf(
      "exprs(%s) should be expr, list of expr, or characters",
      as.character(expr)
    )
    rlang::abort(msg)
  }

  return(result_expr)
}

# Find symbols in expr by matching pattern.
#' @param pattern  A character of regular rule for matching.
#' @param ... Extra arguments to be passed to purrr::map.
#' @describeIn utils_expr  Find symbols in expr by matching pattern.
#' @noRd
find_syms <- function(expr, pattern = NULL, ...) {
  expr <- rlang::enexpr(expr)

  # find syms by matching .preidcate
  result <- switch_expr(expr,
    constant = {
      character()
    },
    symbol = {
      rlang::as_string(expr)
    },
    call = {

      # use recursive to find sysmbs in in call expr
      result_list <- purrr::map(
        as.list(expr),
        find_syms, pattern, ...
      )
      purrr::flatten_chr(result_list)
    }
  )

  # subset result with pattern
  if (!is.null(pattern)) {
    result <- stringr::str_subset(result, pattern = pattern)
  }

  # only return unqiue symbols
  result <- unique(result)


  return(result)
}

# Get type of expr
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)
  | rlang::is_vector(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

# Template for processing different type of expr
switch_expr <- function(x, ...) {
  switch(expr_type(x),
    ...,
    rlang::abort(paste0("Don't know how to handle type ", typeof(x)))
  )
}
