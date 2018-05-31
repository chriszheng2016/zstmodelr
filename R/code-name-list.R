#'@include stock-db.R

# Class definition of code_name_list class -----------------------------------

#'@@include stock-db.R


# Class definition of code_name_list class
setRefClass("code_name_list",
            fields  = c( code = "ANY",
                         name = "ANY"))

setClassUnion("code_name_listOrNull", c("code_name_list","NULL"))
#setClassUnion("code_name_listOrNull", c("code_name_list"))


#' Class creator of code_name_list class
#'
#' code_name_list class creator
#'
#' @param codes a vector of code
#' @param names a vector of name
#'
#' @return a object of code_name_list
#' @export
#'
#' @examples

code_name_list <- function(codes, names) {

  stopifnot(!is.null(codes), !is.null(names))
  stopifnot(length(codes) == length(names))

  # Creat object of s3 class
  # code_name_list <- structure(list(code = codes, name = names),
  #                             class = "code_name_list")

  # create object of S4 class
  code_name_list <- new("code_name_list", code = codes, name = names)

  return(code_name_list)

}


# Generic functions implemetation by code_name_list class ------------------------

# Translate code into name in code_name_list
#' @describeIn code2name Translate code into name in a object of code_name_list
#' @export
code2name.code_name_list <- function(x, code) {

  stopifnot(inherits(x, "code_name_list") ,!is.null(code))

  # translate number code into charater code if stored code is character
  if (is.character(x$code) && is.numeric(code)) {
    code_length <- mean(nchar(x$code), na.rm = TRUE)
    code <- stringr::str_pad(code, width = code_length, pad = "0")
    msg <- "Coerce code to character with padding with 0 on the left as the same
            length of codes in code_name_list"
    warnings(msg)
  }

  # match code into name
  match_index = match(code, x$code)
  name <- x$name[match_index]

  return(name)

}
# Method definition for s4 generic
setMethod("code2name",
          signature(x = "code_name_list"),
          function(x, code, ...) {
            code2name.code_name_list(x, code, ...)
          })

# Translate name into code in code_name_list
#' @describeIn name2code Translate name into code in a object of code_name_list
#' @export
name2code.code_name_list <- function(x, name) {

  stopifnot(inherits(x, "code_name_list") ,!is.null(name), is.character(name))

  match_index = match(name, x$name)
  code <- x$code[match_index]

  return(code)

}
# Method definition for s4 generic
setMethod("name2code",
          signature(x = "code_name_list"),
          function(x, name, ...) {
            name2code.code_name_list(x, name, ...)
          })








