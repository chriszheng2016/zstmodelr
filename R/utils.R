# Useful utility tools

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# get setting value from profile of database
.get_db_profile <- function(profile, variable) {

  stopifnot(!is.null(profile), is.character(profile))
  stopifnot(!is.null(variable), is.character(variable))

  setting_value <- NULL
  profile_setting <- read.csv(profile, stringsAsFactors = FALSE, encoding = "UTF-8")
  if (!is.null(profile_setting)) {
    #get value for the vairable
    setting_value <- profile_setting$VAR_VALUE[profile_setting$VAR_NAME == variable ]
    if (is.null(setting_value) || length(setting_value) == 0) {
      msg <- sprintf("No value of '%s' was found in %s", variable, profile )
      warning(msg)
    }
  }

  return(setting_value)

}

# Convert normal return into cumulated return
#' @export
cumulated_return <- function(normal_return, method = c("compound", "simple")) {

  stopifnot(timeSeries::is.timeSeries(normal_return))

  return_nona <- na.omit(normal_return, method = "z")
  cumulated_return <- timeSeries::cumulated(return_nona, method = method)

  return(cumulated_return)
}

# Convert simple return into compound return
#' @export
simple2compound_return <- function(simple_return) {

  stopifnot(timeSeries::is.timeSeries(simple_return))

  # convert into compound return
  cumulated_return <- cumulated_return(simple_return, method = "simple")
  return_result <- timeSeries::returns0(cumulated_return, method = "compound")

  # restore NAs
  return_result[is.na(simple_return)] <- NA

  return(return_result)
}

# Convert simple return into compound return
#' @export
compound2simple_return <- function(compund_return) {

  stopifnot(timeSeries::is.timeSeries(compund_return))

  # convert into compound return
  cumulated_return <- cumulated_return(compund_return, method = "compound")
  return_result <- timeSeries::returns0(cumulated_return, method = "simple")

  # restore NAs
  return_result[is.na(compund_return)] <- NA

  return(return_result)

}


