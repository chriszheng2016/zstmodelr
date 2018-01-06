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
