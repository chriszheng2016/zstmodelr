# Useful utility tools

#' @@importFrom magrittr %>%
#' @@export
#magrittr::`%>%`



# get table name value from profile of database
.get_db_profile_varible_setting <- function(profile_name, variable) {

  stopifnot(!is.null(profile_name), is.character(profile_name))
  stopifnot(!is.null(variable), is.character(variable))

  setting_value <- NULL
  # profile_setting <- read.csv(profile, stringsAsFactors = FALSE, encoding = "UTF-8")
  varible_setting <- readxl::read_excel(profile_name, sheet = "Variable_Setting")
  if (!is.null(varible_setting)) {
    #get value for the vairable
    setting_value <- varible_setting$var_value[varible_setting$var_name == variable ]
    if (is.null(setting_value) || length(setting_value) == 0) {
      msg <- sprintf("No value of '%s' was found in %s", variable, profile )
      warning(msg)
    }
  }

  return(setting_value)

}

# get factor indicator map from profile of database
.get_db_profile_factor_indicator_map <- function(profile_name, factor_code_list) {

  stopifnot(!is.null(profile_name), is.character(profile_name))
  stopifnot(!is.null(factor_code_list))

  factor_indicator_map <- readxl::read_excel(profile_name,
                                             sheet = "Factor_Indicator_Map")
  matched_indicators <- NULL
  if (!is.null(factor_indicator_map)) {

    #get indicator info for mapped factor
    matched_indicators <- factor_indicator_map %>%
      dplyr::filter(factor_code %in%  factor_code_list )
    if ( nrow(matched_indicators) == 0 ) {
      msg <- sprintf("No entry matched '%s' was found in %s",
                     factor_code_list, profile_name )
      warning(msg)
      matched_indicators = NULL
    }
  }

  return(matched_indicators)
}

# get factors of matched groups from profile of database
.get_db_profile_group_factors <- function(profile_name, factor_groups) {

  stopifnot(!is.null(profile_name), is.character(profile_name))

  factor_indicator_map <- readxl::read_excel(profile_name,
                                             sheet = "Factor_Indicator_Map")

  matched_factors <- NULL
  if (!is.null(factor_indicator_map)) {

    #get factors info for mapped factor group
    if (!is.null(factor_groups)) {
      # get sepcified factor groups
      matched_factors <- factor_indicator_map %>%
      dplyr::filter(factor_group %in% factor_groups )
      if ( nrow(matched_factors) == 0 ) {
        msg <- sprintf("No entry matched '%s' was found in %s",
                       factor_groups, profile_name )
        warning(msg)
        matched_factors = NULL
      }

    } else {
      # get all factor_groups
      matched_factors <- factor_indicator_map
    }

    matched_factors <- matched_factors %>%
       dplyr::select(dplyr::contains("factor"))

  }

  return(matched_factors)

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


