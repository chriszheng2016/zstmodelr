#' Utils functions of profile
#'
#' Utility functions to handle profile, which contains configration
#' information of stock database.
#'
#'
#' @name utils_profile
NULL


.PACKAGE_NAME <- "zstmodelr"
.PROFILE_DIR <- "etc"

# get full path of profile
#' @param profile_name  A character of profile name, e.g. .GTA_PROFILE_FILE
#' or "gta_profile.xlsx".
#' @param profile_dir   A character name of dir where profile is located.
#' Default .PROFILE_DIR is etc in package directory.
#' @return \strong{get_profile_path}: A path of profile if succeed, otherwise "".
#' @describeIn utils_profile  get full path of profile.
#' @export
get_profile_path <- function(profile_name, profile_dir = .PROFILE_DIR ) {

  profile_path <- system.file(profile_dir,
                              profile_name,
                              package = .PACKAGE_NAME )
  if (profile_path == "") {
    msg = sprintf("No profile(%s) exisits in %s of %s",
                  profile_name,
                  profile_dir,
                  .PACKAGE_NAME)
    stop(msg)
  }

  return(profile_path)
}

# Get table name value from profile of database
#' @param profile_path path of profile to get data from.
#' @param variable A character of name of variable to get.
#' @return \strong{others}: A dataframe of setting if succeed, othwise NULL.
#' @describeIn utils_profile  get table name value from profile of database.
#' @export
profile_get_varible_setting <- function(profile_path, variable) {
  stopifnot(!is.null(profile_path), is.character(profile_path))
  stopifnot(!is.null(variable), is.character(variable))

  setting_value <- NULL
  # profile_setting <- read.csv(profile, stringsAsFactors = FALSE, encoding = "UTF-8")
  varible_setting <- readxl::read_excel(profile_path, sheet = "Variable_Setting")
  if (!is.null(varible_setting)) {
    # get value for the vairable
    setting_value <- varible_setting$var_value[varible_setting$var_name == variable ]
    if (is.null(setting_value) || length(setting_value) == 0) {
      msg <- sprintf("No value of '%s' was found in %s", variable, profile)
      warning(msg)
    }
  }

  return(setting_value)
}

# Get info of indicators from profile of database
#' @param factor_codes  A character vector of factor codes to fetch.
#' Default NULL means to fetch all factors.
#' @param factor_groups A character vector of factor groups to fetch.
#' Default NULL means to fetch all factor groups.
#'
#' @describeIn utils_profile  get table name value from profile of database.
#' @export
profile_get_factors <- function(profile_path, factor_codes = NULL,
                                              factor_groups = NULL) {
  stopifnot(!is.null(profile_path), is.character(profile_path))

  factor_indicator_map <- readxl::read_excel(profile_path,
    sheet = "Factor_Indicator_Map"
  )

  # get factors info for matched factor_codes
  matched_factors <- factor_indicator_map
  if (!is.null(factor_codes)) {
    # get scpeficed factor_codes
    matched_factors <- matched_factors %>%
      dplyr::filter(factor_code %in% factor_codes)
    if (nrow(matched_factors) == 0) {
      msg <- sprintf(
        "No entry matched '%s' was found in %s",
        factor_codes, profile_path
      )
      warning(msg)
      matched_factors <- NULL
    }
  }

  # get factors info for matched factor_groups
  if (!is.null(factor_groups) & !is.null(matched_factors)) {
    # get sepcified factor groups
    matched_factors <- matched_factors %>%
      dplyr::filter(factor_group %in% factor_groups)
    if (nrow(matched_factors) == 0) {
      msg <- sprintf(
        "No entry matched '%s' was found in %s",
        factor_groups, profile_path
      )
      warning(msg)
      matched_factors <- NULL
    }
  }

  # validate results
  if (!is.null(matched_factors)) {

    #check whether any indicator_code is NA
    indicator_is_na <- is.na(matched_factors$indicator_code)
    if (any(indicator_is_na)) {
      msg <- sprintf(
        "factor(%s) didn't have vaild indicator_code, please check!!",
        matched_factors$factor_code[indicator_is_na]
      )
      stop(msg)
    }

  }

  # output only active factor
  matched_factors <- matched_factors %>%
    dplyr::filter(is_active == TRUE)

  return(matched_factors)
}

# Get data source for importing raw data
#' @describeIn utils_profile  get data source for importing raw data.
#' @export
profile_get_datasource_files <- function(profile_path) {
  stopifnot(!is.null(profile_path), is.character(profile_path))

  # get Data source setting
  datasource_files <- readxl::read_excel(profile_path,
    sheet = "DataSource_Files"
  )

  # output valid info
  datasource_files <- datasource_files %>%
    dplyr::filter(is_valid == TRUE)

  return(datasource_files)
}

# Get info of indicators from profile of database
#' @param indicator_codes  A character vector of indicator codes to fetch.
#'  Default NULL means to fetch all indicators.
#' @describeIn utils_profile  get info of indicators from profile of database.
#' @export
profile_get_indicators <- function(profile_path, indicator_codes = NULL) {

  #validate params
  stopifnot(!is.null(profile_path), is.character(profile_path))

  # get Data source setting
  indicators_info <- readxl::read_excel(profile_path,
    sheet = "Indicator_list"
  )

  if (!is.null(indicator_codes)) {
    # get matched indicators info
    matched_indicators_info <- indicators_info %>%
      dplyr::filter(field_code %in% indicator_codes)
    if (nrow(matched_indicators_info) == 0) {
      msg <- sprintf(
        "No entry matched '%s' was found in %s",
        indicator_codes, profile_path
      )
      warning(msg)
      matched_indicators_info <- NULL
    }
  } else {
    # get all indicators info
    matched_indicators_info <- indicators_info
  }

  # reformat colname from field_* to ind_*
  if (!is.null(matched_indicators_info)) {
    origin_colnames <- names(matched_indicators_info)
    new_colnames <- stringr::str_replace(origin_colnames,
                                         pattern = "field_",
                                         replacement = "ind_")
    names(matched_indicators_info) <- new_colnames
  }

  return(matched_indicators_info)
}

# Get info of customized indicators from profile of database
#' @describeIn utils_profile  get info of customized indicators from profile
#'  of database.
#' @export
profile_get_customized_indicators <- function(profile_path) {
  stopifnot(!is.null(profile_path), is.character(profile_path))

  # get Data source setting
  customized_indicators_info <- readxl::read_excel(profile_path,
    sheet = "Customized_Indicator"
  )

  # output only active indicators info
  customized_indicators_info <- customized_indicators_info %>%
    dplyr::filter(is_active == TRUE)

  # reformat colname from field_* to ind_*
  if (!is.null(customized_indicators_info)) {
    origin_colnames <- names(customized_indicators_info)
    new_colnames <- stringr::str_replace(origin_colnames,
                                         pattern = "field_",
                                         replacement = "ind_")
    names(customized_indicators_info) <- new_colnames
  }

  return(customized_indicators_info)
}
