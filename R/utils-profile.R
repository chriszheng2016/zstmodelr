
.PACKAGE_NAME <- "zstmodelr"
.PROFILE_DIR <- "etc"


# get full path of profile
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
profile_get_varible_setting <- function(profile_name, variable) {
  stopifnot(!is.null(profile_name), is.character(profile_name))
  stopifnot(!is.null(variable), is.character(variable))

  setting_value <- NULL
  # profile_setting <- read.csv(profile, stringsAsFactors = FALSE, encoding = "UTF-8")
  varible_setting <- readxl::read_excel(profile_name, sheet = "Variable_Setting")
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

# Get mapping between factor and indicator from profile of database
profile_get_factor_indicator_map <- function(profile_name, factor_code_list) {
  stopifnot(!is.null(profile_name), is.character(profile_name))
  stopifnot(!is.null(factor_code_list))

  factor_indicator_map <- readxl::read_excel(profile_name,
    sheet = "Factor_Indicator_Map"
  )
  matched_indicators <- NULL
  if (!is.null(factor_indicator_map)) {

    # get indicator info for mapped factor
    matched_indicators <- factor_indicator_map %>%
      dplyr::filter(factor_code %in% factor_code_list)
    if (nrow(matched_indicators) == 0) {
      msg <- sprintf(
        "No entry matched '%s' was found in %s",
        factor_code_list, profile_name
      )
      warning(msg)
      matched_indicators <- NULL
    }
  }

  return(matched_indicators)
}

# Get factors of matched groups from profile of database
profile_get_group_factors <- function(profile_name, factor_groups) {
  stopifnot(!is.null(profile_name), is.character(profile_name))

  factor_indicator_map <- readxl::read_excel(profile_name,
    sheet = "Factor_Indicator_Map"
  )

  matched_factors <- NULL
  if (!is.null(factor_indicator_map)) {

    # get factors info for mapped factor group
    if (!is.null(factor_groups)) {
      # get sepcified factor groups
      matched_factors <- factor_indicator_map %>%
        dplyr::filter(factor_group %in% factor_groups)
      if (nrow(matched_factors) == 0) {
        msg <- sprintf(
          "No entry matched '%s' was found in %s",
          factor_groups, profile_name
        )
        warning(msg)
        matched_factors <- NULL
      }
    } else {
      # get all factor_groups
      matched_factors <- factor_indicator_map
    }
  }

  return(matched_factors)
}

# Get data source for importing raw data
profile_get_datasource_files <- function(profile_name) {
  stopifnot(!is.null(profile_name), is.character(profile_name))

  # get Data source setting
  datasource_files <- readxl::read_excel(profile_name,
    sheet = "DataSource_Files"
  )
  # output valid info
  datasource_files <- datasource_files %>%
    dplyr::filter(is_valid == TRUE)

  return(datasource_files)
}

# Get info of indicators from profile of database
profile_get_indicators <- function(profile_name) {
  stopifnot(!is.null(profile_name), is.character(profile_name))

  # get Data source setting
  indicators_info <- readxl::read_excel(profile_name,
    sheet = "Indicator_list"
  )

  return(indicators_info)
}

# Get info of customized indicators from profile of database
profile_get_customized_indicators <- function(profile_name) {
  stopifnot(!is.null(profile_name), is.character(profile_name))

  # get Data source setting
  customized_indicators_info <- readxl::read_excel(profile_name,
    sheet = "Customized_Indicator"
  )

  # output only active indicators info
  customized_indicators_info <- customized_indicators_info %>%
    dplyr::filter(is_active == TRUE)

  return(customized_indicators_info)
}
