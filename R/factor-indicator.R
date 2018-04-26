# Normalize the indicators dataset
normalize_indicators <- function(ds_indicators,
                                 indicators = NULL,
                                 group_by = NULL,
                                 clean_extremes_method = c("sigma","mad"),
                                 standard_method = c("normal","rank"), ...) {

  # get compute indicators list
  origin_fields <- colnames(ds_indicators)
  is_numeric_class_field <- purrr::map_lgl(ds_indicators, ~inherits(., "numeric"))
  if (!is.null(indicators)) {
    # use specified indicators as computing fields

    #Make sure compute indicators are valid fields in ds_fators
    is_valid_indicator_field <- indicators %in% origin_fields[is_numeric_class_field]
    if (!all(is_valid_indicator_field)) {
      msg <- sprintf("indicators(%s): not vaild field of dataset",
                     stringr::str_c(indicators[!is_valid_indicator_field], collapse = ","))
      stop(msg)
    }
    compute_indicators <- indicators

  } else {
    # use all fields of numeric class as computing indicators if no specifying indicators
    compute_indicators <- origin_fields[is_numeric_class_field]
  }
  # Build result field names
  output_fields <- c(origin_fields[!is_numeric_class_field], compute_indicators)

  # Group indicators if needed
  if (!is.null(group_by) && length(group_by) > 0) {

    # Make sure group field are valid fields in ds_fators
    is_valid_group_field <- group_by %in% origin_fields[!is_numeric_class_field]
    if (!all(is_valid_group_field)) {
      msg <- sprintf("group fields(%s): not valid field of dataset",
                     stringr::str_c(group_by[!is_valid_group_field], collapse = ","))
      stop(msg)
    }

    ds_indicators_by_group <- dplyr::group_by_at(ds_indicators, group_by )
  } else {
    ds_indicators_by_group <- ds_indicators
  }

  # Normalize each indicators
  ds_result <- ds_indicators_by_group %>%
    dplyr::mutate_at(compute_indicators, normalize,
                     clean_extremes_method,
                     standard_method,...) %>%
    dplyr::select(output_fields)

  return(ds_result)
}

# Pre-process indicators
normalize <- function(x, clean_extremes_method = c("sigma","mad"),
                         standard_method = c("normal","rank"), ...) {

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_is_numeric(x)

  ds_result <- x
  # clean extremes from indicator
  if (!is.null(clean_extremes_method)) {
    clean_extremes_method <- match.arg(clean_extremes_method)
    ds_result <- switch(
      clean_extremes_method,
      sigma = clean_extremes_sigma(ds_result, ...),
      mad   = clean_extremes_mad(ds_result, ...),
      clean_extremes_sigma(ds_result, ...)
    )
  }


  # standardize indicator
  if (!is.null(standard_method)) {
    standard_method <- match.arg(standard_method)
    ds_result <- switch(
      standard_method,
      normal = standardize_normal_scale(ds_result, ...),
      rank   = standardize_rank_scale(ds_result, ...),
      standardize_normal_scale(ds_result, ...)
    )
  }

  return(ds_result)
}



# Clean extremes by max sigma
clean_extremes_sigma <- function(x, n_sigma = 3, extreme_value = c("limit","NA")){

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_all_are_whole_numbers(n_sigma)
  assertive::assert_all_are_positive(n_sigma)

  # Clean extremes
  x_mean  <- mean(x, na.rm = TRUE)
  x_stdev <- sd(x, na.rm = TRUE)
  upper_extreme_limit <- x_mean + n_sigma * x_stdev
  lower_extreme_limit <- x_mean - n_sigma * x_stdev

  x_result <- purrr::map_dbl(x, .f = .clean_extreme_value,
                             upper_extreme_limit,
                             lower_extreme_limit,
                             extreme_value)


  return(x_result)

}

# Clean extremes by mad
clean_extremes_mad <- function(x, n_dmad = 3, extreme_value = c("limit","NA")){

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_all_are_whole_numbers(n_dmad)
  assertive::assert_all_are_positive(n_dmad)

  # Clean extremes
  x_median  <- median(x, na.rm = TRUE)
  x_mad     <- median(abs(x - x_median))
  upper_extreme_limit <- x_median + n_dmad * x_mad
  lower_extreme_limit <- x_median - n_dmad * x_mad

  x_result <- purrr::map_dbl(x, .f = .clean_extreme_value,
                             upper_extreme_limit,
                             lower_extreme_limit,
                             extreme_value)

  return(x_result)
}


# Judge the extreme by standard and clean it by specified value
.clean_extreme_value <- function(x,
                           upper_extreme_limit,
                           lower_extreme_limit,
                           extreme_value = c("limit","NA") ) {

  stopifnot(!is.null(x), !is.null(upper_extreme_limit), !is.null(lower_extreme_limit))

  # don't use following assertive due to bad performance
  # assertive::assert_is_not_null(x)
  # assertive::assert_is_scalar(x)
  # assertive::assert_is_identical_to_true(is.na(x)|is.numeric(x))
  # assertive::assert_is_not_null(upper_extreme_limit)
  # assertive::assert_is_numeric(upper_extreme_limit)
  # assertive::assert_all_are_not_na(upper_extreme_limit)
  # assertive::assert_is_not_null(lower_extreme_limit)
  # assertive::assert_is_numeric(lower_extreme_limit)
  # assertive::assert_all_are_not_na(lower_extreme_limit)

  extreme_value <- match.arg(extreme_value)
  if (!is.na(x) & (x > upper_extreme_limit)) {
    x_clean <- ifelse(extreme_value == "NA", NA, upper_extreme_limit)
  } else if (!is.na(x) & (x < lower_extreme_limit)) {
    x_clean <- ifelse(extreme_value == "NA", NA, lower_extreme_limit)
  } else {
    x_clean <- x
  }

  return(x_clean)
}

# Standardize indicators by applying normal scale method
standardize_normal_scale <- function(x) {

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_is_numeric(x)

  # Standardize x by scaling
  x_result <- scale(x)
  return(x_result)
}

# Standardize indicattors by
standardize_rank_scale <- function(x) {

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_is_numeric(x)

  # Standardize x by ranking
  x_result <- rank(x)

  return(x_result)

}


