# Parse fields of factor dataset to sperate them into compute factors and
# output_fields
parse_factor_fields <- function(ds_factors, factors_list = NULL) {

  # Validate params
  assertive::assert_is_not_null(ds_factors)

  # get compute factors list
  origin_fields <- colnames(ds_factors)
  is_numeric_class_field <- purrr::map_lgl(ds_factors, ~inherits(., "numeric"))
  if (!is.null(factors_list)) {
    # use specified factors_list as computing fields

    #Make sure compute factors_list are valid fields in ds_fators
    is_valid_indicator_field <- factors_list %in% origin_fields[is_numeric_class_field]
    if (!all(is_valid_indicator_field)) {
      msg <- sprintf("factors(%s): not vaild field of dataset",
                     stringr::str_c(factors_list[!is_valid_indicator_field], collapse = ","))
      stop(msg)
    }
    compute_factors <- factors_list

  } else {
    # use all fields of numeric class as computing factors if no specifying factors
    compute_factors <- origin_fields[is_numeric_class_field]
  }
  # Build result field names
  output_fields <- c(origin_fields[!is_numeric_class_field], compute_factors)

  result_fields <- list(compute_factors = compute_factors,
                        output_fields = output_fields)

  return(result_fields)

}

