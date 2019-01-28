
# Compute factor stability coefficient
factor_stabilitity <- function(ds_factors, factors_list = NULL) {
  # Validate params
}

# Compute factor collinearity
factor_collinearity <- function(ds_factors, factors_list = NULL) {

  # Validate params
}

# Compute factor correlation
factor_correlation <- function(ds_factors, factors_list = NULL) {

  # Validate params
  assertive::assert_is_not_null(ds_factors)

  fields <- parse_factor_fields(ds_factors, factors_list)
}
