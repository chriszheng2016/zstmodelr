

#' Build composite factor by aggregating related factors
#'
#' Composite factor is built by aggregating formula coming from weight of
#' similar or correlated factors, which is determined by various methods, such as ....
#'
#' How to build composition
#'
#' @param ds_factors  a factors dataset.
#' @param aggregate_formula a formula to aggregate factors into new factor,
#' e.g. \code{composite_factor ~ 0.33*PE + 0.33*PB + 0.33*PS}.
#' @param drop  a logic to determine whether to drop original factors in the
#' result datasets, by default TRUE.
#'
#'
#' @return            a dataset of new factor.
#' @export
composite_factor <- function(ds_factors,
                                   aggregate_formula,
                                   drop = TRUE) {

  # Validate params
  assertive::assert_is_not_null(ds_factors)
  assertive::assert_is_formula(aggregate_formula)
  assertive::assert_is_logical(drop)

  aggregate_formula <- rlang::enquo(aggregate_formula)
  formula_string <- rlang::quo_text(aggregate_formula)
  deparse_formula <- stringr::str_split(formula_string,
                                        pattern = "\\s*~\\s*",n = 2)[[1]]
  if (length(deparse_formula) == 2) {
    factor_var <- rlang::parse_expr(deparse_formula[1])
    factor_formula <- rlang::parse_expr(deparse_formula[2])
  } else {
    factor_var <- rlang::parse_expr("composite_factor")
    factor_formula <- rlang::parse_expr(deparse_formula[1])
  }

  #Make sure compute factors are valid fields in ds_fators
  compute_factors = all.vars(factor_formula)
  is_valid_formula_factor <- compute_factors %in% names(ds_factors)
  if (!all(is_valid_formula_factor)) {
    msg <- sprintf("factors(%s): not vaild factor field of factors dataset",
                   stringr::str_c(compute_factors[!is_valid_formula_factor], collapse = ","))
    stop(msg)
  }

  ds_result_factors <- ds_factors %>%
       dplyr::mutate( !!factor_var := !!factor_formula)

  if (drop) {

    origin_fields <- names(ds_result_factors)
    output_fields <- origin_fields[!(origin_fields %in% compute_factors)]

    ds_result_factors <- ds_result_factors %>%
      dplyr::select(output_fields)
  }

  return(ds_result_factors)

}

#' Build aggregating formula of composite factor
#'
#' Aggregating formula is built from weight of similar or correlated factors
#' Composite factor is built by aggregating formula coming from weight of
#' similar or correlated factors, which is determined by various methods, such as ....
#'
#'
#' @param factors_weight  a list of factor weight with columns of factor_name and
#' factor_weight.
#' @param new_factor_name a name of composite factor, by default "composite_factor".
#'
#'
#' @return            a formula of compoiste factor,
#' like \code{composite_factor ~ 0.33*PE + 0.33*PB + 0.33*PS}.
#' @export
weight_formula <- function(factors_weight,
                           new_factor_name = "composite_factor") {

  assertive::assert_is_not_null(factors_weight)
  assertive::assert_is_not_null(new_factor_name)
  assertive::assert_is_character(new_factor_name)


  weight_formula_string <- stringr::str_c(factors_weight$factor_name,
                                  round(factors_weight$factor_weight, 3),
                                  sep = "*",
                                  collapse = "+")

  if (!is.null(new_factor_name) && (!length(new_factor_name) == 0)) {
    weight_formula_string <- stringr::str_c(new_factor_name ,
                                            " ~ " ,
                                            weight_formula_string)
  }

  weight_formula <- rlang::parse_expr(weight_formula_string)
  weight_formula <- as.formula(weight_formula)

  return(weight_formula)

}


#' Compute factors weight for compositing factor by equal-weight method
#'
#' Compute factors weight by giving each factor equal weight of
#' \eqn{1/number of factors}. The result of factors weight could be used for
#' \code{\link{weight_formula}} to build a formula of composite factor
#'
#' @param factors_list  a list of factors.
#'
#'
#' @return      a list of factorS weight with columns of factor_name, factor_weight.
#' @export
factors_weight_equal <- function(factors_list) {

  # Validate params
  assertive::assert_is_not_null(factors_list)
  assertive::assert_is_vector(factors_list)

  factor_weight_list <- rep(1/length(factors_list), length(factors_list))

  factors_weight <- list(factor_name = factors_list,
                         factor_weight = factor_weight_list)

  return(factors_weight)

}

# compute factor weights by PCA method
factors_weight_pca <- function(ds_factors_return) {

  # Validate params

}

# compute factor weights by historical returns method
factors_weight_return <- function(ds_factors_return) {

  # Validate params

}

# compute factor weights by histocial ICs method
factors_weight_IC <- function(ds_factors_IC) {

  # Validate params

}
