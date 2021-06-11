
#' Compute z-scores of factors dataset
#'
#' Compute z-scores of factors by scaling all factors according the aggregate
#' formula and groups of fields.
#'
#'
#' @param ds_factors  a factors dataset.
#' @param aggregate_formula a formula to aggregate zscores of all factors,
#' e.g. ROCE + PB, NULL is default which means no need to compute aggregate
#' zscore, but only compute zscore for all  numeric factor fields.
#' @param group_by  a character vector of fields as group data for scaling, NULL
#' is default value which means no group setting.
#' @param clean_extremes_method method of cleaning extremes before standardization
#' , i.e. "sigma","mad", by default "sigma".
#' see details: \code{\link{clean_extremes_sigma}}, \code{\link{clean_extremes_mad}}
#' @param standard_method  method of standardize data, i.e. "normal","rank",
#' by default "normal".
#' see details: \code{\link{standardize_normal_scale}}, \code{\link{standardize_rank_scale}}
#' @param ... additional arguments to clean_extremes_method and standard_method.
#'
#'
#' @return            a dataset of factors z-score
#' @export
factors_zscore <- function(ds_factors,
                           aggregate_formula = NULL,
                           group_by = NULL,
                           clean_extremes_method = c("sigma", "mad"),
                           standard_method = c("normal", "rank"), ...) {

  # Validate params
  stopifnot(!is.null(ds_factors))

  # if there is aggregate_formula, compute factors in aggregate_formula
  # and aggregate_zscore, otherwise just compute zscore for all factors

  # get compute factors list
  aggregate_formula <- rlang::enquo(aggregate_formula)
  origin_fields <- colnames(ds_factors)
  is_numeric_class_field <- purrr::map_lgl(ds_factors, ~ inherits(., "numeric"))
  if (!rlang::quo_is_null(aggregate_formula)) {

    # use factors in aggregate_formula as computing factors
    compute_factors <- all.vars(aggregate_formula)

    # Make sure compute factors are valid fields in ds_fators
    is_valid_formula_factor <- compute_factors %in% origin_fields[is_numeric_class_field]
    if (!all(is_valid_formula_factor)) {
      msg <- sprintf(
        "factors(%s): not vaild factor field of factors dataset",
        stringr::str_c(compute_factors[!is_valid_formula_factor], collapse = ",")
      )
      stop(msg)
    }
  } else {
    # use all fields of numeric class as computing factors if no aggregate_formula is provided
    compute_factors <- origin_fields[is_numeric_class_field]
  }
  # Build result field names
  output_fields <- c(origin_fields[!is_numeric_class_field], compute_factors)

  # Compute z-score of each factors by normalization
  zscore_result <- ds_factors %>%
    normalize_factors(
      factors_list = compute_factors,
      group_by = group_by,
      clean_extremes_method,
      standard_method, ...
    )


  # Compute a stock's aggregate z-scores if need
  if (!rlang::quo_is_null(aggregate_formula)) {
    zscore_result <- zscore_result %>%
      dplyr::mutate(stk_score = !!aggregate_formula)
  }

  return(zscore_result)
}

#' Filter stocks by factors z-score
#'
#' Filter stocks by ranking the z-score of factors.
#' The group of z-score of factors will keep in the results
#'
#' @param ds_zscores      a z-score datasets of factors
#' @param ranking_field   the name of field for ranking
#' @param ranking_number  the number of ranking N, positive number N means top N
#'                        negative number N means last N
#'
#' @return            a datasets of filtered stocks
#' @export
zscore_filter_stocks <- function(ds_zscores,
                                 ranking_field = "stk_score",
                                 ranking_number = 5) {

  # Validate params
  stopifnot(!is.null(ds_zscores))
  stopifnot(is.character(ranking_field), length(ranking_field) == 1)
  stopifnot(!is.null(ranking_number), ranking_number != 0)

  # Filter stocks by ranking field and number
  ranking_field <- rlang::parse_quo(ranking_field, env = rlang::caller_env())
  filter_stocks <- ds_zscores %>%
    dplyr::top_n(ranking_number, !!ranking_field)

  # Build result stocks

  if (is.null(dplyr::groups(filter_stocks))) {
    result_stocks <- filter_stocks %>%
      dplyr::arrange(dplyr::desc(!!ranking_field))
  } else {
    result_stocks <- filter_stocks %>%
      dplyr::arrange(dplyr::desc(!!ranking_field), .by_group = TRUE)
  }

  return(result_stocks)
}
