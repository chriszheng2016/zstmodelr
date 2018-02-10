

#' Compute z-scores of facotrs
#'
#' Compute z-scores of factors by scaling all factors according the aggreate
#' formula and groups of fields.
#' The z-scores are in the range of [-3, 3] by
#' replacing the values of outliers beyond(-3/3) with -3 or 3
#'
#' @param ds_factors  a factors dataset
#' @param aggregate_formula a formula to aggreate zscores of all factors,
#'                    e.g. ROCE + PB, NULL is default which means no need to
#'                    compute aggregate zscore, but only compute zscore for all
#'                    numberic factor fields
#' @param group_by  a character vector of fields as group data for scaling, NULL
#'                    is default value which means no group settting
#' @param max_abs_score maximum absolute value of individual z-score, default value
#'                    is 3.0, any z-score beyond[-max_abs_score, max_abs_score]
#'                    will be set to value of +/- max_abs_score to exclude outliers
#'
#' @return            a z-score datasets of factors
#' @export
#'
#' @examples

factors_zscore <- function(ds_factors,
                           aggregate_formula = NULL,
                           group_by = NULL,
                           max_abs_score = 3.0) {

  # Validate params
  stopifnot(!is.null(ds_factors))

  # if there is aggregate_formula, compute factors in aggregate_formula
  # and aggreate_zscore, otherwise just compute zscore for all factors

  # get compute factors list
  aggregate_formula <- rlang::enquo(aggregate_formula)
  origin_fields <- colnames(ds_factors)
  is_numeric_class_field <- purrr::map_lgl(ds_factors, ~inherits(., "numeric"))
  if (!rlang::quo_is_null(aggregate_formula)) {

    # use factors in aggregate_formula as computing factors
    compute_factors <- stringr::str_split( rlang::quo_text(aggregate_formula), "[+\\-*/]")
    compute_factors <- stringr::str_trim(compute_factors[[1]])

    #Make sure compute factors are valid fields in ds_fators
    is_valid_formula_factor <- compute_factors %in% origin_fields[is_numeric_class_field]
    if (!all(is_valid_formula_factor)) {
      msg <- sprintf("factors(%s): not vaild factor field of factors dataset",
                     stringr::str_c(compute_factors[!is_valid_formula_factor], collapse = ","))
      stop(msg)
    }

  } else {
    # use all fields of numeric class as computing factors if no aggregate_formula is provided
    compute_factors <- origin_fields[is_numeric_class_field]
  }
  # Build result field names
  output_fields <- c(origin_fields[!is_numeric_class_field], compute_factors)

  # Group factors if needed
  if (!is.null(group_by) && length(group_by) > 0) {

    # Make sure group field are valid fields in ds_fators
    is_valid_group_field <- group_by %in% origin_fields[!is_numeric_class_field]
    if (!all(is_valid_group_field)) {
      msg <- sprintf("group fields(%s): not valid group field of factors dataset",
                     stringr::str_c(group_by[!is_valid_group_field], collapse = ","))
      stop(msg)
    }


     # ds_factors_by_group <- dplyr::group_by(ds_factors, !!group_by )
    ds_factors_by_group <- dplyr::group_by_at(ds_factors, group_by )
  } else {
    ds_factors_by_group <- ds_factors
  }

  # Compute z-score of each factors
  zscore_result <- ds_factors_by_group %>%
    dplyr::mutate_at(compute_factors, scale) %>%
    dplyr::select(output_fields)

  # Replace outliers's z-score to max_abs_score(3/-3) to avoid influence of extreme value
  zscore_result <- zscore_result %>%
    dplyr::mutate_at(compute_factors,
           function(x) dplyr::if_else(abs(x) > max_abs_score, sign(x)*max_abs_score, x))

  # Compute a stock's aggregate z-scores if need
  if (!rlang::quo_is_null(aggregate_formula)) {
    zscore_result <- zscore_result %>%
    dplyr::mutate(stk_score = !!aggregate_formula)
  }

  return(zscore_result)

}

#' Filter stocks by facotrs z-score
#'
#' Filter stocks by ranking the z-score of factors.
#' The group of z-score of factors will keep in the results
#'
#' @param ds_zscores      a z-score datasets of factors
#' @param rnaking_field   the name of field for ranking
#' @param ranking_number  the number of ranking N, positive number N means top N
#'                        negative number N means last N
#'
#' @return            a datasets of filtered stocks
#' @export
#'
#' @examples

zscore_filter_stocks <- function(ds_zscores,
                                 ranking_field = "stk_score",
                                 ranking_number  = 5) {

  # Validate params
  stopifnot(!is.null(ds_zscores))
  stopifnot(is.character(ranking_field), length(ranking_field) == 1)
  stopifnot(!is.null(ranking_number), ranking_number != 0)

  # Filter stocks by ranking field and number
  ranking_field <- rlang::parse_quosure(ranking_field)
  filter_stocks <- ds_zscores %>%
    dplyr::top_n( ranking_number, !!ranking_field)

  # Build result stocks

  if (is.null(groups(filter_stocks)))
  {
    result_stocks <- filter_stocks %>%
      dplyr::arrange( desc(!!ranking_field) )
  } else {
    result_stocks <- filter_stocks %>%
      dplyr::arrange( desc(!!ranking_field),.by_group = TRUE )
  }

  return(result_stocks)
}
