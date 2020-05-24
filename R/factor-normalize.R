
#' Normalize the factors dataset
#'
#' Normalze factors dataset by cleaning extremes and standardation.
#'
#'
#' @param ds_factors  A factors dataset.
#' @param factors_list A character vecter of factors.
#' @param group_by  A character vector of fields as group data for scaling, NULL
#' is default value which means no group settting.
#' @param clean_extremes_method Method of cleaning extremes befre standardization
#' , i.e. "sigma","mad", by default "sigma".
#' see details: \code{\link{clean_extremes_sigma}}, \code{\link{clean_extremes_mad}}
#' @param standard_method  method of standardizing data, i.e. "normal","rank",
#' by default "normal".
#' see details: \code{\link{standardize_normal_scale}}, \code{\link{standardize_rank_scale}}
#' @param ... additional arguments to clean_extremes_method and standard_method.
#'
#'
#' @return            a dataset of normalized factors
#' @export
normalize_factors <- function(ds_factors,
                              factors_list = NULL,
                              group_by = NULL,
                              clean_extremes_method = c("sigma","mad"),
                              standard_method = c("normal","rank"), ...) {

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

  # Group factors if needed
  if (!is.null(group_by) && length(group_by) > 0) {

    # Make sure group field are valid fields in ds_fators
    is_valid_group_field <- group_by %in% origin_fields[!is_numeric_class_field]
    if (!all(is_valid_group_field)) {
      msg <- sprintf("group fields(%s): not valid field of dataset",
                     stringr::str_c(group_by[!is_valid_group_field], collapse = ","))
      stop(msg)
    }

    ds_factors_by_group <- dplyr::group_by_at(ds_factors, group_by )
  } else {
    ds_factors_by_group <- ds_factors
  }

  # Normalize each factors
  ds_result <- ds_factors_by_group %>%
    dplyr::mutate_at(compute_factors, normalize,
                     clean_extremes_method,
                     standard_method,...) %>%
    dplyr::select(output_fields)

  return(ds_result)
}

#' Normalize a vector of data
#'
#' Normalize a vector of data by cleaning extremes and standardizing.
#'
#'
#' @param x  a vector of data.
#' @param clean_extremes_method method of cleaning extremes befre standardization
#' , i.e. "sigma","mad", by default "sigma".
#' see details: \code{\link{clean_extremes_sigma}}, \code{\link{clean_extremes_mad}}
#' @param standard_method  method of standardizing data, i.e. "normal","rank",
#' by default "normal".
#' see details: \code{\link{standardize_normal_scale}}, \code{\link{standardize_rank_scale}}
#' @param ... additional arguments to clean_extremes_method and standard_method.
#'
#'
#' @return            a vector of normalized data
#' @export
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
      normal = standardize_normal_scale(ds_result),
      rank   = standardize_rank_scale(ds_result),
      standardize_normal_scale(ds_result)
    )
  }

  return(ds_result)
}



#' Clean extremes by normal sigma method
#'
#' Identify extremes in data vector by normal sigma method, and repalce extremes
#' by NA or max/min limitation
#'
#' Basing on assumption of data as normal distribution, i.e., \eqn{X~N(\mu,\sigma^2)}
#' \deqn{P(|X-\mu|) > k*\sigma = \begin{cases}
#'       0.317 & k=1 \\
#'       0.046 & k=2 \\
#'       0.03 & k=3 \\
#'       \end{cases}}
#' So we could take data beyond \eqn{|3 sigma|} as extremes.
#'
#'
#' @param x  a vector of data.
#' @param n_sigma numbers of sigma to identify extemes, by default 3.
#' @param extreme_value  value to replace extremes, i.e. "limit","NA",
#' by default "limit".
#'
#'
#' @return      a vector of data without extremes
#' @export
clean_extremes_sigma <- function(x, n_sigma = 3, extreme_value = c("limit","NA")){

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_all_are_whole_numbers(n_sigma)
  assertive::assert_all_are_positive(n_sigma)

  # Clean extremes
  x_mean  <- mean(x, na.rm = TRUE)
  x_stdev <- sd(x, na.rm = TRUE)
  if ((!is.na(x_mean)) && (!is.na(x_stdev))) {
    upper_extreme_limit <- x_mean + n_sigma * x_stdev
    lower_extreme_limit <- x_mean - n_sigma * x_stdev

    x_result <- purrr::map_dbl(x,
                               .f = .clean_extreme_value,
                               upper_extreme_limit,
                               lower_extreme_limit,
                               extreme_value)
  } else {
    # Notice: mean(NA) return NA which lead wrong results
    # Notice: sd(0) return NA which lead wrong results
    # So keep original x as result
    x_result <- x
  }

  return(x_result)

}

#' Clean extremes by MAD method
#'
#' Identify extremes in data vector by MAD method, and repalce extremes
#' by NA or max/min limitation
#'
#' @param x  a vector of data.
#' @param n_dmad numbers of dmad to identify extemes, by default 3.
#' @param extreme_value  value to replace extremes, i.e. "limit","NA",
#' by default "limit".
#'
#'
#' @return      a vector of data without extremes
#' @export
clean_extremes_mad <- function(x, n_dmad = 3, extreme_value = c("limit","NA")){

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_all_are_whole_numbers(n_dmad)
  assertive::assert_all_are_positive(n_dmad)

  # Clean extremes
  x_median  <- median(x, na.rm = TRUE)
  x_mad     <- median(abs(x - x_median), na.rm = TRUE )

  if ((!is.na(x_median)) && (!is.na(x_mad))) {
    upper_extreme_limit <- x_median + n_dmad * x_mad
    lower_extreme_limit <- x_median - n_dmad * x_mad

    x_result <- purrr::map_dbl(x,
                               .f = .clean_extreme_value,
                               upper_extreme_limit,
                               lower_extreme_limit,
                               extreme_value)
  } else {
    # Notice: median(NA) return NA which lead wrong results
    # So keep original x as result
    x_result <- x
  }



  return(x_result)
}


# Judge the extreme by standard and clean it by specified value
.clean_extreme_value <- function(x,
                           upper_extreme_limit,
                           lower_extreme_limit,
                           extreme_value = c("limit","NA") ) {

  stopifnot(!is.null(x), !is.null(upper_extreme_limit), !is.null(lower_extreme_limit))
  stopifnot(!is.na(upper_extreme_limit), !is.na(lower_extreme_limit))

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

  if (!is.na(x)) {
    if (x > upper_extreme_limit) {
      x_clean <- ifelse(extreme_value == "NA", NA, upper_extreme_limit)
    } else if (x < lower_extreme_limit) {
      x_clean <- ifelse(extreme_value == "NA", NA, lower_extreme_limit)
    } else {
      x_clean <- x
    }
  } else {
    x_clean <- x
  }

  return(x_clean)
}

#' Standardize data vector by applying normal scale method
#'
#' Scale data into new vector of mean of 0 and stdev of 1
#'
#'
#' @param x  a vector of data.
#'
#' @return      a vector of standardized data
#' @export
standardize_normal_scale <- function(x) {

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_is_numeric(x)

  # Standardize x by scaling
  x_result <- scale(x)
  return(x_result)
}


#' Standardize data vector by applying rank scale method
#'
#' Scale the ranked data by order
#'
#'
#' @param x  a vector of data.
#'
#' @return      a vector of standardized data
#' @export
standardize_rank_scale <- function(x) {

  # Validate params
  assertive::assert_is_not_null(x)
  assertive::assert_is_vector(x)
  assertive::assert_is_numeric(x)

  # Firstly, rank x
  x_rank <- rank(x, na.last = "keep")

  # Secondly, standardize ranked x by scaling
  x_result <- scale(x_rank)

  return(x_result)

}


