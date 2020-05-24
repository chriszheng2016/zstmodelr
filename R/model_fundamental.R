#  Build fundamental factor model
#' Creator of factor_test_unigress class
#'
#' Conduct univarate reggression test for descriptors of factors and build object
#' of factor_test_uniregress class as output.
#'
#'
#'
#' @param ds_build         A timeseries dataset with descriptors of factors for test
#' @param regress_method   method of partitioning data for regression, i.e.
#' "cross_section", "pooling", by default "cross_section".
#' @param regress_fun      A function to conduct regress.
#' @param ...              argments passed to regress_fun.
#' @param output_type      Type of output data, i.e."summary", "raw", if "raw",
#' raw data will be append to output object for dignosis.
#' @param date_field       Name of date field of ds_test, by default "date",
#' Column must be date-like.
#'
#' @return                 A object of factor_test_uniregress class.
#'
#' @export
model_build_fundamental <- function(ds_build,
                              regress_method = c("cross_section", "pooling"),
                              regress_fun,
                              ...,
                              output_type = c("summary", "raw"),
                              date_field = "date") {
  # Validate params
  stopifnot(!is.null(ds_build), inherits(ds_build_model, "data.frame"))
  ds_build_data <- tibble::as_tibble(ds_build)

  stopifnot(!is.null(regress_fun), inherits(regress_fun, "function"))


  # Nest test data by group of factor_name and date
  regress_method <- match.arg(regress_method)
  if (regress_method == "cross_section") {
    #cross section: group data date_field(cross section setting)
    ds_build_groupdata <- ds_build_data %>%
      dplyr::group_by(date_field) %>%
      tidyr::nest()
  } else {
    # pooling: group data by factor, no crossetion
    ds_build_groupdata <- ds_build_data

  }

  # Conduct factors regression
  ds_build_result <- ds_build_groupdata %>%
    dplyr::mutate(
      model = purrr::map(data, purrr::possibly(regress_fun, otherwise = NULL, quiet = TRUE), ...),
      glance = purrr::map(model, broom::glance),
      tidy = purrr::map(model, broom::tidy),
      augument = purrr::map2(model, data, broom::augment)
    )

  return(ds_build_result)
}
