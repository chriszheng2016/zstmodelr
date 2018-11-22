
# Compute Indicator
indicator_compute <- function(ts_vars,
                              def_fun,
                              ...,
                              date_index_field = c("date"),
                              key_fields = NULL) {

  # define method to compute single group
  .indicator_compute_single_group <- function(ts_vars,
                                              def_fun,
                                              ...,
                                              date_index_field = c("date"),
                                              key_fields = NULL) {

    # validate params
    stopifnot(!is.null(ts_vars), inherits(ts_vars, "data.frame"))
    ts_vars <- tibble::as.tibble(ts_vars)

    # compute indicator
    result_indicator <- def_fun(ts_vars, ...)

    # build result dataset
    ts_indicator <- ts_vars %>%
        dplyr::select(date_index_field, key_fields) %>%
        dplyr::bind_cols(result_indicator)

    return(ts_indicator)
  }


  # -- Main Function --
  # work for single/multi group dataset
  if (is.null(key_fields)) {

    # for single group
    ds_indicator <- .indicator_compute_single_group(ts_vars,
      def_fun = def_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {

    # for mutlti groups
    ds_indicator <- plyr::ddply(ts_vars,
      .variables = key_fields,
      .fun = .indicator_compute_single_group,
      def_fun = def_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields,
      .parallel = TRUE,
      .progress = plyr::progress_win(title = "Refreqencing...")
    )
  }

  ds_indicator <- tibble::as.tibble(ds_indicator)

  return(ds_indicator)
}
