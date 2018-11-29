
# Compute Indicator by using definition and variables
compute_indicator <- function(ts_vars,
                              ind_def_fun,
                              ...,
                              date_index_field = c("date"),
                              key_fields = NULL,
                              parallel = TRUE) {

  # Define method to compute single group
  .compute_indicator_single_group <- function(ts_vars,
                                                ind_def_fun,
                                                ...,
                                                date_index_field = c("date"),
                                                key_fields = NULL) {

    # validate params
    assertive::assert_is_data.frame(ts_vars)
    assertive::assert_is_function(ind_def_fun)

    # pre-process ts_vars
    ts_vars <- tibble::as.tibble(ts_vars)
    date_index <- rlang::parse_expr(date_index_field)
    ts_vars <- ts_vars %>%
      dplyr::arrange(!!date_index)

    # compute indicator
    ts_indicator <- ind_def_fun(ts_vars,
      date_index_field = date_index_field,
      key_fields = key_fields,
      ...
    )

    # warn user of failure
    if (is.null(ts_indicator)) {
      if (!is.null(key_fields)) {
        key_id <- as.character(ts_vars[1, key_fields])
        key_id <- paste(key_id, collapse = "-")
      } else {
        key_id <- "NULL"
      }
      msg <- sprintf(
        "faild to compute indictors for %s",
        key_id
      )
      rlang::warn(msg)
    }

    return(ts_indicator)
  }


  # -- Main Function --

  # validate params
  assertive::assert_is_data.frame(ts_vars)
  assertive::assert_is_function(ind_def_fun)


  # Work for single/multi group dataset
  if (is.null(key_fields)) {

    # For single group
    ds_indicator <- .compute_indicator_single_group(ts_vars,
      ind_def_fun = ind_def_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {

    # For mutlti groups by key_fieilds
    ds_indicator <- plyr::ddply(ts_vars,
      .variables = key_fields,
      .fun = .compute_indicator_single_group,
      ind_def_fun = ind_def_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields,
      .parallel = parallel,
      .progress = plyr::progress_win(title = "Computing...")
    )
  }

  ds_indicator <- tibble::as.tibble(ds_indicator)

  return(ds_indicator)
}
