
#' Compute Indicator by Using Definition and Variables
#'
#' Use definition function and variables timeseries to compute indicator.
#'
#' @param ts_vars   A dataframe of variable timeseries to compute indicator.
#' @param ind_def_fun   A function of defining indicator.
#' @param ...       Params to ind_def_fun.
#' @param date_index_field  Name of date index field of ts_vars, default 'date'.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date.
#' @param parallel   A logic to deterimine whether to use parallel processing.
#'
#'
#' @family indicator build functions
#'
#' @return A dataframe of indicators if succeed, otherwise NULL.
#'
#' @export
#'
#' @examples
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
    ts_indicator <- NULL
    tryCatch({
      ts_indicator <- ind_def_fun(ts_vars,
        date_index_field = date_index_field,
        key_fields = key_fields,
        ...
      )
    },
      error = function(cnd) {

        # inform user of failure and return NULL
        key_id = ""
        if (!is.null(key_fields) & NROW(ts_vars) > 0) {
          key_id <- paste0(ts_vars[1, key_fields], collapse = "-")
        }
        msg <- sprintf("Fail to compute indictor for %s:\n  %s",
                       key_id,
                       cnd$message)
        rlang::inform(msg)

        ts_indicator <- NULL
      }
    )
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
