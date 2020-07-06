# Utility functions - timeseries


# Generic functions for timeseries operation------------------------------------

#' Frequency Conversion Function of Resamping timeseries
#'
#' Generic function to convert timeseries to specified frequency by resampling
#' date index
#'
#' Convert timeseries to specified frequency by resampling date index.
#' Optionally provide filling method to pad/backfill missing values. Return
#' aggregating data conformed to a new index with the specified frequency.
#'
#' ts_resample is more appropriate if an operation, such as summarization, is
#' necessary to represent the data at the new frequency.
#'
#' @inheritParams ts_asfreq
#' @param agg_fun    Function to aggregate values of group data for new timestamp,
#'  default setting is mean.
#' @param ...        Arguments passed to agg_fun.
#'
#' @family utils_timeseries
#'
#' @return           A converted timeseres.
#' @export
# S3 generic definition
ts_resample <- function(ts_dataset, freq_rule = c("day", "month", "quarter", "year"),
                        fillna_method = c("nfill", "ffill", "bfill"),
                        agg_fun = mean,
                        ...) {
  UseMethod("ts_resample")
}

# S4 generic definition
# setGeneric(name = "ts_resample",
#            signature = c("ts_dataset"),
#            def = ts_resample <- function (ts_dataset,
#                                          freq_rule = c("day", "month", "quarter"),
#                                          fillna_method = c("nfill", "ffill", "bfill"),
#                                          agg_fun = mean,
#                                          ...) {
#              standardGeneric("ts_resample")
#            })



#' Frequency Conversion Function of Setting Frequency of timeseries
#'
#' Generic function tp convert timeseries at specified frequency by setting new frequency of data index
#'
#' Convert timeseries to specified frequency by refrequencing date index.
#' Optionally provide filling method to pad/backfill missing values.
#' Return original data conformed to a new index with the specified frequency.
#'
#' ts_asfreq is more appropriate if use original the data at the new frequency.
#'
#' @param ts_dataset   A timeseries of tibble/timeSeries.
#' @param freq_rule    A offset string or object representing target conversion,
#'  e.g. "day", "month", "quarter", "year", default "day".
#' @param fillna_method  A method to fill NAs in reindexed Series, e.g.
#'  "nfill", "ffill", "bfill" . Default "nfill" don't fill NAs; "ffill" means to
#'  use data before NAs to fill NAs; "bfill" means to use data after NAs to fill
#'  NAs.
#' @param ... Extra arguments to be passed to methods.
#'
#' @family utils_timeseries
#'
#' @return            A converted timeseres
#'
#' @export
# S3 generic definition
ts_asfreq <- function(ts_dataset,
                      freq_rule = c("day", "month", "quarter", "year"),
                      fillna_method = c("nfill", "ffill", "bfill"),
                      ...) {
  UseMethod("ts_asfreq")
}

#' Compute a lagged version of timeseries
#'
#' Generic function to shift data of timeseries(except date) forward and backward according current
#' timeline
#'
#' @details
#' There are two types of lag operation:
#' \itemize{
#'    \item shift forward: mostly known as lag, i.e. move data to next k periods,
#' which means we use earlier data as current data while keeping current timeline
#'    \item shift backward: mostly known as head, i.e. move previous k periods,
#' which means use later data as current data while keeping current timeline
#' }
#'
#'
#' @param ts_dataset       a timeseries of tibble/timeseries.
#' @param k                an integer value. The number of lags (in units of observations).
#' By default 1.
#' \itemize{
#'     \item k > 0: shift forward, using earlier data as current data
#'     \item k = 0: don't shift, keeping original data
#'     \item k < 0: shift backward, using later data as current data
#' }
#' see details for more info.
#'
#' @param trim    A logic flag of whether to remove the first missing observation in
#'   the return series. Default TRUE,
#' @param ...              Arguments passed to other methods.
#' @param date_index_field Name of date index field of ts_df for resample,
#'   default 'date', Column must be date-like.
#'   Only be used for tibble dataset.
#' @param key_fields    A character vector of key fields, which identify unique
#'   observation in each date. Only be used for tibble dataset.
#'
#' @family utils_timeseries
#'
#' @return            A lagged timeseres
#'
#' @export
# S3 generic definition
ts_lag <- function(ts_dataset,
                   k = 1,
                   trim = TRUE,
                   ...) {
  UseMethod("ts_lag")
}



# Generic function implemetaion by tibble class -------------------------------

# Convert timeseries to specified frequency by resampling for tibble dataset
#' @param date_index_field Name of date index field of ts_df for resample,
#'  default 'date', Column must be date-like.
#'  Only be used for tibble dataset.
#' @param key_fields    A character vector of key fields, which identify unique
#'  observation in each date. Only be used for tibble dataset.
#' @param parallel   A logic to determine whether to use parallel processing.
#'  Only be used for tibble dataset.
#' @describeIn ts_resample Resamping timeseries of tibble dataset
#' @export
# Method definition for s3 generic
ts_resample.tbl_df <- function(ts_dataset,
                               freq_rule = c("day", "month", "quarter", "year"),
                               fillna_method = c("nfill", "ffill", "bfill"),
                               agg_fun = mean,
                               ...,
                               date_index_field = c("date"),
                               key_fields = NULL,
                               parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # define internal function to process single group dataset
  .ts_resample_single_df <- function(ts_dataset,
                                     freq_rule,
                                     fillna_method,
                                     agg_fun,
                                     ...,
                                     date_index_field,
                                     key_fields) {

    # validate params
    stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
    ts_df <- tibble::as_tibble(ts_dataset)

    success <- TRUE
    new_timeseries <- NULL

    origin_date_index <- ts_df[[date_index_field]]

    # judge whether to transform timeseries
    need_refreq <- need_refreq_dateindex(origin_date_index,
      freq_rule = freq_rule
    )
    if (!need_refreq) {
      new_timeseries <- ts_df
    }


    # build date_index with the new frequency
    if (need_refreq && success) {
      new_date_index <- refreq_dateindex(origin_date_index,
        freq_rule = freq_rule
      )
      if (is.null(new_date_index)) {
        success <- FALSE
      }
    }


    # use new date_index to reindex timeseries
    if (need_refreq && success) {
      if (length(unique(origin_date_index)) <= length(unique(new_date_index))) {
        # upsampling with Interpolation : from low frequency to high frequency
        new_timeseries <- reindex_by_replace.tbl_df(ts_df,
          date_index_field = date_index_field,
          new_date_index = new_date_index,
          fillna_method = fillna_method
        )
      } else {
        # downsampling with aggregation : from high frequency to low frequency
        new_timeseries <- reindex_by_regroup.tbl_df(ts_df,
          date_index_field = date_index_field,
          new_date_index = new_date_index,
          agg_fun = agg_fun,
          ...
        )
      }
    }

    # Fix key value since some NAs may exist in key caused by above process
    if (!is.null(key_fields) && !(is.null(new_timeseries))) {
      new_timeseries <- fix_key_field(new_timeseries, key_fields)
    }

    return(new_timeseries)
  }

  # -- Main function --

  # work for single/multi group dataset
  if (is.null(key_fields)) {
    # for single group process
    result_ts <- .ts_resample_single_df(ts_dataset,
      freq_rule = freq_rule,
      fillna_method = fillna_method,
      agg_fun = agg_fun,
      ...,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {
    # for multi groups process
    progress_display <- if (exists("winProgressBar")) {
      plyr::progress_win(title = "Resampling...")
    } else {
      plyr::progress_text()
    }
    suppress_warnings(
      {
        result_ts <- plyr::ddply(
          ts_dataset,
          .variables = key_fields,
          .fun = .ts_resample_single_df,
          freq_rule = freq_rule,
          fillna_method = fillna_method,
          agg_fun = agg_fun,
          ...,
          date_index_field = date_index_field,
          key_fields = key_fields,
          .parallel = parallel,
          .progress = progress_display
        )
      },
      # suppress warnings due to parallel process
      warn_pattern = "<anonymous>: ..."
    )
  }

  result_ts <- tibble::as_tibble(result_ts)

  return(result_ts)
}

# Method definition for s4 generic
# setMethod("ts_resample",
#           signature(ts_dataset = "tbl_df"),
#           function(ts_dataset, ...) {
#             ts_resample.tbl_df(ts_dataset, ...)
#           })

# Convert timeseries to specified frequency by refrequencying for tibble dataset
#' @param date_index_field Name of date index field of ts_df for refrequecy,
#'  default 'date', Column must be date-like. Only be used for tibble dataset.
#' @param key_fields    A character vector of key fields, which identify unique
#'  observation in each date. Only be used for tibble dataset.
#' @param parallel   A logic to determine whether to use parallel processing.
#'  Only be used for tibble dataset.
#' @describeIn ts_asfreq Set new frequency for timeseries of tibble dataset
#' @export
ts_asfreq.tbl_df <- function(ts_dataset,
                             freq_rule = c("day", "month", "quarter", "year"),
                             fillna_method = c("nfill", "ffill", "bfill"),
                             ...,
                             date_index_field = c("date"),
                             key_fields = NULL,
                             parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # define internal function to process single group dataset
  .ts_asfreq_single_df <- function(ts_dataset,
                                   freq_rule,
                                   fillna_method,
                                   date_index_field,
                                   key_fields) {

    # validate params
    stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
    ts_df <- tibble::as_tibble(ts_dataset)

    success <- TRUE
    new_timeseries <- NULL

    origin_date_index <- ts_df[[date_index_field]]

    # judge whether to transform timeseries
    need_refreq <- need_refreq_dateindex(origin_date_index,
      freq_rule = freq_rule
    )
    if (!need_refreq) {
      new_timeseries <- ts_df
    }

    # build date_index with the new frequency
    if (need_refreq && success) {
      new_date_index <- refreq_dateindex(origin_date_index,
        freq_rule = freq_rule
      )
      if (is.null(new_date_index)) {
        success <- FALSE
      }
    }


    # use new date_index to reindex timeseries
    if (need_refreq && success) {
      new_timeseries <- reindex_by_replace.tbl_df(ts_df,
        new_date_index = new_date_index,
        date_index_field = date_index_field,
        fillna_method = fillna_method
      )
    }

    # Fix key value since some NAs may exist in key caused by above process
    if (!is.null(key_fields) && !(is.null(new_timeseries))) {
      new_timeseries <- fix_key_field(new_timeseries, key_fields)
    }

    return(new_timeseries)
  }

  # -- Main function --

  # work for single/multi group dataset
  if (is.null(key_fields)) {
    # for single group
    result_ts <- .ts_asfreq_single_df(ts_dataset,
      freq_rule = freq_rule,
      fillna_method = fillna_method,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {
    # for multi groups
    progress_display <- if (exists("winProgressBar")) {
      plyr::progress_win(title = "Refreqencing...")
    } else {
      plyr::progress_text()
    }
    suppress_warnings(
      {
        result_ts <- plyr::ddply(
          ts_dataset,
          .variables = key_fields,
          .fun = .ts_asfreq_single_df,
          freq_rule = freq_rule,
          fillna_method = fillna_method,
          date_index_field = date_index_field,
          key_fields = key_fields,
          .parallel = parallel,
          .progress = progress_display
        )
      },
      # suppress warnings due to parallel process
      warn_pattern = "<anonymous>: ..."
    )
  }

  result_ts <- tibble::as_tibble(result_ts)

  return(result_ts)
}

# Compute a lagged version of timeseries for tibble
#' @param parallel   A logic to determine whether to use parallel processing.
#'   default TRUE means to use parallel processing.
#'
#' @describeIn ts_lag  Compute a lagged version of timeseries for tibble dataset
#' @export
ts_lag.tbl_df <- function(ts_dataset,
                          k = 1,
                          trim = TRUE,
                          ...,
                          date_index_field = c("date"),
                          key_fields = NULL,
                          parallel = getOption("zstmodelr.common.parallel", TRUE)) {

  # compute lag timeseries for single group dataset
  .ts_lag_single_df <- function(ts_dataset,
                                k,
                                trim,
                                ...,
                                date_index_field,
                                key_fields) {

    # validate params
    stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))

    ts_df <- tibble::as_tibble(ts_dataset)
    origin_group_vars <- dplyr::group_vars(ts_df)

    # Shift data at current timeline
    if (k > 0) {
      # shift backward
      lag_ts <- ts_df %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!rlang::parse_expr(date_index_field)) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(-c(!!date_index_field, !!key_fields)),
          .fun = dplyr::lag,
          n = k,
          order_by = rlang::parse_expr(date_index_field)
        )
    } else if (k < 0) {
      # shift forward
      lag_ts <- ts_df %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!rlang::parse_expr(date_index_field)) %>%
        dplyr::mutate_at(
          .vars = dplyr::vars(-c(!!date_index_field, !!key_fields)),
          .fun = dplyr::lead,
          n = abs(k),
          order_by = rlang::parse_expr(date_index_field)
        )
    } else {
      # don't shift
      lag_ts <- ts_df
    }

    # trim NA rows from shifting
    if (trim) {
      ts_length <- nrow(lag_ts)
      if (abs(k) < ts_length) {
        if (k >= 0) {
          lag_ts <- lag_ts[(k + 1):ts_length, ]
        } else {
          lag_ts <- lag_ts[1:(ts_length + k), ]
        }
      } else {
        lag_ts <- lag_ts[0, ]
      }
    }

    # restore group info
    if (length(origin_group_vars) != 0) {
      origin_group_vars <- rlang::parse_expr(origin_group_vars)
      lag_ts <- lag_ts %>%
        dplyr::group_by(!!origin_group_vars)
    }

    # Fix key value since some NAs may exist in key caused by above process
    if (!is.null(key_fields) && !(is.null(lag_ts))) {
      lag_ts <- fix_key_field(lag_ts, key_fields)
    }

    return(lag_ts)
  }

  # -- Main function --

  # work for single/multi group dataset
  if (is.null(key_fields)) {
    # for single group
    result_ts <- .ts_lag_single_df(ts_dataset,
      k = k,
      trim = trim,
      date_index_field = date_index_field,
      key_fields = key_fields
    )
  } else {
    # for multi groups
    progress_display <- if (exists("winProgressBar")) {
      plyr::progress_win(title = "Lagging...")
    } else {
      plyr::progress_text()
    }
    suppress_warnings(
      {
        result_ts <- plyr::ddply(
          ts_dataset,
          .variables = key_fields,
          .fun = .ts_lag_single_df,
          k = k,
          trim = trim,
          date_index_field = date_index_field,
          key_fields = key_fields,
          .parallel = parallel,
          .progress = progress_display
        )
      },
      # suppress warnings due to parallel process
      warn_pattern = "<anonymous>: ..."
    )
  }

  result_ts <- tibble::as_tibble(result_ts)

  return(result_ts)
}





# Generic function implemetaion by timeSeries class -------------------------


# Convert timeSeries to specified frequency by resampling for timeSeries dataset
#' @describeIn ts_resample Resamping timeSeries of timeSeries dataset
#' @export
ts_resample.timeSeries <- function(ts_dataset,
                                   freq_rule = c("day", "month", "quarter"),
                                   fillna_method = c("nfill", "ffill", "bfill"),
                                   agg_fun = c("mean", "sum"), ...) {

  # validate params
  stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "timeSeries"))

  success <- TRUE
  new_timeseries <- NULL

  origin_date_index <- lubridate::as_date(time(ts_dataset))

  # judge whether to transform timeseries
  need_refreq <- need_refreq_dateindex(origin_date_index,
    freq_rule = freq_rule
  )
  if (!need_refreq) {
    new_timeseries <- ts_dataset
  }

  # build date_index with the new frequency
  if (need_refreq && success) {
    new_date_index <- refreq_dateindex(origin_date_index,
      freq_rule = freq_rule
    )
    if (is.null(new_date_index)) {
      success <- FALSE
    }
  }

  # use new date_index to reindex timeseries
  if (need_refreq && success) {
    if (length(unique(origin_date_index)) <= length(unique(new_date_index))) {
      # upsampling with Interpolation : from low frequency to high frequency
      new_timeseries <- reindex_by_replace.timeSeries(ts_dataset,
        new_date_index = new_date_index,
        fillna_method = fillna_method
      )
    } else {
      # downsampling with aggregation : from high frequency to low frequency
      new_timeseries <- reindex_by_regroup.timeSeries(ts_dataset,
        new_date_index = new_date_index,
        agg_fun = agg_fun, ...
      )
    }
  }

  return(new_timeseries)
}

# Convert timeSeries to specified frequency by refrequencying for timeSeries dataset
#' @describeIn ts_asfreq Set new frequency for timeSeries of timeSeries dataset
#' @export
ts_asfreq.timeSeries <- function(ts_dataset,
                                 freq_rule = c("day", "month", "quarter"),
                                 fillna_method = c("nfill", "ffill", "bfill"),
                                 ...) {
  # validate params
  stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "timeSeries"))

  success <- TRUE
  new_timeseries <- NULL

  origin_date_index <- lubridate::as_date(time(ts_dataset))

  # judge whether to transform timeseries
  need_refreq <- need_refreq_dateindex(origin_date_index,
    freq_rule = freq_rule
  )
  if (!need_refreq) {
    new_timeseries <- ts_dataset
  }

  # build date_index with the new frequency
  if (need_refreq && success) {
    new_date_index <- refreq_dateindex(origin_date_index,
      freq_rule = freq_rule
    )
    if (is.null(new_date_index)) {
      success <- FALSE
    }
  }

  # use new date_index to reindex timeseries
  if (need_refreq && success) {
    new_timeseries <- reindex_by_replace.timeSeries(ts_dataset,
      new_date_index = new_date_index,
      fillna_method = fillna_method
    )
  }

  return(new_timeseries)
}


# Compute a lagged version of timeSeries for timeSeries
#' @describeIn ts_lag  Compute a lagged version of timeSeries for timeSeries dataset
#' @export
ts_lag.timeSeries <- function(ts_dataset,
                              k = 1,
                              trim = TRUE,
                              ...) {

  # validate params
  stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "timeSeries"))

  # if (abs(k) > nrow(ts_dataset)) {
  #   msg <- sprintf("Absolute shift offset(%d) mustn't be longer than length of dataset(%d)",
  #                  abs(k), nrow(ts_dataset))
  #   stop(msg)
  # }

  origin_colnames <- timeSeries::colnames(ts_dataset)

  # compute lag timeseries
  if (abs(k) < nrow(ts_dataset)) {
    # use normal lag operation of timeseries
    lag_ts <- timeSeries::lag(ts_dataset, k = k, trim = trim, ...)
  } else {
    # when abs(k) is larger than length of ts, produce result according trim
    if (trim != TRUE) {
      # set data as NA but keep date, because timeSeries::lag can't deal with the cases
      lag_ts <- ts_dataset
      lag_ts[, ] <- NA
    } else {
      # return a null timeseries because all data are NA
      lag_ts <- ts_dataset[0, ]
    }
  }

  timeSeries::colnames(lag_ts) <- origin_colnames

  return(lag_ts)
}

# Internal tools functions --------------------------------

# Reindex the timeseries by replacing with new date_index for tibble dataset
reindex_by_replace.tbl_df <- function(ts_df,
                                      date_index_field = c("date"),
                                      new_date_index,
                                      fillna_method = c("nfill", "ffill", "bfill")) {

  # validate params
  stopifnot(!is.null(ts_df), inherits(ts_df, "data.frame"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))

  # blend new index with timeseries
  new_date_index.tib <- tibble::as_tibble(new_date_index)
  colnames(new_date_index.tib) <- date_index_field
  ts_new_df <- new_date_index.tib %>%
    dplyr::full_join(ts_df, by = date_index_field)

  # fill NAs
  fillna_method <- match.arg(fillna_method)
  switch(fillna_method,
    nfill = {
      ts_result <- ts_new_df
    },
    ffill = {
      ts_result <- tidyr::fill(ts_new_df, dplyr::everything(), .direction = "down")
    },
    bfill = {
      ts_result <- tidyr::fill(ts_new_df, dplyr::everything(), .direction = "up")
    }
  )

  # reindex timeseries by replacing with new index
  ts_result <- new_date_index.tib %>%
    dplyr::left_join(ts_result, by = date_index_field)

  return(ts_result)
}

# Reindex the timeseries by gouping into new date_index for tibble dataset
reindex_by_regroup.tbl_df <- function(ts_df,
                                      date_index_field = c("date"),
                                      new_date_index,
                                      agg_fun = mean,
                                      ...) {
  # validate params
  stopifnot(!is.null(ts_df), inherits(ts_df, "data.frame"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))
  stopifnot(!is.null(agg_fun))

  # Combine group index with timeseries
  ts_new_df <- ts_df %>%
    dplyr::mutate(group_index = lubridate::as_date(cut(date,
      breaks = lubridate::as_date(c(0, new_date_index)),
      labels = new_date_index,
      right = TRUE
    )))

  # reindex timeseries by grouping into new index

  # aggregating number fields by agg_fun for each group
  ts_result_numbers <- ts_new_df %>%
    dplyr::group_by(group_index) %>%
    dplyr::summarise_if(~ inherits(., "numeric"), agg_fun, ...)

  # aggregaing non-number fields by using value of first observatio of each group
  ts_result_non_numbers <- ts_new_df %>%
    dplyr::group_by(group_index) %>%
    dplyr::summarise_if(~ !inherits(., "numeric"), dplyr::first)

  # combine non_number and number fields
  ts_result <- ts_result_non_numbers %>%
    dplyr::left_join(ts_result_numbers, by = "group_index") %>%
    dplyr::select(-!!rlang::parse_quo(date_index_field,
      env = rlang::caller_env()
    )) %>%
    dplyr::select(!!date_index_field := group_index, dplyr::everything())

  return(ts_result)
}

# Reindex the timeseries by replacing with new date_index for timeSeries dataset
reindex_by_replace.timeSeries <- function(ts_timeseries,
                                          new_date_index,
                                          fillna_method = c("nfill", "ffill", "bfill")) {

  # validate params
  stopifnot(!is.null(ts_timeseries), inherits(ts_timeseries, "timeSeries"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))

  # Expand timeseries into daily series
  fillna_method <- match.arg(fillna_method)
  timeseries_method <- switch(fillna_method,
    nfill = "fillNA",
    ffill = "before",
    bfill = "after"
  )
  ts_align_daily <- timeSeries::alignDailySeries(ts_timeseries,
    method = timeseries_method,
    include.weekends = TRUE
  )

  # filter daily series according new_date_index
  filter_index <- (lubridate::as_date(timeSeries::time(ts_align_daily))) %in% new_date_index
  ts_result <- ts_align_daily[filter_index, ]

  return(ts_result)
}

# Reindex the timeseries by gouping into new date_index for timeSeries dataset
reindex_by_regroup.timeSeries <- function(ts_timeseries,
                                          new_date_index,
                                          agg_fun = mean,
                                          ...) {
  # validate params
  stopifnot(!is.null(ts_timeseries), inherits(ts_timeseries, "timeSeries"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))
  stopifnot(!is.null(agg_fun))


  # Aggregate time series according new_date_index
  ts_result <- timeSeries::aggregate(ts_timeseries,
    by = timeDate::as.timeDate(new_date_index),
    FUN = agg_fun, ...
  )

  return(ts_result)
}


# Judge whether to refreq dateindex to avoid unnecessary transform
need_refreq_dateindex <- function(date_index,
                                  freq_rule = c(
                                    "day", "month",
                                    "quarter", "year"
                                  )) {

  # validate params
  stopifnot(!is.null(date_index), lubridate::is.Date(date_index))

  # judge whether need to change frequency of date index
  need_refreq <- TRUE
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
    "day" = {
      if (is_periodic_dates(date_index, freq_rule = "day", regular = TRUE)) {
        need_refreq <- FALSE
      } else {
        need_refreq <- TRUE
      }
    },
    "month" = {
      if (is_periodic_dates(date_index, freq_rule = "month", regular = TRUE)) {
        need_refreq <- FALSE
      } else {
        need_refreq <- TRUE
      }
    },
    "quarter" = {
      if (is_periodic_dates(date_index, freq_rule = "quarter", regular = TRUE)) {
        need_refreq <- FALSE
      } else {
        need_refreq <- TRUE
      }
    },
    "year" = {
      if (is_periodic_dates(date_index, freq_rule = "year", regular = TRUE)) {
        need_refreq <- FALSE
      } else {
        need_refreq <- TRUE
      }
    }
  )

  return(need_refreq)
}

# Set new Frequency of date-like index
refreq_dateindex <- function(date_index,
                             freq_rule = c("day", "month", "quarter", "year")) {

  # validate params
  stopifnot(!is.null(date_index), lubridate::is.Date(date_index))

  origin_date_index <- timeDate::as.timeDate(date_index)
  origin_date_index <- sort(origin_date_index)

  # change frequency of date index
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
    "day" = {
      new_date_index <- origin_date_index %>%
        timeDate::alignDaily(include.weekends = TRUE)
    },
    "month" = {
      new_date_index <- origin_date_index %>%
        timeDate::alignDaily(include.weekends = TRUE) %>%
        timeDate::alignMonthly(include.weekends = TRUE)
    },
    "quarter" = {
      new_date_index <- origin_date_index %>%
        timeDate::alignDaily(include.weekends = TRUE) %>%
        timeDate::alignQuarterly(include.weekends = TRUE)
    },
    "year" = {

      # normalize into dialy dates
      daily_dates <- timeDate::alignDaily(origin_date_index,
        include.weekends = TRUE
      )
      # set daily date as last day of year
      new_date_index <- as.Date(daily_dates)
      new_date_index <- lubridate::ceiling_date(new_date_index,
        unit = "year"
      ) - 1
      new_date_index <- timeDate::as.timeDate(new_date_index)
    }
  )

  # build new date index
  new_date_index <- new_date_index %>%
    lubridate::as_date() %>%
    unique() %>%
    sort()

  return(new_date_index)
}

# fix key fields of tibble timeseries
fix_key_field <- function(ts_dataset, key_fields) {
  # Validate params
  stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
  ts_df <- tibble::as_tibble(ts_dataset)

  # check key filed is valid field of dataset
  stopifnot(!is.null(key_fields), is.character(key_fields))
  is_valid_field <- key_fields %in% names(ts_df)
  if (!all(is_valid_field)) {
    msg <- sprintf(
      "%s: not valid field of %s",
      stringr::str_c(key_fields[!is_valid_field],
        collapse = ","
      ),
      deparse(substitute(ts_dataset))
    )
    stop(msg)
  }

  # replace NA is key fields
  fix_df <- ts_df %>%
    tidyr::fill(key_fields, .direction = "down") %>%
    tidyr::fill(key_fields, .direction = "up")

  return(fix_df)
}
