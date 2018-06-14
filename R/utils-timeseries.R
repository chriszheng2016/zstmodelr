# Generic functions for timeseries operation------------------------------------

#' Utility Frequency Conversion Method of Resamping Timeseries
#'
#' Convert TimeSeries to specified frequency by resamploing date index
#'
#' Convert timeseries to specified frequency by resampling date index.
#' Optionally provide filling method to pad/backfill missing values.
#' Return aggregating data conformed to a new index with the specified frequency.
#'
#' resample is more appropriate if an operation, such as summarization, is
#' necessary to represent the data at the new frequency.
#'
#' @param ts_dataset a timeseries of tibble or timeSeries.
#'
#' @param freq_rule  the offset string or object representing target conversion,
#'  e.g. "day", "month", "quarter", default "Day".
#' @param fillna_method method to fill holes in reindexed Series, e.g.
#' "nfill", "bfill","ffill", default nafill(fill NA)
#' @param agg_fun    function to aggregate values of group data for new timestamp,
#' default setting is mean
#' @param ...        argments passed to agg_fun
#'
#' @param date_index_field the name of date index field of ts_df for resample,
#' default 'date', Column must be date-like.
#' Only be used for tibble dataset.
#' @param by_group   a character vector of fields as group data for resampling.
#' Only be used for tibble dataset.
#'
#' @return           a converted timeseres.
#' @export
#'
#' @examples
# S3 generic definition
ts_resample <- function(ts_dataset,freq_rule = c("day", "month", "quarter"),
                        fillna_method = c("nfill", "ffill", "bfill"),
                        agg_fun = mean,
                        ...){
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



#' Utility Frequency Conversion Method of Setting Frequncey of Timeseries
#'
#' Convert timeseries at specified frequency by setting new frequency of data index
#'
#' Convert timeseries to specified frequency by refrequencing date index.
#' Optionally provide filling method to pad/backfill missing values.
#' Return originaldata conformed to a new index with the specified frequency.
#'
#' ts_asfreq is more appropriate if use original the data at the new frequency.
#'
#' @param ts_dataset   a timeseries of tibble/timeSeries.#
#' @param freq_rule,   the offset string or object representing target conversion,
#' e.g. "Day", "Month", "Quarter", default "Day".
#' @param fillna_method, method to fill holes in reindexed Series, e.g.
#' "nfill", "bfill","ffill", default nfill(fiil with NA).
#' @param date_index_field the name of date index field of ts_df for resample,
#' default 'date', Column must be date-like.
#' Only be used for tibble dataset.
#' @param by_group    a character vector of fields as group data for asfreq.
#' Only be used for tibble dataset.
#'
#' @return            a converted timeseres
#' @export
#'
#' @examples
# S3 generic definition
ts_asfreq <- function(ts_dataset,
                      freq_rule =c("day", "month", "quarter"),
                      fillna_method = c("nfill", "ffill", "bfill"),
                      ...) {
  UseMethod("ts_asfreq")
}

#' Compute a lagged version of Timeseries
#'
#' Shift data of timeseries(except date) forward and backward according current
#' timeline
#'
#' @details
#' There are two types of lag operation:
#' \itemize{
#'    \item shift forward: mostly known as lag, i.e. move data to next k periods,
#' which means we use ealier data as current data while keeping current timeline
#'    \item shfit backward: mostly known as head, i.e. move previous k periods,
#' which means use later data as current data while keeping current timeline
#'}
#'
#'
#' @param ts_dataset       a timeseries of tibble/timeSeries.
#' @param k                an integer value. The number of lags (in units of observations).
#' By default 1.
#' \itemize{
#'     \item k > 0: shfit forward, using earlier data as current data
#'     \item k = 0: don't shift, keeping original data
#'     \item k < 0: shift backward, using later data as current data
#' }
#' see details for more info.
#'
#' @param trim             a logical value. By default TRUE, the first missing observation in the return series
#' will be removed.
#' @param ...              argments passed to other methods
#' @param date_index_field the name of date index field of ts_df for resample,
#' default 'date', Column must be date-like.
#' Only be used for tibble dataset.
#' @param by_group         a character vector of fields as group data for asfreq.
#' Only be used for tibble dataset.
#'
#' @return            a lagged timeseres
#' @export
#'
#' @examples
#S3 generic definition
ts_lag <- function(ts_dataset,
                   k = 1,
                   trim = TRUE,
                   ...
                   ){
  UseMethod("ts_lag")
}



# Generic function implemetaion by tibble class -------------------------------

# Convert TimeSeries to specified frequency by resampling for tibble dataset
#' @describeIn ts_resample Resamping timeseries of tibble dataset
#' @export
# Method definition for s3 generic
ts_resample.tbl_df <- function(ts_dataset,
                              freq_rule = c("day", "month", "quarter"),
                              fillna_method = c("nfill", "ffill", "bfill"),
                              agg_fun = mean,
                              ...,
                              date_index_field = c("date"),
                              by_group = NULL) {

    # define internal function to process single group dataset
    .ts_resample_single_df <- function(ts_dataset,
                                 freq_rule = c("day", "month", "quarter"),
                                 fillna_method = c("nfill", "ffill", "bfill"),
                                 agg_fun = mean,
                                 ...,
                                 date_index_field = c("date")
                                 ) {

      # validate params
      stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
      ts_df <- tibble::as.tibble(ts_dataset)

      success <- TRUE
      new_timeseries <- NULL

      origin_date_index <- ts_df[[date_index_field]]

      # judge whether to transform timeseries
      need_refreq <- need_refreq_dateindex(origin_date_index,
                            freq_rule = freq_rule)
      if (!need_refreq) {
        new_timeseries <- ts_df
      }


      # build date_index with the new frequency
      if (need_refreq && success) {

        new_date_index <- refreq_dateindex(origin_date_index,
                                          freq_rule = freq_rule)
        if (is.null(new_date_index)) {
          success <- FALSE
        }
      }


      # use new date_index to reindex timesereis
      if (need_refreq && success) {

        if (length(unique(origin_date_index)) <= length(unique(new_date_index))) {
          # upsampling with Interpolation : from low frequency to high frequency
          new_timeseries <- reindex_by_replace.tbl_df(ts_df,
                                               date_index_field = date_index_field,
                                               new_date_index = new_date_index,
                                               fillna_method = fillna_method)
        } else {
          # downsampling with aggregation : from high frequency to low frequency
          new_timeseries <- reindex_by_regroup.tbl_df(ts_df,
                                               date_index_field = date_index_field,
                                               new_date_index = new_date_index,
                                               agg_fun = agg_fun,
                                               ...)
        }

      }

      return(new_timeseries)

    }

    # work for single/multi group dataset
    if (is.null(by_group)) {

      # for single group process
      result_ts <- .ts_resample_single_df(ts_dataset,
                                    freq_rule = freq_rule,
                                    fillna_method = fillna_method,
                                    agg_fun = agg_fun,
                                    ...,
                                    date_index_field = date_index_field)

    } else {

      # for multi groups process

      result_ts <- plyr::ddply(ts_dataset,
                               .variables =  by_group,
                               .fun = .ts_resample_single_df,
                               freq_rule = freq_rule,
                               fillna_method = fillna_method,
                               agg_fun = agg_fun,
                               ...,
                               date_index_field = date_index_field,
                               .parallel = TRUE,
                               .progress = plyr::progress_win(title = "Resampling..."))
    }

    result_ts <- tibble::as.tibble(result_ts)

    return(result_ts)

  }

# Method definition for s4 generic
# setMethod("ts_resample",
#           signature(ts_dataset = "tbl_df"),
#           function(ts_dataset, ...) {
#             ts_resample.tbl_df(ts_dataset, ...)
#           })

# Convert TimeSeries to specified frequency by refrequencying for tibble dataset
#' @describeIn ts_asfreq Set new frequency for timeseries of tibble dataset
#' @export
ts_asfreq.tbl_df <- function(ts_dataset,
                          freq_rule =c("day", "month", "quarter"),
                          fillna_method = c("nfill", "ffill", "bfill"),
                          date_index_field = c("date"),
                          by_group = NULL){

  # define internal function to process single group dataset
  .ts_asfreq_single_df <- function(ts_dataset,
                             freq_rule =c("day", "month", "quarter"),
                             fillna_method = c("nfill", "ffill", "bfill"),
                             date_index_field = c("date")){

    # validate params
    stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
    ts_df <- tibble::as.tibble(ts_dataset)

    success <- TRUE
    new_timeseries <- NULL

    origin_date_index <- ts_df[[date_index_field]]

    # judge whether to transform timeseries
    need_refreq <- need_refreq_dateindex(origin_date_index,
                          freq_rule = freq_rule)
    if (!need_refreq) {
      new_timeseries <- ts_df
    }

    # build date_index with the new frequency
    if (need_refreq && success) {
      new_date_index <- refreq_dateindex(origin_date_index,
                                         freq_rule = freq_rule)
      if (is.null(new_date_index)) {
        success <- FALSE
      }
    }


    # use new date_index to reindex timesereis
    if (need_refreq && success) {
      new_timeseries <- reindex_by_replace.tbl_df(ts_df,
                                           new_date_index = new_date_index,
                                           fillna_method = fillna_method)
    }

    return(new_timeseries)

  }

  # work for single/multi group dataset
  if (is.null(by_group)) {

    # for single group
    result_ts <- .ts_asfreq_single_df(ts_dataset,
                                freq_rule = freq_rule,
                                fillna_method = fillna_method,
                                date_index_field = date_index_field)

  } else {

    # for multi groups
    result_ts <- plyr::ddply(ts_dataset,
                             .variables =  by_group,
                             .fun = .ts_asfreq_single_df,
                             freq_rule = freq_rule,
                             fillna_method = fillna_method,
                             date_index_field = date_index_field,
                             .parallel = TRUE,
                             .progress = plyr::progress_win(title = "Refreqencing..."))
  }

  result_ts <- tibble::as.tibble(result_ts)

  return(result_ts)


}

# Compute a lagged version of timeseries for tibble
#' @describeIn ts_lag  Compute a lagged version of timeseries for tibble dataset
#' @export
ts_lag.tbl_df <- function(ts_dataset,
                          k = 1,
                          trim = TRUE,
                          ...,
                          date_index_field = c("date"),
                          by_group = NULL) {

  # compute lag timeseries for single group dataset
  .ts_lag_single_df <- function(ts_dataset,
                          k = 1,
                          trim = TRUE,
                          ...,
                          date_index_field = c("date")) {

    # validate params
    stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "data.frame"))
    ts_df <- tibble::as.tibble(ts_dataset)

    # Shift data at current timeline
    origin_group_vars <- dplyr::group_vars(ts_df)
    date_index_field <- rlang::parse_quosure(date_index_field)
    if (k > 0) {
      # shift backward
      lag_ts <- ts_df %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!date_index_field) %>%
        dplyr::mutate_at(.vars = dplyr::vars(-!!date_index_field),
                               .fun = dplyr::lag,
                               n = k,
                               order_by = rlang::quo_expr(date_index_field))


    } else if (k < 0) {
      # shift forward
      lag_ts <- ts_df %>%
        dplyr::ungroup() %>%
        dplyr::arrange(!!date_index_field) %>%
        dplyr::mutate_at(.vars = dplyr::vars(-!!date_index_field),
                         .fun = dplyr::lead,
                         n = abs(k),
                         order_by = rlang::quo_expr(date_index_field))

    } else {
      # don't shift
      lag_ts <- ts_df
    }

    # trim NA rows from shifting
    if (trim) {
      ts_length <- nrow(lag_ts)
      if (abs(k) < ts_length) {
        if ( k >= 0) {
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
      origin_group_vars <- rlang::parse_quosure(origin_group_vars)
      lag_ts <- lag_ts %>%
        dplyr::group_by(!!origin_group_vars)
    }



    return(lag_ts)

  }

  # work for single/multi group dataset
  if (is.null(by_group)) {

    # for single group
    result_ts <- .ts_lag_single_df(ts_dataset,
                                k = k,
                                trim = trim,
                                date_index_field = date_index_field)

  } else {

    # for multi groups
    result_ts <- plyr::ddply(ts_dataset,
                             .variables =  by_group,
                             .fun = .ts_lag_single_df,
                             k = k,
                             trim = trim,
                             date_index_field = date_index_field,
                             .parallel = TRUE,
                             .progress = plyr::progress_win(title = "Lagging..."))
  }

  result_ts <- tibble::as.tibble(result_ts)

  return(result_ts)

}


# Reindex the timesereis by replacing with new date_index for tibble dataset
reindex_by_replace.tbl_df <- function(ts_df,
                               date_index_field = c("date"),
                               new_date_index,
                               fillna_method = c("nfill", "ffill", "bfill")) {

  # validate params
  stopifnot(!is.null(ts_df), inherits(ts_df, "data.frame"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))

  # blend new index with timeseries
  new_date_index.tib <- tibble::as.tibble(new_date_index)
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
    dplyr::left_join(ts_result, by = "date")

  return(ts_result)

}

# Reindex the timesereis by gouping into new date_index for tibble dataset
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
                                    right = TRUE)))

  # reindex timeseries by grouping into new index

  # aggregating number fields by agg_fun for each group
  ts_result_numbers <- ts_new_df %>%
    dplyr::group_by(group_index) %>%
    dplyr::summarise_if(~inherits(., "numeric"), agg_fun, ... )

  #aggregaing non-number fields by using value of first observatio of each group
  ts_result_non_numbers <- ts_new_df %>%
    dplyr::group_by(group_index) %>%
    dplyr::summarise_if(~!inherits(., "numeric"), dplyr::first)

  #combine non_number and number fields
  ts_result <- ts_result_non_numbers %>%
    dplyr::left_join(ts_result_numbers, by = "group_index") %>%
    dplyr::select(-!!rlang::parse_quosure(date_index_field)) %>%
    dplyr::select(!!date_index_field := group_index, dplyr::everything())

  return(ts_result)

}






# Generic function implemetaion by timeSeries class -------------------------


# Convert TimeSeries to specified frequency by resampling for timeSeries dataset
#' @describeIn ts_resample Resamping timeseries of timeSeries dataset
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
                             freq_rule = freq_rule)
  if (!need_refreq) {
    new_timeseries <- ts_dataset
  }

  # build date_index with the new frequency
  if (need_refreq && success) {
    new_date_index <- refreq_dateindex(origin_date_index,
                                     freq_rule = freq_rule)
    if (is.null(new_date_index)) {
      success <- FALSE
    }
  }

  # use new date_index to reindex timesereis
  if (need_refreq && success) {

    if(length(unique(origin_date_index)) <= length(unique(new_date_index))) {
      # upsampling with Interpolation : from low frequency to high frequency
      new_timeseries <- reindex_by_replace.timeSeries(ts_dataset,
                                                  new_date_index = new_date_index,
                                                  fillna_method = fillna_method)
    } else {
      # downsampling with aggregation : from high frequency to low frequency
      new_timeseries <- reindex_by_regroup.timeSeries(ts_dataset,
                                                  new_date_index = new_date_index,
                                                  agg_fun = agg_fun, ...)
    }

  }

  return(new_timeseries)


}

# Convert TimeSeries to specified frequency by refrequencying for timeSeries dataset
#' @describeIn ts_asfreq Set new frequency for timeseries of timeSeries dataset
#' @export
ts_asfreq.timeSeries <- function(ts_dataset,
                             freq_rule =c("day", "month", "quarter"),
                             fillna_method = c("nfill", "ffill", "bfill")){
  # validate params
  stopifnot(!is.null(ts_dataset), inherits(ts_dataset, "timeSeries"))

  success <- TRUE
  new_timeseries <- NULL

  origin_date_index <- lubridate::as_date(time(ts_dataset))

  # judge whether to transform timeseries
  need_refreq <- need_refreq_dateindex(origin_date_index,
                                       freq_rule = freq_rule)
  if (!need_refreq) {
    new_timeseries <- ts_dataset
  }

  # build date_index with the new frequency
  if (need_refreq && success) {
    new_date_index <- refreq_dateindex(origin_date_index,
                                     freq_rule = freq_rule)
    if (is.null(new_date_index)) {
      success <- FALSE
    }
  }

  # use new date_index to reindex timesereis
  if (need_refreq && success) {
    new_timeseries <- reindex_by_replace.timeSeries(ts_dataset,
                                                new_date_index = new_date_index,
                                                fillna_method = fillna_method)
  }

  return(new_timeseries)
}


# Compute a lagged version of timeseries for timeSeries
#' @describeIn ts_lag  Compute a lagged version of timeseries for timeSeries dataset
#' @export
ts_lag.timeSeries <- function(ts_dataset,
                          k = 1,
                          trim = TRUE,
                          ... ) {

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
    # use normal lag operation of timeSeries
    lag_ts <- timeSeries::lag(ts_dataset, k = k, trim = trim, ...)
  } else {
    # when abs(k) is larger than length of ts, produce result according trim
    if (trim != TRUE) {
      # set data as NA but keep date, because timeSeries::lag can't deal with the cases
      lag_ts <- ts_dataset
      lag_ts[,] <- NA
    } else {
      # return a null timeSeries because all data are NA
      lag_ts <- ts_dataset[0,]
    }
  }

  timeSeries::colnames(lag_ts) <- origin_colnames

  return(lag_ts)
}

# Reindex the timesereis by replacing with new date_index for timeSeries dataset
reindex_by_replace.timeSeries <- function(ts_timeSeries,
                                      new_date_index,
                                      fillna_method = c("nfill", "ffill", "bfill")) {

  # validate params
  stopifnot(!is.null(ts_timeSeries), inherits(ts_timeSeries, "timeSeries"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))

  # Expand timeseries into daily series
  fillna_method <- match.arg(fillna_method)
  timeSeries_method <- switch(fillna_method,
         nfill = "fillNA",
         ffill = "before",
         bfill = "after"
  )
  ts_align_daily <- timeSeries::alignDailySeries(ts_timeSeries,
                                            method = timeSeries_method,
                                            include.weekends = TRUE )

  # filter daily sereis according new_date_index
  filter_index <- (lubridate::as_date(timeSeries::time(ts_align_daily)))  %in% new_date_index
  ts_result <- ts_align_daily[filter_index, ]

  return(ts_result)


}

# Reindex the timesereis by gouping into new date_index for timeSeries dataset
reindex_by_regroup.timeSeries <- function(ts_timeSeries,
                                      new_date_index,
                                      agg_fun = mean,
                                      ...) {
  # validate params
  stopifnot(!is.null(ts_timeSeries), inherits(ts_timeSeries, "timeSeries"))
  stopifnot(!is.null(new_date_index), lubridate::is.Date(new_date_index))
  stopifnot(!is.null(agg_fun))


  # Aggregate time sereis according new_date_index
   ts_result <- timeSeries::aggregate(ts_timeSeries,
                                     by = timeDate::as.timeDate(new_date_index),
                                     FUN = agg_fun, ...)

  return(ts_result)

}

# Internal tools functions --------------------------------

# Judge whether to refreq dateindex to avoid unnecessary transform
need_refreq_dateindex <- function(date_index,
                                  freq_rule =c("day", "month", "quarter")) {

  # validate params
  stopifnot(!is.null(date_index), lubridate::is.Date(date_index))

  date_index <- timeDate::as.timeDate(date_index)


  # judge whether need to change frequency of date index
  need_refreq <- TRUE
  freq_rule <- match.arg(freq_rule)
  switch(freq_rule,
         "day" = {
           if (timeDate::isDaily(date_index)) {
             need_refreq <- FALSE
           } else {
             # no need to refeq irregluar daily time series
             if (mean(lag(date_index) - date_index, na.rm = TRUE) <= 2) {
               need_refreq <- FALSE
             }
             need_refreq <- TRUE
           }
         },
         "month" = {
           if (timeDate::isMonthly(date_index)) {
             need_refreq <- FALSE
           } else {
             need_refreq <- TRUE
           }
         },
         "quarter" = {
           if (timeDate::isQuarterly(date_index)) {
             need_refreq <- FALSE
           } else {
             need_refreq <- TRUE
           }
         }
  )

  return(need_refreq)

}

# Set new frequncey of date-like index
refreq_dateindex <- function(date_index,
                             freq_rule =c("day", "month", "quarter")) {

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
         }
  )

  # build new date index
  new_date_index <- new_date_index %>%
    lubridate::as_date() %>%
    unique() %>%
    sort()

  return(new_date_index)

}


