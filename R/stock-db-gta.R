
# CONSTANT DEFINATION -----------------------------------------------------

.PACKAGE_NAME <- "zstmodelr"

# profile variable defination
# Since R devtools dosen't suport loading Chinese charaters in script file, we
# have to use reference name of varible in profile for refering Chinese table
# name in DB
.GTA_RPROFILE_DIR <- "etc"
.GTA_PROFILE_FILE <- "gta_profile.xlsx"


.GTA_TABLE_NAME_LIST = list(

  "gta_fieldname_list", # name for gta_fieldname_list

  "TRD_Co",        # name for TRD_Co_公司基本情况

  "TRD_DALYR",        # name for TRD_Dalyr_日个股回报率
  "TRD_WEEK",        # name for TRD_Week_周个股回报率
  "TRD_MNTH",        # name for TRD_Mnth_月个股回报率
  "TRD_YEAR",        # name for TRD_Year_年个股回报率

  "TRD_CNDALYM",     # name for TRD_Cndalym_综合市场日度回报
  "TRD_WEEKCM",      # name for TRD_Weekcm_综合市场周度回报
  "TRD_CNMONT",      # name for TRD_Cnmont_综合市场月度回报
  "TRD_YEARCM"       # name for TRD_Yearcm_综合市场年度回报
)

.GTA_XXXX <- "中文字符"  # not supported


# Interface Implementation of stock_db class by gta_db ------------------------

#' Creator of gta_db class
#'
#' Create a object for gta_db class
#'
#' @param dsn odbc dsn string
#' @return an object of gta_db class
#' @export
#'
#' @examples
#' gta_db <- gta_db()
gta_db <- function(dsn = "GTA_SQLData") {

  stopifnot(!is.null(dsn))

  # use envir for class member storage
  class_env <- new.env()
  class_env$dsn <- dsn
  class_env$connection <- NULL

  #create the class object
  structure(class_env, class = c("gta_db", "stock_db"))

}

# Open the stock database
#' @describeIn open_stock_db Open a database of gta_db class
#' @export
open_stock_db.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  con_stock_db <- tryCatch( RODBC::odbcConnect(dsn = stock_db$dsn),
                            error = function(e) e)
  if (inherits(con_stock_db, "error")) {
    msg <- conditionMessage(con_stock_db)
    success <- FALSE
  } else {
    msg <- sprintf("Connect data source of %s successfully", stock_db$dsn)
    stock_db$connection <- con_stock_db
    success <- TRUE
  }

  message(msg)

  return(invisible(success))

}

# Init param of stock db
#' @describeIn init_stock_db Init param of database of gta_db class
#' @export
init_stock_db.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  success <- TRUE

  # set up table name mapping for referece
  gta_profile_name <- system.file(.GTA_RPROFILE_DIR,
                                  .GTA_PROFILE_FILE, package = .PACKAGE_NAME )
  if (gta_profile_name == "") {
    msg = sprintf("No profile of % exisits in % for %",
                  .GTA_PROFILE_FILE,
                  .GTA_RPROFILE_DIR,
                  .PACKAGE_NAME)
    stop(msg)
  }

  # set up table name list
  stock_db$table_list <- list()
  for (i in seq_along(.GTA_TABLE_NAME_LIST)) {
    table_name_id <- .GTA_TABLE_NAME_LIST[[i]]
    table_name_value <- .get_db_profile_varible_setting(gta_profile_name, table_name_id)
    if (is.null(table_name_value)) {
      msg = sprintf("failed to load %s from %", table_name_id,
                    gta_profile_name)
      stop(msg)
      success = FALSE

    } else {
      stock_db$table_list[table_name_id] <- table_name_value
    }
  }

  # set up field_name list
  if (success) {
    stock_db$stock_field_list <- stock_field_list.gta_db(stock_db)
    if (is.null(stock_db$stock_field_list)) {
      warning("failed to set up field_name_list")
      success = FALSE
    }
  }

  # set up stock_name list
  if (success) {
    stock_db$stock_name_list <- stock_name_list.gta_db(stock_db)
    if (is.null(stock_db$stock_name_list)) {
      warning("failed to set up stock_name_list")
      success = FALSE
    }
  }

  # set up industry_name list
  if (success) {
    stock_db$industry_name_list <- industry_name_list.gta_db(stock_db)
    if (is.null(stock_db$industry_name_list)) {
      warning("failed to set up industry_name_list")
      success = FALSE
    }
  }

  return(invisible(success))

}

# Close the stock database
#' @describeIn close_stock_db Close a database of gta_db class
#' @export
close_stock_db.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  if (!is.null(stock_db$connection)) {
    success <- tryCatch(RODBC::odbcClose(stock_db$connection),
                        error = function(e) e)
    if (inherits(success, "error")) {

      # fail to close the connect
      msg <- sprintf("fail to close the connection of %s", stock_db$dsn)
      success <- FALSE

    } else {

      # close the connection succesfully
      msg <- sprintf("close the connection of %s successfully", stock_db$dsn)
      stock_db$connection <- NULL
      success <- TRUE

    }

    message(msg)
  }

  return(invisible(success))

}

# List all tables of stck_db
#' @describeIn list_stock_tables List names of tables in database of gta_db class
#' @export
list_stock_tables.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  db_tables <- RODBC::sqlTables(stock_db$connection, tableType = "TABLE")
  db_tables <- db_tables[db_tables$TABLE_SCHEM == "dbo", "TABLE_NAME" ]

  return(db_tables)
}

# Translate name into code for field or stock
#' @describeIn name2code Translate name into code in a database of gta_db class
#' @export
name2code.gta_db <- function(stock_db, name,
                             type=c("stock", "field", "industry")) {

    stopifnot(inherits(stock_db, "gta_db"), !missing(name))

    target_type <- match.arg(type)
    code = switch(target_type,
              field = name2code(stock_db$stock_field_list, name = name),
              stock = name2code(stock_db$stock_name_list, name = name),
              industry = name2code(stock_db$industry_name_list, name = name)

    )

    return(code)

}

# Translate code into name for field or stock
#' @describeIn code2name Translate code into name in a database of gta_db class
#' @export
code2name.gta_db <- function(stock_db, code,
                             type=c("stock", "field", "industry")) {

  stopifnot(inherits(stock_db, "gta_db"), !missing(code))

  target_type <- match.arg(type)
  name = switch(target_type,
                field = code2name(stock_db$stock_field_list, code = code),
                stock = code2name(stock_db$stock_name_list, code = code),
                industry = code2name(stock_db$industry_name_list, code = code)
  )

  return(name)
}


# Get a dataset from a table in stock_db
#' @describeIn get_table_dataset get a table dataset from a database of gta_db class
#' @export
get_table_dataset.gta_db <- function(stock_db, table_name ) {

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name) ) {
    stop("Table name must be character string")
  }

  # Get table data  from database
  ds_result <- tryCatch(RODBC::sqlFetch(stock_db$connection, table_name,
                                        stringsAsFactors = FALSE),
                         error = function(e) e)
  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf("get data from %s successfully", table_name)
    colnames(ds_result) <- tolower(colnames(ds_result))
  }

  message(msg)

  return(invisible(ds_result))
}

# Get a dataset of a list of stock_cd from table in stock
#' @describeIn get_stock_dataset get a dataset of a list of stock_cd from table
#' in a database of gta_db class
#' @export
get_stock_dataset.gta_db <- function(stock_db, table_name, stock_cd_list = NULL) {

  # Validate param
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name) ) {
    stop("Table name must be character string")
  }

  # if (missing(stock_cd_list) || !is.character(stock_cd_list) ) {
  #   stop("stock_cd_list must be character string")
  # }

  # Build sql command for data query
  stock_cd_list_str <- NULL
  if (!is.null(stock_cd_list)) {
    for (stock_cd in stock_cd_list) {

      if (is.null(stock_cd_list_str)) {
        stock_cd_list_str = sprintf("'%06d'", stock_cd)
      } else {
        stock_cd_list_str = paste(stock_cd_list_str, sprintf("'%06d'", stock_cd),
                                  sep = ",")
      }
    }
  }

  if (length(stock_cd_list_str) != 0 ) {
    sql_cmd <- sprintf("select * from %s where Stkcd in (%s)",
                       table_name, stock_cd_list_str)
  } else {
    sql_cmd <- sprintf("select * from %s", table_name)
  }

  # Select stock data from table
  ds_result <- tryCatch(RODBC::sqlQuery(stock_db$connection, sql_cmd,
                                        stringsAsFactors = FALSE),
                        error = function(e) e)
  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf("get stock data of(%s) from %s successfully", stock_cd_list_str,
                   table_name)
    colnames(ds_result) <- tolower(colnames(ds_result))
  }

  message(msg)

  return(invisible(ds_result))

}

# Fetch many datasets from stock_db
#' @describeIn fetch_table_dataset get many datasets from a database of gta_db class
#' @export
fetch_table_dataset.gta_db <- function(stock_db, table_list) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_list) || length(table_list) == 0 ) {
    stop("table_list must contain one table at least")
  }

  # get datasets from stock db
  result_table_list <- list(length(table_list))
  for (table_index in seq_along(table_list)) {

    the_table <- table_list[table_index]
    # get stock data for specified stock
    ds_result <- get_table_dataset.gta_db(stock_db,
                                          table_name = the_table)

    # keep the result_ts in GlobalEnv for debug check
    if (!is.null(ds_result)) {

      # get table successfully
      ds_name <- sprintf("ds_%s.df", the_table)
      #assign(ds_name, ds_result, pos = .GlobalEnv)
      assign(ds_name, ds_result, pos = parent.frame())
      result_table_list[table_index] <- ds_name
    } else {

      # fail to get table
      result_table_list[table_index] <- NULL

    }

  }

  return( result_table_list)

}

# Get stock return timeseries from stock_db
#' @describeIn get_stock_return get stock return timeseries from a database of gta_db class
#' @export
get_stock_return.gta_db <- function(stock_db, stock_cd_list = NULL,
                  period_type = c("daily", "weekly", "monthly", "annual"),
                  return_type = c("simple", "compound"),
                  use_stock_name = TRUE,
                  cumulated = FALSE,
                  output_type = c("timeSeries", "tibble")
                  ) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success = TRUE

  # get stock return dataset
  field_stkcd <- quo(stkcd)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    daily = {
      table_name   <- stock_db$table_list[["TRD_DALYR"]]
      field_date   <- quo(trddt)
      field_return <- quo(dretwd)
      date_format <- "ymd"
      date_ceiling_unit <-"day"
    },
    weekly  = {
      table_name <- stock_db$table_list[["TRD_WEEK"]]
      field_date   <- quo(trdwnt)
      field_return <- quo(wretwd)
      date_ceiling_unit <-"week"
    },
    monthly = {
      table_name <- stock_db$table_list[["TRD_MNTH"]]
      field_date   <- quo(trdmnt)
      field_return <- quo(mretwd)
      date_format <- "ymd"
      date_ceiling_unit <-"month"
    },
    annual  = {
      table_name <- stock_db$table_list[["TRD_YEAR"]]
      field_date   <- quo(trdynt)
      field_return <- quo(yretwd)
      date_format <- "y"
      date_ceiling_unit <-"year"
    }
  )

  #Warning: ds_return is simple return in database by default !!
  ds_return <- get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list)
  if (!is.null(ds_return)) {
    ds_return <- tibble::as.tibble(ds_return)
    msg <- sprintf("return table: %s , return field: %s, date field: %s",
                   table_name, rlang::quo_text(field_return), rlang::quo_text(field_date))
    message(msg)

  } else {
    success = FALSE
  }

  # Build simple return results
  ts_return <- NULL
  if (success) {

    ds_return <- ds_return %>%
      dplyr::select(date = !!field_date, stkcd = !!field_stkcd,
                    return = !!field_return) %>%
      tidyr::spread(key = stkcd, value = return)

    # arrange colname as the order of stock_cd_list
    if (!is.null(stock_cd_list) && length(stock_cd_list) != 0 )
      ds_return <- dplyr::select(ds_return, date, as.character(stock_cd_list))


    # Change date format: datetime --> date
    # Use the last day of the period as the date for weekly, monthly, annual data
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)
    if (period_type != "daily") {

      #last day of period = ceiling_date -1
      ceiling_date <- lubridate::ceiling_date(origin_date,
                                           unit = date_ceiling_unit,
                                           change_on_boundary = TRUE)
      charvec <- ceiling_date -1

    } else {
      charvec <- origin_date
    }

    # Build time series
    ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)

    # Format the colname by stkcd or stckname
    if (use_stock_name) {
      field_names <- code2name(stock_db, as.numeric(names(ts_return.fts)))
    } else {
      field_names <- sprintf("%06d", as.numeric(names(ts_return.fts)))
    }
    names(ts_return.fts) <- field_names

  } else {
    success <- FALSE
  }

  # transform simple return into requried return type
  if (success) {
    if (cumulated) {
      ts_return.fts <- cumulated(na.omit(ts_return.fts, method = "z"), method = "simple")
    } else {
      if (match.arg(return_type) == "compound")
         ts_return.fts <- simple2compound_return(ts_return.fts)
    }

  }

  # Build final return results by output format
  if (success) {
    output_type = match.arg(output_type)
    switch( output_type,
      tibble = {

        # take back date column from rowsname
        ts_return.tib <- tibble::rownames_to_column(tibble::as.tibble(ts_return.fts),
                                                    var = "date")
        # turn width format back into long format
        ts_return.tib <- ts_return.tib %>%
          dplyr::mutate(date = lubridate::ymd(date)) %>%
          tidyr::gather(key="stkcd", value = "return", -date)

        # rebuild stkcd coloumn
        if (use_stock_name) {
          # change stkname into stkcd number
          ts_return.tib <- ts_return.tib %>%
            dplyr::mutate(stkcd = name2code(stock_db, name = stkcd))

        } else {
          # normalize stkcd string to stkcd number(when turn timeSeries into tibble,
          # the stkcd were added prefix of "X" )
          ts_return.tib <- ts_return.tib %>%
            dplyr::mutate(stkcd = as.integer(stringr::str_replace(stkcd, "^X", "")))
        }

        ts_return <- ts_return.tib
      },
      timeSeries = {
        ts_return <- ts_return.fts
      }
    )
  }

  return(ts_return)

}

# Get market return timesereis from stock_db
#' @describeIn get_market_return get market return timeseries from a database of gta_db class
#' @export
get_market_return.gta_db <- function(stock_db,
                    period_type = c("daily", "weekly", "monthly", "annual"),
                    return_type = c("simple", "compound"),
                    cumulated = FALSE,
                    output_type = c("timeSeries", "tibble")) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success = TRUE

  # get market return dataset
  field_markettype <- quo(markettype)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    daily = {
      table_name   <- stock_db$table_list[["TRD_CNDALYM"]]
      field_date   <- quo(trddt)
      field_return <- quo(cdretwdtl)
      date_format <- "ymd"
      date_ceiling_unit <-"day"
    },
    weekly  = {
      table_name <- stock_db$table_list[["TRD_WEEKCM"]]
      field_date   <- quo(trdwnt)
      field_return <- quo(cwretwdtl)
      date_ceiling_unit <-"week"
    },
    monthly = {
      table_name <- stock_db$table_list[["TRD_CNMONT"]]
      field_date   <- quo(trdmnt)
      field_return <- quo(cmretwdtl)
      date_format <- "ym"
      date_ceiling_unit <-"month"
    },
    annual  = {
      table_name <- stock_db$table_list[["TRD_YEARCM"]]
      field_date   <- quo(trdynt)
      field_return <- quo(cyretwdtl)
      date_format <- "y"
      date_ceiling_unit <-"year"
    }
  )

  #Warning: ds_return is simple return in database by default !!
  ds_return <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_return)) {
    ds_return <- tibble::as.tibble(ds_return)
    msg <- sprintf("return table: %s , return field: %s, date field: %s",
                   table_name, rlang::quo_text(field_return), rlang::quo_text(field_date))
    message(msg)
  }
  else{
    success <- FALSE
  }


   # Build simple return results
  ts_return <- NULL
  if (success) {

    ds_return <- dplyr::filter(ds_return, UQ(field_markettype) == 21)
    ds_return <- ds_return %>%
      dplyr::select(date = !!field_date, market_index = !!field_return)

    # Change date format: datetime --> date
    # Use the last day of the period as the date for weekly, monthly, annual data
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)
    if (period_type != "daily") {

      #last day of period = ceiling_date -1
      ceiling_date <- lubridate::ceiling_date(origin_date,
                                              unit = date_ceiling_unit,
                                              change_on_boundary = TRUE)
      charvec <- ceiling_date - 1

    } else {
      charvec <- origin_date
    }

    # Build timeseries
    ts_return.fts <- timeSeries::timeSeries(ds_return[,-1], charvec)

  }

  # Transform simple return into requried return type
  if (success) {
    if (cumulated) {
      ts_return.fts <- cumulated(na.omit(ts_return.fts, method = "z"), method = "simple")
    } else {
      if (match.arg(return_type) == "compound")
        ts_return.fts <- simple2compound_return(ts_return.fts)
    }
  }

  # Build final return results by output format
  if (success) {
    output_type = match.arg(output_type)
    switch( output_type,
            tibble = {
              # take back date column from rowsname
              ts_return.tib <- tibble::rownames_to_column(tibble::as.tibble(ts_return.fts),
                                                           var = "date")
              ts_return.tib <- ts_return.tib %>%
                dplyr::mutate(date = lubridate::ymd(date))

              ts_return <- ts_return.tib
            },
            timeSeries = {
              ts_return <- ts_return.fts
            }
    )
  }

  return(ts_return)
}

# Get Get factor indicator timeseries for stock_db
#' @describeIn get_stock_return get stock return timeseries from a database of gta_db class
#' @export
get_factor_indicator.gta_db <- function(stock_db, factor_list){

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  stopifnot(!is.null(factor_list))

  success = TRUE

  # get file table name mapping for referece
  gta_profile_name <- system.file(.GTA_RPROFILE_DIR,
                                  .GTA_PROFILE_FILE, package = .PACKAGE_NAME )
  if (gta_profile_name == "") {
    msg = sprintf("No file of % exisits in % for %",
                  .GTA_PROFILE_FILE,
                  .GTA_RPROFILE_DIR,
                  .PACKAGE_NAME)
    stop(msg)
    success = FALSE
  }


  # get indcator info of matched factor
  if (success) {
    matched_indicator_map <- .get_db_profile_factor_indicator_map(gta_profile_name,
                                                                  factor_list)
    if (!is.null(matched_indicator_map)) {

      # build table_list for fetching indicators
      indicator_table_list<- matched_indicator_map %>%
        dplyr::filter(factor_list %in% factor_code) %>%
        dplyr::group_by(indicator_table) %>%
        tidyr::nest()

    } else {
      success = FALSE
    }
  }

  # get indicators dataset from indicator_data_table
  if (success) {

    ds_all_indicators = NULL
    for (i in seq_len(nrow(indicator_table_list))) {

      indicator_table <- indicator_table_list[i,]$indicator_table
      indicator_params <- indicator_table_list[i, ]$data[[1]]
      indicator_code_list <- indicator_params$indicator_code
      factor_code_list <- indicator_params$factor_code

      # Get a indicator dataset from database
      if (success) {
        ds_indicators <- get_table_dataset(stock_db,
                                          table_name = indicator_table)
        if (!is.null(ds_indicators)) {

          indicators_is_existed <- indicator_code_list %in% colnames(ds_indicators)
          if (all(indicators_is_existed)) {

            # proceed only if all indicators exists in result dataset

            ds_indicators <- tibble::as.tibble(ds_indicators)

            # filter consolidated report data
            if ( "typrep" %in% names(ds_indicators)) {
              ds_indicators <- dplyr::filter(ds_indicators, typrep == 'A')
            }

            # get indicators from dataset
            ds_indicators <- ds_indicators %>%
              dplyr::mutate( date = lubridate::as_date(accper),
                  periodtype = ifelse(lubridate::month(accper) == 12, "annual","quarter")) %>%
              dplyr::select(date , periodtype, stkcd, indcd, indicator_code_list)

            # rename indicator_code to factor_code
            col_names <- colnames(ds_indicators)
            col_names <- col_names[1:(length(col_names) - length(indicator_code_list))]
            col_names <- c(col_names, factor_code_list)
            colnames(ds_indicators) <- col_names

            # message
            msg <- sprintf("get factor: %s from indicator table: %s at indicator field: %s(%s)",
                           stringr::str_c(factor_code_list, collapse = ","),
                           indicator_table,
                           stringr::str_c(indicator_code_list, collapse = ","),
                           stringr::str_c(code2name(stock_db,
                                                    indicator_code_list,
                                                    type = "field"),
                                          collapse = ","))
            message(msg)

          } else {

            # some indicators miss from result dataset
            msg <- sprintf("indicator fields: %s aren't in the table of %s",
                           stringr::str_c(indicator_code_list[!indicators_is_existed], collapse = ","),
                           indicator_table)
            warn(msg)

            success <- FALSE

          }

        } else {
          success <- FALSE
        }

      }


      # combine the indicators dataset into all indicators datasets
      if (success) {
        if (is.null(ds_all_indicators)) {
          ds_all_indicators <- ds_indicators
        } else {
          ds_all_indicators <- ds_all_indicators %>%
              dplyr::full_join(ds_indicators,
                               by = c("date", "periodtype", "stkcd", "indcd"))

        }
      }
    }
  }


  # Build final indicator results
  ts_indicator = NULL
  if (success) {

    # sort resutls by time and stkcd
    ds_all_indicators <- ds_all_indicators %>%
              dplyr::arrange(date, indcd, stkcd)

    ts_indicator = ds_all_indicators

  }

  return(ts_indicator)

}


#' Create a stock_filed_list for a database of gta_db class
stock_field_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build field_name list
  field_name_list <- NULL
  table_name <- stock_db$table_list[["gta_fieldname_list"]]
  field_list.df <- get_table_dataset.gta_db(stock_db, table_name )
  if (!is.null(field_list.df)) {

    codes <- field_list.df[, "field_code"]
    codes <- tolower(codes)
    names <- field_list.df[, "field_name"]

    field_name_list <- code_name_list(codes, names)

  } else {
    warning("can't create code_name_list due to failing
            to get data from stock db")
  }

  return(field_name_list)

}

#' Create a stock_name_list for a database of gta_db class
stock_name_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  #build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {

    codes <- ds_trd_company.df[, "stkcd"]
    names <- ds_trd_company.df[, "stknme"]

    stock_name_list <- code_name_list(codes, names)

  } else {
    warning("can't create code_name_list due to failing
            to get data from stock db")
  }

  return(stock_name_list)

}

#' Create a industry_name_list for a database of gta_db class
industry_name_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  #build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {

    ds_indistry <- ds_trd_company.df %>%
      dplyr::select(nnindcd, nnindnme) %>%
      dplyr::distinct()

    codes <- ds_indistry[, "nnindcd"]
    names <- ds_indistry[, "nnindnme"]

    stock_name_list <- code_name_list(codes, names)

  } else {
    warning("can't create code_name_list due to failing
            to get data from stock db")
  }

  return(stock_name_list)

}



