
# CONSTANT DEFINATION -----------------------------------------------------

.PACKAGE_NAME <- "zstmodelr"

# profile variable defination
# Since R devtools dosen't suport loading Chinese charaters in script file, we
# have to use reference name of varible in profile for refering Chinese table
# name in DB
.GTA_RPROFILE_DIR <- "etc"
.GTA_PROFILE_FILE <- "gta_profile.csv"


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
    msg = sprintf("No profileof % exisits in % for %",
                  .GTA_PROFILE_FILE,
                  .GTA_RPROFILE_DIR,
                  .PACKAGE_NAME)
    stop(msg)
  }

  # set up table name list
  stock_db$table_list <- list()
  for (i in seq_along(.GTA_TABLE_NAME_LIST)) {
    table_name_id <- .GTA_TABLE_NAME_LIST[[i]]
    table_name_value <- .get_db_profile(gta_profile_name, table_name_id)
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
    stock_db$stock_field_list <- stock_field_list(stock_db)
    if (is.null(stock_db$stock_field_list)) {
      warning("failed to set up field_name_list")
      success = FALSE
    }
  }

  # set up stock_name list
  if (success) {
    stock_db$stock_name_list <- stock_name_list(stock_db)
    if (is.null(stock_db$stock_name_list)) {
      warning("failed to set up stock_name_list")
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
name2code.gta_db <- function(stock_db, name, type=c("stock", "field")) {

    stopifnot(inherits(stock_db, "gta_db"), !missing(name))

    target_type <- match.arg(type)
    code = switch(target_type,
              field = name2code(stock_db$stock_field_list, name = name),
              stock = name2code(stock_db$stock_name_list, name = name)
    )

    return(code)

}

# Translate code into name for field or stock
#' @describeIn code2name Translate code into name in a database of gta_db class
#' @export
code2name.gta_db <- function(stock_db, code, type=c("stock", "field")) {

  stopifnot(inherits(stock_db, "gta_db"), !missing(code))

  target_type <- match.arg(type)
  name = switch(target_type,
                field = code2name(stock_db$stock_field_list, code = code),
                stock = code2name(stock_db$stock_name_list, code = code)
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
                  handleNA = c("r", "s", "z", "ir", "iz", "ie"),
                  interpNA_method = c("before", "linear", "after")) {

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
    },
    weekly  = {
      table_name <- stock_db$table_list[["TRD_WEEK"]]
      field_date   <- quo(trdwnt)
      field_return <- quo(wretwd)
    },
    monthly = {
      table_name <- stock_db$table_list[["TRD_MNTH"]]
      field_date   <- quo(trdmnt)
      field_return <- quo(mretwd)
      date_format <- "ymd"
    },
    annual  = {
      table_name <- stock_db$table_list[["TRD_YEAR"]]
      field_date   <- quo(trdynt)
      field_return <- quo(yretwd)
      date_format <- "y"
    }
  )

  ds_return <- get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list)
  if (!is.null(ds_return)) {
    ds_return <- tibble::as.tibble(ds_return)
    msg <- sprintf("return table: %s , return field: %s, date field: %s",
                   table_name, deparse(field_return), deparse(field_date))
    message(msg)

  } else {
    success = FALSE
  }

  # Build return results
  if (success) {

    # Filter return
    if (!is.null(stock_cd_list) && length(stock_cd_list) != 0 )
      ds_return <- dplyr::filter(ds_return, UQ(field_stkcd) %in% stock_cd_list)

    ds_return <- ds_return %>%
      dplyr::select(date = !!field_date, stkcd = !!field_stkcd,
                    return = !!field_return) %>%
      tidyr::spread(key = stkcd, value = return)

    # Set colname as order of stock_cd_list
    if (!is.null(stock_cd_list) && length(stock_cd_list) != 0 )
      ds_return <- dplyr::select(ds_return, date, as.character(stock_cd_list))

    # Build time series
    charvec <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)

    ts_return <- ts_return.fts
    field_names <- sprintf("%06d", as.numeric(names(ts_return)))
    names(ts_return) <- field_names

    # deal with the NAs
    ts_return <- na.omit(ts_return, method = handleNA, interp = interpNA_method )

  } else {
    ts_return <- NULL
  }

  return(ts_return)

}

# Get market return timesereis from stock_db
#' @describeIn get_market_return get market return timeseries from a database of gta_db class
#' @export
get_market_return.gta_db <- function(stock_db,
                    period_type = c("daily", "weekly", "monthly", "annual"),
                    handleNA = c("r", "s", "z", "ir", "iz", "ie"),
                    interpNA_method = c("before", "linear", "after")) {

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
    },
    weekly  = {
      table_name <- stock_db$table_list[["TRD_WEEKCM"]]
      field_date   <- quo(trdwnt)
      field_return <- quo(cwretwdtl)
    },
    monthly = {
      table_name <- stock_db$table_list[["TRD_CNMONT"]]
      field_date   <- quo(trdmnt)
      field_return <- quo(cmretwdtl)
      date_format <- "ym"
    },
    annual  = {
      table_name <- stock_db$table_list[["TRD_YEARCM"]]
      field_date   <- quo(trdynt)
      field_return <- quo(cyretwdtl)
      date_format <- "y"
    }
  )

  ds_return <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_return)) {
    ds_return <- tibble::as.tibble(ds_return)
    msg <- sprintf("return table: %s , return field: %s, date field: %s",
                   table_name, deparse(field_return), deparse(field_date))
    message(msg)
  }
  else{
    success <- FALSE
  }


  # Build return results
  if (success) {
    ds_return <- dplyr::filter(ds_return, UQ(field_markettype) == 21)
    ds_return <- ds_return %>%
      dplyr::select(date = !!field_date, market_index = !!field_return)


    # Build timeseries
    charvec <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    ts_return.fts <- timeSeries::timeSeries(ds_return[,-1], charvec)
    ts_return <- ts_return.fts

    # deal with the NAs
    ts_return <- na.omit(ts_return, method = handleNA, interp = interpNA_method )

  } else {
    ts_return <- NULL
  }

  return(ts_return)
}

# Interface Implementation of stock_field_list class by gta_db -----------------

# stock_field class creator
#' @describeIn stock_field_list create a stock_filed_list for a database of gta_db class
#' @export
stock_field_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build field_name list
  field_name_list <- NULL
  table_name <- stock_db$table_list[["gta_fieldname_list"]]
  field_list.df <- get_table_dataset.gta_db(stock_db, table_name )
  field_list <- field_list.df[, c(1, 2)]
  colnames(field_list) <- c("field_code", "field_name")
  field_list["field_code"] <- lapply(field_list["field_code"], tolower)
  field_name_list <- structure(field_list, class = "stock_field_list")

  return(field_name_list)

}


# Interface Implementation of stock_name_list class by gta_db -----------------


# stock_name_list class creator
#' @describeIn stock_name_list create a stock_name_list for a database of gta_db class
#' @export
stock_name_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  #build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {
    stock_name_list <- ds_trd_company.df[,c("stkcd", "stknme")]
    names(stock_name_list) <- c("stock_code","stock_name")
    stock_name_list <- structure(stock_name_list, class = "stock_name_list")
  } else {
    stop("can't get data from stock db")
  }

  return(stock_name_list)

}



