
# CONSTANT DEFINATION -----------------------------------------------------

# profile variable defination
# Since R devtools dosen't suport loading Chinese charaters in script file, we
# have to use reference name of varible in profile for refering Chinese table
# name in DB
.GTA_PROFILE_FILE <- "gta_profile.xlsx"


.GTA_TABLE_NAME_LIST <- list(
  "gta_fieldname_list", # name for gta_fieldname_list

  "TRD_Co", # name for TRD_Co_公司基本情况

  "TRD_DALYR", # name for TRD_Dalyr_日个股回报率
  "TRD_WEEK", # name for TRD_Week_周个股回报率
  "TRD_MNTH", # name for TRD_Mnth_月个股回报率
  "TRD_YEAR", # name for TRD_Year_年个股回报率

  "TRD_CNDALYM", # name for TRD_Cndalym_综合市场日度回报
  "TRD_WEEKCM", # name for TRD_Weekcm_综合市场周度回报
  "TRD_CNMONT", # name for TRD_Cnmont_综合市场月度回报
  "TRD_YEARCM" # name for TRD_Yearcm_综合市场年度回报
)

.GTA_XXXX <- "中文字符" # not supported

# Class definition of gta_db class -----------------------------------------


#' @include stock-db.R
#' @include code-name-list.R

# Definition of gta_db class
setRefClass("gta_db",
  fields = list(
    table_list = "list",
    stock_field_list = "code_name_listOrNull",
    stock_name_list = "code_name_listOrNull",
    industry_name_list = "code_name_listOrNull"
  ),
  contains = "stock_db"
)


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

  # use envir for class member storage - created only for s3 class
  # class_env <- new.env()
  # class_env$dsn <- dsn
  # class_env$connection <- NULL

  # create object of S3 Class
  # stock_db <- structure(class_env, class = c("gta_db", "stock_db"))

  # create object of S4 Class
  stock_db <- new("gta_db", dsn = dsn, connection = NULL)

  return(stock_db)
}

# Generic functions implemetation by gta_db class ------------------------

# Open the stock database
# Method definition for s3 generic
#' @describeIn open_stock_db Open a database of gta_db class
#' @export
open_stock_db.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  # Use data-engine of RODBC
  # con_stock_db <- tryCatch( RODBC::odbcConnect(dsn = stock_db$dsn),
  #                           error = function(e) e)

  # Use data-engine of DBI:odbc
  con_stock_db <- tryCatch(DBI::dbConnect(odbc::odbc(),
    dsn = stock_db$dsn,
    encoding = "CP936"
  ),
  error = function(e) e
  )


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
# Method definition for s4 generic
#' @describeIn open_stock_db Open a database of gta_db class
#' @export
setMethod(
  "open_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    open_stock_db.gta_db(stock_db, ...)
  }
)

# Close the stock database
# Method definition for s3 generic
#' @describeIn close_stock_db Close a database of gta_db class
#' @export
close_stock_db.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  success <- TRUE

  if (!is.null(stock_db$connection)) {

    # Use data-engine of RODBC
    # success <- tryCatch(RODBC::odbcClose(stock_db$connection),
    #                     error = function(e) e)

    # Use data-engine of DBI:odbc
    success <- tryCatch(DBI::dbDisconnect(stock_db$connection),
      error = function(e) e
    )

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
# Method definition for s4 generic
#' @describeIn close_stock_db Close a database of gta_db class
#' @export
setMethod(
  "close_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    close_stock_db.gta_db(stock_db, ...)
  }
)



# Get profile of stock_db
# Method definition for s3 generic
#' @describeIn get_profile get profile of a database of gta_db class
#' @export
get_profile.gta_db <- function(stock_db, profile_name = .GTA_PROFILE_FILE) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # set default profile_name
  if (missing(profile_name)) {
    profile_name <- .GTA_PROFILE_FILE
  }

  gta_profile <- get_profile_path(profile_name)

  return(gta_profile)
}

# Method definition for s4 generic
#' @describeIn get_profile get profile of a database of gta_db class
#' @export
setMethod(
  "get_profile",
  signature(stock_db = "gta_db"),
  function(stock_db, profile_name, ...) {
    get_profile.gta_db(stock_db, profile_name)
  }
)

# Init param of stock db
# Method definition for s3 generic
#' @describeIn init_stock_db Init param of database of gta_db class
#' @export
init_stock_db.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  success <- TRUE

  # set up table name mapping for referece
  gta_profile_name <- get_profile.gta_db(stock_db)

  # set up table name list
  stock_db$table_list <- list()
  for (i in seq_along(.GTA_TABLE_NAME_LIST)) {
    table_name_id <- .GTA_TABLE_NAME_LIST[[i]]
    table_name_value <- profile_get_varible_setting(gta_profile_name, table_name_id)
    if (is.null(table_name_value)) {
      msg <- sprintf(
        "failed to load %s from %", table_name_id,
        gta_profile_name
      )
      stop(msg)
      success <- FALSE
    } else {
      stock_db$table_list[table_name_id] <- table_name_value
    }
  }

  # set up field_name list
  if (success) {
    stock_db$stock_field_list <- stock_field_list.gta_db(stock_db)
    if (is.null(stock_db$stock_field_list)) {
      warning("failed to set up field_name_list")
      success <- FALSE
    }
  }

  # set up stock_name list
  if (success) {
    stock_db$stock_name_list <- stock_name_list.gta_db(stock_db)
    if (is.null(stock_db$stock_name_list)) {
      warning("failed to set up stock_name_list")
      success <- FALSE
    }
  }

  # set up industry_name list
  if (success) {
    stock_db$industry_name_list <- industry_name_list.gta_db(stock_db)
    if (is.null(stock_db$industry_name_list)) {
      warning("failed to set up industry_name_list")
      success <- FALSE
    }
  }

  return(invisible(success))
}
# Method definition for s4 generic
#' @describeIn init_stock_db Init param of database of gta_db class
#' @export
setMethod(
  "init_stock_db",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    init_stock_db.gta_db(stock_db, ...)
  }
)



# List all tables of stck_db
# Method definition for s3 generic
#' @describeIn list_stock_tables List names of tables in database of gta_db class
#' @export
list_stock_tables.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # Use data-engine of RODBC
  # db_tables <- RODBC::sqlTables(stock_db$connection, schema = "dbo")
  # db_tables <- db_tables[db_tables$TABLE_SCHEM == "dbo", "TABLE_NAME" ]

  # Use data-engine of DBI:odbc
  db_tables <- DBI::dbListTables(stock_db$connection, schema_name = "dbo")
  db_tables <- iconv(db_tables, from = "UTF-8", to = "CP936")

  return(db_tables)
}
# Method definition for s4 generic
#' @describeIn list_stock_tables List names of tables in database of gta_db class
#' @export
setMethod(
  "list_stock_tables",
  signature(stock_db = "gta_db"),
  function(stock_db, ...) {
    list_stock_tables.gta_db(stock_db, ...)
  }
)


# Translate name into code for field or stock
# Method definition for s3 generic
#' @describeIn name2code Translate name into code in a database of gta_db class
#' @export
name2code.gta_db <- function(stock_db, name,
                             type = c("stock", "field", "industry")) {
  stopifnot(inherits(stock_db, "gta_db"), !missing(name))

  target_type <- match.arg(type)
  code <- switch(target_type,
    field = name2code(stock_db$stock_field_list, name = name),
    stock = name2code(stock_db$stock_name_list, name = name),
    industry = name2code(stock_db$industry_name_list, name = name)
  )

  return(code)
}
# Method definition for s4 generic
# Translate name into code for field or stock
#' @describeIn name2code Translate name into code in a database of gta_db class
#' @export
setMethod(
  "name2code",
  signature(x = "gta_db"),
  function(x, name, ...) {
    name2code.gta_db(stock_db = x, name, ...)
  }
)

# Translate code into name for field or stock
# Method definition for s3 generic
#' @describeIn code2name Translate code into name in a database of gta_db class
#' @export
code2name.gta_db <- function(stock_db, code,
                             type = c("stock", "field", "industry")) {
  stopifnot(inherits(stock_db, "gta_db"), !missing(code))

  target_type <- match.arg(type)
  name <- switch(target_type,
    field = code2name(stock_db$stock_field_list, code = code),
    stock = code2name(stock_db$stock_name_list, code = code),
    industry = code2name(stock_db$industry_name_list, code = code)
  )

  return(name)
}
# Method definition for s4 generic
#' @describeIn code2name Translate code into name in a database of gta_db class
#' @export
setMethod(
  "code2name",
  signature(x = "gta_db"),
  function(x, code, ...) {
    code2name.gta_db(stock_db = x, code, ...)
  }
)
# Get a dataset from a table in stock_db
# Method definition for s3 generic
#' @describeIn get_table_dataset get a table dataset from a database of gta_db class
#' @export
get_table_dataset.gta_db <- function(stock_db, table_name) {

  # Validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name)) {
    stop("Table name must be character string")
  }

  # Get table data  from database

  # Use data-engine of RODBC
  # ds_result <- tryCatch(RODBC::sqlFetch(stock_db$connection, table_name,
  #                                       stringsAsFactors = FALSE),
  #                        error = function(e) e)

  # Use data-engine of DBI:odbc
  table_name <- iconv(table_name, from = "UTF-8", to = "CP936")
  ds_result <- tryCatch(DBI::dbReadTable(stock_db$connection, table_name),
    error = function(e) e
  )
  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf("get data from %s successfully", table_name)
    colnames(ds_result) <- tolower(colnames(ds_result))
  }
  message(msg)

  # Coerce stock code to 6 digit of characters
  if (!is.null(ds_result)) {
    field_names <- names(ds_result)
    stkcd_names <- field_names[field_names %in% c("stkcd")]
    if (length(stkcd_names) != 0) {
      if (!is.character(ds_result[, stkcd_names])) {
        ds_result[, stkcd_names] <- stringr::str_pad(ds_result[, stkcd_names],
          width = 6, pad = "0"
        )
        msg <- "Coerce stock cd to character of 6 digits if it were number"
        warnings(msg)
      }
    }
  }

  return(invisible(ds_result))
}
# Method definition for s4 generic
#' @describeIn get_table_dataset get a table dataset from a database of gta_db class
#' @export
setMethod(
  "get_table_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_name, ...) {
    get_table_dataset.gta_db(stock_db, table_name)
  }
)

# Get a dataset of a list of stock_cd from table in stock
# Method definition for s3 generic
#' @describeIn get_stock_dataset get a dataset of a list of stock_cd from table
#' in a database of gta_db class
#' @export
get_stock_dataset.gta_db <- function(stock_db, table_name, stock_cd_list = NULL) {

  # Validate param
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name)) {
    stop("Table name must be character string")
  }

  # if (missing(stock_cd_list) || !is.character(stock_cd_list) ) {
  #    stop("stock_cd_list must be character string")
  # }

  # Build sql command for data query
  stock_cd_list_str <- NULL
  if (!is.null(stock_cd_list)) {

    # Coerce stock code to 6 digit of characters
    if (!is.character(stock_cd_list)) {
      stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
      msg <- "Coerce stock cd to character of 6 digits if it were number"
      warnings(msg)
    }

    # build stock_cd list string
    for (stock_cd in stock_cd_list) {
      if (is.null(stock_cd_list_str)) {
        stock_cd_list_str <- stock_cd
      } else {
        stock_cd_list_str <- paste(stock_cd_list_str, stock_cd, sep = ",")
      }
    }
  }

  if (length(stock_cd_list_str) != 0) {
    sql_cmd <- sprintf(
      "select * from %s where Stkcd in (%s)",
      table_name, stock_cd_list_str
    )
  } else {
    sql_cmd <- sprintf("select * from %s", table_name)
  }

  # Select stock data from table

  # Use data-engine of RODBC
  # ds_result <- tryCatch(RODBC::sqlQuery(stock_db$connection, sql_cmd,
  #                                       stringsAsFactors = FALSE),
  #                       error = function(e) e)

  # Use data-engine of DBI:odbc
  sql_cmd <- iconv(sql_cmd, from = "UTF-8", to = "CP936")
  ds_result <- tryCatch(DBI::dbGetQuery(stock_db$connection, sql_cmd),
    error = function(e) e
  )

  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf(
      "get stock data of(%s) from %s successfully", stock_cd_list_str,
      table_name
    )
    colnames(ds_result) <- tolower(colnames(ds_result))
  }
  message(msg)

  # Coerce stock code to 6 digit of characters
  if (!is.null(ds_result)) {
    field_names <- names(ds_result)
    stkcd_names <- field_names[field_names %in% c("stkcd")]
    if (length(stkcd_names) != 0) {
      if (!is.character(ds_result[, stkcd_names])) {
        ds_result[, stkcd_names] <- stringr::str_pad(ds_result[, stkcd_names],
          width = 6, pad = "0"
        )
        msg <- "Coerce stock cd to character of 6 digits if it were number"
        warnings(msg)
      }
    }
  }


  return(invisible(ds_result))
}
# Method definition for s4 generic
#' @describeIn get_stock_dataset get a dataset of a list of stock_cd from table
#' in a database of gta_db class
#' @export
setMethod(
  "get_stock_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_name, stock_cd_list = NULL, ...) {
    get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list)
  }
)

# Fetch many datasets from stock_db
# Method definition for s3 generic
#' @describeIn fetch_table_dataset get many datasets from a database of gta_db class
#' @export
fetch_table_dataset.gta_db <- function(stock_db, table_list) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_list) || length(table_list) == 0) {
    stop("table_list must contain one table at least")
  }

  # get datasets from stock db
  result_table_list <- list(length(table_list))
  for (table_index in seq_along(table_list)) {
    the_table <- table_list[table_index]
    # get stock data for specified stock
    ds_result <- get_table_dataset.gta_db(stock_db,
      table_name = the_table
    )

    # keep the result_ts in GlobalEnv for debug check
    if (!is.null(ds_result)) {

      # get table successfully
      ds_name <- sprintf("ds_%s.df", the_table)
      assign(ds_name, ds_result, envir = .GlobalEnv)
      # assign(ds_name, ds_result, envir = parent.frame())
      result_table_list[table_index] <- ds_name
    } else {

      # fail to get table
      result_table_list[table_index] <- NULL
    }
  }

  return(result_table_list)
}
# Method definition for s4 generic
#' @describeIn fetch_table_dataset get many datasets from a database of gta_db class
#' @export
setMethod(
  "fetch_table_dataset",
  signature(stock_db = "gta_db"),
  function(stock_db, table_list, ...) {
    fetch_table_dataset.gta_db(stock_db, table_list)
  }
)

# Get stock return timeseries from stock_db
# Method definition for s3 generic
#' @describeIn get_stock_return get stock return timeseries from a database of gta_db class
#' @export
get_stock_return.gta_db <- function(stock_db, stock_cd_list = NULL,
                                    period_type = c("day", "month", "year"),
                                    period_date = c("start", "end"),
                                    output_type = c("timeSeries", "tibble")) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get stock return dataset
  field_stkcd <- rlang::quo(stkcd)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    "day" = {
      table_name <- stock_db$table_list[["TRD_DALYR"]]
      field_date <- rlang::quo(trddt)
      field_return <- rlang::quo(dretwd)
      date_format <- "ymd"
      period_unit <- "day"
    },
    "month" = {
      table_name <- stock_db$table_list[["TRD_MNTH"]]
      field_date <- rlang::quo(trdmnt)
      field_return <- rlang::quo(mretwd)
      date_format <- "ym"
      period_unit <- "month"
    },
    "year" = {
      table_name <- stock_db$table_list[["TRD_YEAR"]]
      field_date <- rlang::quo(trdynt)
      field_return <- rlang::quo(yretwd)
      date_format <- "y"
      period_unit <- "year"
    }
  )

  # Coerce stock code to 6 digit of characters
  if (!is.null(stock_cd_list) && !is.character(stock_cd_list)) {
    stock_cd_list <- stringr::str_pad(stock_cd_list, width = 6, pad = "0")
    msg <- "Coerce stock cd to character of 6 digits if it were number"
    warnings(msg)
  }

  # Warning: ds_return is simple return in database by default !!
  ds_return <- get_stock_dataset.gta_db(stock_db, table_name, stock_cd_list)
  if (!is.null(ds_return)) {
    ds_return <- ds_return %>%
      tibble::as.tibble() %>%
      dplyr::select(
        date = !!field_date, stkcd = !!field_stkcd,
        return = !!field_return
      )

    msg <- sprintf(
      "return table: %s , return field: %s, date field: %s",
      table_name, rlang::quo_text(field_return), rlang::quo_text(field_date)
    )
    message(msg)
  } else {
    success <- FALSE
  }

  # Set date format of date field
  if (success) {

    # Change date format: datetime --> date
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)

    # Set date of period(start date/end date of the period)
    period_date <- match.arg(period_date)
    switch(period_date,
      "start" = {
        # first day of period = floor_date
        floor_date <- lubridate::floor_date(origin_date,
          unit = period_unit
        )
        ds_return$date <- floor_date
      },
      "end" = {
        # last day of period = ceiling_date -1
        ceiling_date <- lubridate::ceiling_date(origin_date,
          unit = period_unit,
          change_on_boundary = TRUE
        )
        ds_return$date <- ceiling_date - 1
      }
    )
  }

  # Build simple return results
  ts_return <- NULL
  if (success) {
    output_type <- match.arg(output_type)
    switch(output_type,
      tibble = {
        ts_return <- ds_return
      },
      timeSeries = {

        # spread stkcd as columns
        ds_return <- ds_return %>%
          tidyr::spread(key = stkcd, value = return)

        # arrange colname as the order of stock_cd_list
        if (!is.null(stock_cd_list) && length(stock_cd_list) != 0) {
          ds_return <- dplyr::select(ds_return, date, stock_cd_list)
        }

        # Build time series
        charvec <- ds_return$date
        ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)

        ts_return <- ts_return.fts
      }
    )
  }

  return(ts_return)
}
# Method definition for s4 generic
#' @describeIn get_stock_return get stock return timeseries from a database of gta_db class
#' @export
setMethod(
  "get_stock_return",
  signature(stock_db = "gta_db"),
  function(stock_db,
             stock_cd_list = NULL,
             period_type = c("day", "month", "year"),
             period_date = c("start", "end"),
             output_type = c(
               "timeSeries",
               "tibble"
             ),
             ...) {
    get_stock_return.gta_db(
      stock_db = stock_db,
      stock_cd_list = stock_cd_list,
      period_type = period_type,
      period_date = period_date,
      output_type = output_type
    )
  }
)

# Get market return timesereis from stock_db
# Method definition for s3 generic
#' @describeIn get_market_return get market return timeseries from a database of gta_db class
#' @export
get_market_return.gta_db <- function(stock_db,
                                     period_type = c("day", "month", "year"),
                                     period_date = c("start", "end"),
                                     output_type = c("timeSeries", "tibble")) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  success <- TRUE

  # get market return dataset
  field_markettype <- rlang::quo(markettype)
  period_type <- match.arg(period_type)
  switch(
    period_type,
    "day" = {
      table_name <- stock_db$table_list[["TRD_CNDALYM"]]
      field_date <- rlang::quo(trddt)
      field_return <- rlang::quo(cdretwdtl)
      date_format <- "ymd"
      period_unit <- "day"
    },
    "month" = {
      table_name <- stock_db$table_list[["TRD_CNMONT"]]
      field_date <- rlang::quo(trdmnt)
      field_return <- rlang::quo(cmretwdtl)
      date_format <- "ym"
      period_unit <- "month"
    },
    "year" = {
      table_name <- stock_db$table_list[["TRD_YEARCM"]]
      field_date <- rlang::quo(trdynt)
      field_return <- rlang::quo(cyretwdtl)
      date_format <- "y"
      period_unit <- "year"
    }
  )

  # Warning: ds_return is simple return in database by default !!
  ds_return <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_return)) {
    ds_return <- ds_return %>%
      tibble::as.tibble() %>%
      dplyr::filter(!!field_markettype == 21) %>%
      dplyr::select(date = !!field_date, market_index = !!field_return)

    msg <- sprintf(
      "return table: %s , return field: %s, date field: %s",
      table_name, rlang::quo_text(field_return), rlang::quo_text(field_date)
    )
    message(msg)
  } else {
    success <- FALSE
  }

  # Set date format of date field
  if (success) {

    # Change date format: datetime --> date
    origin_date <- lubridate::parse_date_time(as.character(ds_return$date), date_format)
    origin_date <- lubridate::as_date(origin_date)

    # Set date of period(start date/end date of the period)
    period_date <- match.arg(period_date)
    switch(period_date,
      "start" = {
        # first day of period = floor_date
        floor_date <- lubridate::floor_date(origin_date,
          unit = period_unit
        )
        ds_return$date <- floor_date
      },
      "end" = {
        # last day of period = ceiling_date -1
        ceiling_date <- lubridate::ceiling_date(origin_date,
          unit = period_unit,
          change_on_boundary = TRUE
        )
        ds_return$date <- ceiling_date - 1
      }
    )
  }

  # Build simple return results
  ts_return <- NULL
  if (success) {
    output_type <- match.arg(output_type)
    switch(output_type,
      tibble = {
        ts_return <- ds_return
      },
      timeSeries = {
        # Build time series
        charvec <- ds_return$date
        ts_return.fts <- timeSeries::timeSeries(ds_return[, -1], charvec)

        ts_return <- ts_return.fts
      }
    )
  }


  return(ts_return)
}
# Method definition for s4 generic
#' @describeIn get_market_return get market return timeseries from a database of gta_db class
#' @export
setMethod(
  "get_market_return",
  signature(stock_db = "gta_db"),
  function(stock_db,
             period_type = c("day", "month", "year"),
             period_date = c("start", "end"),
             output_type = c("timeSeries", "tibble"),
             ...) {
    get_market_return.gta_db(
      stock_db = stock_db,
      period_type = period_type,
      period_date = period_date,
      output_type = output_type
    )
  }
)

# Get indicators from specified data source(table/file) in stock_db
# Method definition for s3 generic
#' @describeIn get_indicators_from_source   get indicator timeseries from
#' specified source in a database of gta_db class
#'
#' @export
get_indicators_from_source.gta_db <- function(stock_db,
                                              indicator_source,
                                              indicator_codes = NULL,
                                              ouput_format = c("long", "wide"),
                                              report_type_field = c("Typrep"),
                                              date_fields = c(
                                                "date", "Accper",
                                                "Trddt", "Trdmnt", "Trdynt",
                                                "Clsdt", "Shrchdgt"
                                              ),
                                              retain_fields = c("Stkcd", "Indcd")) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(indicator_source) || !is.character(indicator_source)) {
    stop("data source name must be character string")
  }

  if (!is.null(indicator_codes) & !is.character(indicator_codes)) {
    stop("indicator_list name must be character vector")
  }

  success <- TRUE

  # get dataset according different type of data source
  # if source_name is not a filename, data source should be a table
  is_table <- (tools::file_ext("source_name") == "")
  ds_indicators_raw <- NULL
  if (is_table) {
    ds_indicators_raw <- get_table_dataset(stock_db,
      table_name = indicator_source
    )
    if (is.null(ds_indicators_raw)) success <- FALSE
  } else {
    # TODO: get dataset from file
  }


  # transform indicators data
  ds_indicators <- NULL
  if (success) {
    ds_indicators <- tibble::as.tibble(ds_indicators_raw)
    ds_field_names <- names(ds_indicators)
    output_fields <- NULL
    output_indicators <- NULL
    output_non_indicators <- NULL

    # determine indicators output
    if (!is.null(indicator_codes)) {
      # output specified indicators
      indicator_codes <- tolower(indicator_codes)
      indicators_is_existed <- indicator_codes %in% ds_field_names
      output_indicators <- c(indicator_codes)
    } else {
      # output all indicators if indicator_list is null
      indicators_is_existed <- TRUE
      output_indicators <- c("dplyr::everything()")
    }

    # tranfrom fields for output
    if (all(indicators_is_existed)) {

      # filter consolidated report data
      if (!is.null(report_type_field) & is.character(report_type_field)) {
        typrep <- tolower(report_type_field)[[1]]
        if (typrep %in% ds_field_names) {
          # typrep == "A" means consolidated report
          ds_indicators <- dplyr::filter(
            ds_indicators,
            !!rlang::sym(typrep) == "A"
          )
          if (NROW(ds_indicators) == 0) {
            success <- FALSE
            # invalid typrep field
            msg <- sprintf(
              "there isn't data of consolidated report, since %s field doesn't contain 'A'",
              typrep
            )
            stop(msg)
          }
        }
      }

      # rename and transform date-related field if needed
      if (!is.null(date_fields) & (length(date_fields) > 0) &
        is.character(date_fields)) {
        date_fields <- tolower(date_fields)
        match_date_fields <- ds_field_names[ds_field_names %in% date_fields]
        if (length(match_date_fields) >= 1) {
          # rename date field as "date" if needed
          date_field <- match_date_fields[[1]]
          ds_indicators <- dplyr::rename(ds_indicators, date = !!date_field)

          # put "date" field into output non-indicator field
          output_non_indicators <- c(output_non_indicators, "date")

          if (length(match_date_fields) > 1) {
            msg <- sprintf(
              "More than one possible date field(%s), the first field(%s) is renamed to 'date'",
              stringr::str_c(match_date_fields, collapse = ","),
              date_field
            )
          } else {
            msg <- sprintf("date field(%s) is renamed to 'date'", date_field)
          }

          message(msg)
        }
      }


      # transform date field
      if ("date" %in% names(ds_indicators)) {
        ts_date <- NULL

        # transform date field into date format
        if (lubridate::is.timepoint(ds_indicators$date)) {
          # translate all time format into date format
          ts_date <- lubridate::as_date(ds_indicators$date)
        } else if (is.character(ds_indicators$date)) {
          # try translate charactor date as possible
          ts_date <- lubridate::parse_date_time(ds_indicators$date,
            orders = c("y", "ym", "yW", "ymd")
          )
          ts_date <- lubridate::as_date(ts_date)
        } else {
          # invalid type for date field
          msg <- sprintf(
            "fail to translate date field of type of %s",
            typeof(ds_indicators$date)
          )
          stop(msg)
        }

        # translate date into last day of period, add period field
        if (!is.null(ts_date)) {

          # turn it into the last day of period
          dates_period <- guess_dates_period(ts_date)
          switch(dates_period,
            "D" = {
              period_unit <- "day"
            },
            "M" = {
              period_unit <- "month"
            },
            "Q" = {
              period_unit <- "quarter"
            },
            "Y" = {
              period_unit <- "year"
            },
            "U" = {
              period_unit <- "day"
            }
          )

          # last day of period = ceiling_date -1
          ceiling_date <- lubridate::ceiling_date(ts_date,
            unit = period_unit,
            change_on_boundary = TRUE
          )
          ts_date <- ceiling_date - 1

          # add period field and translated date field
          ds_indicators <- dplyr::mutate(ds_indicators,
            period = period_unit,
            date = ts_date
          )

          # put "period" field into output non-indicators field
          output_non_indicators <- c(output_non_indicators, "period")
        }
      }

      # Output indicators fields from dataset

      # put remain fields into output field
      if (!is.null(retain_fields) & (length(retain_fields) > 0) &
        is.character(retain_fields)) {
        retain_fields <- tolower(retain_fields)
        match_retain_fields <- ds_field_names[ds_field_names %in% retain_fields]

        # put "retain" field into output non-indicators field
        if (length(match_retain_fields) >= 1) {
          output_non_indicators <- c(output_non_indicators, match_retain_fields)
        }
      }

      # output select fields
      output_fields <- c(output_non_indicators, output_indicators)
      ds_indicators <- dplyr::select(
        ds_indicators,
        !!!rlang::parse_exprs(output_fields)
      )

      # message results
      if (!is.null(indicator_codes)) {
        msg <- sprintf(
          "get indicators: %s(%s) from %s",
          stringr::str_c(indicator_codes, collapse = ","),
          stringr::str_c(code2name(stock_db, indicator_codes, type = "field"),
            collapse = ","
          ),
          indicator_source
        )
      } else {
        msg <- sprintf(
          "get all indicators from %s",
          indicator_source
        )
      }

      message(msg)
    } else {

      # some indicators miss from result dataset
      msg <- sprintf(
        "indicators: %s aren't in the table of %s",
        stringr::str_c(indicator_codes[!indicators_is_existed], collapse = ","),
        indicator_source
      )
      warning(msg)

      success <- FALSE
    }
  } else {
    success <- FALSE
  }

  # trasnform by ouptput format
  if (success) {
    ouput_format <- match.arg(ouput_format)

    # by default, ds_indicatgor is shortrer and wider format
    # transform it into longer and narrower format if specified
    if (ouput_format == "long") {
      is_numeric_field <- purrr::map_lgl(
        ds_indicators,
        ~inherits(., "numeric")
      )
      value_fields <- names(ds_indicators)[is_numeric_field]
      if (length(value_fields) > 0) {
        ds_indicators <- ds_indicators %>%
          tidyr::gather(
            key = "ind_name",
            value = "ind_value",
            !!value_fields
          )
      } else {
        # raise error for no numeric fields
        msg <- sprintf(
          "can't tranfrom into long-formt dataset, since no numeric field in the table of %s",
          indicator_source
        )
        stop(msg)
      }
    }
  }

  return(ds_indicators)
}
# Method definition for s4 generic
#' @describeIn get_indicators_from_source  get indicator timeseries from
#'  specified source in a database of gta_db class
#' @export
setMethod(
  "get_indicators_from_source",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_source, indicator_codes, ouput_format, ...) {
    get_indicators_from_source.gta_db(
      stock_db, indicator_source,
      indicator_codes, ouput_format
    )
  }
)

# Get indicators timeseries from stock_db
# Method definition for s3 generic
#' @describeIn get_indicators get indicator timeseries from
#'  a database of gta_db class
#' @export
get_indicators.gta_db <- function(stock_db, indicator_codes) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_not_null(indicator_codes)
  assertive::assert_is_character(indicator_codes)

  success <- TRUE

  # get file table name mapping for referece
  gta_profile_name <- get_profile.gta_db(stock_db)

  # get indcator info of matched factor
  matched_indicators <- profile_get_indicators(
    gta_profile_name,
    indicator_codes
  )
  if (!is.null(matched_indicators)) {
    # build table_list for fetching indicators
    indicator_sources <- matched_indicators %>%
      dplyr::filter(ind_code %in% indicator_codes) %>%
      dplyr::group_by(ind_source) %>%
      tidyr::nest()
  } else {
    success <- FALSE
  }


  # get indicators dataset from indicator sources
  ds_combined_indicators <- NULL
  if (success) {
    for (i in seq_len(nrow(indicator_sources))) {
      indicator_source <- indicator_sources[i, ]$ind_source
      indicator_params <- indicator_sources[i, ]$data[[1]]
      indicator_codes <- indicator_params$ind_code

      # Get a indicator dataset from database
      if (success) {
        ds_indicators <- get_indicators_from_source.gta_db(stock_db,
          indicator_source = indicator_source,
          indicator_codes = indicator_codes,
          ouput_format = "long"
        )
        if (!is.null(ds_indicators)) {
        } else {
          success <- FALSE
        }
      }

      # bind the factors dataset
      if (success) {
        if (is.null(ds_combined_indicators)) {
          ds_combined_indicators <- ds_indicators
        } else {
          ds_combined_indicators <- ds_combined_indicators %>%
            dplyr::bind_rows(ds_indicators)
          # dplyr::bind_rows(ds_indicators, .id = "table")
        }
      }
    }
  }

  return(ds_combined_indicators)
}
# Method definition for s4 generic
#' @describeIn get_indicators get indicator timeseries from
#'  a database of gta_db class
#' @export
setMethod(
  "get_indicators",
  signature(stock_db = "gta_db"),
  function(stock_db, indicator_codes, ...) {
    get_indicators.gta_db(stock_db, indicator_codes)
  }
)

# Get factors timeseries from stock_db
# Method definition for s3 generic
#' @describeIn get_factors get factor timeseries from
#'  a database of gta_db class
#' @export
get_factors.gta_db <- function(stock_db, factor_codes) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  assertive::assert_is_not_null(factor_codes)
  assertive::assert_is_character(factor_codes)

  success <- TRUE

  # get file table name mapping for referece
  gta_profile_name <- get_profile.gta_db(stock_db)

  # get indcator info of matched factor
  matched_factors_info <- profile_get_factors(gta_profile_name, factor_codes)
  if (is.null(matched_factors_info)) {
    success <- FALSE
  }

  # get indicators for matched factors
  ds_indicators <- NULL
  if (success) {
    indicator_codes <- matched_factors_info$indicator_code
    ds_indicators <- get_indicators.gta_db(stock_db,
      indicator_codes = indicator_codes
    )
    if (is.null(ds_indicators)) {
      success <- FALSE
    }
  }

  # translate indicators into factors
  ds_factors <- NULL
  if (success) {

    # use lookup table to translate value of ind_name to value of factor_name
    lookup <- factor_codes
    names(lookup) <- tolower(indicator_codes)
    ds_factors <- ds_indicators %>%
      dplyr::mutate(ind_name = unname(lookup[ind_name]))

    # change colname: ind_name/value into factor_name/value
    ds_factors <- ds_factors %>%
      dplyr::rename(
        factor_name = ind_name,
        factor_value = ind_value
      )


  }

  return(ds_factors)
}
# Method definition for s4 generic
#' @describeIn get_factors get factor timeseries from
#'  a database of gta_db class
#' @export
setMethod(
  "get_factors",
  signature(stock_db = "gta_db"),
  function(stock_db, factor_codes, ...) {
    get_factors.gta_db(stock_db, factor_codes)
  }
)

# Get factors Info from stock_db
# Method definition for s3 generic
#' @describeIn get_factors_info get factors info from a database of gta_db class
#' @export
get_factors_info.gta_db <- function(stock_db,
                                    factor_codes = NULL,
                                    factor_groups = NULL) {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  # get file table name mapping for referece
  gta_profile_name <- get_profile.gta_db(stock_db)

  # get factors info of matched by factor_codes/factor_group
  matched_factors_info <- profile_get_factors(gta_profile_name,
    factor_codes = factor_codes,
    factor_groups = factor_groups
  )

  # build specified result of matched factors
  matched_factors_info <- matched_factors_info %>%
    dplyr::select(
      factor_code = factor_code,
      factor_name = factor_name,
      factor_type = factor_type,
      factor_group = factor_group,
      factor_description = factor_description
    )

  return(matched_factors_info)
}

# Method definition for s4 generic
#' @describeIn get_factors_info get factors info from a database of gta_db class
#' @export
setMethod(
  "get_factors_info",
  signature(stock_db = "gta_db"),
  function(stock_db, factor_codes, factor_groups, ...) {
    get_factors_info.gta_db(stock_db, factor_codes, factor_groups)
  }
)



# Non-generic internal functions for gta_db operation ---------------------------------

#' Create a stock_filed_list for a database of gta_db class
stock_field_list.gta_db <- function(stock_db) {
  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build field_name list
  field_name_list <- NULL
  table_name <- stock_db$table_list[["gta_fieldname_list"]]
  field_list.df <- get_table_dataset.gta_db(stock_db, table_name)
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

  # build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {
    codes <- ds_trd_company.df[, "stkcd"]
    # Coerce stock code to 6 digit of characters of format of "xxxxxxx"
    if (!is.character(codes)) {
      codes <- stringr::str_pad(codes, width = 6, pad = "0")
      msg <- "Coerce stock cd to character of 6 digits if it were number"
      warnings(msg)
    }

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

  # build stock_name_list
  stock_name_list <- NULL
  table_name <- stock_db$table_list[["TRD_Co"]]
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db, table_name)
  if (!is.null(ds_trd_company.df)) {
    ds_indistry <- ds_trd_company.df %>%
      dplyr::select(nnindcd, nnindnme) %>%
      dplyr::distinct()

    codes <- ds_indistry[, "nnindcd"]
    codes <- as.character(codes)
    names <- ds_indistry[, "nnindnme"]

    stock_name_list <- code_name_list(codes, names)
  } else {
    warning("can't create code_name_list due to failing
            to get data from stock db")
  }

  return(stock_name_list)
}
