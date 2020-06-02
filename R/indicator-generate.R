
#' Generate indicators in batch mode
#'
#' Generate customized indicators of stock_db in batch, and save into files.
#'
#' @details
#'
#'    There are two methods of generating indicators:
#' \itemize{
#'   \item \strong{formal production}: produce final result of indicators on full dataset
#'    from stock_db, which will take longer time ;
#'   \item \strong{validating definition}: produce trial results of indicators on small
#'   dataset from stock_db, which will take shorter time.
#'   }
#'
#' @param stock_db  A stock database object to operate.
#' @param ds_indicator_defs  A dataframe of indicator definition to generate.
#' @param validate_def A logic flag to determine whether to validate indicator
#'   definition or not.Default FALSE, means to produce indicators on full
#'   dataset, TRUE means to validate definition on small datasets.
#' @param validate_stkcds A character vector of stock codes used for validating
#'   indicator definition.
#' @param parallel   A logic to determine whether to use parallel processing.
#'   Default TRUE means to use parallel processing.
#' @param log_file_prefix  A character of log file prefix to name log file. Log
#'   file is named as format of "log_file_prefix(current).csv" Default is
#'   "generate_indicator_log".
#' @param log_dir Path to save log file. If NULL, don't save log file by default
#'   "./log".
#'
#' @family indicator generate functions
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
generate_indicators <- function(stock_db,
                                ds_indicator_defs,
                                validate_def = FALSE,
                                validate_stkcds = c(
                                  "600031", "000157",
                                  "600066", "000550"
                                ),
                                parallel = TRUE,
                                log_file_prefix = "generate_indicator_log",
                                log_dir = "./log") {

  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ds_indicator_defs)
  assertive::assert_is_logical(validate_def)
  if (validate_def) assertive::assert_is_character(validate_stkcds)
  assertive::assert_is_logical(parallel)
  assertive::assert_is_character(log_file_prefix)
  assertive::assert_is_character(log_dir)

  success <- TRUE

  # load vars dataset for generating indicators
  ds_all_vars <- get_indicator_vars(stock_db,
    indicator_defs = ds_indicator_defs
  )
  if (is.null(ds_all_vars)) success <- FALSE

  # Set validate params
  debug <- FALSE
  if (success) {
    if (validate_def) {

      # filter vars to small scale dataset to validate indicator definition
      validate_stkcds <- c(validate_stkcds, NA)
      ds_all_vars <- ds_all_vars %>%
        dplyr::filter(stkcd %in% validate_stkcds)

      # turn on debug on create indicators
      debug <- TRUE

      msg <- sprintf(
        "\nOnly validate definition of indicators on %s, not a real production!\n",
        paste(validate_stkcds, collapse = ",")
      )
      rlang::inform(msg)
    }
  }

  # add attribute of indcd
  if (success) {
    new_attr_indcd <- ind_attr_def_indcd(stock_db)
  }

  # setup log info params
  log_file_path <- NULL
  ds_log <- ds_indicator_defs %>%
    dplyr::select(ind_code, ind_category, ind_source) %>%
    dplyr::mutate(success = FALSE)

  # generate indicators in batch
  if (success) {
    for (i in seq_len(NROW(ds_indicator_defs))) {
      indicator_def <- ds_indicator_defs[i, ]
      ind_def_fun <- indicator_def$ind_def_fun[[1]]
      ind_vars <- indicator_def$ind_vars[[1]]
      key_fields <- indicator_def$ind_keys[[1]]

      success <- TRUE
      if (!is.null(ind_def_fun)) {
        msg <- sprintf(
          "\nGenerate indicator: %s(%s) ...\n", indicator_def$ind_code,
          indicator_def$ind_name
        )
        rlang::inform(msg)

        # filter vars of the indicator
        ds_def_vars <- ds_all_vars %>%
          dplyr::filter(ind_code %in% ind_vars)

        # create a indicator from vars dataset.
        ts_indicator <- create_indicator(ds_def_vars,
          ind_def_fun = ind_def_fun,
          debug = debug,
          date_index_field = "date",
          key_fields = key_fields,
          parallel = parallel
        )
        if (!is.null(ts_indicator)) {
          msg <- sprintf(
            "..succeed in creating %s..",
            indicator_def$ind_code
          )
        } else {
          msg <- sprintf(
            "..fail to create %s..",
            indicator_def$ind_code
          )
          success <- FALSE
        }
        rlang::inform(msg)

        # add attribute of industry code(indcd)
        # Notice: indcd must use stkcd as key_fields
        if (success) {
          if (!("indcd" %in% names(ts_indicator)) ||
            any(is.na(ts_indicator$indcd))) {
            # add indcd attribute if no indcd or indcd is not completed
            ts_indicator <- modify_indicator(
              ts_indicator = ts_indicator,
              modify_fun = new_attr_indcd,
              replace_exist = TRUE,
              date_index_field = "date",
              key_fields = "stkcd",
              parallel = parallel
            )
          }
          if (!is.null(ts_indicator)) {
            msg <- sprintf(
              "..succeed in adding indcd to %s..",
              indicator_def$ind_code
            )
          } else {
            msg <- sprintf(
              "..fail to add indcd to %s..",
              indicator_def$ind_code
            )
            success <- FALSE
          }
          rlang::inform(msg)
        }


        # save indicators into source
        if (success) {
          save_indicators_with_default <- purrr::possibly(
            save_indicators_to_source,
            otherwise = FALSE,
            quiet = TRUE
          )
          result <- save_indicators_with_default(stock_db,
            indicator_source = indicator_def$ind_source,
            ts_indicators = ts_indicator
          )

          if (is.null(result)) {
            msg <- sprintf(
              "..succeed in saving %s..",
              indicator_def$ind_code
            )
          } else {
            msg <- sprintf(
              "..fail to save %s..",
              indicator_def$ind_code
            )
            success <- FALSE
          }
          rlang::inform(msg)
        }

        if (success) {
          msg <- sprintf(
            "Generate indicator successfully, save %s(%s) in %s.\n",
            indicator_def$ind_code,
            indicator_def$ind_name,
            indicator_def$ind_source
          )
          rlang::inform(msg)
        } else {
          msg <- sprintf(
            "Fail to generate indicator: %s(%s).\n",
            indicator_def$ind_code,
            indicator_def$ind_name
          )
          rlang::warn(msg)
        }
      } else {
        msg <- sprintf(
          "Cann't to generate indicator: %s(%s), because ind_def_fun is NULL.\n",
          indicator_def$ind_code,
          indicator_def$ind_name
        )
        rlang::warn(msg)
      }

      # record results
      ds_log$success[i] <- success
      # write log for generating operation
      log_file_path <- save_log(ds_log,
        log_file_prefix = log_file_prefix,
        log_dir = log_dir
      )
    }
  }

  # Notify user log fileinfo
  if (!is.null(log_file_path)) {
    msg <- sprintf(
      "\nFor more info about generated indicators, plese check %s for detail.\n",
      log_file_path
    )

    rlang::inform(msg)
  }
}


#' Delete indicators in batch mode
#'
#' Delete customized indicators of stock_db in batch, which clear all customized
#' built indicator files.
#'
#' @param stock_db  A stock database object to operate.
#' @param ds_indicator_defs  A dataframe of indicator definition to delete.
#'
#' @family indicator generate functions
#'
#' @return NULL invisibly. Raise error if anything goes wrong.
#'
#' @export
delete_indicators <- function(stock_db,
                              ds_indicator_defs) {
  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ds_indicator_defs)

  # remove indicators files in indicator dir
  dir_indicators <- dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")
  path_target_files <- paste0(dir_indicators, "/", ds_indicator_defs$ind_source)
  quiet_file_remove <- purrr::possibly(file.remove, otherwise = FALSE)
  purrr::walk(path_target_files, ~ quiet_file_remove(.x))

  return(invisible(NULL))
}

#' Backup indicators in batch mode
#'
#' Backup customized indicators of stock_db in batch into backup dir.
#'
#' @param stock_db  A stock database object to operate.
#' @param ds_indicator_defs  A dataframe of indicator definition to delete.
#' @param backup_dir     A path of dir to save backup indicators. Default
#'   "backup" is the subdirectory of indicator dir which is specified by
#'   `dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")`
#'
#' @family indicator generate functions
#'
#' @return Path of backup dir. Raise error if anything goes wrong.
#'
#' @export
backup_indicators <- function(stock_db,
                              ds_indicator_defs,
                              backup_dir = "backup") {
  # validate params
  stopifnot(!is.null(stock_db), inherits(stock_db, "stock_db"))
  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }
  assertive::assert_is_data.frame(ds_indicator_defs)
  assertive::assert_is_character(backup_dir)

  dir_indicators <- dir_path_db(stock_db, "DIR_DB_DATA_INDICATOR")

  # make backup dir existed
  backup_date <- as.character(Sys.Date())
  backup_dir_path <- file.path(dir_indicators, backup_dir, backup_date)
  if (!dir.exists(backup_dir_path)) {
    dir.create(backup_dir_path)
  }

  # backup indicators files into indicator dir
  path_source_files <- paste0(dir_indicators, "/", ds_indicator_defs$ind_source)
  quiet_file_copy <- purrr::possibly(file.copy, otherwise = FALSE)
  purrr::walk(path_source_files, ~ quiet_file_copy(.x, to = backup_dir_path))

  return(backup_dir_path)
}
