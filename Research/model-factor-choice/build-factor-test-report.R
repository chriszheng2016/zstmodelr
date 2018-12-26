library(zstmodelr)


# build factors test report
factor_test_report <- function(factors_list,
                               factors_group = NULL,
                               test_start_date = "2016-01-01",
                               test_end_date = "2017-12-31",
                               test_stkcds = NULL,
                               report_type = c("batch", "single"),
                               report_template = "model-factor-choice.Rmd",
                               output_format = "html_document",
                               output_dir = "output") {

  # validate params
  assertive::assert_is_not_null(factors_list)
  assertive::assert_is_character(factors_list)
  assertive::assert_is_character(test_start_date)
  assertive::assert_is_character(test_end_date)
  if (!is.null(factors_group)) assertive::assert_is_character(factors_group)
  if (!is.null(test_stkcds)) assertive::assert_is_character(test_stkcds)

  # build report params
  params <- list(
    test_start_date = test_start_date,
    test_end_date = test_end_date,
    test_stkcds = test_stkcds
  )

  report_type <- match.arg(report_type)
  if (report_type == "single") {
    for (factor in factors_list) {
      msg <- sprintf("..generate test report for factor:%s ...\n", factor)
      message(msg)

      params <- c(list(factors_list = factor), params)
      build_report(
        report_template = report_template,
        report_params = params,
        output_format = output_format,
        output_sn = factor,
        output_dir = output_dir
      )
    }
  } else {
    msg <- sprintf(
      "..generate test report for factor:%s ...\n",
      stringr::str_c(factors_list, collapse = ",")
    )
    message(msg)

    params <- c(list(factors_list = factors_list), params)
    build_report(
      report_template = report_template,
      report_params = params,
      output_format = output_format,
      output_sn = factors_group,
      output_dir = output_dir
    )
  }
}


# build factor test reports in batch mode
build_factor_test_reports <- function(dsn = c("GTA_SQLData"),
                                      test_start_date = "1990-01-01",
                                      test_end_date = as.character(Sys.Date()),
                                      test_portfolio = NULL,
                                      report_type = c("batch", "single"),
                                      output_format = "html_document") {

  # conect to target stock db
  stock_db <- stock_db(gta_db, dsn)
  open_stock_db(stock_db)
  init_stock_db(stock_db)

  # read test portfolio
  test_stkcds <- "all"
  if (!is.null(test_portfolio)) {
    if (file.exists(test_portfolio)) {
      ds_test_portfolio <- readr::read_csv(test_portfolio)
      test_stkcds <- stringr::str_trim(ds_test_portfolio$stkcd)
      msg <- sprintf(
        "Test factors on stock portfolio(%s)",
        paste0(test_stkcds, collapse = ",")
      )
      rlang::inform(msg)
    } else {
      msg <- sprintf(
        "Test portfolio file(%s) dosen't exist, test factors on all stocks",
        test_portfolio
      )
      rlang::warn(msg)
    }
  } else {
    msg <- sprintf("Test factors on all stocks")
    rlang::inform(msg)
  }

  # read factor info
  ds_factors_info <- get_factors_info(stock_db, factor_groups = NULL)

  # build reports by factor_group
  factors_info_by_group <- ds_factors_info %>%
    dplyr::group_by(factor_group) %>%
    dplyr::summarise(factors_list = list(factor_code))

  for (i in seq_along(factors_info_by_group$factor_group)) {

    factors_group <- factors_info_by_group$factor_group[i]
    factors_list <- factors_info_by_group$factors_list[i][[1]]

    msg <- sprintf(
      "\nBuild report for factor group of %s ...\n",
      factors_group
    )
    rlang::inform(msg)

    # build report for a group
    factor_test_report(factors_list,
      factors_group = factors_group,
      test_start_date = test_start_date,
      test_end_date = test_end_date,
      test_stkcds = test_stkcds,
      report_type = "batch",
      output_format = "html_document"
    )
  }

  close_stock_db(stock_db)
}

# Run build_factor_test_reports
#
# Build factor test reports on all stocks
# build_factor_test_reports()
#
# Build factor test reports on test stocks
# build_factor_test_reports(test_portfolio = "test_portfolio.csv")
