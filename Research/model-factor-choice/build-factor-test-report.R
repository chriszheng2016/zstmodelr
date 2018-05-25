library(zstmodelr)


# Build factors test report
factor_test_report <- function(factors_list,
         backtest_start_date = "2016-01-01",
         backtest_end_date  =  "2017-12-31",
         report_type = c("batch", "single"),
         report_template = "Research/model-factor-choice/model-factor-choice.Rmd",
         output_format = "html_document",
         output_dir = "output"
         )
{

  params <- list( backtest_start_date = backtest_start_date,
                  backtest_end_date = backtest_end_date,
                  debug_mode = FALSE)

  report_type <- match.arg(report_type)
  if (report_type == "single") {
    for (factor in factors_list) {

      msg <- sprintf("generate report for factor:%s ...\n", factor)
      message(msg)

      params <- c(list(factors_list = factor ), params)
      build_report(report_template = report_template,
                   report_params = params,
                   output_format = output_format,
                   output_sn = factor,
                   output_dir = output_dir)
    }
  } else{

    msg <- sprintf("generate report for factor:%s ...\n",
                   stringr::str_c(factors_list, collapse = ","))
    message(msg)

    params = c(list(factors_list = factors_list), params)
    build_report(report_template = report_template,
                 report_params = params,
                 output_format = output_format,
                 output_sn = NULL,
                 output_dir = output_dir)
   }
}


# Build report...

# 读取因子信息
stock_db <- stock_db(gta_db, "GTA_SQLData")
open_stock_db(stock_db)
ds_factors_info <- get_factors_info(stock_db, factor_groups = "Financial Risk")

factors_info_by_group <- ds_factors_info %>%
   dplyr::group_by(factor_group) %>%
   dplyr::summarise(factors_list = list(factor_code))

for (i in seq_along(factors_info_by_group)) {

  factors_group <- factors_info_by_group$factor_group[i]
  factors_list <- factors_info_by_group$factors_list[i][[1]]

  msg <- sprintf("generate report for factor group of :%s ...\n", factors_group)
  message(msg)

  factor_test_report(factors_list,
                     backtest_start_date = "2016-01-01",
                     backtest_end_date  =  "2017-12-31",
                     report_type = "batch",
                     output_format = "html_document"
  )

}


# fundamental_factors_list <- c("GPM", "ROCE", "PE", "PB", "CUR", "QR")
# fundamental_factors_list <- c("PE")


# fundamental_factors_list <- factor_test_report(fundamental_factors_list,
#                    backtest_start_date = "2016-01-01",
#                    backtest_end_date  =  "2017-12-31",
#                    report_type = "single",
#                    output_format = "rticles::ctex")


