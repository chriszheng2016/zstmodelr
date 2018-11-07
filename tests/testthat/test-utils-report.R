context("Tests for report utitlity functions")

test_that("build_report, with various arguments", {

  report_template <- "./data/test_report.Rmd"

  # build_report with default argurments ====
  build_report(report_template = report_template)
  expect_true(file.exists("output/test_report.html"))

  # build_report with html_document argurments ====
  build_report(report_template = report_template,
               report_params = list(custom_argument = "Hello ZB"),
               output_format = "html_document",
               output_sn = "01",
               output_dir = "output")
  expect_true(file.exists("output/test_report_01.html"))

  # build_report with pdf_document argurments ====
  build_report(report_template = report_template,
               report_params = list(custom_argument = "Hello ZB"),
               output_format = "pdf_document",
               output_sn = "01",
               output_dir = "output")
  expect_true(file.exists("output/test_report_01.pdf"))

  # build_report with word_document argurments ====
  build_report(report_template = report_template,
               report_params = list(custom_argument = "Hello ZB"),
               output_format = "word_document",
               output_sn = "01",
               output_dir = "output")
  expect_true(file.exists("output/test_report_01.docx"))

  # build_report with rticles::ctex argurments ====
  report_template <- "./data/test_report_ctex.Rmd"
  build_report(report_template = report_template,
               report_params = list(custom_argument = "Hello ZB"),
               output_format = "rticles::ctex",
               output_sn = "01",
               output_dir = "output")
  expect_true(file.exists("output/test_report_ctex_01.pdf"))


})
