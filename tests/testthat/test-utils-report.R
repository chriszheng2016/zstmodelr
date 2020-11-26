# Tests for utility functions of report ----

context("Tests for utility functions of report")

test_that("build_report, with various arguments", {
  report_template_en <- "./data/test_report.Rmd"

  # build_report with default arguments ====
  build_report(report_template = report_template_en)
  expect_true(file.exists("output/test_report.nb.html"))

  # build_report with various arguments ====

  #>> build_report with output_format: html_document ----
  build_report(
    report_template = report_template_en,
    report_params = list(custom_argument = "Hello ZB"),
    output_format = "html_document",
    output_sn = "01",
    output_dir = "output"
  )
  expect_true(file.exists("output/test_report_01.html"))

  #>> build_report with output_format: html_notebook ----
  build_report(
    report_template = report_template_en,
    report_params = list(custom_argument = "Hello ZB"),
    output_format = "html_notebook",
    output_sn = "01",
    output_dir = "output"
  )
  expect_true(file.exists("output/test_report_01.nb.html"))

  #>> build_report with output_format: word_document ----
  build_report(
    report_template = report_template_en,
    report_params = list(custom_argument = "Hello ZB"),
    output_format = "word_document",
    output_sn = "01",
    output_dir = "output"
  )
  expect_true(file.exists("output/test_report_01.docx"))

  # Due to checking errors in Github actions led by installing tinytex, we have
  # to skip tests about pdf
  skip_on_ci()

  #>> build_report with output_format: pdf_document ----
  # suppress deprecated warnings from ctex pkgs
  suppressWarnings(
    build_report(
      report_template = report_template_en,
      report_params = list(custom_argument = "Hello ZB"),
      output_format = "pdf_document",
      output_sn = "01",
      output_dir = "output"
    )
  )

  expect_true(file.exists("output/test_report_01.pdf"))

  #>> build_report with output_format: rticles::ctex ----
  report_template_cn <- "./data/test_report_ctex.Rmd"
  # suppress deprecated warnings from ctex pkgs
  suppressWarnings(
    build_report(
      report_template = report_template_cn,
      report_params = list(custom_argument = "Hello ZB"),
      output_format = "rticles::ctex",
      output_sn = "01",
      output_dir = "output"
    )
  )

  expect_true(file.exists("output/test_report_ctex_01.pdf"))

  #>> build_report with output_format: "all" ----
  # Notice:
  # There is a bug in rmarkdown::render:
  # when producing multiple formats, if 'html_document' is
  # ahead of 'html_notebook', producing html_notebook will
  # remove output file produced by 'html_document'. So I
  # have to make 'html_notebook' ahead of 'html_document' in YMAL in template

  # suppress deprecated warnings from ctex pkgs
  suppressWarnings(
    build_report(
      report_template = report_template_en,
      report_params = list(custom_argument = "Hello ZB"),
      output_format = "all",
      output_sn = "01",
      output_dir = "output"
    )
  )


  expect_true(file.exists("output/test_report_01.html"))
  expect_true(file.exists("output/test_report_01.nb.html"))
  expect_true(file.exists("output/test_report_01.docx"))
  expect_true(file.exists("output/test_report_01.pdf"))

  #>> build_report with output_format: multi-formats ----
  # Notice:
  # There is a bug in rmarkdown::render:
  # when producing multiple formats, if 'html_document' is
  # ahead of 'html_notebook', producing html_notebook will
  # remove output file produced by 'html_document'. So I
  # have to put 'html_notebook' ahead of 'html_document' in
  # variable output_format
  output_format <- c("html_notebook", "html_document",
                     "pdf_document", "word_document")
  # suppress deprecated warnings from ctex pkgs
  suppressWarnings(
    build_report(
      report_template = report_template_en,
      report_params = list(custom_argument = "Hello ZB"),
      output_format = output_format,
      output_sn = "01",
      output_dir = "output"
    )
  )

  expect_true(file.exists("output/test_report_01.html"))
  expect_true(file.exists("output/test_report_01.nb.html"))
  expect_true(file.exists("output/test_report_01.docx"))
  expect_true(file.exists("output/test_report_01.pdf"))


})
