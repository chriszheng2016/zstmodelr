# Utility functions - report

#' Build report from template
#'
#' It's a wrapper of [rmarkdown::render()] to build report from template by
#' customized arguments.
#'
#'
#' @param report_template A rmd file as report template.
#' See input argument of [rmarkdown::render()] for details.
#' @param report_params   A list of report params, by default NULL.
#' See params argument of [rmarkdown::render()] for details.
#' @param output_format   The R Markdown output format to convert to.
#' See output_format argument of [rmarkdown::render()] for details.
#' @param output_sn   Serial number or id for output file. Default NULL
#' @param output_dir  Path of dir to save output file. Default "output" means to
#'   save file in output in current dir. If NULL save in directory of template
#'   file, otherwise in output_dir.
#' @param quiet  A logic flag of whether to suppress printing of the pandoc
#'   command line.
#' @param ... Arguments passed to [rmarkdown::render()].
#'
#' @family utils_report
#'
#' @return No return.
#'
#' @export
build_report <- function(report_template,
                         report_params = NULL,
                         output_format = NULL,
                         output_sn = NULL,
                         output_dir = "output",
                         quiet = TRUE,
                         encoding = "UTF-8",
                         ...) {
  assertive::assert_all_are_non_empty_character(report_template)

  working_dir <- fs::path_dir(fs::path_norm(report_template))
  if (is.null(output_dir)) {
    output_dir <- working_dir
  }

  output_filename_main <- fs::path_ext_remove(fs::path_file(report_template))
  output_filename <- if (!is.null(output_sn)) {
    paste0(
      output_filename_main, "_", as.character(output_sn)
    )
  } else {
    output_filename_main
  }

  # Set output_filename for multi-formats if needed, .e.g:
  # When output_format ="all", all formats in template will be produce,
  # which need provide multiple



  if (is.character(output_format)) {
    if ((length(output_format) == 1) && (output_format == "all")) {
      output_format_counts <- length(
        rmarkdown::all_output_formats(report_template)
      )
    } else {
      output_format_counts <- length(output_format)
    }
    output_filename <- rep(output_filename, output_format_counts)
  }

  msg <- sprintf("Get report template from %s ...\n", report_template)
  rlang::inform(msg)

  result <- rmarkdown::render(
    input = report_template,
    params = report_params,
    output_format = output_format,
    output_file = output_filename,
    output_dir = output_dir,
    intermediates_dir = output_dir,
    run_pandoc = TRUE,
    quiet = quiet,
    encoding = encoding,
    ...
  )

  msg <- sprintf(
    "Generate %s into %s successfully.\n",
    fs::path_file(result), output_dir
  )
  if(length(msg) >1) {
    msg <- paste0(msg, collapse = "\n")
  }

  rlang::inform(msg)
}

#' Custom Knit function for RStudio to build report
#'
#' A wrapper of [build_report()] as a custom knit function for Knit button
#' in RStuido.
#'
#' @param input The input file to be rendered as as report template, which is
#' required by customized knit function.
#'
#' @inheritParams build_report
#'
#' @export
knit_report <- function(input,
                        report_params = "ask",
                        output_format = NULL,
                        output_sn = Sys.Date(),
                        output_dir = "output",
                        ...) {

  # Build report with parameters
  build_report(
    report_template = input,
    report_params = report_params,
    output_format = output_format,
    output_sn = output_sn,
    output_dir = output_dir,
    quiet = FALSE,
    envir = globalenv(),
    ...
  )
}
