
#' Build report from template
#'
#' Use data rank as new data
#'
#'
#' @param report_template a rmd file as report template.
#' @param report_params   a list of report parmas, by default NULL.
#' @param output_format   output file format i.e. "html_document","pdf",
#' by default "html_document".
#' @param output_sn  NULL, serial number or id for output file.
#' @param output_dir  dir to save output file, i.e. "output", if NULL,
#' save in directory of template file, otherwise in outpout_dir
#' @quiet TRUE to suppress printing of the pandoc command line.
#'
#' @return
#' @export
#'
#' @examples
build_report <- function(report_template ,
                         report_params = NULL,
                         output_format = c("html_document", "pdf_document",
                                            "word_document", "rticles::ctex"),
                         output_sn = NULL,
                         output_dir = "output",
                         quiet = TRUE,
                         ...)
{

  assertive::assert_all_are_non_empty_character(report_template)

  working_dir = dirname(normalizePath(report_template))

  output_dir <- if (!is.null(output_dir)) {
    paste0(working_dir, "/" , output_dir)
  } else {
    working_dir
  }

  output_filename_main <- stringr::str_split(basename(report_template), pattern = "\\.")[[1]][1]

  output_format = match.arg(output_format)
  outout_filename_ext <- switch(output_format,
                                "html_document" = ".html",
                                "pdf_document" = ".pdf",
                                "word_document" = ".docx",
                                "rticles::ctex" = ".pdf" )

  output_filename <- if (!is.null(output_sn)) {
    paste0(output_filename_main, "_",
           as.character(output_sn), outout_filename_ext)
  } else {
    paste0(output_filename_main, outout_filename_ext)
  }

  msg <- sprintf("generate report for %s ...\n", report_template)
  message(msg)

  rmarkdown::render(input = report_template,
                    params = report_params,
                    output_format = output_format,
                    output_file = output_filename,
                    output_dir = output_dir,
                    quiet = quiet,
                    encoding = "UTF-8",
                    ...)

  msg <- sprintf("generate %s in %s\n", output_filename, output_dir)
  message(msg)

}