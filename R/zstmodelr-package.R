#' @details
#' The main goals of zstmodelr is to leverage traditional financial theory and
#' modern ML method to build stock models for China stock market.
#'
#' To learn more about zstmodelr, start with the vignettes:
#'  `browseVignettes(package = "zstmodelr")`


#' @section Options:
#' - `zstmodelr.data_mgt.guess_max`: maximum number of records to use for
#' guessing column types in importing txt/csv files(default:`20000`).
#' - `zstmodelr.common.parallel`: whether process will be executed in parallel
#'    or not (default: `TRUE`).
#' - `zstmodelr.common.clusters`: number of clusters to be used for parallel
#'    processing(default: `parallel::detectCores() - 1`).

#' @examples
#' \dontrun{
#' library(zstmodelr)
#'
#' # adjust options for reading importing files
#' options(zstmodelr.data_mgt.guess_max = 300000)
#'
#' # read some importing files....
#' ds_import_data <- read_import_file(input_file)
#' }
#' @keywords internal
"_PACKAGE"

# Import frequently-used functions from other packages
#' @importFrom stats mad median sd cor na.omit
#' @importFrom utils head
#' @importFrom graphics par
#' @importFrom rlang .data

## Quiets notes of R CMD check for pipelines codes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("where", ".", ":=")
  )
}


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL


# Import frequently-used funs from other pkgs
#' @importFrom methods new
#' @importFrom stats as.formula coef cor lm mad median na.omit
#' @importFrom stats profile quantile sd shapiro.test t.test time
#' @importFrom utils data head

# Global vars of pkg
.pkg_globals <- new.env(parent = emptyenv())
.pkg_globals$.cluster <- NULL
.pkg_globals$.parallel_log <- "parallel.log"

# Options of pkg
pkg_options <- list(
  zstmodelr.data_mgt.guess_max = 200000,
  zstmodelr.common.parallel = TRUE,
  zstmodelr.common.clusters = parallel::detectCores() - 1
)

.onLoad <- function(libname, pkgname) {

  # Set default options of package
  op <- options()
  toset <- !(names(pkg_options) %in% names(op))
  if (any(toset)) {
    options(as.list(pkg_options)[toset])
  }

  invisible(NULL)
}

.onUnload <- function(libpath) {

  # Stop parallel processing
  disable_parallel(.pkg_globals)
}
