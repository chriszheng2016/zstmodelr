#' @details
#' The main goals of zstmodelr is to leverage traditional financial theory and
#' modern ML method to build stock models for China stock market.
#'
#' To learn more about zstmodelr, start with the vignettes:
#'  `browseVignettes(package = "zstmodelr")`


#' @section Options:
#' - `testthat.use_colours`: Should the output be coloured? (Default: `TRUE`).
#' - `testthat.summary.max_reports`: The maximum number of detailed test
#'    reports printed for the summary reporter (default: 10).
#' - `testthat.summary.omit_dots`: Omit progress dots in the summary reporter
#'    (default: `FALSE`).

#' @examples
#' \dontrun{
#' library(testthat)
#' a <- 9
#' expect_that(a, is_less_than(10))
#' expect_lt(a, 10)
#' }

#' @keywords internal
"_PACKAGE"


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


cluster <- NULL
parallel_enable <- FALSE

.onLoad <- function(libname, pkgname) {
  #Set up for parallel processing
  if (requireNamespace("parallel", quietly = FALSE)) {
    rlang::inform("Initiate clusters for parallel process...\n")
    cluster <<- parallel::makeCluster(parallel::detectCores() - 1)
    if (requireNamespace("doParallel", quietly = FALSE)) {
      doParallel::registerDoParallel(cluster)
      parallel_enable <- TRUE
    } else {
      rlang::warn("parallel is needed for parallel processing,
                  please install it.\n")
    }
  } else {
    rlang::warn("doParallel is needed for parallel processing,
                please install it.\n")
  }

  if (!parallel_enable) {
    rlang::inform("Parallel process is disabled due failure of initialization.\n")
  }

  op <- options()
  op.zstmodelr <- list(
    zstmodelr.name = "Chris Zheng",
    zstmodelr.desc.author = "Chris Zheng <first.last@example.com> [aut, cre]",
    zstmodelr.parallel = ifelse(parallel_enable, "TRUE", "FALSE")

  )
  toset <- !(names(op.zstmodelr) %in% names(op))
  if (any(toset))
    options(op.zstmodelr[toset])

  invisible()
}


.onUnload <- function(libpath) {
  # Clear up for parallel processing
  if (!is.null(cluster)) {
    rlang::inform("Stop existed clusters for parallel process...\n")
    parallel::stopCluster(cluster)
  }
}


