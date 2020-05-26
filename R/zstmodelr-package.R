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


# Register parallel backend for parallel process
# cores_for_process <- parallel::detectCores() - 1
# doParallel::registerDoParallel(cores = cores_for_process)

#Set up for parallel processing
cluster <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cluster)

# Import frequently-used funs from other pkgs
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom stats as.formula coef cor lm mad median na.omit
#' @importFrom stats profile quantile sd shapiro.test t.test time
#' @importFrom utils data head

#' @export
magrittr::`%>%`


# Register parallel backend for parallel process
# cores_for_process <- parallel::detectCores() - 1
# doParallel::registerDoParallel(cores = cores_for_process)

#Set up for parallel processing
cluster <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cluster)


