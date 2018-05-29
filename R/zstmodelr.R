#' zstmodelr: A package for building analysis model for China stock market.
#'
#' The main goals of zstmodelr is to leverage traditional finacial theory and
#' modern ML method to build stock models for China stock market.
#'
#' @details
#' In this package, `tidyquant` functions and supporting data sets are
#' provided to seamlessly combine tidy tools with existing quantitative
#' analytics packages. The main advantage is being able to use tidy
#' functions with purrr for mapping and tidyr for nesting to extend modeling to
#' many stocks. See the tidyquant website for more information, documentation
#' and examples.
#'
#'
#' To learn more about zstmodelr, start with the vignettes:
#'  `browseVignettes(package = "zstmodelr")`
#'
#' @docType package
#' @name zstmodelr
#'
#' @@import rlang
#' @@import methods




NULL

# Register parallel backend for parallel process
cores_for_process <- parallel::detectCores() - 1
doParallel::registerDoParallel(cores = cores_for_process)
