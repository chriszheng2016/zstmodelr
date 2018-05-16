#' ZStockModels: A package for computating the notorious bar statistic.
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section tools functions:
#' The add functions ...
#'
#' @docType package
#' @name zstmodelr
#'
#' @import rlang
#' @import methods




NULL

# Register parallel backend for parallel process
cores_for_process <- parallel::detectCores() - 1
doParallel::registerDoParallel(cores = cores_for_process)
