# Utility functions - parallel

#' Utils functions to control parallel process
#'
#' Control parallel process for parallel computation.
#'
#'
#' @details There are two levels of controlling parallel process for
#' computation:
#'
#' * Global level: enable/disable parallel process for R session.
#'
#' Through `enable_parallel`, we could set parallel process ready for current R
#' session by initiating back-end clusters which normally are processes running
#' on multiple cores in a computer, and setting `zstmodelr.common.parallel`
#' option to TRUE.
#' we could use `disable_parallel` to shut parallel process by closing
#' back-end clusters and setting `zstmodelr.common.parallel` option to FALSE
#'
#' * Function level: enable/disable parallel process for running functions.
#'
#' In a R session enabling parallel process, we could control a function being
#'  able to running parallel process, for example:
#'
#' ```
#'         compute_indicator(ts_compute_vars, compute_fun, ...,
#'                   date_index_field = "stkcd"
#'                   c("date"), key_fields = NULL,
#'                   parallel = getOption("zstmodelr.common.parallel", TRUE))
#'
#' ```
#'
#' Its argument `parallel` controls whether the function run in parallel process
#' or sequential process, whose default value is from options of
#' `zstmodelr.common.parallel` or TRUE. By default, `zstmodelr.common.parallel`
#' is controlled by `enable_parallel` and `disable_parallel` as well.
#'
#' In addition, `parallel_status` could provide current status of parallel
#' process and configuration options, which return a list including following
#' fields:
#' * cluster: a object with class of c("SOCKcluster", "cluster") containing
#' information about back-end clusters for parallel process.
#' * parallel_log: a path of parallel log file containing log info from parallel
#' process.
#' * foreach_workers: a number of current process workers ready for foreach
#' computation.
#' * zstmodelr.common.clusters: a number of clusters to be used for parallel
#' process.
#' - zstmodelr.common.parallel: a logical as default value for parallel argument
#' of function with ability of parallel computation.
#'
#' @examples
#' \dontrun{
#'
#' # Set clusters number of parallel process
#' options(zstmodelr.common.clusters = 4)
#'
#' # Enable parallel process for the R session
#' enable_parallel()
#'
#' # Inspect status of parallel process
#' parallel_status()
#'
#' # Conduct parallel computation ...
#'
#' # compute indicators by parallel process
#' ds_indicator <- compute_indicator(ds_vars,
#'   compute_fun = ind_def_fun,
#'   date_index_field = "date",
#'   key_fields = "stkcd"
#' )
#'
#' # compute indicators by sequential process
#' ds_indicator <- compute_indicator(ds_vars,
#'   compute_fun = ind_def_fun,
#'   date_index_field = "date",
#'   key_fields = "stkcd",
#'   parallel = FALSE
#' )
#'
#' # Disable parallel process for the R session
#' disable_parallel()
#' }
#' @name utils_parallel
NULL

# Enable parallel process
#' @param env_globals a environment with parallel process information.
#'   Default .pkg_globals means to use global vars of the package. NULL means
#'   to use environment of caller.
#'
#' @param parallel_switch_option a string name of option to turn on/off
#' parallel computation in functions with parallel feature.
#' Default is "zstmodelr.common.parallel".
#'
#' @describeIn utils_parallel enable parallel process by initiating back-end
#'  clusters.
#' @export
enable_parallel <- function(env_globals = .pkg_globals,
                            parallel_switch_option = "zstmodelr.common.parallel") {

  # env_globals must be a environment to store parallel info
  if (!is.null(env_globals)) {
    assertive::assert_is_environment(env_globals)
  } else {
    env_globals <- parent.frame()
  }

  if (!is.null(parallel_switch_option)) {
    assertive::assert_is_character(parallel_switch_option)
  }

  # Get current status of parallel process
  status <- parallel_status(env_globals)
  cluster <- status$cluster
  parallel_log <- status$parallel_log

  # Set up clusters for parallel process
  if (is.null(cluster) || !inherits(cluster, what = "cluster")) {
    if (requireNamespace("parallel", quietly = FALSE)) {
      rlang::inform("Initiate clusters for parallel process...\n")

      # Only use 2 cores in CRAN/Travis/AppVeyor
      cran_check_limt_cores <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
      if (nzchar(cran_check_limt_cores) && cran_check_limt_cores == "TRUE") {
        options(zstmodelr.common.clusters = 2)
        rlang::warn("Only use 2 clusters by CRAN/Travis/AppVeyor.\n")
      }

      cluster <- parallel::makeCluster(
        getOption("zstmodelr.common.clusters", parallel::detectCores() - 1),
        outfile = parallel_log
      )

      if (requireNamespace("doParallel", quietly = FALSE)) {

        # Register clusters
        doParallel::registerDoParallel(cluster)

        # Fix bugs from github action R-CMD-check on windows platform
        # Reason: the package should be loaded into each cluster on windows,
        # otherwise foreach::`%dopar% can't find the parallel function defined
        # in the package to run.
        sysname <- tolower(Sys.info()[["sysname"]])
        if (sysname == "windows") {
          this_pkg <- packageName(rlang::current_env())

          # Function to run on every cluster initially
          cluster_init <- function(require_pkgs) {
            for (pkg in require_pkgs) {
              if (!require(pkg, character.only = TRUE)) {
                cat(paste0("Fail to load ", pkg, "\n"))
              }
            }
          }

          # Call cluster_init on every cluster
          parallel::clusterCall(
            cluster,
            cluster_init,
            require_pkgs = this_pkg
          )
        }

        # Turn on parallel switch options
        if (!is.null(parallel_switch_option)) {
          switch_options <- as.list(rep(TRUE, length(parallel_switch_option)))
          names(switch_options) <- parallel_switch_option
          options(switch_options)
        }

        if (!is.null(env_globals)) {
          env_globals$.cluster <- cluster
          env_globals$.parallel_log <- parallel_log
        }
      } else {
        rlang::warn("parallel is needed for parallel process,
                  please install it.\n")
      }
    } else {
      rlang::warn("doParallel is needed for parallel process,
                please install it.\n")
    }
  } else {
    assertive::assert_is_any_of(cluster, c("SOCKcluster", "cluster"))
    msg <- sprintf(
      "There are already %d clusters been working.\n",
      length(cluster)
    )
    msg <- paste0(msg, "Use parallel_status() to show more information.\n")
    rlang::inform(msg)
  }
}


# Disable parallel process
#' @describeIn utils_parallel disable parallel process by stopping back-end
#'  clusters.
#' @export
disable_parallel <- function(env_globals = .pkg_globals,
                             parallel_switch_option = "zstmodelr.common.parallel") {

  # env_globals must be a environment to store parallel info
  if (!is.null(env_globals)) {
    assertive::assert_is_environment(env_globals)
  } else {
    env_globals <- parent.frame()
  }

  if (!is.null(parallel_switch_option)) {
    assertive::assert_is_character(parallel_switch_option)
  }

  # Get current status of parallel process
  status <- parallel_status(env_globals)
  cluster <- status$cluster
  parallel_log <- status$parallel_log


  # Close clusters
  if (!is.null(cluster)) {
    rlang::inform("Stop existed clusters for parallel process...\n")
    parallel::stopCluster(env_globals$.cluster)
    env_globals$.cluster <- NULL
  } else {
    doParallel::stopImplicitCluster()
  }
  foreach::registerDoSEQ()

  # wait a moment for stopping clusters
  max_wait_time <- 10 # Max wait time in second unit
  start_time <- proc.time()
  repeat {
    Sys.sleep(0.5)
    wait_time <- (proc.time() - start_time)["elapsed"]
    foreach_workers <- foreach::getDoParWorkers()
    if ((foreach_workers == 1) || wait_time >= max_wait_time) {
      break
    }
  }

  # Turn off parallel switch options
  # Turn on parallel switch options
  if (!is.null(parallel_switch_option)) {
    switch_options <- as.list(rep(FALSE, length(parallel_switch_option)))
    names(switch_options) <- parallel_switch_option
    options(switch_options)
  }

  # Clean log file
  if (!is.null(parallel_log)) {
    pkg_path <- system.file(package = utils::packageName())
    log_dir <- file.path(pkg_path, "log")
    parallel_log <- file.path(log_dir, basename(parallel_log))
    if (fs::file_exists(parallel_log)) {
      try({
        fs::file_delete(parallel_log)
      })
    }
  }
}

#' Get status parallel process
#'
#' @return `parallel_status` return a list containing parallel status, see details.
#'
#' @describeIn utils_parallel get current status of parallel process.
#' @export
parallel_status <- function(env_globals = .pkg_globals) {

  # env_globals must be a environment to store parallel info
  if (!is.null(env_globals)) {
    assertive::assert_is_environment(env_globals)
  } else {
    env_globals <- parent.frame()
  }

  # Get cluster setting info
  if (exists(".cluster", where = env_globals, inherits = FALSE)) {
    cluster <- env_globals$.cluster
  } else {
    cluster <- NULL
  }
  if (exists(".parallel_log", where = env_globals, inherits = FALSE)) {
    if (!is.null(env_globals$.parallel_log)) {
      pkg_path <- system.file(package = utils::packageName())
      log_dir <- file.path(pkg_path, "log")
      fs::dir_create(log_dir)
      parallel_log <- file.path(log_dir, basename(env_globals$.parallel_log))
    } else {
      parallel_log <- NULL
    }
  } else {
    parallel_log <- NULL
  }

  # workers for foreach Dopar
  foreach_workers <- foreach::getDoParWorkers()

  # options about parallel
  zstmodelr.common.clusters <- getOption("zstmodelr.common.clusters")
  zstmodelr.common.parallel <- getOption("zstmodelr.common.parallel")

  # return parallel status
  parallel_status <- list(
    cluster = cluster,
    parallel_log = parallel_log,
    foreach_workers = foreach_workers,
    zstmodelr.common.clusters = zstmodelr.common.clusters,
    zstmodelr.common.parallel = zstmodelr.common.parallel
  )
  parallel_status
}

#' Judge whether parallel process is on or not
#'
#' @return return a logic, TRUE mean parallel process is on.
#'
#' @describeIn utils_parallel Judge whether parallel process is on or not.
#' @export
parallel_is_on <- function() {
  status <- parallel_status()
  parallel_is_on <- !is.null(status$cluster)
  parallel_is_on
}
