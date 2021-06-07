# Tests for utility functions of parallel ----
skip_on_ci()

context("Tests for utility functions of parallel")

test_that("enable/disable_parallel, with various arguments", {

  # number of clusters used for testing
  test_clusters <- 4

  withr::local_options(list(
    zstmodelr.common.clusters = test_clusters,
    zstmodelr.common.parallel = TRUE
  ))

  # enable/disable_parallel with default arguments ====

  # enable parallel processing
  enable_parallel()

  # check results of enable parallel
  status <- parallel_status()
  expect_is(status$cluster, "cluster")
  if (!is.null(status$parallel_log)) {
    expect_type(status$parallel_log, "character")
    expect_true(file.exists(status$parallel_log))
  }
  expect_true(status$foreach_workers == test_clusters)

  # enable parallel processing again
  expect_message(
    enable_parallel(),
    regexp = "clusters been working"
  )

  # disable parallel processing
  disable_parallel()

  # check results of disable parallel
  status <- parallel_status()
  expect_null(status$cluster)
  expect_true(status$foreach_workers == 1)
  expect_true(!file.exists(status$parallel_log))

  # Enable/disable parallel in check by CRAN which only allow 2 cluster at most.
  withr::with_options(
    list(
      zstmodelr.common.clusters = test_clusters,
      zstmodelr.common.parallel = TRUE
    ),
    {
      withr::with_envvar(list("_R_CHECK_LIMIT_CORES_" = "TRUE"), {

        expect_warning(
          enable_parallel(),
          regexp = "Only use 2 clusters by CRAN"
        )

        # check results of enable parallel
        status <- parallel_status()

        # CRAN only allow 2 cores/clusters
        expect_equal(getOption("zstmodelr.common.clusters"), 2)

        # disable parallel processing
        disable_parallel()
      })
    }
  )


  # enable/disable_parallel with various arguments ====

  # >> env_globals = NULL ----

  # enable parallel processing
  enable_parallel(NULL)

  # check results of enable parallel
  status <- parallel_status(NULL)
  expect_is(status$cluster, "cluster")
  if (!is.null(status$parallel_log)) {
    expect_type(status$parallel_log, "character")
    expect_true(file.exists(status$parallel_log))
  }
  expect_true(status$foreach_workers == test_clusters)

  # disable parallel processing
  disable_parallel(NULL)

  # check results of disable parallel
  status <- parallel_status(NULL)
  expect_null(status$cluster)
  expect_true(status$foreach_workers == 1)
  if (!is.null(status$parallel_log)) {
    expect_type(status$parallel_log, "character")
    expect_true(!file.exists(status$parallel_log))
  }

  # >> env_globals = environment ----

  # set environment and configuration for testing
  test_env <- new.env(parent = emptyenv())
  test_env$.cluster <- NULL
  test_env$.parallel_log <- "parallel.log"

  # enable parallel processing
  enable_parallel(test_env)

  # check results of enable parallel
  status <- parallel_status(test_env)
  expect_is(status$cluster, "cluster")
  if (!is.null(status$parallel_log)) {
    expect_type(status$parallel_log, "character")
    expect_true(file.exists(status$parallel_log))
  }
  expect_true(status$foreach_workers == test_clusters)

  # disable parallel processing
  disable_parallel(test_env)

  # check results of disable parallel
  status <- parallel_status(test_env)
  expect_null(status$cluster)
  expect_true(status$foreach_workers == 1)
  if (!is.null(status$parallel_log)) {
    expect_type(status$parallel_log, "character")
    expect_true(!file.exists(status$parallel_log))
  }
})

test_that("parallel_status, with various arguments", {

  # parallel_status with default arguments ====

  expect_fields <- c(
    "cluster", "parallel_log", "foreach_workers",
    "zstmodelr.common.clusters", "zstmodelr.common.parallel"
  )

  status <- parallel_status()
  expect_true(all(expect_fields %in% names(status)))
  if (!is.null(status$.cluster)) {
    expect_is(status$.cluster, "cluster")
  }
  if (!is.null(status$.parallel_log)) {
    expect_type(status$.parallel_log, "character")
  }
  expect_type(status$foreach_workers, "integer")
  expect_type(status$zstmodelr.common.clusters, "double")
  expect_type(status$zstmodelr.common.parallel, "logical")

  # parallel_status with various arguments ====

  # >> env_globals = NULL ----

  expect_fields <- c(
    "cluster", "parallel_log", "foreach_workers",
    "zstmodelr.common.clusters", "zstmodelr.common.parallel"
  )


  status <- parallel_status(NULL)
  expect_true(all(expect_fields %in% names(status)))
  if (!is.null(status$.cluster)) {
    expect_is(status$.cluster, "cluster")
  }
  if (!is.null(status$.parallel_log)) {
    expect_type(status$.parallel_log, "character")
  }
  expect_type(status$foreach_workers, "integer")
  expect_type(status$zstmodelr.common.clusters, "double")
  expect_type(status$zstmodelr.common.parallel, "logical")


  # >> env_globals = enviormment ----

  test_env <- new.env(parent = emptyenv())
  test_env$.cluster <- NULL
  test_env$.parallel_log <- "parallel.log"

  expect_fields <- c(
    "cluster", "parallel_log", "foreach_workers",
    "zstmodelr.common.clusters", "zstmodelr.common.parallel"
  )

  status <- parallel_status(test_env)
  expect_true(all(expect_fields %in% names(status)))
  if (!is.null(status$.cluster)) {
    expect_is(status$.cluster, "cluster")
  }
  if (!is.null(status$.parallel_log)) {
    expect_type(status$.parallel_log, "character")
  }
  expect_type(status$foreach_workers, "integer")
  expect_type(status$zstmodelr.common.clusters, "double")
  expect_type(status$zstmodelr.common.parallel, "logical")
})

test_that("parallel_is_on, with various arguments", {

  # parallel_status with default arguments ====
  enable_parallel()

  expect_true(parallel_is_on())

  disable_parallel()

  expect_false(parallel_is_on())

})
