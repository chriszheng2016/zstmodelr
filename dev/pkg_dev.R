#' ---
#' title: "Notes about Building R Package"
#' date:  "`r Sys.Date()`"
#' author: Chris Zheng
#' email: chrizheng@vip.sina.com.cn
#' output:
#'   html_document:
#'      fig_caption: yes
#'      number_sections: yes
#'      theme: cerulean
#'      highlight: pygments
#' ---

#' # Build R package

#+ develop_r_pkg, eval=FALSE

## Add package to dependency ----
## Add one line by package you want to add as dependency
usethis::use_package("package") # Use package from cran
usethis::use_dev_package(
  "pkgname",
  remote = "onwer/project@branch"
) # Use package in development from github
usethis::use_tidy_description()

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)


## Tests ----
## Add one line by test you want to create
usethis::use_test("app")
devtools::wd("tests/testthat")
testthat::test_file("test-app.R") # test a file
devtools::test_file_coverage("test-app.R") # test a file coverage

## Test a file which contains "skip_on_cran()/skip_on_ci()/skip_on_covr()"
# Method A:
withr::with_envvar(
  new = c(
    "NOT_CRAN" = "true",
    "CI" = "false",
    "R_COVR" = "false"
  ),
  {
    testthat::test_file("test-app.R")
    devtools::test()
  }
)
# Method B:
Sys.setenv("NOT_CRAN" = "true")
testthat::test_file("test-app.R")
devtools::test()
Sys.setenv("NOT_CRAN" = "")

## Simulate test in environment without stock db
# Method A:
withr::with_envvar(
  new = c("NO_STOCK_DB" = "true"),
  {
    testthat::test_file("test-app.R")
    devtools::test()
  }
)

# Method B:
Sys.setenv("NO_STOCK_DB" = "true")
testthat::test_file("test-app.R")
devtools::test()
Sys.setenv("NO_STOCK_DB" = "")


## Edit vignette
usethis::use_vignette("pkg_name")
devtools::build_vignettes()

## Check before merging ----

## Test package
devtools::test()

## Test coverage
devtools::test_coverage(quiet = FALSE, clean = FALSE)

## Style package
## Styles source code according to the tidyverse style guide.
usethis::use_tidy_style() ## It will overwrite files!

## Update doc and check spelling
devtools::document()
usethis::use_spell_check() # Set up spelling check in doc
devtools::spell_check()

## Check doc
devtools::check_man()

## R CMD Check
result <- devtools::check(
  cran = FALSE, args = c("--timings", "--no-tests"),
  check_dir = "check"
)
cat(result$errors)
cat(result$warnings)
cat(result$notes)

# Update document of pkgdown
devtools::build_site(quiet = FALSE)
