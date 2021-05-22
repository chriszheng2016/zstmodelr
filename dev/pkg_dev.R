## Add module to dependency ----
usethis::use_package("package")
usethis::use_tidy_description()

## Add internal datasets ----
# If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Style codes ----
usethis::use_tidy_style()

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")
devtools::wd("tests/testthat")
testthat::test_file("test-app.R") # test a file

# Test a file which contains "skip_on_cran()/skip_on_ci()/skip_on_covr()"
# Method A:
withr::with_envvar(
  new = c("NOT_CRAN" = "true",
          "CI" = "false",
          "R_COVR" = "false"),
  testthat::test_file("test-app.R")
)
# Method B:
Sys.setenv("NOT_CRAN" = "true")
testthat::test_file("test-app.R")

devtools::test() # test package

## Document ----

# Update function doc
devtools::document()
?fun_abc

# Check spelling in doc
devtools::document() # update all doc
usethis::use_spell_check() # set up spelling check in doc
devtools::spell_check() # check spelling in doc
spelling::update_wordlist() # accept new words if need

# Vignette
usethis::use_vignette("pkgname")
devtools::build_vignettes()


## Check before merging ----

# Test package
devtools::test()

# Check spelling
devtools::document()  # Update document for checking
usethis::use_spell_check() # Set up spelling check by creating word list
devtools::spell_check() # Check spelling according to word list

# Check doc
devtools::check_man()

# R CMD Check
result <- devtools::check(cran = FALSE, args = c("--timings", "--no-tests"),
                          check_dir = "check")
cat(result$errors)
cat(result$warnings)
cat(result$notes)

# Update document of pkgdown
devtools::build_site(quiet = FALSE)
