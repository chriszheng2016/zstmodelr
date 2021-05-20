


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
