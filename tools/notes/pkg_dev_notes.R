# After you finish a branch, you should check followings before merging into
# main branch

# Test package
devtools::test()

# Update doc and check spelling
devtools::document()

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
