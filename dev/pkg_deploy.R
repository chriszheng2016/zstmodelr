#' ---
#' title: "Notes about Deploy R Package project"
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

#' # Test R package for deploy

#+ test_r_pkg_develop, eval = FALSE
## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()


#' # Deploy R package

#+ deploy_r_package, eval = FALSE

## Install locally ----
devtools::install()

## Install from CRAN if could
install.packages("pkgname")

## Install from github
remotes::install_github("owner/project")
