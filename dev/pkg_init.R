#' ---
#' title: "Notes about Setup R Package project"
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

#/*
# **README**: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
#
# * pkg_init.R should be done after creating package project.
#
# * pkg_dev.R should be used to keep track of your development during the project.
#
# * pkg_deploy.R should be used once you need to deploy your package.
#
# * ops_tools.R should be used to run your app in production/debug mode
#*/

#' # Create R package Project
#+ create_r_pkg, eval=FALSE

## Creates a new package, immediately applies as many of the tidyverse
## conventions as possible, issues a few reminders, and activates the
## new package.
usethis::create_tidy_package("pkg_path")

#' # Setup R package project

#+ setup_r_pkg, eval=FALSE
devtools::load_all(".") # load pkg in development

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "zstexplorer", # The Name of the package containing the App
  pkg_title = "Explorer for China Stock Market Investment", # The Title of the package containing the App
  pkg_description = "An interactive tool to explore data of China stock market.", # The Description of the package containing the App
  author_first_name = "Chris", # Your First Name
  author_last_name = "Zheng", # Your Last Name
  author_email = "chriszheng@vip.sina.com.cn", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional)
)
usethis::use_tidy_description() # tidy description

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license(copyright_holder = "Chris Zheng")
usethis::use_code_of_conduct()
usethis::use_news_md(open = FALSE)

# Add readme files
usethis::use_readme_rmd(open = FALSE)
usethis::use_lifecycle_badge("Experimental")
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)
usethis::use_git_ignore("*.html")
usethis::use_build_ignore("*.html")

## Setup git/github ----
## Setup git
usethis::use_git() # use git version control
usethis::use_git_protocol("https") # use https as transport protocol
usethis::git_sitrep()  # get current status of git setting

## Setup github
usethis::use_github()  # set up initial repo in github
## Add auxiliary files for tidy github project
usethis::use_tidy_github()
## Don't forget to render it again rmarkdown::render()
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)



## Setup CI if need ----

## Setup Github actions for CI

# Method A: all-in-one setup
## 1. Run R CMD check on the current release, devel, and four previous versions of R.
## 2. Report test coverage.
## 3. Build and deploy a pkgdown site.
## 4. Provide two commands to be used in pull requests:
##   /document to run roxygen2::roxygenise() and update the PR, and
##   /style to run styler::style_pkg() and update the PR
usethis::use_tidy_github_actions()


## Method B: individual setup

## R CMD check
## Check on the latest release of R on macOS
usethis::use_github_action_check_release()

## Check on the three major operating systems
## (linux, macOS, and Windows) on the latest release of R and on R-devel.
usethis::use_github_action_check_standard()

## Check on at least once on each of the three major operating systems
## (linux, macOS, and Windows) and on the current release, devel,
## and four previous versions of R.
usethis::use_github_action_check_full()

## Set PR actions
## This workflow enables the use of two R-specific commands in pull request issue comments:
## * /document to run roxygen2::roxygenise() and update the PR
## * /style to run styler::style_pkg() and update the PR
usethis::use_github_action_pr_commands()

## Set code coverage report
usethis::use_coverage()

## Set pkgdown site
usethis::use_pkgdown()

## Don't forget to render it again rmarkdown::render()
rmarkdown::render("README.Rmd", quiet = TRUE, clean = TRUE)

## Setup other CI facilities
usethis::use_travis()
usethis::use_appveyor()





