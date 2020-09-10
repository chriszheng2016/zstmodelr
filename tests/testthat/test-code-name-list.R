# Tests for functions of stock_db  - generic functions ----
context("Tests for function of code_name_list - generic functions")


code_name_list

test_that("code_name_list, with various arguments", {

  # code_name_list with default arguments ====
  codes <- paste0("code_",1:10)
  names <- paste0("name_",1:10)
  code_name_list <- code_name_list(codes, names)

  expect_is(code_name_list, "code_name_list")
  expect_true(all(c("code", "name") %in% names(code_name_list)))

})

test_that("code2name/name2code, with various arguments", {

  codes <- paste0("code_",1:10)
  names <- paste0("name_",1:10)
  code_name_list <- code_name_list(codes, names)

  # code2name/name2code with default arguments ====

  # Convert single code/name with exact match = TRUE(default)
  search_codes <- expect_codes <- c("code_1")
  search_names <- expect_names <- c("name_1")
  result_names <- code2name(code_name_list, code = search_codes)
  expect_equal(result_names, expect_names)
  result_codes <- name2code(code_name_list, name = search_names)
  expect_equal(result_codes, expect_codes)

  # Convert multiple code/name with exact match = TRUE(default)
  search_codes <- expect_codes <- codes[1:5]
  search_names <- expect_names <- names[1:5]
  result_names <- code2name(code_name_list, code = expect_codes)
  expect_equal(result_names, expect_names)
  result_codes <- name2code(code_name_list, name = expect_names)
  expect_equal(result_codes, expect_codes)

  # code2name/name2code with various arguments ====

  # Convert single code/name with exact match = FALSE
  search_codes <- c("code_1")
  expect_names <- c("name_1", "name_10")
  result_names <- code2name(code_name_list, code = search_codes,
                            exact_match = FALSE)
  expect_equal(result_names, expect_names)
  search_names <- c("name_1")
  expect_codes <- c("code_1", "code_10")
  result_codes <- name2code(code_name_list, name = search_names,
                            exact_match = FALSE)
  expect_equal(result_codes, expect_codes)

  # Convert multiple code/name with exact match = FALSE
  search_codes <- c("code_1", "code_2")
  expect_names <- c("name_1", "name_10", "name_2")
  result_names <- code2name(code_name_list, code = search_codes,
                            exact_match = FALSE)
  expect_equal(result_names, expect_names)
  search_names <- c("name_1", "name_2")
  expect_codes <- c("code_1", "code_10", "code_2")
  result_codes <- name2code(code_name_list, name = search_names,
                            exact_match = FALSE)
  expect_equal(result_codes, expect_codes)


})

context("Tests for function of code_name_list - non generic functions")


