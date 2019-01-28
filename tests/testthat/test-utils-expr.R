# Tests for utility functions of expr ----

context("Tests for utitlity functions of expr")

test_that("create_expr, with various arguments", {

  # single expr
  expect_expr <- rlang::expr({
    a <- 1
    b <- 2
    c <- a + b
  })

  # muti expr
  expect_exprs <- rlang::exprs(
    a <- 1,
    b <- 2,
    c <- a + b
  )

  # expr string
  expr_string <- rlang::expr_text(expect_expr)
  expr_string <- stringr::str_remove_all(expr_string, pattern = "[\\{\\}]")

  # create_expr on expr ====
  actual_expr <- create_expr(!!expect_expr)
  expect_true(rlang::is_call(actual_expr))
  expect_identical(expect_expr, actual_expr)

  # create_expr on exprs ====
  actual_expr <- create_expr(!!expect_expr)
  expect_true(rlang::is_call(actual_expr))
  expect_identical(expect_expr, actual_expr)

  # create_expr on expr string ====
  actual_expr <- create_expr(!!expr_string)
  expect_true(rlang::is_call(actual_expr))

  # create_expr on NSE
  actual_expr <- create_expr(a <- b + 1)
  expect_true(rlang::is_call(actual_expr))
})

test_that("find_syms, with various arguments", {
  test_expr <- rlang::expr({
    a <- 1
    b <- 2
    c <- ((a + b) * (a - b)) / b + b^a
  })


  # find_syms in symbol obj ===
  expect_true(find_syms(x) == "x")

  # find_syms in constant obj ===
  expect_identical(find_syms(1), character(0))

  # find_syms in expr obj ===
  expect_syms <- c("a", "b", "c")
  actual_syms <- find_syms(!!test_expr)
  expect_true(all(expect_syms %in% actual_syms))

  # find_syms in expr obj with patern===
  expect_syms <- c("a", "b", "c")
  actual_syms <- find_syms(!!test_expr,
    pattern = "[^\\{\\+\\-\\*\\/\\(\\^\\<-]"
  )
  expect_equal(expect_syms, actual_syms)
})
