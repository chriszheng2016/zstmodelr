# unit testing for stock_db (generic class of stock database)

library(zstmodelr)
context("Testing features of stock_db class")

# set up testing context
stock_db <- stock_db(gta_db, "GTA_SQLData")
suppressMessages(open_stock_db(stock_db))

test_that("Open and close stock_db", {
  stock_db1 <- stock_db(gta_db, "GTA_SQLData")
  expect_is(stock_db1, "stock_db")
  expect_true(open_stock_db(stock_db1))
  expect_true(close_stock_db(stock_db1))
})

test_that("Init param of stock db", {
  expect_true(init_stock_db(stock_db))
  expect_error(init_stock_db(non_stock_db))
})

test_that("Translation between code and name", {
  # expect_equal(name2code(stock_db, "资产报酬率A", type = "field"), "f050101b")
  # expect_equal(code2name(stock_db, "f050101b", type = "field"), "资产报酬率A")
  # expect_equal(name2code(stock_db, "宇通客车", type = "stock"), "600066")
  # expect_equal(code2name(stock_db, "600066", type = "stock"), "宇通客车")
})




# clear up testing conext
suppressMessages(close_stock_db(stock_db))





