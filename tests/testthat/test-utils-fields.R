# Tests for utility functions of fields ----

context("Tests for utitlity functions of fields")

# prepare test datasets
rows <- 10
ds_various_type <- tibble::tibble(
  numeric = rep(1.0, rows),
  integer = rep(1L, rows),
  character = rep("a", rows),
  date = rep(as.Date("2008-10-1"), rows),
  factor = as.factor(character),
  "NA" = rep(NA, rows)
)

test_that("check_fields, with various arguments", {

  # check_fields on existed fields ====
  for (field in names(ds_various_type)) {
    check_fields(ds_various_type,
                 .fields = field)
  }

  # check_fields on non-existed fields ====
  expect_error(check_fields(ds_various_type,
                            .fields = "Date"))

})

test_that("expect_type_fields, with various arguments", {

  # expect_type_fields on regualar data type ====
  expect_field_types <- names(ds_various_type)
  for (i in seq_along(expect_field_types)) {
    expect_field_type <- expect_field_types[i]
    expect_fields <- expect_type_fields(ds_various_type,
      .expect_type = expect_field_type
    )
    expect_true(expect_field_type %in% expect_fields)
  }

  # expect_type_fields on negate regualar data type ====
  expect_field_types <- names(ds_various_type)
  for (i in seq_along(expect_field_types)) {
    expect_field_type <- expect_field_types[i]
    expect_fields <- expect_type_fields(ds_various_type,
      .expect_type = expect_field_type,
      .negate = TRUE
    )
    expect_true(!(expect_field_type %in% expect_fields))
  }

  # expect_type_fields on expect_true_fun  ====
  expect_fields <- expect_type_fields(ds_various_type,
                                      .expect_type = expect_field_type,
                                      .expect_true_fun = inherits,
                                      what = "Date"
                                      )
  expect_true(expect_fields == "date")

})
