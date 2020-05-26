# Tests for utility functions of fields ----

context("Tests for utility functions of fields")

# prepare test datasets
rows <- 10
ds_various_type <- tibble::tibble(
  numeric = rep(1.0, rows),
  integer = rep(1L, rows),
  double = rep(1.0, rows),
  character = rep("a", rows),
  date = rep(as.Date("2008-10-1"), rows),
  factor = as.factor(character),
  list = rep(list(NULL), rows),
  "NA" = rep(NA, rows)
)

test_that("verify_fields, with various arguments", {

  # verify_fields on existed fields ====
  for (field in names(ds_various_type)) {
    verify_fields(ds_various_type,
      fields = field
    )
  }

  # verify_fields on non-existed fields ====
  expect_error(verify_fields(ds_various_type,
    fields = "Date"
  ))
})

test_that("is_type_field, with various arguments", {

  # is_type_field on regualar data type ====
  expect_type_fields <- names(ds_various_type)
  for (i in seq_along(expect_type_fields)) {
    expect_field_type <- expect_type_fields[i]
    expect_type_true <- is_type_field(ds_various_type,
                                   expect_type = expect_field_type)
    expect_fields <- expect_type_fields[expect_type_true]
    expect_true(expect_field_type %in% expect_fields)

  }
  # numeric fields should include numeric, integer and double
  expect_type_true <- is_type_field(
    ds_various_type,
    "numeric"
  )
  expect_fields <- expect_type_fields[expect_type_true]
  expect_true(all(expect_fields %in% c("numeric", "integer", "double")))

  # double fields don't include date
  expect_type_true <- is_type_field(
    ds_various_type,
    "double"
  )
  expect_fields <- expect_type_fields[expect_type_true]
  expect_true(all(expect_fields %in% c("numeric", "double")))
  expect_true(!("date" %in% expect_fields))

  # is_type_field on negate regualar data type ====
  expect_field_types <- names(ds_various_type)
  for (i in seq_along(expect_field_types)) {
    expect_field_type <- expect_field_types[i]
    expect_type_true <- is_type_field(ds_various_type,
                                        expect_type = expect_field_type,
                                        negate = TRUE
    )
    expect_fields <- expect_type_fields[expect_type_true]
    expect_true(!(expect_field_type %in% expect_fields))
  }

  # non-numeric fields shouldn't include numeric, integer and double
  expect_type_true <- is_type_field(ds_various_type,
                                      "numeric",
                                      negate = TRUE
  )
  expect_fields <- expect_type_fields[expect_type_true]
  expect_true(all(!(expect_fields %in% c("numeric", "integer", "double"))))

  # non-double fields should include date
  expect_type_true <- is_type_field(
    ds_various_type,
    "double",
    negate = TRUE
  )
  expect_fields <- expect_type_fields[expect_type_true]
  expect_true(all(!expect_fields %in% c("numeric", "double")))
  expect_true("date" %in% expect_fields)


  # is_type_field on predicate_fun ====
  expect_type_true <- is_type_field(ds_various_type,
                                    expect_type = expect_field_type,
                                    predicate_fun = inherits,
                                    what = "Date"
  )
  expect_fields <- expect_type_fields[expect_type_true]
  expect_true(expect_fields == "date")

})

test_that("expect_type_fields, with various arguments", {

  # expect_type_fields on regualar data type ====
  expect_type_fields <- names(ds_various_type)
  for (i in seq_along(expect_type_fields)) {
    expect_field_type <- expect_type_fields[i]
    expect_fields <- expect_type_fields(ds_various_type,
      expect_type = expect_field_type
    )
    expect_true(expect_field_type %in% expect_fields)
  }
  # numeric fields should include numeric, integer and double
  expect_fields <- expect_type_fields(
    ds_various_type,
    "numeric"
  )
  expect_true(all(expect_fields %in% c("numeric", "integer", "double")))

  # double fields don't include date
  expect_fields <- expect_type_fields(
    ds_various_type,
    "double"
  )
  expect_true(all(expect_fields %in% c("numeric", "double")))
  expect_true(!("date" %in% expect_fields))

  # expect_type_fields on negate regualar data type ====
  expect_field_types <- names(ds_various_type)
  for (i in seq_along(expect_field_types)) {
    expect_field_type <- expect_field_types[i]
    expect_fields <- expect_type_fields(ds_various_type,
      expect_type = expect_field_type,
      negate = TRUE
    )
    expect_true(!(expect_field_type %in% expect_fields))
  }

  # non-numeric fields shouldn't include numeric, integer and double
  expect_fields <- expect_type_fields(ds_various_type,
    "numeric",
    negate = TRUE
  )
  expect_true(all(!(expect_fields %in% c("numeric", "integer", "double"))))

  # non-double fields should include date
  expect_fields <- expect_type_fields(
    ds_various_type,
    "double",
    negate = TRUE
  )
  expect_true(all(!expect_fields %in% c("numeric", "double")))
  expect_true("date" %in% expect_fields)


  # expect_type_fields on predicate_fun ====
  expect_fields <- expect_type_fields(ds_various_type,
    expect_type = expect_field_type,
    predicate_fun = inherits,
    what = "Date"
  )
  expect_true(expect_fields == "date")
})
