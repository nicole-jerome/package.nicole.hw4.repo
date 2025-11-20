test_that("select_columns includes columns by name", {
  result <- suppressMessages(
    select_columns(mtcars, columns = c("mpg", "hp"))
  )

  expected <- mtcars[, c("mpg", "hp"), drop = FALSE]

  expect_equal(result, expected)
})

test_that("select_columns includes columns by index", {
  result <- suppressMessages(
    select_columns(mtcars, columns = c(1, 3))
  )

  expected <- mtcars[, c(1, 3), drop = FALSE]

  expect_equal(result, expected)
})

test_that("select_columns excludes columns by name", {
  result <- suppressMessages(
    select_columns(mtcars, exclude = c("mpg", "hp"))
  )

  expected <- mtcars[, setdiff(names(mtcars), c("mpg", "hp")), drop = FALSE]

  expect_equal(result, expected)
})

test_that("select_columns excludes columns by index", {
  result <- suppressMessages(
    select_columns(mtcars, exclude = c(1, 3))
  )

  expected <- mtcars[, setdiff(seq_len(ncol(mtcars)), c(1, 3)), drop = FALSE]

  expect_equal(result, expected)
})

test_that("select_columns throws error if both columns and exclude are provided", {
  expect_error(
    select_columns(mtcars, columns = 1, exclude = 2),
    "either 'columns' or 'exclude', not both"
  )
})

test_that("select_columns throws error if neither columns nor exclude provided", {
  expect_error(
    select_columns(mtcars),
    "must provide either 'columns' or 'exclude'"
  )
})

test_that("select_columns errors on invalid column names", {
  expect_error(
    select_columns(mtcars, columns = "not_a_column"),
    "do not exist"
  )
})

test_that("select_columns errors on out-of-range indices", {
  expect_error(
    select_columns(mtcars, columns = 999),
    "out of range"
  )
})

test_that("select_columns errors on invalid exclude names", {
  expect_error(
    select_columns(mtcars, exclude = "not_a_column"),
    "do not exist"
  )
})

test_that("select_columns errors on invalid exclude indices", {
  expect_error(
    select_columns(mtcars, exclude = 999),
    "out of range"
  )
})

test_that("select_columns errors if df is not a data.frame", {
  expect_error(select_columns(1:10, columns = 1), "data.frame")
})

test_that("select_columns runs without message when suppressed", {
  expect_silent(
    suppressMessages(select_columns(mtcars, columns = 1))
  )
})
