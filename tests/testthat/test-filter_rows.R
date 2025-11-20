test_that("filter_rows filters correctly with simple conditions", {
  result <- suppressMessages(filter_rows(mtcars, mpg > 20))

  # Expected base R version
  expected <- mtcars[mtcars$mpg > 20, , drop = FALSE]

  expect_equal(result, expected)
})

test_that("filter_rows handles multiple conditions", {
  result <- suppressMessages(filter_rows(mtcars, mpg > 20 & cyl == 4))

  expected <- mtcars[mtcars$mpg > 20 & mtcars$cyl == 4, , drop = FALSE]

  expect_equal(result, expected)
})

test_that("filter_rows returns an empty data frame when condition is always FALSE", {
  result <- suppressMessages(filter_rows(mtcars, mpg > 100))

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), ncol(mtcars))
})

test_that("filter_rows throws an error for non-data-frame input", {
  expect_error(
    filter_rows(1:10, TRUE),
    "Input must be a data.frame"
  )
})

test_that("filter_rows throws error when condition does not evaluate to logical vector", {
  # Condition evaluates to numeric instead of logical
  expect_error(
    filter_rows(mtcars, mpg + 1),
    "Condition must evaluate to a logical vector"
  )
})

test_that("filter_rows error if condition evaluates to wrong length", {
  expect_error(
    filter_rows(mtcars, rep(TRUE, 5)),   # too short
    "logical vector of length equal to nrow"
  )
})

test_that("filter_rows keeps row count message consistent (smoke test)", {
  # We don't test the exact message text,
  # just check that no errors occur when message is produced
  expect_silent(suppressMessages(filter_rows(mtcars, mpg > 20)))
})
