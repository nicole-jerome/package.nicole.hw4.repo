test_that("print_function prints and returns correct message", {
  expect_output(print_function(x), "print function")
  expect_equal(print_function(x), "print function")
})


#dataframe input validation (non-df input gives error)
#filtering correctness
#empty results, missing row/column
