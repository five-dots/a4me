
test_that("helper functions work", {

  f <- function(a) .get_first_arg_name(f)
  expect_equal(f(), "a")
  b <- 1
  expect_equal(f(b), "b")

  f <- function() .get_first_arg_name(f)
  expect_null(f())

})
