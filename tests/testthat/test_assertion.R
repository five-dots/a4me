
test_that("string assertion functions work", {

  expect_equal(a_str("foo"), "foo")
  expect_null(a_str(NULL, allow_null = TRUE))

  expect_error(a_str(c("foo", "bar")))
  expect_error(a_str(NULL))
  expect_error(a_str(Inf))
  expect_error(a_str(NaN))
  expect_error(a_str(NA))
  expect_error(a_str(NA_character_))
  expect_error(a_str(1), "string must be a character scalar.")
  dummy_string <- 1
  expect_error(a_str(dummy_string), "dummy_string must be a character scalar.")

})

test_that("count assertion functions work", {

  expect_equal(a_count(1), 1)
  expect_equal(a_count(Inf), Inf)
  expect_null(a_count(NULL, allow_null = TRUE))

  expect_error(a_count(c(1, 2)))
  expect_error(a_count(NULL))
  expect_error(a_count(NaN))
  expect_error(a_count(NA_integer_))
  expect_error(a_count(NA), "count must be a positive integer scalar.")
  dummy_count <- 0
  expect_error(a_count(dummy_count),
               "dummy_count must be a positive integer scalar.")

})

test_that("flag assertion functions work", {

  expect_true(a_flag(TRUE))

  expect_error(a_flag(c(TRUE, FALSE)))
  expect_error(a_flag(NULL))
  expect_error(a_flag(Inf))
  expect_error(a_flag(NaN))
  expect_error(a_flag(NA), "flag must be a logical scalar.")
  dummy_flag <- 1
  expect_error(a_flag(dummy_flag), "dummy_flag must be a logical scalar.")

})

test_that("dir assertion functions work", {

  expect_equal(a_dir("~/"), "~/")
  expect_error(a_dir("~/.profile"))
  expect_error(a_dir("/"))
  expect_null(a_dir(NULL, allow_null = TRUE))

})

test_that("class assertion functions work", {

  expect_equal(a_class("hoge", "character"), "hoge")
  expect_equal(a_class(NA, "logical"), NA)
  expect_equal(a_class(Inf, "numeric"), Inf)
  expect_null(a_class(NULL, "character", allow_null = TRUE))

  expect_error(a_class(NULL, "character", allow_null = FALSE))
  expect_error(a_class("hoge", "list"))
  dummy_object <- 1
  expect_error(a_class(dummy_object, "character"),
               "dummy_object must be a character class.")
  expect_error(a_class(1, 1),
               "class must be a character scalar.")
  expect_error(a_class(1, "numerics", 1),
               "allow_null must be a logical scalar.")

})
