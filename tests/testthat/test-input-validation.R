library(testthat)
library(linearR)

test_that("my_lm validates inputs correctly", {
  # Invalid formula
  expect_error(my_lm(invalid ~ wt, data = mtcars))

  # Missing data
  expect_error(my_lm(mpg ~ wt, data = NULL))

  # Empty data
  expect_error(my_lm(mpg ~ wt, data = data.frame()))

  # Non-numeric response
  mtcars$char_mpg <- as.character(mtcars$mpg)
  expect_error(my_lm(char_mpg ~ wt, data = mtcars))
})
