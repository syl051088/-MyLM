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

test_that("linearR handles numerical precision issues", {
  # Large numbers
  x <- 1:10 * 1e6
  y <- 2*x + rnorm(10)
  dat <- data.frame(x = x, y = y)
  expect_error(my_lm(y ~ x, data = dat), NA)

  # Very small numbers
  x_small <- 1:10 * 1e-6
  y_small <- 2*x_small + rnorm(10, sd = 1e-6)
  dat_small <- data.frame(x = x_small, y = y_small)
  expect_error(my_lm(y ~ x, data = dat_small), NA)
})

test_that("linearR handles various data types", {
  # Integer predictor
  expect_error(my_lm(mpg ~ as.integer(cyl), data = mtcars), NA)

  # Logical predictor
  mtcars$high_mpg <- mtcars$mpg > mean(mtcars$mpg)
  expect_error(my_lm(wt ~ high_mpg, data = mtcars), NA)

  # Multiple factor levels
  mtcars$cyl_f <- factor(mtcars$cyl)
  expect_error(my_lm(mpg ~ cyl_f, data = mtcars), NA)
})
