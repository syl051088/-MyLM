library(testthat)
library(linearR)

test_that("summary_linearR produces correct statistics", {
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  fit_lm <- lm(mpg ~ wt + cyl, data = mtcars)

  sum_my <- summary_linearR(fit_my)
  sum_lm <- summary(fit_lm)

  # Test R-squared
  expect_equal(sum_my$r.squared, sum_lm$r.squared)
  expect_equal(sum_my$adj.r.squared, sum_lm$adj.r.squared)

  # Test coefficient matrix values without checking attributes
  expect_equal(
    as.vector(sum_my$coefficients[, 2]),  # Std. Error
    as.vector(sum_lm$coefficients[, 2])
  )

  expect_equal(
    as.vector(sum_my$coefficients[, 3]),  # t value
    as.vector(sum_lm$coefficients[, 3])
  )

  expect_equal(
    as.vector(sum_my$coefficients[, 4]),  # p-value
    as.vector(sum_lm$coefficients[, 4])
  )
})

test_that("print_summary_linearR works correctly", {
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  sum_my <- summary_linearR(fit_my)

  # Test that print doesn't error
  expect_error(print_summary_linearR(sum_my), NA)

  # Test output capture
  output <- capture.output(print_summary_linearR(sum_my))
  expect_true(any(grepl("Coefficients:", output)))
  expect_true(any(grepl("Multiple R-squared:", output)))
})
