library(testthat)
library(linearR)

test_that("my_lm produces correct coefficients", {
  # Test with mtcars dataset
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  fit_lm <- lm(mpg ~ wt + cyl, data = mtcars)

  # Compare numeric values, not attributes
  expect_equal(as.vector(fit_my$coefficients), as.vector(coef(fit_lm)))
  expect_equal(as.vector(fit_my$fitted), as.vector(fitted(fit_lm)))
  expect_equal(as.vector(fit_my$residuals), as.vector(resid(fit_lm)))
})

test_that("my_lm handles single predictor correctly", {
  fit_my <- my_lm(mpg ~ wt, data = mtcars)
  fit_lm <- lm(mpg ~ wt, data = mtcars)

  # Compare numeric values
  expect_equal(as.vector(fit_my$coefficients), as.vector(coef(fit_lm)))
  expect_equal(fit_my$r.squared, summary(fit_lm)$r.squared)
})

test_that("my_lm handles categorical variables", {
  mtcars$cyl_f <- factor(mtcars$cyl)
  fit_my <- my_lm(mpg ~ wt + cyl_f, data = mtcars)
  fit_lm <- lm(mpg ~ wt + cyl_f, data = mtcars)

  expect_equal(as.vector(fit_my$coefficients), as.vector(coef(fit_lm)))
})

test_that("my_lm handles edge cases", {
  # Single observation
  expect_error(my_lm(mpg ~ wt, data = mtcars[1,]))

  # Perfect collinearity
  mtcars$wt2 <- mtcars$wt
  expect_error(my_lm(mpg ~ wt + wt2, data = mtcars))

  # Missing values
  mtcars_na <- mtcars
  mtcars_na$wt[1] <- NA
  expect_error(my_lm(mpg ~ wt, data = mtcars_na))
})
