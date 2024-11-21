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

test_that("my_lm handles model statistics correctly", {
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  fit_lm <- lm(mpg ~ wt + cyl, data = mtcars)

  # Test degrees of freedom
  expect_equal(fit_my$df.residual, fit_lm$df.residual)

  # Test variance-covariance matrix
  expect_equal(as.vector(fit_my$vcov), as.vector(vcov(fit_lm)))

  # Test sigma2
  expect_equal(fit_my$sigma2, summary(fit_lm)$sigma^2)

  # Test dimensions
  expect_equal(fit_my$n, nrow(mtcars))
  expect_equal(fit_my$p, length(coef(fit_lm)))
})

test_that("my_lm handles different formula types", {
  # Create test data with factors first
  test_data <- mtcars
  test_data$cyl_f <- factor(test_data$cyl)
  test_data$am_f <- factor(test_data$am)

  # Intercept-only model
  expect_error(my_lm(mpg ~ 1, data = test_data), NA)

  # No-intercept model
  fit_no_int <- my_lm(mpg ~ 0 + wt, data = test_data)
  fit_lm_no_int <- lm(mpg ~ 0 + wt, data = test_data)
  expect_equal(as.vector(fit_no_int$coefficients),
               as.vector(coef(fit_lm_no_int)))

  # Multiple categorical variables
  fit_cat <- my_lm(mpg ~ cyl_f + am_f, data = test_data)
  fit_lm_cat <- lm(mpg ~ cyl_f + am_f, data = test_data)
  expect_equal(as.vector(fit_cat$coefficients),
               as.vector(coef(fit_lm_cat)))

  # Interaction terms
  fit_int <- my_lm(mpg ~ wt * cyl, data = test_data)
  fit_lm_int <- lm(mpg ~ wt * cyl, data = test_data)
  expect_equal(as.vector(fit_int$coefficients),
               as.vector(coef(fit_lm_int)))
})

test_that("my_lm preserves variable names", {
  fit <- my_lm(mpg ~ wt + cyl, data = mtcars)

  # Check coefficient names
  expect_equal(names(fit$coefficients), names(coef(lm(mpg ~ wt + cyl, data = mtcars))))

  # Check that standard errors have names
  expect_equal(names(fit$se), names(fit$coefficients))

  # Check that t-statistics have names
  expect_equal(names(fit$tstat), names(fit$coefficients))
})
