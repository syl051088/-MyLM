library(testthat)
library(linearR)

test_that("predict_linearR produces correct predictions", {
  # Fit models
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)
  fit_lm <- lm(mpg ~ wt + cyl, data = mtcars)

  # Create new data
  newdata <- data.frame(
    wt = c(2.5, 3.0),
    cyl = c(6, 8)
  )

  # Test point predictions
  pred_my <- predict_linearR(fit_my, newdata)
  pred_lm <- predict(fit_lm, newdata)
  expect_equal(as.vector(pred_my), as.vector(pred_lm))

  # Test confidence intervals
  pred_my_ci <- predict_linearR(fit_my, newdata, interval = "confidence")
  pred_lm_ci <- predict(fit_lm, newdata, interval = "confidence")
  expect_equal(as.vector(pred_my_ci), as.vector(pred_lm_ci))

  # Test prediction intervals
  pred_my_pi <- predict_linearR(fit_my, newdata, interval = "prediction")
  pred_lm_pi <- predict(fit_lm, newdata, interval = "prediction")
  expect_equal(as.vector(pred_my_pi), as.vector(pred_lm_pi))
})

test_that("predict_linearR handles edge cases", {
  fit_my <- my_lm(mpg ~ wt + cyl, data = mtcars)

  # Missing values in newdata
  newdata_na <- data.frame(wt = c(2.5, NA), cyl = c(6, 8))
  expect_error(predict_linearR(fit_my, newdata_na))

  # Wrong variable names
  newdata_wrong <- data.frame(weight = c(2.5, 3.0), cylinders = c(6, 8))
  expect_error(predict_linearR(fit_my, newdata_wrong))
})
