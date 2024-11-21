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

test_that("predict_linearR handles various interval types", {
  fit <- my_lm(mpg ~ wt + cyl, data = mtcars)
  newdata <- data.frame(wt = c(2.5, 3.0), cyl = c(6, 8))

  # Test default behavior
  expect_error(predict_linearR(fit), NA)

  # Test different confidence levels
  pred_90 <- predict_linearR(fit, newdata, interval = "confidence", level = 0.90)
  pred_99 <- predict_linearR(fit, newdata, interval = "confidence", level = 0.99)
  expect_true(all(pred_90[,"upr"] - pred_90[,"lwr"] <
                    pred_99[,"upr"] - pred_99[,"lwr"]))

  # Test interval types with no newdata
  expect_error(predict_linearR(fit, interval = "confidence"), NA)
  expect_error(predict_linearR(fit, interval = "prediction"), NA)
})

test_that("predict_linearR handles factor variables correctly", {
  mtcars$cyl_f <- factor(mtcars$cyl)
  fit <- my_lm(mpg ~ wt + cyl_f, data = mtcars)

  newdata <- data.frame(
    wt = c(2.5, 3.0),
    cyl_f = factor(c(6, 8), levels = levels(mtcars$cyl_f))
  )

  expect_error(predict_linearR(fit, newdata), NA)
})

test_that("predict_linearR errors with unseen factor levels", {
  # Prepare training data without one of the factor levels
  mtcars$cyl_f <- factor(mtcars$cyl)
  training_data <- subset(mtcars, cyl != 4)  # Exclude level 4

  # Drop unused factor levels
  training_data$cyl_f <- droplevels(training_data$cyl_f)

  # Fit the model
  fit <- my_lm(mpg ~ wt + cyl_f, data = training_data)

  # Create new data that includes an unseen factor level
  newdata <- data.frame(
    wt = c(2.5),
    cyl_f = factor(4, levels = levels(training_data$cyl_f))  # Level 4 not in training
  )

  # Expect an error when predicting with unseen factor levels
  expect_error(
    predict_linearR(fit, newdata)
  )
})
