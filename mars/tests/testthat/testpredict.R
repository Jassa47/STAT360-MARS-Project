library(testthat)
library(mars)

load(test_path("testmars.RData"))
load(test_path("testpredict.RData"))

test_that("predict.mars() on training data matches expected", {
  expect_equal(predict(testmars), testpredict)
})

test_that("predict.mars() with newdata matches expected", {
  expect_equal(predict(testmars, newdata = marstestdata), testpredict)
})
