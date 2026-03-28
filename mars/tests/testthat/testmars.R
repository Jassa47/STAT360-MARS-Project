library(testthat)
library(mars)

load(test_path("testmars.RData"))

test_that("mars() returns correct output", {
  m <- mars(y ~ ., data = marstestdata, control = testmc)
  expect_equal(m, testmars, ignore_attr = TRUE)
})
