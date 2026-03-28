library(testthat)
library(mars)

load(test_path("testfwd_stepwise.RData"))
load(test_path("testbwd_stepwise.RData"))

test_that("bwd_stepwise() returns correct output", {
  bwd <- bwd_stepwise(testfwd, testmc)
  expect_equal(bwd, testbwd)
})
