library(testthat)
library(mars)

load(test_path("testfwd_stepwise.RData"))

test_that("fwd_stepwise() returns correct output", {
  fwd <- fwd_stepwise(testy, testx, testmc)
  expect_equal(fwd, testfwd)
})
