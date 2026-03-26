library(testthat)
library(mars)

load(test_path("testfwd_stepwise.RData"))
load(test_path("testLOF.RData"))

test_that("LOF() returns correct GCV value", {
  dat <- data.frame(y = testfwd$y, testfwd$B)
  expect_equal(LOF(y ~ . - 1, dat, testmc), testLOF)
})
