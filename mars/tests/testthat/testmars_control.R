library(testthat)
library(mars)

load(test_path("mctest.RData"))
load(test_path("testmc.RData"))

test_that("mars.control() defaults are correct", {
  expect_equal(mars.control(), mctest)
})

test_that("mars.control(Mmax=10) is correct", {
  expect_equal(mars.control(Mmax = 10), testmc)
})
