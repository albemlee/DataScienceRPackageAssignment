library(testthat)
library(dplyr)
library(tidyr)
library(DataScienceRPackageAssignment)

context("Test fars_summarize_years function")

test_that("DataScienceRPackageAssignment", {
  expect_that(sum(fars_summarize_years(2013)), equals(30280))
  expect_that(length(fars_summarize_years(2013:2015)), equals(4))
})