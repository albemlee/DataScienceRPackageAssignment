library(testthat)
test_that("fars_summarize_years", {
  expect_that(sum(fars_summarize_years(2013)), equals(30280))
  expect_that(length(fars_summarize_years(2013:2015)), equals(4))
})