context("test-rorpy")

test_that("website test cases", {
  expect_equal(rorpy("https://google.com")$r, 0)
  expect_true(rorpy("http://dplyr.tidyverse.org")$r > 0.99)
  expect_true(rorpy("https://keras.io")$py > 0.98)
})
