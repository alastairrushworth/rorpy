context("test-rorpy")

test_that("website test cases", {
  expect_equal(rorpy("https://google.com")$r, 0)
  expect_true(rorpy("http://dplyr.tidyverse.org")$r > 0.95)
  expect_true(rorpy("https://keras.io")$py > 0.95)
})

test_that("Vector of urls", {
  wbs <- c("https://google.com", "http://dplyr.tidyverse.org", "https://keras.io")
  expect_is(rorpy(wbs, show_progress = FALSE), 'tbl')
})

test_that("List of html pages", {
  wbs <- c("http://dplyr.tidyverse.org", "https://keras.io")
  pages <- lapply(wbs, xml2::read_html)
  expect_is(rorpy(wbs, show_progress = FALSE), 'tbl')
})