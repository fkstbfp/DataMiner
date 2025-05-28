test_that("desc_stats works correctly", {
  test_data <- data.frame(
    num = c(1, 2, 3, NA),
    char = letters[1:4]
  )

  result <- desc_stats(test_data)
  expect_equal(names(result), "num")
  expect_equal(result$num$na_count, 1)
})

test_that("plot functions don't throw errors", {
  test_data <- data.frame(x = rnorm(100))
  expect_error(plot_distribution(test_data, "x"), NA)
  expect_error(plot_correlations(test_data), NA)
})
