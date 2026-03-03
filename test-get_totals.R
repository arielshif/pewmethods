test_that("get_totals works with basic unweighted data", {
  # Test basic unweighted crosstab
  result <- get_totals("q1", dec13_excerpt)
  
  expect_s3_class(result, "data.frame")
  expect_true("q1" %in% names(result))
  expect_true("unweighted" %in% names(result))
  expect_equal(sum(result$unweighted, na.rm = TRUE), 100)
})

test_that("get_totals works with weighted data", {
  # Test weighted crosstab
  result <- get_totals("receduc", dec13_excerpt, wt = "weight")
  
  expect_s3_class(result, "data.frame")
  expect_true("receduc" %in% names(result))
  expect_true("weight" %in% names(result))
  expect_equal(sum(result$weight, na.rm = TRUE), 100, tolerance = 0.01)
})

test_that("get_totals works with grouping variable", {
  # Test weighted crosstab by grouping variable
  result <- get_totals("q1", dec13_excerpt, wt = "weight", by = "receduc")
  
  expect_s3_class(result, "data.frame")
  expect_true("q1" %in% names(result))
  expect_true("weight_name" %in% names(result))
  expect_true(ncol(result) > 2)  # Should have multiple columns for categories
})

test_that("get_totals returns totals when percent = FALSE", {
  # Test totals instead of percentages
  result <- get_totals("q1", dec13_excerpt, percent = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(any(result$unweighted > 100))  # Totals should be larger than percentages
})

test_that("get_totals handles NA removal", {
  # Create data with NAs
  test_data <- dec13_excerpt
  test_data$q1[1:5] <- NA
  
  result_with_na <- get_totals("q1", test_data, na.rm = FALSE)
  result_without_na <- get_totals("q1", test_data, na.rm = TRUE)
  
  expect_true(nrow(result_with_na) >= nrow(result_without_na))
})
