test_that("calculate_deff calculates design effect", {
  skip_if_not_installed("survey")
  
  # Test basic design effect calculation
  test_data <- dec13_excerpt
  test_data$weight_std <- standardize_weights(test_data$weight)
  
  result <- calculate_deff(test_data, "q1", "weight_std")
  
  expect_type(result, "double")
  expect_true(result > 0)
  expect_true(is.finite(result))
})

test_that("trim_weights trims extreme weights", {
  # Test trim_weights function
  weights <- c(0.1, 0.5, 1.0, 2.0, 10.0)
  result <- trim_weights(weights, max_ratio = 4)
  
  expect_equal(length(result), length(weights))
  expect_true(max(result) / min(result) <= 4)
})

test_that("create_raking_targets creates proper target structure", {
  # Test create_raking_targets
  test_data <- dec13_excerpt
  
  result <- create_raking_targets(
    test_data,
    sex = c("Male" = 0.49, "Female" = 0.51)
  )
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("sex" %in% names(result))
})
