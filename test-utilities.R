test_that("dummify_factors creates dummy variables", {
  # Test dummify_factors
  test_data <- data.frame(
    fct_var = factor(c("A", "B", "A", "C")),
    num_var = 1:4
  )
  
  result <- dummify_factors(test_data, "fct_var")
  
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > ncol(test_data))
  expect_true(any(grepl("fct_var_", names(result))))
})

test_that("syssamp performs systematic sampling", {
  # Test syssamp
  test_data <- data.frame(id = 1:100, value = rnorm(100))
  
  result <- syssamp(test_data, n = 10)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
  expect_true(all(result$id %in% test_data$id))
})

test_that("keyboardize_punctuation replaces punctuation", {
  # Test keyboardize_punctuation
  test_str <- "This is a "smart quote" with —dashes—"
  result <- keyboardize_punctuation(test_str)
  
  expect_type(result, "character")
  expect_true(!grepl("\u201C|\u201D|\u2014", result))
})

test_that("timefactory creates time variables", {
  # Test timefactory
  times <- c("2023-01-15 10:30:00", "2023-06-20 15:45:00")
  result <- timefactory(times)
  
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > 1)
  expect_true(any(grepl("year|month|day", names(result), ignore.case = TRUE)))
})
