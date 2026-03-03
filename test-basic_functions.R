test_that("tablena displays tables with NAs", {
  # Test basic tablena functionality
  result <- tablena(dec13_excerpt$q2, dec13_excerpt$receduc)
  
  expect_s3_class(result, "table")
  expect_true(length(result) > 0)
})

test_that("dk_to_na converts Don't Know responses to NA", {
  # Test dk_to_na function
  test_vec <- factor(c("Yes", "No", "Don't know/Refused (VOL.)", "Yes"))
  result <- dk_to_na(test_vec)
  
  expect_true(is.na(result[3]))
  expect_false(is.na(result[1]))
  expect_equal(as.character(result[1]), "Yes")
})

test_that("replace_if replaces values conditionally", {
  # Test replace_if function
  test_vec <- c(1, 2, NA, 4, 5)
  result <- replace_if(test_vec, NA, 0)
  
  expect_equal(result[3], 0)
  expect_equal(result[1], 1)
  expect_false(anyNA(result))
})

test_that("standardize_weights normalizes weight vector", {
  # Test standardize_weights
  weights <- c(0.5, 1.0, 1.5, 2.0)
  result <- standardize_weights(weights)
  
  expect_equal(mean(result), 1.0)
  expect_equal(length(result), length(weights))
})

test_that("fct_case_when works with factors", {
  # Test fct_case_when
  test_fct <- factor(c("a", "b", "c", "d"))
  result <- fct_case_when(
    test_fct == "a" ~ "group1",
    test_fct == "b" ~ "group1",
    TRUE ~ "group2"
  )
  
  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("group1", "group2"))
})
