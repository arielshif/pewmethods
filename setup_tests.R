# Setup script for creating test directories and preparing package

# Create test directories
if (!dir.exists("tests")) dir.create("tests")
if (!dir.exists("tests/testthat")) dir.create("tests/testthat")

cat("Test directories created successfully\n")

# Install testthat if needed
if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("Installing testthat package...\n")
  install.packages("testthat", repos = "https://cloud.r-project.org")
}

cat("Setup complete!\n")
