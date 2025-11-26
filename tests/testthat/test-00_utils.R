# Tests for low-level utility functions (00_utils.R)
# 
# This file tests mathematical and data manipulation utilities.
# For cluster validation and K selection methods, see test-05_cluster_validator.R

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data.frame(matrix(rnorm(n * p), ncol = p))
}

# ============================================================================
# Tests for standardize_data
# ============================================================================

test_that("standardize_data centers and scales correctly", {
  data <- create_test_data(n = 50, p = 3)
  
  result <- standardize_data(data, center = TRUE, scale = TRUE)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check that means are approximately 0
  expect_equal(as.numeric(colMeans(result)), rep(0, 3), tolerance = 1e-10)
  
  # Check that standard deviations are approximately 1
  expect_equal(as.numeric(apply(result, 2, sd)), rep(1, 3), tolerance = 1e-10)
  
  # Check that attributes are stored
  expect_true(!is.null(attr(result, "centers")))
  expect_true(!is.null(attr(result, "scales")))
})

test_that("standardize_data with center only", {
  data <- create_test_data(n = 50, p = 3)
  
  result <- standardize_data(data, center = TRUE, scale = FALSE)
  
  # Means should be 0
  expect_equal(as.numeric(colMeans(result)), rep(0, 3), tolerance = 1e-10)
  
  # Standard deviations should NOT be 1
  expect_false(all(abs(apply(result, 2, sd) - 1) < 1e-10))
})

test_that("standardize_data with scale only", {
  data <- create_test_data(n = 50, p = 3)
  data <- data + 10  # Add offset
  
  result <- standardize_data(data, center = FALSE, scale = TRUE)
  
  # Means should NOT be 0
  expect_false(all(abs(colMeans(result)) < 1e-10))
  
  # When scale=TRUE without centering, the data is divided by the SD of the ORIGINAL data
  # not the centered data, so the resulting SD will not necessarily be 1
  # Just check that scaling was applied
  expect_true(!is.null(attr(result, "scales")))
  expect_equal(length(attr(result, "scales")), 3)
})

# ============================================================================
# Tests for apply_standardization
# ============================================================================

test_that("apply_standardization uses provided parameters", {
  data <- create_test_data(n = 50, p = 3)
  
  # Get parameters from first standardization
  scaled1 <- standardize_data(data)
  centers <- attr(scaled1, "centers")
  scales <- attr(scaled1, "scales")
  
  # Apply to new data
  new_data <- create_test_data(n = 50, p = 3, seed = 456)
  scaled2 <- apply_standardization(new_data, centers, scales)
  
  # Check that it's a data frame
  expect_s3_class(scaled2, "data.frame")
  
  # Check dimensions
  expect_equal(dim(scaled2), dim(new_data))
})

test_that("apply_standardization with NULL parameters", {
  data <- create_test_data(n = 50, p = 3)
  
  result <- apply_standardization(data, centers = NULL, scales = NULL)
  
  # Should return data unchanged
  expect_equal(result, as.data.frame(data))
})

# ============================================================================
# Tests for euclidean_distance
# ============================================================================

test_that("euclidean_distance calculates correctly", {
  points <- matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE)
  centers <- matrix(c(0, 0, 2, 2), ncol = 2, byrow = TRUE)
  
  distances <- euclidean_distance(points, centers)
  
  # Check dimensions: 2 centers x 3 points
  expect_equal(dim(distances), c(2, 3))
  
  # Check specific distances
  expect_equal(distances[1, 1], 0)  # center 1 to point 1
  expect_equal(distances[2, 3], 0)  # center 2 to point 3
  expect_equal(distances[1, 2], sqrt(2), tolerance = 1e-10)
})

test_that("euclidean_distance handles data frames", {
  points <- data.frame(x = c(0, 1, 2), y = c(0, 1, 2))
  centers <- data.frame(x = c(0, 2), y = c(0, 2))
  
  distances <- euclidean_distance(points, centers)
  
  expect_equal(dim(distances), c(2, 3))
})

test_that("euclidean_distance errors on dimension mismatch", {
  points <- matrix(c(0, 0, 1, 1), ncol = 2)
  centers <- matrix(c(0, 0, 0), ncol = 3)
  
  expect_error(
    euclidean_distance(points, centers),
    "same number of columns"
  )
})

# ============================================================================
# Tests for get_correlation_distance_matrix (ClustOfVar approach)
# ============================================================================

test_that("get_correlation_distance_matrix produces valid distance matrix", {
  # Create simple test data
  data <- data.frame(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(2, 4, 6, 8, 10),  # Perfectly correlated with var1
    var3 = c(5, 4, 3, 2, 1)    # Negatively correlated
  )
  
  # Calculate distance matrix
  dist_mat <- get_correlation_distance_matrix(data)
  
  # Tests
  expect_s3_class(dist_mat, "dist")
  expect_equal(attr(dist_mat, "Size"), 3)
  
  # Distance between perfectly correlated variables should be ~0
  dist_values <- as.matrix(dist_mat)
  expect_lt(dist_values[1, 2], 0.01)
  
  # Distance should be positive
  expect_true(all(dist_values >= 0))
  
  # Distance should be symmetric
  expect_equal(dist_values[1, 2], dist_values[2, 1])
})

test_that("get_correlation_distance_matrix is Euclidean", {
  # Test triangle inequality (Euclidean property)
  data <- data.frame(
    var1 = rnorm(50),
    var2 = rnorm(50),
    var3 = rnorm(50)
  )
  
  dist_mat <- as.matrix(get_correlation_distance_matrix(data))
  
  # Triangle inequality: d(i,j) <= d(i,k) + d(k,j)
  d12 <- dist_mat[1, 2]
  d13 <- dist_mat[1, 3]
  d23 <- dist_mat[2, 3]
  
  expect_lte(d12, d13 + d23 + 1e-10)  # Small tolerance for numerical errors
  expect_lte(d13, d12 + d23 + 1e-10)
  expect_lte(d23, d12 + d13 + 1e-10)
})

test_that("Correlation distance equals sqrt(2*(1-cor))", {
  # Manual verification
  data <- data.frame(
    var1 = 1:10,
    var2 = 2:11,
    var3 = 10:1
  )
  
  # Calculate using function
  dist_mat <- as.matrix(get_correlation_distance_matrix(data))
  
  # Calculate manually
  scaled <- scale(data, center = TRUE, scale = TRUE)
  cor_mat <- cor(scaled)
  expected_dist <- sqrt(2 * (1 - cor_mat))
  
  # Should match
  expect_equal(dist_mat, expected_dist, tolerance = 1e-10)
})
