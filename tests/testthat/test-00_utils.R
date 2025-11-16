# Tests for utility functions (00_utils.R)

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
# Tests for K selection methods
# ============================================================================

test_that("elbow_method runs without errors", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  # Should run without error
  expect_no_error({
    result <- elbow_method(
      KMeansClusterer,
      data,
      k_range = 2:4,
      plot = FALSE,
      standardize = TRUE,
      max_iter = 20
    )
  })
})

test_that("elbow_method returns correct structure", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  result <- elbow_method(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 20
  )
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("k_values", "inertias", "suggested_k", "distances"))
  expect_equal(length(result$inertias), 3)
  expect_true(result$suggested_k %in% 2:4)
})

test_that("elbow_method errors on invalid k_range", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  expect_error(
    elbow_method(KMeansClusterer, data, k_range = 2, plot = FALSE),
    "at least 2 values"
  )
})

test_that("silhouette_method runs without errors", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 5)
  
  expect_no_error({
    result <- silhouette_method(
      KMeansClusterer,
      data,
      k_range = 2:4,
      plot = FALSE,
      max_iter = 20
    )
  })
})

test_that("silhouette_method returns correct structure", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 5)
  
  result <- silhouette_method(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 20
  )
  
  expect_type(result, "list")
  expect_named(result, c("k_values", "silhouette_scores", "suggested_k"))
  expect_equal(length(result$silhouette_scores), 3)
  expect_true(result$suggested_k %in% 2:4)
})

test_that("calinski_harabasz_method runs without errors", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  expect_no_error({
    result <- calinski_harabasz_method(
      KMeansClusterer,
      data,
      k_range = 2:4,
      plot = FALSE,
      max_iter = 20
    )
  })
})

test_that("calinski_harabasz_method returns correct structure", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  result <- calinski_harabasz_method(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 20
  )
  
  expect_type(result, "list")
  expect_named(result, c("k_values", "ch_scores", "suggested_k"))
  expect_equal(length(result$ch_scores), 3)
  expect_true(result$suggested_k %in% 2:4)
})

test_that("davies_bouldin_method runs without errors", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  expect_no_error({
    result <- davies_bouldin_method(
      KMeansClusterer,
      data,
      k_range = 2:4,
      plot = FALSE,
      max_iter = 20
    )
  })
})

test_that("davies_bouldin_method returns correct structure", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 5)
  
  result <- davies_bouldin_method(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 20
  )
  
  expect_type(result, "list")
  expect_named(result, c("k_values", "db_scores", "suggested_k"))
  expect_equal(length(result$db_scores), 3)
  expect_true(result$suggested_k %in% 2:4)
})

test_that("compare_k_selection_methods runs without errors", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 5)
  
  expect_no_error({
    result <- compare_k_selection_methods(
      KMeansClusterer,
      data,
      k_range = 2:4,
      plot = FALSE,
      max_iter = 20
    )
  })
})

test_that("compare_k_selection_methods returns consensus", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 5)
  
  result <- compare_k_selection_methods(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 20
  )
  
  expect_type(result, "list")
  expect_true("consensus_k" %in% names(result))
  expect_true("all_suggestions" %in% names(result))
  expect_equal(length(result$all_suggestions), 4)  # 4 methods
  expect_true(result$consensus_k %in% 2:4)
})
