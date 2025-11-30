# Tests for cluster validation functions (05_cluster_validator.R)

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data.frame(matrix(rnorm(n * p), ncol = p))
}

# ============================================================================
# Tests for elbow_method
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

# ============================================================================
# Tests for silhouette_method
# ============================================================================

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

test_that("silhouette_method with well-separated clusters produces good scores", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")

  # Create well-structured data with 3 groups of correlated variables
  set.seed(123)
  n <- 50
  data <- data.frame(
    # Group 1: highly correlated
    v1 = rnorm(n),
    v2 = rnorm(n),
    v3 = rnorm(n),
    # Group 2: highly correlated
    v4 = rnorm(n, mean = 5),
    v5 = rnorm(n, mean = 5),
    v6 = rnorm(n, mean = 5),
    # Group 3: highly correlated
    v7 = rnorm(n, mean = -3),
    v8 = rnorm(n, mean = -3),
    v9 = rnorm(n, mean = -3)
  )

  # Make groups more correlated internally
  data$v2 <- data$v1 + rnorm(n, sd = 0.3)
  data$v3 <- data$v1 + rnorm(n, sd = 0.3)
  data$v5 <- data$v4 + rnorm(n, sd = 0.3)
  data$v6 <- data$v4 + rnorm(n, sd = 0.3)
  data$v8 <- data$v7 + rnorm(n, sd = 0.3)
  data$v9 <- data$v7 + rnorm(n, sd = 0.3)

  result <- silhouette_method(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    standardize = TRUE,
    seed = 42
  )

  # Scores should be in valid range [-1, 1]
  expect_true(all(result$silhouette_scores >= -1))
  expect_true(all(result$silhouette_scores <= 1))

  # Should suggest K=3 (the true structure)
  expect_equal(result$suggested_k, 3)
})

# ============================================================================
# Tests for calinski_harabasz_method (homogeneity-based)
# ============================================================================

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

test_that("homogeneity_method produces valid Pseudo-F scores", {
  skip_if_not_installed("R6")

  data <- create_test_data(n = 100, p = 8)

  result <- calinski_harabasz_method(
    KMeansClusterer,
    data,
    k_range = 2:5,
    plot = FALSE,
    max_iter = 20,
    seed = 42
  )

  # Scores should be positive
  expect_true(all(result$ch_scores > 0))
  expect_true(all(is.finite(result$ch_scores)))

  # Note: We no longer check that score <= 1, because Pseudo-F can be large!
  # Pseudo-F can be 5, 20, 100... depending on clustering quality
})

# ============================================================================
# Tests for compare_k_selection_methods
# ============================================================================

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
  expect_equal(length(result$all_suggestions), 3) # 3 methods
  expect_true(result$consensus_k %in% 2:4)
})

test_that("validation metrics are reproducible with same seed", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")

  data(mtcars)

  # Run twice with same seed
  result1 <- compare_k_selection_methods(
    KMeansClusterer,
    mtcars,
    k_range = 2:3,
    plot = FALSE,
    standardize = TRUE,
    seed = 123
  )

  result2 <- compare_k_selection_methods(
    KMeansClusterer,
    mtcars,
    k_range = 2:3,
    plot = FALSE,
    standardize = TRUE,
    seed = 123
  )

  # Results should be identical
  expect_equal(result1$consensus_k, result2$consensus_k)
  expect_equal(result1$all_suggestions, result2$all_suggestions)
})

test_that("K selection methods work with small datasets", {
  skip_if_not_installed("R6")

  # Create minimal dataset (5 variables, 10 observations)
  # Need at least n_clusters + 1 variables for KMeansClusterer
  small_data <- data.frame(
    var1 = rnorm(10),
    var2 = rnorm(10),
    var3 = rnorm(10),
    var4 = rnorm(10),
    var5 = rnorm(10)
  )

  # Should not crash with K=2 or K=3 (less than 5 variables)
  expect_no_error(
    elbow_method(
      KMeansClusterer,
      small_data,
      k_range = 2:3,
      plot = FALSE,
      standardize = TRUE,
      seed = 42
    )
  )

  expect_no_error(
    calinski_harabasz_method(
      KMeansClusterer,
      small_data,
      k_range = 2:3,
      plot = FALSE,
      standardize = TRUE,
      seed = 42
    )
  )
})
