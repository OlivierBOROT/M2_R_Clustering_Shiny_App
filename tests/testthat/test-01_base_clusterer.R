# Tests for BaseClusterer (01_base_clusterer.R)

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(rnorm(n * p), ncol = p))
  colnames(data) <- paste0("Var", 1:p)
  data
}

# ============================================================================
# Tests for initialization
# ============================================================================

test_that("BaseClusterer initializes correctly", {
  data <- create_test_data(n = 100, p = 5)

  clusterer <- BaseClusterer$new(
    data = data,
    n_clusters = 3,
    standardize = TRUE,
    max_iter = 100,
    tol = 1e-4
  )

  expect_s3_class(clusterer, "BaseClusterer")
  expect_equal(clusterer$n_clusters, 3)
  expect_true(clusterer$standardize)
  expect_equal(clusterer$max_iter, 100)
  expect_equal(clusterer$tol, 1e-4)
  expect_false(clusterer$fitted)
  expect_null(clusterer$clusters)
})

test_that("BaseClusterer stores variable names", {
  data <- create_test_data(n = 100, p = 5)

  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_equal(clusterer$variable_names, colnames(data))
  expect_equal(length(clusterer$variable_names), 5)
})

test_that("BaseClusterer creates variable names if missing", {
  data <- as.matrix(create_test_data(n = 100, p = 5))
  colnames(data) <- NULL

  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_equal(clusterer$variable_names, paste0("Var", 1:5))
})

# ============================================================================
# Tests for parameter validation
# ============================================================================

test_that("BaseClusterer validates n_clusters", {
  data <- create_test_data(n = 100, p = 5)

  # n_clusters < 2
  expect_error(
    BaseClusterer$new(data = data, n_clusters = 1),
    "n_clusters must be an integer >= 2"
  )

  # n_clusters not numeric
  expect_error(
    BaseClusterer$new(data = data, n_clusters = "two"),
    "n_clusters must be an integer >= 2"
  )
})

test_that("BaseClusterer validates max_iter", {
  data <- create_test_data(n = 100, p = 5)

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 3, max_iter = 0),
    "max_iter must be a positive integer"
  )

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 3, max_iter = -10),
    "max_iter must be a positive integer"
  )
})

test_that("BaseClusterer validates tol", {
  data <- create_test_data(n = 100, p = 5)

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 3, tol = 0),
    "tol must be a positive number"
  )

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 3, tol = -1e-4),
    "tol must be a positive number"
  )
})

test_that("BaseClusterer validates n_clusters vs n_variables", {
  data <- create_test_data(n = 100, p = 5)

  # Test n_clusters > n_vars
  expect_error(
    BaseClusterer$new(data = data, n_clusters = 10),
    "must be strictly less than number of variables"
  )

  # Test n_clusters == n_vars (also invalid)
  expect_error(
    BaseClusterer$new(data = data, n_clusters = 5),
    "must be strictly less than number of variables"
  )
})

# ============================================================================
# Tests for data validation
# ============================================================================

test_that("BaseClusterer validates data type", {
  # Not a data.frame or matrix
  expect_error(
    BaseClusterer$new(data = list(a = 1:10, b = 1:10), n_clusters = 2),
    "data must be a data.frame or matrix"
  )
})

test_that("BaseClusterer validates minimum variables", {
  data <- data.frame(x = rnorm(100))

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 2),
    "data must have at least 2 variables"
  )
})

test_that("BaseClusterer validates minimum observations", {
  data <- data.frame(x = 1, y = 2)

  expect_error(
    BaseClusterer$new(data = data, n_clusters = 2),
    "data must have at least 2 observations"
  )
})

# ============================================================================
# Tests for public methods
# ============================================================================

test_that("get_n_variables returns correct count", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_equal(clusterer$get_n_variables(), 5)
})

test_that("get_n_observations returns correct count", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_equal(clusterer$get_n_observations(), 100)
})

test_that("get_cluster_sizes requires fitted model", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_error(
    clusterer$get_cluster_sizes(),
    "not fitted yet"
  )
})

test_that("get_cluster_members requires fitted model", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_error(
    clusterer$get_cluster_members(1),
    "not fitted yet"
  )
})

test_that("get_cluster_members validates cluster_id", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Manually set fitted and clusters for testing
  clusterer$fitted <- TRUE
  clusterer$clusters <- rep(1:3, length.out = 5)

  expect_error(
    clusterer$get_cluster_members(0),
    "cluster_id must be between"
  )

  expect_error(
    clusterer$get_cluster_members(5),
    "cluster_id must be between"
  )
})

test_that("reset clears fitted state", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Manually set fitted
  clusterer$fitted <- TRUE
  clusterer$clusters <- rep(1:3, length.out = 5)

  clusterer$reset()

  expect_false(clusterer$fitted)
  expect_null(clusterer$clusters)
})

test_that("get_results requires fitted model", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  expect_error(
    clusterer$get_results(),
    "not fitted yet"
  )
})

test_that("get_results returns correct structure", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Manually set fitted
  clusterer$fitted <- TRUE
  clusterer$clusters <- rep(1:3, length.out = 5)

  results <- clusterer$get_results()

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 5)
  expect_named(results, c("variable", "cluster"))
  expect_equal(results$variable, clusterer$variable_names)
})

test_that("save_results creates file", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Manually set fitted
  clusterer$fitted <- TRUE
  clusterer$clusters <- rep(1:3, length.out = 5)

  temp_file <- tempfile(fileext = ".csv")

  clusterer$save_results(temp_file)

  expect_true(file.exists(temp_file))

  # Read and verify
  saved_data <- read.csv(temp_file)
  expect_equal(nrow(saved_data), 5)
  expect_named(saved_data, c("variable", "cluster"))

  # Cleanup
  unlink(temp_file)
})

# ============================================================================
# Tests for print and summary methods
# ============================================================================
# Note: BaseClusterer is an abstract class, so print() and summary() are
# minimal implementations. These methods are tested in derived classes
# (e.g., test-02_kmeans_clusterer.R) where they are properly implemented.

test_that("print method exists and runs without error", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Just check it runs without error (abstract class has minimal implementation)
  expect_no_error(print(clusterer))
})

test_that("summary method exists and runs without error", {
  data <- create_test_data(n = 100, p = 5)
  clusterer <- BaseClusterer$new(data = data, n_clusters = 3)

  # Just check it runs without error (abstract class has minimal implementation)
  expect_no_error(clusterer$summary())
})
