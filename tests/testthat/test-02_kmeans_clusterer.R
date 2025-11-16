# Tests for KMeansClusterer (02_kmeans_clusterer.R)

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(rnorm(n * p), ncol = p))
  colnames(data) <- paste0("Var", 1:p)
  data
}

# Helper to create structured data with clear clusters
create_clustered_data <- function(n_per_cluster = 30, seed = 123) {
  set.seed(seed)
  
  # 3 clusters with different means
  cluster1 <- data.frame(
    V1 = rnorm(n_per_cluster, mean = 0, sd = 0.5),
    V2 = rnorm(n_per_cluster, mean = 0, sd = 0.5),
    V3 = rnorm(n_per_cluster, mean = 0, sd = 0.5)
  )
  
  cluster2 <- data.frame(
    V1 = rnorm(n_per_cluster, mean = 5, sd = 0.5),
    V2 = rnorm(n_per_cluster, mean = 5, sd = 0.5),
    V3 = rnorm(n_per_cluster, mean = 5, sd = 0.5)
  )
  
  cluster3 <- data.frame(
    V1 = rnorm(n_per_cluster, mean = -5, sd = 0.5),
    V2 = rnorm(n_per_cluster, mean = -5, sd = 0.5),
    V3 = rnorm(n_per_cluster, mean = -5, sd = 0.5)
  )
  
  # Transpose to have variables in columns
  t(rbind(cluster1, cluster2, cluster3))
}

# ============================================================================
# Tests for initialization
# ============================================================================

test_that("KMeansClusterer initializes correctly", {
  data <- create_test_data(n = 100, p = 5)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    standardize = TRUE,
    max_iter = 100,
    tol = 1e-4,
    seed = 123,
    n_init = 10,
    init_method = "homogeneity++"
  )
  
  expect_s3_class(clusterer, "KMeansClusterer")
  expect_s3_class(clusterer, "BaseClusterer")
  expect_equal(clusterer$n_clusters, 3)
  expect_equal(clusterer$get_n_init(), 10)
  expect_equal(clusterer$get_init_method(), "homogeneity++")
})

test_that("KMeansClusterer validates n_init", {
  data <- create_test_data(n = 100, p = 5)
  
  expect_error(
    KMeansClusterer$new(data = data, n_clusters = 3, n_init = 0),
    "n_init"
  )
  
  expect_error(
    KMeansClusterer$new(data = data, n_clusters = 3, n_init = -5),
    "n_init"
  )
})

test_that("KMeansClusterer validates init_method", {
  data <- create_test_data(n = 100, p = 5)
  
  expect_error(
    KMeansClusterer$new(data = data, n_clusters = 3, init_method = "invalid"),
    "init_method"
  )
})

test_that("KMeansClusterer accepts valid init_methods", {
  data <- create_test_data(n = 100, p = 5)
  
  expect_no_error(
    KMeansClusterer$new(data = data, n_clusters = 3, init_method = "homogeneity++")
  )
  
  expect_no_error(
    KMeansClusterer$new(data = data, n_clusters = 3, init_method = "correlation")
  )
  
  expect_no_error(
    KMeansClusterer$new(data = data, n_clusters = 3, init_method = "random")
  )
})

test_that("KMeansClusterer validates numeric data", {
  data <- data.frame(
    x = c("a", "b", "c"),
    y = c("d", "e", "f")
  )
  
  expect_error(
    KMeansClusterer$new(data = data, n_clusters = 2),
    "numeric"
  )
})

# ============================================================================
# Tests for fit method
# ============================================================================

test_that("fit method runs successfully", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    max_iter = 50,
    n_init = 3,
    seed = 123
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_length(clusterer$clusters, 6)
  expect_true(all(clusterer$clusters %in% 1:3))
})

test_that("fit with homogeneity++ initialization", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    init_method = "homogeneity++",
    max_iter = 50,
    seed = 123
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit with correlation initialization", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    init_method = "correlation",
    max_iter = 50
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit with random initialization", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    init_method = "random",
    max_iter = 50,
    seed = 123
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit produces valid cluster assignments", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    seed = 123
  )
  
  clusterer$fit()
  
  # All variables assigned
  expect_equal(length(clusterer$clusters), ncol(data))
  
  # All clusters represented
  expect_equal(length(unique(clusterer$clusters)), 3)
  
  # All assignments valid
  expect_true(all(clusterer$clusters >= 1 & clusterer$clusters <= 3))
})

test_that("fit with seed produces reproducible results", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer1 <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    seed = 123,
    init_method = "random",
    n_init = 1
  )
  clusterer1$fit()
  
  clusterer2 <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    seed = 123,
    init_method = "random",
    n_init = 1
  )
  clusterer2$fit()
  
  expect_equal(clusterer1$clusters, clusterer2$clusters)
})

# ============================================================================
# Tests for getters
# ============================================================================

test_that("get_homogeneity returns valid score", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  homog <- clusterer$get_homogeneity()
  
  expect_type(homog, "double")
  expect_true(homog >= 0 && homog <= 1)
})

test_that("get_cluster_homogeneity returns vector", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  cluster_homog <- clusterer$get_cluster_homogeneity()
  
  expect_length(cluster_homog, 3)
  expect_true(all(cluster_homog >= 0 & cluster_homog <= 1))
})

test_that("get_inertia returns negative homogeneity", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  inertia <- clusterer$get_inertia()
  homog <- clusterer$get_homogeneity()
  
  expect_equal(inertia, -homog)
})

test_that("get_iterations returns valid count", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, max_iter = 50, seed = 123)
  clusterer$fit()
  
  iters <- clusterer$get_iterations()
  
  expect_type(iters, "integer")
  expect_true(iters >= 1 && iters <= 50)
})

test_that("get_actual_runs returns valid count", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, n_init = 5, seed = 123)
  clusterer$fit()
  
  runs <- clusterer$get_actual_runs()
  
  expect_type(runs, "integer")
  expect_true(runs >= 1 && runs <= 5)
})

test_that("get_cluster_pca returns list", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  pca_list <- clusterer$get_cluster_pca()
  
  expect_type(pca_list, "list")
  expect_length(pca_list, 3)
})

test_that("get_pca returns global PCA", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  pca <- clusterer$get_pca()
  
  expect_s3_class(pca, "prcomp")
})

test_that("get_loadings returns data frame", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  loadings <- clusterer$get_loadings()
  
  expect_s3_class(loadings, "data.frame")
  expect_equal(nrow(loadings), 6)  # 6 variables
})

# ============================================================================
# Tests for predict method
# ============================================================================

test_that("predict assigns new variables to clusters", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  
  predictions <- clusterer$predict(new_data, return_scores = FALSE)
  
  expect_s3_class(predictions, "data.frame")
  expect_equal(nrow(predictions), 3)
  expect_true("cluster" %in% names(predictions))
  expect_true(all(predictions$cluster %in% 1:3))
})

test_that("predict with return_scores includes scores", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  
  predictions <- clusterer$predict(new_data, return_scores = TRUE)
  
  expect_true("score_1" %in% names(predictions))
  expect_true("score_2" %in% names(predictions))
  expect_true("score_3" %in% names(predictions))
  
  # Scores should be numeric and non-negative
  expect_type(predictions$score_1, "double")
  expect_true(all(predictions$score_1 >= 0))
})

test_that("predict requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  
  expect_error(
    clusterer$predict(new_data),
    "not fitted"
  )
})

# ============================================================================
# Tests for plot methods
# ============================================================================

test_that("plot_fit runs without error", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  expect_no_error(clusterer$plot_fit())
})

test_that("plot_fit requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)
  
  expect_error(
    clusterer$plot_fit(),
    "not fitted|must be fitted"
  )
})

test_that("plot_predict runs without error", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  clusterer$predict(new_data)
  
  expect_no_error(clusterer$plot_predict())
})

# ============================================================================
# Tests for print and summary
# ============================================================================

test_that("print method shows model info", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  expect_output(print(clusterer), "Variable Clusterer")
  expect_output(print(clusterer), "ClustOfVar-like")
  expect_output(print(clusterer), "3 clusters")
})

test_that("summary method shows detailed info", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  expect_output(clusterer$summary(), "Cluster")
  expect_output(clusterer$summary(), "Homogeneity")
})

# ============================================================================
# Tests for cluster quality
# ============================================================================

test_that("clustering improves with more iterations", {
  data <- create_clustered_data(n_per_cluster = 30)
  
  clusterer_few <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    max_iter = 5,
    seed = 123,
    n_init = 1
  )
  clusterer_few$fit()
  
  clusterer_many <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    max_iter = 100,
    seed = 123,
    n_init = 1
  )
  clusterer_many$fit()
  
  # More iterations should generally not decrease homogeneity
  expect_gte(
    clusterer_many$get_homogeneity(),
    clusterer_few$get_homogeneity() - 0.01  # small tolerance
  )
})

test_that("all clusters have at least one variable", {
  data <- create_test_data(n = 100, p = 10)
  
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  clusterer$fit()
  
  cluster_sizes <- table(clusterer$clusters)
  
  expect_equal(length(cluster_sizes), 3)
  expect_true(all(cluster_sizes >= 1))
})
