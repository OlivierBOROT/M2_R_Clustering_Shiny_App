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

test_that("KMeansClusterer validates supported data types", {
  # Should accept numeric and factor (character auto-converted)
  # Should reject unsupported types like lists
  data_list <- data.frame(x = 1:10)
  data_list$y <- I(lapply(1:10, function(x) c(x, x+1))) # Column with lists
  
  expect_error(
    KMeansClusterer$new(data = data_list, n_clusters = 2),
    "unsupported type"
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

# ============================================================================
# Tests for mixed data (quantitative + qualitative)
# ============================================================================

test_that("KMeansClusterer accepts pure quantitative data (mtcars)", {
  data(mtcars)
  
  clusterer <- KMeansClusterer$new(
    data = mtcars,
    n_clusters = 3,
    standardize = TRUE,
    seed = 42
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_equal(length(clusterer$clusters), ncol(mtcars))
  expect_true(all(clusterer$clusters %in% 1:3))
})

test_that("KMeansClusterer accepts pure qualitative data", {
  skip_if_not_installed("PCAmixdata")
  
  data_quali <- data.frame(
    Color = factor(sample(c("Red", "Green", "Blue"), 32, replace = TRUE)),
    Size = factor(sample(c("Small", "Medium", "Large"), 32, replace = TRUE)),
    Shape = factor(sample(c("Circle", "Square", "Triangle"), 32, replace = TRUE)),
    Texture = factor(sample(c("Smooth", "Rough"), 32, replace = TRUE))
  )
  
  clusterer <- KMeansClusterer$new(
    data = data_quali,
    n_clusters = 2,
    seed = 42
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_equal(length(clusterer$clusters), ncol(data_quali))
})

test_that("KMeansClusterer accepts mixed data (quantitative + qualitative)", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars
  data_mixed$gear <- as.factor(data_mixed$gear)
  data_mixed$carb <- as.factor(data_mixed$carb)
  data_mixed$am <- as.factor(data_mixed$am)
  
  clusterer <- KMeansClusterer$new(
    data = data_mixed,
    n_clusters = 3,
    standardize = TRUE,
    seed = 42
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_equal(length(clusterer$clusters), ncol(data_mixed))
  
  # Check homogeneity is valid
  expect_true(clusterer$get_homogeneity() > 0)
  expect_true(clusterer$get_homogeneity() <= 1)
})

test_that("correlation initialization works with mixed data", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars[, 1:6]
  data_mixed$gear <- as.factor(mtcars$gear)
  
  clusterer <- KMeansClusterer$new(
    data = data_mixed,
    n_clusters = 2,
    standardize = TRUE,
    seed = 42,
    init_method = "correlation"
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("predict works with mixed supplementary variables", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars
  data_mixed$am <- as.factor(data_mixed$am)
  
  clusterer <- KMeansClusterer$new(
    data = data_mixed,
    n_clusters = 3,
    standardize = TRUE,
    seed = 42
  )
  clusterer$fit()
  
  # New mixed variables
  new_vars <- data.frame(
    new_quant = mtcars$hp * 2,
    new_qual = factor(sample(c("A", "B", "C"), 32, replace = TRUE))
  )
  
  expect_no_error(predictions <- clusterer$predict(new_vars))
  expect_equal(nrow(predictions), 2)
  expect_true(all(predictions$cluster %in% 1:3))
})

test_that("validation rejects missing values", {
  data_with_na <- mtcars
  data_with_na$mpg[1] <- NA
  
  expect_error(
    KMeansClusterer$new(data_with_na, n_clusters = 2),
    "missing values"
  )
})

test_that("validation converts character to factor", {
  data_char <- data.frame(
    x = rnorm(20),
    y = sample(c("A", "B", "C"), 20, replace = TRUE),  # character
    z = rnorm(20),
    stringsAsFactors = FALSE
  )
  
  clusterer <- KMeansClusterer$new(data_char, n_clusters = 2)
  
  # Check that y was converted to factor
  expect_true(is.factor(clusterer$data$y))
})

test_that("homogeneity calculation via PCAmix is correct", {
  skip_if_not_installed("PCAmixdata")
  
  # Dataset with clear structure
  set.seed(42)
  data_struct <- data.frame(
    v1 = rnorm(50),
    v2 = rnorm(50),
    v3 = rnorm(50) + 0.8 * rnorm(50),
    v4 = rnorm(50) + 0.8 * rnorm(50)
  )
  data_struct$v3 <- data_struct$v3 + 0.9 * data_struct$v1
  data_struct$v4 <- data_struct$v4 + 0.9 * data_struct$v1
  
  clusterer <- KMeansClusterer$new(
    data = data_struct,
    n_clusters = 2,
    standardize = TRUE,
    seed = 42
  )
  clusterer$fit()
  
  # Homogeneity should be relatively high
  expect_true(clusterer$get_homogeneity() > 0.3)
})

test_that("mixed distance (correlation init) computes Cramer's V", {
  skip_if_not_installed("PCAmixdata")
  
  data_quali <- data.frame(
    v1 = factor(sample(c("A", "B"), 30, replace = TRUE)),
    v2 = factor(sample(c("X", "Y", "Z"), 30, replace = TRUE)),
    v3 = factor(sample(c("1", "2"), 30, replace = TRUE))
  )
  
  clusterer <- KMeansClusterer$new(
    data = data_quali,
    n_clusters = 2,
    init_method = "correlation",
    seed = 42
  )
  
  # Should not crash with qualitative variables
  expect_no_error(clusterer$fit())
})

test_that("visualization coordinates include quantitative and qualitative", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars[, 1:6]
  data_mixed$gear <- as.factor(mtcars$gear)
  data_mixed$am <- as.factor(mtcars$am)
  
  clusterer <- KMeansClusterer$new(
    data = data_mixed,
    n_clusters = 2,
    seed = 42
  )
  clusterer$fit()
  
  coords <- clusterer$get_plot_data()$coords
  
  expect_true(nrow(coords) == ncol(data_mixed))
  expect_true(all(c("PC1", "PC2", "variable", "cluster") %in% names(coords)))
})
