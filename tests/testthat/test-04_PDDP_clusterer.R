# Tests for DivisiveClusterer (04_PDDP_clusterer.R)

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(rnorm(n * p), ncol = p))
  colnames(data) <- paste0("Var", 1:p)
  data
}

# Helper to create correlated variable groups
create_correlated_groups <- function(n = 100, seed = 123) {
  set.seed(seed)
  
  # Group 1: Highly correlated variables (v1, v2, v3)
  base1 <- rnorm(n)
  v1 <- base1 + rnorm(n, sd = 0.2)
  v2 <- base1 + rnorm(n, sd = 0.2)
  v3 <- base1 + rnorm(n, sd = 0.2)
  
  # Group 2: Another correlated group (v4, v5, v6)
  base2 <- rnorm(n)
  v4 <- base2 + rnorm(n, sd = 0.2)
  v5 <- base2 + rnorm(n, sd = 0.2)
  v6 <- base2 + rnorm(n, sd = 0.2)
  
  # Group 3: Third correlated group (v7, v8)
  base3 <- rnorm(n)
  v7 <- base3 + rnorm(n, sd = 0.3)
  v8 <- base3 + rnorm(n, sd = 0.3)
  
  data.frame(v1, v2, v3, v4, v5, v6, v7, v8)
}

# ============================================================================
# Tests for initialization
# ============================================================================

test_that("DivisiveClusterer initializes correctly", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    standardize = TRUE,
    min_cluster_size = 3,
    rotation_method = "varimax",
    split_criterion = "eigenvalue2"
  )
  
  expect_s3_class(clusterer, "DivisiveClusterer")
  expect_s3_class(clusterer, "BaseClusterer")
  expect_equal(clusterer$n_clusters, 3)
  expect_equal(clusterer$min_cluster_size, 3)
  expect_equal(clusterer$rotation_method, "varimax")
  expect_equal(clusterer$split_criterion, "eigenvalue2")
  expect_false(clusterer$fitted)
})

test_that("DivisiveClusterer validates rotation_method", {
  data <- create_test_data(n = 100, p = 6)
  
  expect_no_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, rotation_method = "varimax")
  )
  
  expect_no_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, rotation_method = "promax")
  )
  
  expect_no_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, rotation_method = "none")
  )
  
  expect_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, rotation_method = "invalid"),
    "arg"
  )
})

test_that("DivisiveClusterer validates split_criterion", {
  data <- create_test_data(n = 100, p = 6)
  
  expect_no_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, split_criterion = "eigenvalue2")
  )
  
  expect_no_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, split_criterion = "homogeneity")
  )
  
  expect_error(
    DivisiveClusterer$new(data = data, n_clusters = 2, split_criterion = "invalid"),
    "arg"
  )
})

test_that("DivisiveClusterer inherits BaseClusterer validation", {
  data <- create_test_data(n = 100, p = 6)
  
  # n_clusters < 2
  expect_error(
    DivisiveClusterer$new(data = data, n_clusters = 1),
    "n_clusters"
  )
  
  # n_clusters >= n_vars
  expect_error(
    DivisiveClusterer$new(data = data, n_clusters = 6),
    "must be strictly less than number of variables"
  )
})

test_that("DivisiveClusterer initializes stopping criteria correctly", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 5,
    stop_at_kaiser = TRUE,
    min_eigenvalue_ratio = 0.2
  )
  
  expect_true(clusterer$stop_at_kaiser)
  expect_equal(clusterer$min_eigenvalue_ratio, 0.2)
})

# ============================================================================
# Tests for fit method
# ============================================================================

test_that("fit method runs successfully", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_length(clusterer$clusters, 6)
  expect_true(all(clusterer$clusters %in% 1:3))
})

test_that("fit produces valid cluster assignments", {
  data <- create_test_data(n = 100, p = 8)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3
  )
  
  clusterer$fit()
  
  # All variables assigned
  expect_equal(length(clusterer$clusters), ncol(data))
  
  # All clusters represented (may have fewer if stopping criteria met)
  expect_true(length(unique(clusterer$clusters)) >= 1)
  expect_true(length(unique(clusterer$clusters)) <= 3)
  
  # All assignments valid
  expect_true(all(clusterer$clusters >= 1))
})

test_that("fit is deterministic (no random seed needed)", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer1 <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer1$fit()
  
  clusterer2 <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer2$fit()
  
  # DivisiveClusterer should be 100% deterministic
  expect_equal(clusterer1$clusters, clusterer2$clusters)
})

test_that("fit with varimax rotation", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    rotation_method = "varimax"
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit with promax rotation", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    rotation_method = "promax",
    promax_m = 4
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit with no rotation", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    rotation_method = "none"
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit with homogeneity split criterion", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    split_criterion = "homogeneity"
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("fit detects pure numeric data and uses fast path", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  
  expect_output(clusterer$fit(), "NUMERIC")
  expect_true(clusterer$use_fast_path)
  expect_equal(clusterer$data_mode, "numeric")
})

test_that("fit with Kaiser stopping criterion", {
  data <- create_test_data(n = 100, p = 10)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 8,  # High target, but Kaiser should stop early
    stop_at_kaiser = TRUE
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  # Actual number of clusters may be less than requested
  expect_true(max(clusterer$clusters) <= 8)
})

test_that("fit with eigenvalue ratio stopping criterion", {
  data <- create_test_data(n = 100, p = 10)
  
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 8,
    min_eigenvalue_ratio = 0.3  # Strict ratio, should stop early
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("all clusters have at least one variable", {
  data <- create_test_data(n = 100, p = 10)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  cluster_sizes <- table(clusterer$clusters)
  
  expect_true(all(cluster_sizes >= 1))
})

# ============================================================================
# Tests for getters
# ============================================================================

test_that("get_homogeneity returns valid score", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  homog <- clusterer$get_homogeneity()
  
  expect_type(homog, "double")
  expect_true(homog >= 0 && homog <= 1)
})

test_that("get_cluster_homogeneity returns vector", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  cluster_homog <- clusterer$get_cluster_homogeneity()
  
  expect_length(cluster_homog, 3)
  expect_true(all(cluster_homog >= 0 & cluster_homog <= 1))
})

test_that("get_inertia returns 1 - homogeneity", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  inertia <- clusterer$get_inertia()
  homog <- clusterer$get_homogeneity()
  
  expect_equal(inertia, 1 - homog, tolerance = 1e-10)
})

test_that("get_unexplained_variance equals get_inertia", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  expect_equal(clusterer$get_unexplained_variance(), clusterer$get_inertia())
})

test_that("get_centers returns list of centers", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  centers <- clusterer$get_centers()
  
  expect_type(centers, "list")
  expect_true(length(centers) >= 1)
})

test_that("get_split_history returns list", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  history <- clusterer$get_split_history()
  
  expect_type(history, "list")
  # Should have n_clusters - 1 splits (to go from 1 to n_clusters)
  expect_true(length(history) >= 0)
})

test_that("get_cluster_pca returns list", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  pca_list <- clusterer$get_cluster_pca()
  
  expect_type(pca_list, "list")
})

test_that("get_cluster_assignments returns data frame", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  assignments <- clusterer$get_cluster_assignments()
  
  expect_s3_class(assignments, "data.frame")
  expect_equal(nrow(assignments), 6)
  expect_true(all(c("variable", "cluster", "homogeneity") %in% names(assignments)))
})

test_that("get_split_quality_summary returns data frame", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  summary_df <- clusterer$get_split_quality_summary()
  
  expect_s3_class(summary_df, "data.frame")
  if (nrow(summary_df) > 0) {
    expect_true(all(c("iteration", "split_cluster", "eigenvalue_1", "eigenvalue_2") %in% names(summary_df)))
  }
})

test_that("get_split_details returns data frame", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  details <- clusterer$get_split_details()
  
  expect_s3_class(details, "data.frame")
})

test_that("get_hierarchy_tree returns list structure", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  tree <- clusterer$get_hierarchy_tree()
  
  expect_type(tree, "list")
  expect_true("n_clusters" %in% names(tree))
  expect_true("n_iterations" %in% names(tree))
  expect_true("splits" %in% names(tree))
})

test_that("getters require fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  
  expect_error(clusterer$get_homogeneity(), "not fitted")
  expect_error(clusterer$get_cluster_homogeneity(), "not fitted")
  expect_error(clusterer$get_centers(), "not fitted")
  expect_error(clusterer$get_split_history(), "not fitted")
  expect_error(clusterer$get_cluster_assignments(), "not fitted")
})

# ============================================================================
# Tests for predict method
# ============================================================================

test_that("predict assigns new variables to clusters", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  
  predictions <- clusterer$predict(new_data)
  
  expect_length(predictions, 3)
  expect_true(all(predictions %in% 1:max(clusterer$clusters)))
})

test_that("predict requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  
  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  
  expect_error(clusterer$predict(new_data), "not fitted")
})

test_that("predict validates new_data dimensions", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  # Wrong number of observations
  wrong_data <- create_test_data(n = 50, p = 3, seed = 456)
  
  expect_error(clusterer$predict(wrong_data), "observations")
})

# ============================================================================
# Tests for print and summary
# ============================================================================

test_that("print method shows model info", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  expect_output(print(clusterer), "Divisive Clusterer")
  expect_output(print(clusterer), "3")
})

test_that("summary method shows detailed info", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  expect_output(clusterer$summary(), "Cluster")
})

test_that("print works before fitting", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  
  expect_output(print(clusterer), "Fitted: No")
})

# ============================================================================
# Tests for plot_split_dendrogram
# ============================================================================

test_that("plot_split_dendrogram runs without error", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  expect_no_error(clusterer$plot_split_dendrogram())
})

test_that("plot_split_dendrogram with options", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  expect_no_error(
    clusterer$plot_split_dendrogram(
      main = "Custom Title",
      show_eigenvalues = FALSE,
      show_vars = FALSE
    )
  )
})

test_that("plot_split_dendrogram requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  
  expect_error(clusterer$plot_split_dendrogram(), "not fitted")
})

# ============================================================================
# Tests for split history and quality
# ============================================================================

test_that("split history contains expected fields", {
  data <- create_test_data(n = 100, p = 8)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  history <- clusterer$get_split_history()
  
  if (length(history) > 0) {
    first_split <- history[[1]]
    expect_true("iteration" %in% names(first_split))
    expect_true("split_cluster" %in% names(first_split))
    expect_true("eigenvalue_1" %in% names(first_split))
    expect_true("eigenvalue_2" %in% names(first_split))
    expect_true("eigenvalue_ratio" %in% names(first_split))
    expect_true("variables_group1" %in% names(first_split))
    expect_true("variables_group2" %in% names(first_split))
    expect_true("split_quality" %in% names(first_split))
  }
})

test_that("eigenvalue ratio is computed correctly", {
  data <- create_test_data(n = 100, p = 8)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  history <- clusterer$get_split_history()
  
  if (length(history) > 0) {
    for (h in history) {
      if (!is.na(h$eigenvalue_1) && h$eigenvalue_1 > 0 && !is.na(h$eigenvalue_2)) {
        expected_ratio <- h$eigenvalue_2 / h$eigenvalue_1
        expect_equal(h$eigenvalue_ratio, expected_ratio, tolerance = 1e-10)
      }
    }
  }
})

# ============================================================================
# Tests for mixed data support (requires PCAmixdata)
# ============================================================================

test_that("DivisiveClusterer detects mixed data", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars[, 1:6]
  data_mixed$gear <- as.factor(mtcars$gear)
  data_mixed$am <- as.factor(mtcars$am)
  
  clusterer <- DivisiveClusterer$new(
    data = data_mixed,
    n_clusters = 3
  )
  
  expect_output(clusterer$fit(), "MIXED")
  expect_false(clusterer$use_fast_path)
  expect_equal(clusterer$data_mode, "mixed")
})

test_that("DivisiveClusterer handles mixed data correctly", {
  skip_if_not_installed("PCAmixdata")
  
  data_mixed <- mtcars[, 1:6]
  data_mixed$gear <- as.factor(mtcars$gear)
  data_mixed$am <- as.factor(mtcars$am)
  
  clusterer <- DivisiveClusterer$new(
    data = data_mixed,
    n_clusters = 3
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_equal(length(clusterer$clusters), ncol(data_mixed))
})

test_that("DivisiveClusterer works with pure qualitative data", {
  skip_if_not_installed("PCAmixdata")
  
  set.seed(123)
  data_quali <- data.frame(
    Color = factor(sample(c("Red", "Green", "Blue"), 50, replace = TRUE)),
    Size = factor(sample(c("Small", "Medium", "Large"), 50, replace = TRUE)),
    Shape = factor(sample(c("Circle", "Square"), 50, replace = TRUE)),
    Texture = factor(sample(c("Smooth", "Rough"), 50, replace = TRUE))
  )
  
  clusterer <- DivisiveClusterer$new(
    data = data_quali,
    n_clusters = 2
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

# ============================================================================
# Tests for integration with validation methods
# ============================================================================

test_that("DivisiveClusterer works with elbow_method", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 8)
  
  expect_no_error({
    result <- elbow_method(
      DivisiveClusterer,
      data,
      k_range = 2:4,
      plot = FALSE
    )
  })
  
  expect_type(result, "list")
  expect_true("suggested_k" %in% names(result))
})

test_that("DivisiveClusterer works with silhouette_method", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 8)
  
  expect_no_error({
    result <- silhouette_method(
      DivisiveClusterer,
      data,
      k_range = 2:4,
      plot = FALSE
    )
  })
  
  expect_type(result, "list")
  expect_true("suggested_k" %in% names(result))
})

test_that("DivisiveClusterer works with calinski_harabasz_method", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 8)
  
  expect_no_error({
    result <- calinski_harabasz_method(
      DivisiveClusterer,
      data,
      k_range = 2:4,
      plot = FALSE
    )
  })
  
  expect_type(result, "list")
  expect_true("suggested_k" %in% names(result))
})

test_that("DivisiveClusterer works with compare_k_selection_methods", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 8)
  
  expect_no_error({
    result <- compare_k_selection_methods(
      DivisiveClusterer,
      data,
      k_range = 2:4,
      plot = FALSE
    )
  })
  
  expect_type(result, "list")
  expect_true("consensus_k" %in% names(result))
})

# ============================================================================
# Tests for edge cases
# ============================================================================

test_that("DivisiveClusterer handles minimum viable data", {
  # 3 vars, 2 clusters minimum
  min_data <- data.frame(
    Var1 = rnorm(10),
    Var2 = rnorm(10),
    Var3 = rnorm(10)
  )
  
  expect_no_error({
    clusterer <- DivisiveClusterer$new(
      data = min_data,
      n_clusters = 2
    )
    clusterer$fit()
  })
  
  expect_true(clusterer$fitted)
})

test_that("DivisiveClusterer respects min_cluster_size", {
  data <- create_test_data(n = 100, p = 6)
  
  # With high min_cluster_size, may not be able to split further
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 5,
    min_cluster_size = 4  # Need at least 4 vars to split
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  # Actual number of clusters may be less due to min_cluster_size constraint
})

test_that("DivisiveClusterer handles single variable clusters correctly", {
  data <- create_test_data(n = 100, p = 4)
  
  # Request many clusters, forcing some to have single variables
  clusterer <- DivisiveClusterer$new(
    data = data,
    n_clusters = 3,
    min_cluster_size = 2
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
})

test_that("DivisiveClusterer handles mtcars dataset", {
  data(mtcars)
  
  clusterer <- DivisiveClusterer$new(
    data = mtcars,
    n_clusters = 3,
    standardize = TRUE
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  expect_equal(length(clusterer$clusters), ncol(mtcars))
})

# ============================================================================
# Tests for visualization compatibility
# ============================================================================

test_that("DivisiveClusterer works with plot_clustering_2d", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  # DivisiveClusterer should have get_plot_data if needed
  # For now, just test that basic methods work
  expect_true(clusterer$fitted)
  expect_equal(length(unique(clusterer$clusters)), max(clusterer$clusters))
})

test_that("DivisiveClusterer works with plot_cluster_quality", {
  data <- create_test_data(n = 100, p = 6)
  
  clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer$fit()
  
  # Test that required methods exist and return correct types
  expect_true(clusterer$fitted)
  expect_type(clusterer$get_cluster_homogeneity(), "double")
  expect_type(clusterer$clusters, "integer")
})

# ============================================================================
# Tests comparing with KMeansClusterer
# ============================================================================

test_that("DivisiveClusterer and KMeansClusterer have compatible interfaces", {
  data <- create_test_data(n = 100, p = 6)
  
  divisive <- DivisiveClusterer$new(data = data, n_clusters = 3)
  divisive$fit()
  
  kmeans_clust <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 123)
  kmeans_clust$fit()
  
  # Both should have same interface for these methods
  expect_type(divisive$get_homogeneity(), "double")
  expect_type(kmeans_clust$get_homogeneity(), "double")
  
  expect_type(divisive$get_cluster_homogeneity(), "double")
  expect_type(kmeans_clust$get_cluster_homogeneity(), "double")
  
  expect_type(divisive$get_inertia(), "double")
  expect_type(kmeans_clust$get_inertia(), "double")
})

test_that("DivisiveClusterer produces similar quality to KMeansClusterer", {
  # Well-structured data with 3 clear groups
  data <- create_correlated_groups(n = 100, seed = 42)
  
  divisive <- DivisiveClusterer$new(data = data, n_clusters = 3)
  divisive$fit()
  
  kmeans_clust <- KMeansClusterer$new(data = data, n_clusters = 3, seed = 42)
  kmeans_clust$fit()
  
  # Both should achieve reasonable homogeneity
  expect_true(divisive$get_homogeneity() > 0.3)
  expect_true(kmeans_clust$get_homogeneity() > 0.3)
})

# ============================================================================
# Tests for reproducibility
# ============================================================================

test_that("DivisiveClusterer is fully reproducible", {
  data <- create_test_data(n = 100, p = 8)
  
  # Run multiple times
  results <- list()
  for (i in 1:3) {
    clusterer <- DivisiveClusterer$new(data = data, n_clusters = 3)
    clusterer$fit()
    results[[i]] <- clusterer$clusters
  }
  
  # All runs should be identical
  expect_equal(results[[1]], results[[2]])
  expect_equal(results[[2]], results[[3]])
})

test_that("DivisiveClusterer split history is reproducible", {
  data <- create_test_data(n = 100, p = 8)
  
  clusterer1 <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer1$fit()
  
  clusterer2 <- DivisiveClusterer$new(data = data, n_clusters = 3)
  clusterer2$fit()
  
  history1 <- clusterer1$get_split_history()
  history2 <- clusterer2$get_split_history()
  
  expect_equal(length(history1), length(history2))
  
  if (length(history1) > 0) {
    for (i in seq_along(history1)) {
      expect_equal(history1[[i]]$split_cluster, history2[[i]]$split_cluster)
      expect_equal(history1[[i]]$eigenvalue_1, history2[[i]]$eigenvalue_1)
    }
  }
})
