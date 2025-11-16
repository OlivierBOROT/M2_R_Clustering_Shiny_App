# Integration tests for the full clustering workflow

test_that("Complete workflow: fit, predict, visualize, export", {
  skip_if_not_installed("R6")
  
  # 1. Create data
  set.seed(123)
  data <- data.frame(matrix(rnorm(100 * 8), ncol = 8))
  colnames(data) <- paste0("Var", 1:8)
  
  # 2. Create and fit clusterer
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    standardize = TRUE,
    max_iter = 50,
    seed = 123
  )
  
  expect_no_error(clusterer$fit())
  expect_true(clusterer$fitted)
  
  # 3. Get results
  results <- clusterer$get_results()
  expect_equal(nrow(results), 8)
  expect_named(results, c("variable", "cluster"))
  
  # 4. Predict on new data
  new_data <- data.frame(matrix(rnorm(100 * 4), ncol = 4))
  colnames(new_data) <- paste0("NewVar", 1:4)
  
  predictions <- clusterer$predict(new_data)
  expect_equal(nrow(predictions), 4)
  
  # 5. Get cluster info
  sizes <- clusterer$get_cluster_sizes()
  expect_equal(sum(sizes), 8)
  
  members <- clusterer$get_cluster_members(1)
  expect_true(length(members) >= 1)
  
  # 6. Get quality metrics
  homog <- clusterer$get_homogeneity()
  expect_true(homog >= 0 && homog <= 1)
  
  cluster_homog <- clusterer$get_cluster_homogeneity()
  expect_length(cluster_homog, 3)
  
  # 7. Visualizations
  expect_no_error(clusterer$plot_fit())
  expect_no_error(plot_correlation_heatmap(clusterer))
  expect_no_error(plot_cluster_quality(clusterer))
  
  # 8. Export
  temp_file <- tempfile(fileext = ".csv")
  clusterer$save_results(temp_file)
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("Workflow with different initialization methods", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 8)
  
  # Test all three initialization methods
  methods <- c("homogeneity++", "correlation", "random")
  
  for (method in methods) {
    clusterer <- KMeansClusterer$new(
      data = data,
      n_clusters = 3,
      init_method = method,
      max_iter = 30,
      seed = 123
    )
    
    expect_no_error(clusterer$fit())
    expect_true(clusterer$fitted)
    expect_equal(length(unique(clusterer$clusters)), 3)
  }
})

test_that("K selection workflow", {
  skip_if_not_installed("R6")
  skip_if_not_installed("cluster")
  
  data <- create_test_data(n = 100, p = 8)
  
  # Run comparison
  results <- compare_k_selection_methods(
    KMeansClusterer,
    data,
    k_range = 2:4,
    plot = FALSE,
    max_iter = 30
  )
  
  # Check all methods ran
  expect_true("elbow" %in% names(results))
  expect_true("silhouette" %in% names(results))
  expect_true("calinski_harabasz" %in% names(results))
  expect_true("davies_bouldin" %in% names(results))
  expect_true("consensus_k" %in% names(results))
  
  # Consensus should be in range
  expect_true(results$consensus_k %in% 2:4)
  
  # Fit with consensus K
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = results$consensus_k,
    seed = 123
  )
  clusterer$fit()
  
  expect_equal(clusterer$n_clusters, results$consensus_k)
})

test_that("Multiple runs with early stopping", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 8)
  
  # With n_init > 1
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    n_init = 10,
    max_iter = 50,
    seed = 123
  )
  
  clusterer$fit()
  
  actual_runs <- clusterer$get_actual_runs()
  
  # Should run at least 2 times (for early stopping to work)
  expect_true(actual_runs >= 2)
  # Should not exceed n_init
  expect_true(actual_runs <= 10)
})

test_that("Standardization workflow", {
  skip_if_not_installed("R6")
  
  # Create data with different scales
  set.seed(123)
  data <- data.frame(
    small = rnorm(100, mean = 0, sd = 0.1),
    medium = rnorm(100, mean = 0, sd = 1),
    large = rnorm(100, mean = 0, sd = 10),
    xlarge = rnorm(100, mean = 0, sd = 100)
  )
  data <- t(data)  # Variables in columns
  colnames(data) <- paste0("Obs", 1:100)
  data <- as.data.frame(data)
  
  # Without standardization
  clusterer_no_std <- KMeansClusterer$new(
    data = data,
    n_clusters = 2,
    standardize = FALSE,
    seed = 123
  )
  clusterer_no_std$fit()
  
  # With standardization
  clusterer_std <- KMeansClusterer$new(
    data = data,
    n_clusters = 2,
    standardize = TRUE,
    seed = 123
  )
  clusterer_std$fit()
  
  # Results should potentially differ
  # (This is expected behavior, not necessarily better/worse)
  expect_true(clusterer_no_std$fitted)
  expect_true(clusterer_std$fitted)
})

test_that("Reset and refit workflow", {
  skip_if_not_installed("R6")
  
  data <- create_test_data(n = 100, p = 8)
  
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = 3,
    seed = 123
  )
  
  # First fit
  clusterer$fit()
  first_clusters <- clusterer$clusters
  first_homog <- clusterer$get_homogeneity()
  
  # Reset
  clusterer$reset()
  expect_false(clusterer$fitted)
  expect_null(clusterer$clusters)
  
  # Refit (should be reproducible with same seed)
  clusterer$fit()
  second_clusters <- clusterer$clusters
  second_homog <- clusterer$get_homogeneity()
  
  expect_equal(first_clusters, second_clusters)
  expect_equal(first_homog, second_homog)
})

test_that("Handling edge cases in workflow", {
  skip_if_not_installed("R6")
  
  # Minimum viable data (3 vars, 2 obs, 2 clusters)
  # Note: n_clusters must be < n_vars for KMeansClusterer
  min_data <- data.frame(
    Var1 = c(1, 2),
    Var2 = c(3, 4),
    Var3 = c(5, 6)
  )
  
  expect_no_error({
    clusterer <- KMeansClusterer$new(
      data = min_data,
      n_clusters = 2,
      seed = 123
    )
    clusterer$fit()
  })
  
  # Test that n_clusters >= n_vars is properly rejected
  expect_error(
    KMeansClusterer$new(
      data = min_data,
      n_clusters = 3,  # Same as n_vars
      seed = 123
    ),
    "must be strictly less than number of variables"
  )
  
  # Many clusters (K = n_vars - 1)
  data <- create_test_data(n = 100, p = 10)
  
  expect_no_error({
    clusterer <- KMeansClusterer$new(
      data = data,
      n_clusters = 9,
      seed = 123
    )
    clusterer$fit()
  })
})

test_that("Full visualization suite", {
  skip_if_not_installed("R6")
  
  clusterer <- create_fitted_clusterer(n = 100, p = 8, k = 3)
  
  # Make predictions
  new_data <- create_test_data(n = 100, p = 4, seed = 456)
  clusterer$predict(new_data)
  
  # Run all visualizations
  expect_no_error({
    plot_clustering_2d(clusterer)
    plot_clustering_with_supp(clusterer)
    plot_correlation_heatmap(clusterer)
    plot_variable_contributions(clusterer)
    plot_cluster_quality(clusterer)
    plot_scree_by_cluster(clusterer)
  })
  
  # Optional visualizations (may skip if packages unavailable)
  skip_if_not_installed("igraph")
  expect_no_error(plot_network_graph(clusterer))
  
  skip_if_not_installed("fmsb")
  expect_no_error(plot_radar_chart(clusterer))
})
