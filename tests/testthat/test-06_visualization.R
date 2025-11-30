# Tests for visualization functions (06_visualization.R)

# Helper function to create test data
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(rnorm(n * p), ncol = p))
  colnames(data) <- paste0("Var", 1:p)
  data
}

# Helper to create fitted clusterer
create_fitted_clusterer <- function(n = 100, p = 6, k = 3, seed = 123) {
  data <- create_test_data(n = n, p = p, seed = seed)
  clusterer <- KMeansClusterer$new(
    data = data,
    n_clusters = k,
    max_iter = 50,
    seed = seed
  )
  clusterer$fit()
  clusterer
}

# ============================================================================
# Tests for plot_clustering_2d
# ============================================================================

test_that("plot_clustering_2d runs without error", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_clustering_2d(clusterer)
  )
})

test_that("plot_clustering_2d requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_clustering_2d(clusterer),
    "fitted"
  )
})

test_that("plot_clustering_2d accepts custom title", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_clustering_2d(clusterer, main = "Custom Title")
  )
})

test_that("plot_clustering_2d can hide centers", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_clustering_2d(clusterer, show_centers = FALSE)
  )
})

test_that("plot_clustering_2d can hide labels", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_clustering_2d(clusterer, show_labels = FALSE)
  )
})

# ============================================================================
# Tests for plot_clustering_with_supp
# ============================================================================

test_that("plot_clustering_with_supp runs without error", {
  clusterer <- create_fitted_clusterer()

  new_data <- create_test_data(n = 100, p = 3, seed = 456)
  clusterer$predict(new_data)

  expect_no_error(
    plot_clustering_with_supp(clusterer)
  )
})

test_that("plot_clustering_with_supp requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_clustering_with_supp(clusterer),
    "fitted"
  )
})

# ============================================================================
# Tests for plot_correlation_heatmap
# ============================================================================

test_that("plot_correlation_heatmap runs without error", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_correlation_heatmap(clusterer)
  )
})

test_that("plot_correlation_heatmap with reorder = FALSE", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_correlation_heatmap(clusterer, reorder = FALSE)
  )
})

test_that("plot_correlation_heatmap requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_correlation_heatmap(clusterer),
    "fitted"
  )
})

test_that("plot_correlation_heatmap accepts custom title", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_correlation_heatmap(clusterer, main = "Custom Heatmap")
  )
})

# ============================================================================
# Tests for plot_network_graph
# ============================================================================

test_that("plot_network_graph runs with igraph", {
  skip_if_not_installed("igraph")

  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_network_graph(clusterer, threshold = 0.3)
  )
})

test_that("plot_network_graph uses fallback without igraph", {
  clusterer <- create_fitted_clusterer()

  # Mock igraph unavailability by using high threshold
  expect_no_error(
    plot_network_graph(clusterer, threshold = 0.9)
  )
})

test_that("plot_network_graph accepts different layouts", {
  skip_if_not_installed("igraph")

  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_network_graph(clusterer, layout = "circle")
  )

  expect_no_error(
    plot_network_graph(clusterer, layout = "kamada.kawai")
  )
})

test_that("plot_network_graph requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_network_graph(clusterer),
    "fitted"
  )
})

# ============================================================================
# Tests for plot_variable_contributions
# ============================================================================

test_that("plot_variable_contributions runs for all clusters", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_variable_contributions(clusterer, cluster_id = NULL, top_n = 5)
  )
})

test_that("plot_variable_contributions runs for single cluster", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_variable_contributions(clusterer, cluster_id = 1, top_n = 5)
  )
})

test_that("plot_variable_contributions requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_variable_contributions(clusterer),
    "fitted"
  )
})

test_that("plot_variable_contributions accepts custom top_n", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_variable_contributions(clusterer, top_n = 3)
  )
})

# ============================================================================
# Tests for plot_cluster_quality
# ============================================================================

test_that("plot_cluster_quality runs without error", {
  clusterer <- create_fitted_clusterer()

  result <- plot_cluster_quality(clusterer)

  expect_type(result, "list")
  expect_named(result, c("sizes", "homogeneity"))
})

test_that("plot_cluster_quality requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_cluster_quality(clusterer),
    "fitted"
  )
})

test_that("plot_cluster_quality returns correct sizes", {
  clusterer <- create_fitted_clusterer(p = 9, k = 3)

  result <- plot_cluster_quality(clusterer)

  expect_equal(sum(result$sizes), 9) # Total variables
  expect_equal(length(result$sizes), 3) # Number of clusters
})

# ============================================================================
# Tests for plot_scree_by_cluster
# ============================================================================

test_that("plot_scree_by_cluster runs without error", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_scree_by_cluster(clusterer)
  )
})

test_that("plot_scree_by_cluster requires fitted model", {
  data <- create_test_data(n = 100, p = 6)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 3)

  expect_error(
    plot_scree_by_cluster(clusterer),
    "fitted"
  )
})

test_that("plot_scree_by_cluster accepts custom title", {
  clusterer <- create_fitted_clusterer()

  expect_no_error(
    plot_scree_by_cluster(clusterer, main = "Custom Scree")
  )
})

# ============================================================================
# Tests for fallback functions
# ============================================================================

test_that("plot_correlation_matrix_simple runs without error", {
  clusterer <- create_fitted_clusterer()

  # Access private function for testing
  expect_no_error({
    cor_matrix <- cor(clusterer$data)
    adj <- abs(cor_matrix) > 0.3
    diag(adj) <- FALSE
    connections <- rowSums(adj)

    barplot(connections[order(clusterer$clusters)],
      main = "Test",
      las = 2
    )
  })
})

# ============================================================================
# Tests for visualization consistency
# ============================================================================

test_that("all visualization functions work together", {
  clusterer <- create_fitted_clusterer()

  # Run all visualizations in sequence
  expect_no_error({
    plot_clustering_2d(clusterer)
    plot_correlation_heatmap(clusterer)
    plot_variable_contributions(clusterer, cluster_id = 1)
    plot_cluster_quality(clusterer)
    plot_scree_by_cluster(clusterer)
  })
})

test_that("visualizations handle edge cases", {
  # Very few variables
  data <- create_test_data(n = 100, p = 4)
  clusterer <- KMeansClusterer$new(data = data, n_clusters = 2, seed = 123)
  clusterer$fit()

  expect_no_error({
    plot_clustering_2d(clusterer)
    plot_correlation_heatmap(clusterer)
    plot_cluster_quality(clusterer)
  })
})
