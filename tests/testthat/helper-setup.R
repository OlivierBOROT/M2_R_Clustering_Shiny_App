# Helper functions for tests

#' Create standard test data
#' 
#' @param n Number of observations
#' @param p Number of variables
#' @param seed Random seed
#' @return Data frame with n observations and p variables
create_test_data <- function(n = 100, p = 5, seed = 123) {
  set.seed(seed)
  data <- data.frame(matrix(rnorm(n * p), ncol = p))
  colnames(data) <- paste0("Var", 1:p)
  data
}

#' Create data with clear cluster structure
#' 
#' @param n_per_cluster Number of observations per cluster
#' @param seed Random seed
#' @return Data frame with clustered structure (transposed)
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

#' Create a fitted KMeansClusterer for testing
#' 
#' @param n Number of observations
#' @param p Number of variables
#' @param k Number of clusters
#' @param seed Random seed
#' @return Fitted KMeansClusterer object
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

#' Skip test if package is not installed
#' 
#' @param pkg Package name
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}
