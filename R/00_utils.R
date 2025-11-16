#' @title Utility Functions for Clustering
#' @description This script contains utility functions used by all the implemented clustering algorithms.
#' @keywords internal

#' Standardize data (center and scale)
#' 
#' @param data A data frame or matrix to standardize
#' @param center Logical or numeric vector of centers. If TRUE, centering is done by subtracting the column means.
#' @param scale Logical or numeric vector of scales. If TRUE, scaling is done by dividing by the standard deviations.
#' @return A list containing the scaled data, the centers, and the scales
#' @export
standardize_data <- function(data, center = TRUE, scale = TRUE) {
  scaled_data <- scale(data, center = center, scale = scale)
  
  # Extract centering and scaling parameters
  centers <- attr(scaled_data, "scaled:center")
  scales <- attr(scaled_data, "scaled:scale")
  
  # Return as data.frame with attributes
  result <- as.data.frame(scaled_data)
  attr(result, "centers") <- centers
  attr(result, "scales") <- scales
  
  return(result)
}

#' Apply standardization using pre-computed parameters
#' 
#' @param data A data frame or matrix to standardize
#' @param centers Numeric vector of centers (means) from training data
#' @param scales Numeric vector of scales (standard deviations) from training data
#' @return Standardized data frame
#' @export
apply_standardization <- function(data, centers, scales) {
  if (is.null(centers) && is.null(scales)) {
    return(as.data.frame(data))
  }
  
  scaled_data <- scale(data, center = centers, scale = scales)
  return(as.data.frame(scaled_data))
}

#' Calculate Euclidean distance between points and centers
#' 
#' @param points Matrix or data frame of points (rows = observations)
#' @param centers Matrix or data frame of cluster centers
#' @return Distance matrix (rows = centers, cols = points)
#' @export
euclidean_distance <- function(points, centers) {
  # Convert to matrices to avoid column name conflicts
  points_mat <- as.matrix(points)
  centers_mat <- as.matrix(centers)
  
  # Ensure same number of columns
  if (ncol(points_mat) != ncol(centers_mat)) {
    stop("points and centers must have the same number of columns")
  }
  
  # Combine centers and points, then calculate all pairwise distances
  combined <- rbind(centers_mat, points_mat)
  dist_matrix <- as.matrix(dist(combined))
  
  # Extract only distances from centers to points
  n_centers <- nrow(centers_mat)
  n_points <- nrow(points_mat)
  
  return(dist_matrix[1:n_centers, (n_centers + 1):(n_centers + n_points), drop = FALSE])
}

#' Elbow Method
#' 
#' Calculates the within-cluster inertia for different values of K and identifies
#' the "elbow" of the curve which suggests the optimal number of clusters.
#' 
#' @param clusterer_class R6 class of the clusterer (e.g., KMeansClusterer)
#' @param data Dataset to be clustered
#' @param k_range Vector of K values to test (default: 2:10)
#' @param plot Logical, whether to display the plot (default: TRUE)
#' @param ... Additional parameters for the clusterer
#' @return List with inertias, k_values, and suggested_k
#' @export
elbow_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (length(k_range) < 2) {
    stop("k_range must contain at least 2 values")
  }
  
  inertias <- numeric(length(k_range))
  
  cat("Calculating elbow method...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))
    
    # Create and train the clusterer
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()
    
    # Retrieve inertia
    inertias[i] <- clusterer$get_inertia()
  }
  
  # Detect elbow using the point farthest from the line method
  # Line between first and last point
  x1 <- k_range[1]
  y1 <- inertias[1]
  x2 <- k_range[length(k_range)]
  y2 <- inertias[length(inertias)]
  
  # Perpendicular distance of each point to the line
  distances <- numeric(length(k_range))
  for (i in seq_along(k_range)) {
    x0 <- k_range[i]
    y0 <- inertias[i]
    
    # Point-line distance
    num <- abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1)
    den <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
    distances[i] <- num / den
  }
  
  # The elbow is the point with the greatest distance
  suggested_k <- k_range[which.max(distances)]
  
  # Plot
  if (plot) {
    plot(k_range, inertias, type = "b", pch = 19, col = "steelblue",
         xlab = "Number of clusters (K)", ylab = "Within-cluster inertia",
         main = "Elbow Method",
         las = 1, lwd = 2)
    
    # Reference line
    lines(c(x1, x2), c(y1, y2), col = "gray50", lty = 2)
    
    # Mark the suggested K
    points(suggested_k, inertias[which(k_range == suggested_k)], 
           pch = 19, col = "red", cex = 2)
    text(suggested_k, inertias[which(k_range == suggested_k)], 
         labels = sprintf("K = %d", suggested_k), 
         pos = 3, col = "red", font = 2)
    
    grid()
  }
  
  cat(sprintf("\nK suggested by elbow method: %d\n", suggested_k))
  
  return(list(
    k_values = k_range,
    inertias = inertias,
    suggested_k = suggested_k,
    distances = distances
  ))
}

#' Silhouette Method
#' 
#' Calculates the average silhouette coefficient for different values of K.
#' The optimal K is the one that maximizes this coefficient.
#' 
#' @param clusterer_class R6 class of the clusterer
#' @param data Dataset to be clustered
#' @param k_range Vector of K values to test (default: 2:10)
#' @param plot Logical, whether to display the plot (default: TRUE)
#' @param ... Additional parameters for the clusterer
#' @return List with silhouette_scores, k_values, and suggested_k
#' @export
silhouette_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("The 'cluster' package is required. Install it with install.packages('cluster')")
  }
  
  if (length(k_range) < 2) {
    stop("k_range must contain at least 2 values")
  }
  
  silhouette_scores <- numeric(length(k_range))
  
  cat("Calculating silhouette method...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))
    
    # Create and train the clusterer
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()
    
    # Retrieve loadings and clusters
    loadings <- clusterer$get_loadings()
    clusters <- clusterer$clusters
    
    # Calculate silhouette
    sil <- cluster::silhouette(clusters, dist(loadings))
    silhouette_scores[i] <- mean(sil[, 3])
  }
  
  # Optimal K = maximum silhouette
  suggested_k <- k_range[which.max(silhouette_scores)]
  
  # Plot
  if (plot) {
    plot(k_range, silhouette_scores, type = "b", pch = 19, col = "steelblue",
         xlab = "Number of clusters (K)", ylab = "Average silhouette coefficient",
         main = "Silhouette Method",
         las = 1, lwd = 2, ylim = c(0, max(silhouette_scores) * 1.1))
    
    # Mark the suggested K
    points(suggested_k, silhouette_scores[which(k_range == suggested_k)], 
           pch = 19, col = "red", cex = 2)
    text(suggested_k, silhouette_scores[which(k_range == suggested_k)], 
         labels = sprintf("K = %d", suggested_k), 
         pos = 3, col = "red", font = 2)
    
    abline(h = 0, col = "gray50", lty = 2)
    grid()
  }
  
  cat(sprintf("\nSuggested K by silhouette method: %d\n", suggested_k))
  cat(sprintf("Silhouette coefficient: %.3f\n", silhouette_scores[which(k_range == suggested_k)]))
  
  return(list(
    k_values = k_range,
    silhouette_scores = silhouette_scores,
    suggested_k = suggested_k
  ))
}

#' Calinski-Harabasz Method (Variance Ratio Criterion)
#' 
#' Calculates the Calinski-Harabasz index for different values of K.
#' The optimal K is the one that maximizes this index.
#' 
#' @param clusterer_class R6 class of the clusterer
#' @param data Dataset to be clustered
#' @param k_range Vector of K values to test (default: 2:10)
#' @param plot Logical, whether to display the plot (default: TRUE)
#' @param ... Additional parameters for the clusterer
#' @return List with ch_scores, k_values, and suggested_k
#' @export
calinski_harabasz_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (length(k_range) < 2) {
    stop("k_range must contain at least 2 values")
  }
  
  ch_scores <- numeric(length(k_range))
  
  cat("Calculating Calinski-Harabasz index...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))
    
    # Create and train the clusterer
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()
    
    # Retrieve loadings and clusters
    loadings <- as.matrix(clusterer$get_loadings())
    clusters <- clusterer$clusters
    
    # Calculate CH index
    n <- nrow(loadings)
    k_actual <- length(unique(clusters))
    
    # Global center
    global_center <- colMeans(loadings)
    
    # Between-cluster variance
    between_ss <- 0
    for (j in 1:k_actual) {
      cluster_points <- loadings[clusters == j, , drop = FALSE]
      n_j <- nrow(cluster_points)
      cluster_center <- colMeans(cluster_points)
      between_ss <- between_ss + n_j * sum((cluster_center - global_center)^2)
    }
    
    # Within-cluster variance
    within_ss <- clusterer$get_inertia()
    
    # CH index
    if (within_ss > 0 && k_actual > 1) {
      ch_scores[i] <- (between_ss / (k_actual - 1)) / (within_ss / (n - k_actual))
    } else {
      ch_scores[i] <- 0
    }
  }
  
  # Optimal K = Maximal CH
  suggested_k <- k_range[which.max(ch_scores)]
  
  # Plot
  if (plot) {
    plot(k_range, ch_scores, type = "b", pch = 19, col = "steelblue",
         xlab = "Number of clusters (K)", ylab = "Calinski-Harabasz Index",
         main = "Calinski-Harabasz Method",
         las = 1, lwd = 2)
    
    # Mark the suggested K
    points(suggested_k, ch_scores[which(k_range == suggested_k)], 
           pch = 19, col = "red", cex = 2)
    text(suggested_k, ch_scores[which(k_range == suggested_k)], 
         labels = sprintf("K = %d", suggested_k), 
         pos = 3, col = "red", font = 2)
    
    grid()
  }
  
  cat(sprintf("\nK suggested by Calinski-Harabasz: %d\n", suggested_k))
  cat(sprintf("Calinski-Harabasz score: %.2f\n", ch_scores[which(k_range == suggested_k)]))
  
  return(list(
    k_values = k_range,
    ch_scores = ch_scores,
    suggested_k = suggested_k
  ))
}

#' Davies-Bouldin Method
#' 
#' Calculates the Davies-Bouldin index for different values of K.
#' The optimal K is the one that MINIMIZES this index.
#' 
#' @param clusterer_class R6 class of the clusterer
#' @param data Dataset to be clustered
#' @param k_range Vector of K values to test (default: 2:10)
#' @param plot Logical, whether to display the plot (default: TRUE)
#' @param ... Additional parameters for the clusterer
#' @return List with db_scores, k_values, and suggested_k
#' @export
davies_bouldin_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (length(k_range) < 2) {
    stop("k_range must contain at least 2 values")
  }
  
  db_scores <- numeric(length(k_range))
  
  cat("Calculating Davies-Bouldin index...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))
    
    # Create and train the clusterer
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()
    
    # Retrieve loadings, centers, and clusters
    loadings <- as.matrix(clusterer$get_loadings())
    centers <- as.matrix(clusterer$get_centers())
    clusters <- clusterer$clusters
    
    # Calculate DB index
    k_actual <- length(unique(clusters))
    
    # Calculate intra-cluster dispersions
    dispersions <- numeric(k_actual)
    for (j in 1:k_actual) {
      cluster_points <- loadings[clusters == j, , drop = FALSE]
      if (nrow(cluster_points) > 0) {
        # Extract center vector and match dimensions with cluster_points
        n_dims <- ncol(cluster_points)
        n_center_dims <- ncol(centers)
        # Use only the dimensions available in both
        dims_to_use <- min(n_dims, n_center_dims)
        center_vec <- centers[j, seq_len(dims_to_use)]
        cluster_subset <- cluster_points[, seq_len(dims_to_use), drop = FALSE]
        dispersions[j] <- mean(sqrt(rowSums((sweep(cluster_subset, 2, center_vec))^2)))
      }
    }
    
    # Calculate DB index
    db_values <- numeric(k_actual)
    for (j in 1:k_actual) {
      max_ratio <- 0
      for (l in 1:k_actual) {
        if (j != l) {
          dist_centers <- sqrt(sum((centers[j, ] - centers[l, ])^2))
          if (dist_centers > 0) {
            ratio <- (dispersions[j] + dispersions[l]) / dist_centers
            max_ratio <- max(max_ratio, ratio)
          }
        }
      }
      db_values[j] <- max_ratio
    }
    
    db_scores[i] <- mean(db_values)
  }
  
  # Optimal K = Minimal DB
  suggested_k <- k_range[which.min(db_scores)]
  
  # Plot
  if (plot) {
    plot(k_range, db_scores, type = "b", pch = 19, col = "steelblue",
         xlab = "Number of clusters (K)", ylab = "Davies-Bouldin Index",
         main = "Davies-Bouldin Method\n(lower is better)",
         las = 1, lwd = 2)
    
    # Mark the suggested K
    points(suggested_k, db_scores[which(k_range == suggested_k)], 
           pch = 19, col = "red", cex = 2)
    text(suggested_k, db_scores[which(k_range == suggested_k)], 
         labels = sprintf("K = %d", suggested_k), 
         pos = 3, col = "red", font = 2)
    
    grid()
  }
  
  cat(sprintf("\nSuggested K by Davies-Bouldin: %d\n", suggested_k))
  cat(sprintf("DB Score: %.3f\n", db_scores[which(k_range == suggested_k)]))
  
  return(list(
    k_values = k_range,
    db_scores = db_scores,
    suggested_k = suggested_k
  ))
}

#' Comparison of all K selection methods
#' 
#' Runs all selection methods and displays a comparative summary.
#' 
#' @param clusterer_class R6 class of the clusterer
#' @param data Dataset to be clustered
#' @param k_range Vector of K values to test (default: 2:10)
#' @param plot Logical, whether to display plots (default: TRUE)
#' @param ... Additional parameters for the clusterer
#' @return List with results from all methods
#' @export
compare_k_selection_methods <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  cat("========================================\n")
  cat("COMPARISON OF K SELECTION METHODS\n")
  cat("========================================\n\n")
  
  # Plot configuration
  if (plot) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  }
  
  # 1. Elbow Method
  cat("\n1. ELBOW METHOD\n")
  cat("-------------------\n")
  elbow_res <- elbow_method(clusterer_class, data, k_range, plot = plot, ...)
  
  # 2. Silhouette Method
  cat("\n2. SILHOUETTE METHOD\n")
  cat("---------------------------\n")
  silhouette_res <- silhouette_method(clusterer_class, data, k_range, plot = plot, ...)
  
  # 3. Calinski-Harabasz
  cat("\n3. CALINSKI-HARABASZ METHOD\n")
  cat("-------------------------------\n")
  ch_res <- calinski_harabasz_method(clusterer_class, data, k_range, plot = plot, ...)
  
  # 4. Davies-Bouldin
  cat("\n4. DAVIES-BOULDIN METHOD\n")
  cat("----------------------------\n")
  db_res <- davies_bouldin_method(clusterer_class, data, k_range, plot = plot, ...)
  
  # Summary
  cat("\n========================================\n")
  cat("SUMMARY OF RECOMMENDATIONS\n")
  cat("========================================\n")
  cat(sprintf("Elbow Method:       K = %d\n", elbow_res$suggested_k))
  cat(sprintf("Silhouette Method:     K = %d\n", silhouette_res$suggested_k))
  cat(sprintf("Calinski-Harabasz:      K = %d\n", ch_res$suggested_k))
  cat(sprintf("Davies-Bouldin:         K = %d\n", db_res$suggested_k))
  
  # Majority vote
  suggestions <- c(elbow_res$suggested_k, silhouette_res$suggested_k, 
                  ch_res$suggested_k, db_res$suggested_k)
  vote_table <- table(suggestions)
  consensus_k <- as.numeric(names(vote_table)[which.max(vote_table)])
  
  cat(sprintf("\nConsensus (majority vote): K = %d\n", consensus_k))
  cat("========================================\n")
  
  return(list(
    elbow = elbow_res,
    silhouette = silhouette_res,
    calinski_harabasz = ch_res,
    davies_bouldin = db_res,
    consensus_k = consensus_k,
    all_suggestions = suggestions
  ))
}