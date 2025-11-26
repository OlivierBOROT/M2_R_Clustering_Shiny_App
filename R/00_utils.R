#' Low-Level Utility Functions for Clustering
#' 
#' This module contains low-level mathematical and data manipulation utilities
#' for clustering algorithms. For cluster validation and K selection methods,
#' see \code{\link{cluster-validator}}.
#' 
#' @name M2RClust-utils
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom graphics abline axis barplot grid image legend lines matplot mtext par plot.new points text
#' @importFrom stats cor dist as.dist chisq.test
#' @importFrom utils head
NULL

#' Standardize data (center and scale)
#'
#' @param data A data frame or matrix to standardize.
#' @param center Logical or numeric vector of centers (default \code{TRUE}).
#'   If \code{TRUE}, centering is done by subtracting the column means.
#' @param scale Logical or numeric vector of scales (default \code{TRUE}).
#'   If \code{TRUE}, scaling is done by dividing by the standard deviations.
#' @return A data frame containing the scaled data with attributes:
#' \item{centers}{Numeric vector of column means used for centering.}
#' \item{scales}{Numeric vector of column standard deviations used for scaling.}
#'
#' @seealso \code{\link{apply_standardization}} to apply pre-computed parameters.
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
#' @param data A data frame or matrix to standardize.
#' @param centers Numeric vector of centers (means) from training data.
#'   If \code{NULL}, no centering is applied.
#' @param scales Numeric vector of scales (standard deviations) from training data.
#'   If \code{NULL}, no scaling is applied.
#' @return A standardized data frame. If both \code{centers} and \code{scales}
#'   are \code{NULL}, returns the data unchanged.
#'
#' @seealso \code{\link{standardize_data}} to compute parameters and standardize.
#' @export
apply_standardization <- function(data, centers, scales) {
  if (is.null(centers) && is.null(scales)) {
    return(as.data.frame(data))
  }
  
  scaled_data <- scale(data, center = centers, scale = scales)
  return(as.data.frame(scaled_data))
}

#' Generate Euclidean Distance Matrix Based on Correlation (ClustOfVar Approach)
#'
#' Computes the Euclidean distance matrix between variables using the correlation-based
#' distance metric. For quantitative variables: \code{d(i,j) = sqrt(2 * (1 - cor(i,j)))}.
#' For mixed data, uses appropriate association measures (R², η², Cramér's V²).
#' This distance is guaranteed to be Euclidean and is the foundation of the ClustOfVar algorithm.
#'
#' @param data Data frame or matrix with variables in columns, observations in rows.
#'   Supports numeric, integer, and factor variables.
#' @return A \code{dist} object containing pairwise distances between variables.
#' @export
get_correlation_distance_matrix <- function(data) {
  n_vars <- ncol(data)
  is_num <- sapply(data, is.numeric)
  
  # Fast path: Pure quantitative data
  if (all(is_num)) {
    scaled_data <- scale(data, center = TRUE, scale = TRUE)
    cor_matrix <- cor(scaled_data)
    
    # Apply Euclidean transformation: sqrt(2 * (1 - cor))
    # This is a proper Euclidean distance metric
    dist_matrix <- sqrt(2 * (1 - cor_matrix))
    
    return(as.dist(dist_matrix))
  }
  
  # Mixed data: Calculate association-based distances
  # Uses same logic as KMeansClusterer$initialize_correlation_based
  dist_mat <- matrix(0, n_vars, n_vars)
  
  for (i in 1:n_vars) {
    for (j in i:n_vars) {
      if (i == j) {
        # Distance to self = 0
        dist_mat[i, j] <- 0
      } else {
        var_i <- data[[i]]
        var_j <- data[[j]]
        is_i_num <- is_num[i]
        is_j_num <- is_num[j]
        
        if (is_i_num && is_j_num) {
          # Quanti-Quanti: Pearson correlation
          r <- cor(var_i, var_j)
          sim <- r^2
        } else if (!is_i_num && !is_j_num) {
          # Quali-Quali: Cramér's V²
          contingency <- table(var_i, var_j)
          chi2 <- suppressWarnings(chisq.test(contingency, correct = FALSE)$statistic)
          n <- sum(contingency)
          min_dim <- min(nrow(contingency), ncol(contingency)) - 1
          if (min_dim > 0) {
            cramers_v <- sqrt(chi2 / (n * min_dim))
            sim <- cramers_v^2
          } else {
            sim <- 0
          }
        } else {
          # Quanti-Quali: η² (correlation ratio)
          if (is_i_num) {
            quant_var <- var_i
            qual_var <- var_j
          } else {
            quant_var <- var_j
            qual_var <- var_i
          }
          
          # Compute η² = Between-group variance / Total variance
          group_means <- tapply(quant_var, qual_var, mean, na.rm = TRUE)
          overall_mean <- mean(quant_var, na.rm = TRUE)
          
          ss_between <- sum(tapply(quant_var, qual_var, function(x) {
            length(x) * (mean(x, na.rm = TRUE) - overall_mean)^2
          }), na.rm = TRUE)
          
          ss_total <- sum((quant_var - overall_mean)^2, na.rm = TRUE)
          
          if (ss_total > 0) {
            eta_squared <- ss_between / ss_total
            sim <- eta_squared
          } else {
            sim <- 0
          }
        }
        
        # Convert similarity to distance: d = sqrt(2 * (1 - similarity))
        # Bounded in [0, sqrt(2)] (like correlation-based distance)
        dist_val <- sqrt(2 * (1 - sim))
        dist_mat[i, j] <- dist_val
        dist_mat[j, i] <- dist_val
      }
    }
  }
  
  return(as.dist(dist_mat))
}

#' Calculate Euclidean distance between points and centers
#'
#' @param points Matrix or data frame of points (rows = observations, columns = features).
#' @param centers Matrix or data frame of cluster centers (rows = centers, columns = features).
#'   Must have the same number of columns as \code{points}.
#' @return A numeric matrix of distances with dimensions \code{nrow(centers)} by \code{nrow(points)}.
#'   Element \code{[i, j]} contains the Euclidean distance from center \code{i} to point \code{j}.
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