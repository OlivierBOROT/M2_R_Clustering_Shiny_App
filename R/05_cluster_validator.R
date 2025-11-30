#' @title Cluster Validation and K Selection Methods
#' @description Functions to help choose the optimal number of clusters using
#'   various statistical methods (Elbow, Silhouette, Homogeneity-based).
#' @name cluster-validator
NULL

#' Elbow Method for Optimal K Selection
#'
#' Calculates a quality criterion for different values of K and identifies
#' the "elbow" of the curve which suggests the optimal number of clusters.
#' For variable clustering (KMeansClusterer), uses (1 - homogeneity) as criterion.
#'
#' @param clusterer_class R6 class of the clusterer (e.g., \code{KMeansClusterer}).
#' @param data Data frame or matrix to be clustered.
#' @param k_range Integer vector of K values to test (default: \code{2:10}).
#'   Must contain at least 2 values.
#' @param plot Logical. Whether to display the elbow plot (default: \code{TRUE}).
#' @param ... Additional parameters passed to the clusterer constructor.
#'
#' @return A list with the following components:
#' \item{k_values}{Numeric vector of the K values that were tested.}
#' \item{inertias}{Numeric vector of the quality criterion (lower is better)
#'   corresponding to each K value. For KMeansClusterer, this is (1 - homogeneity).}
#' \item{suggested_k}{The K value identified by the elbow method as the
#'   potential optimum.}
#' \item{distances}{Numeric vector of perpendicular distances of each K-point
#'   to the start-end line, used to find the elbow.}
#'
#' @seealso
#' \code{\link{silhouette_method}}, \code{\link{calinski_harabasz_method}},
#' \code{\link{compare_k_selection_methods}}
#'
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

    # Retrieve criterion (inertia or 1-homogeneity)
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
    plot(k_range, inertias,
      type = "b", pch = 19, col = "steelblue",
      xlab = "Number of clusters (K)", ylab = "Quality criterion (lower is better)",
      main = "Elbow Method",
      las = 1, lwd = 2
    )

    # Reference line
    lines(c(x1, x2), c(y1, y2), col = "gray50", lty = 2)

    # Mark the suggested K
    points(suggested_k, inertias[which(k_range == suggested_k)],
      pch = 19, col = "red", cex = 2
    )
    text(suggested_k, inertias[which(k_range == suggested_k)],
      labels = sprintf("K = %d", suggested_k),
      pos = 3, col = "red", font = 2
    )

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

#' Silhouette Method for Optimal K Selection
#'
#' Calculates the average silhouette coefficient for different values of K.
#' The optimal K is the one that maximizes this coefficient.
#'
#' @param clusterer_class R6 class of the clusterer (e.g., \code{KMeansClusterer}).
#' @param data Data frame or matrix to be clustered.
#' @param k_range Integer vector of K values to test (default: \code{2:10}).
#'   Must contain at least 2 values.
#' @param plot Logical. Whether to display the silhouette plot (default: \code{TRUE}).
#' @param ... Additional parameters passed to the clusterer constructor.
#'
#' @return A list with the following components:
#' \item{k_values}{Numeric vector of the K values that were tested.}
#' \item{silhouette_scores}{Numeric vector of the average silhouette coefficients
#'   corresponding to each K value. Values range from -1 to 1, with higher values
#'   indicating better-defined clusters.}
#' \item{suggested_k}{The K value with the highest silhouette coefficient.}
#'
#' @details
#' The silhouette coefficient measures how similar an object is to its own cluster
#' compared to other clusters. A high value indicates that the object is well matched
#' to its own cluster and poorly matched to neighboring clusters.
#'
#' For variable clustering (KMeansClusterer), this method uses the correlation-based
#' Euclidean distance: \code{d(i,j) = sqrt(2 * (1 - cor(i,j)))}.
#' This is appropriate for variable clustering as it respects the correlation structure.
#'
#' Requires the \code{cluster} package.
#'
#' @seealso
#' \code{\link{elbow_method}}, \code{\link{calinski_harabasz_method}},
#' \code{\link{compare_k_selection_methods}}
#'
#' @export
silhouette_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("The 'cluster' package is required. Install it with install.packages('cluster')")
  }

  if (length(k_range) < 1) {
    stop("k_range must contain at least 1 value")
  }

  silhouette_scores <- numeric(length(k_range))

  cat("Calculating silhouette method...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))

    # Create and train the clusterer
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()

    # Retrieve cluster assignments
    clusters <- clusterer$clusters

    # Use correlation-based Euclidean distance (ClustOfVar approach)
    # This is more appropriate for variable clustering than PCA-based distances
    dist_matrix <- get_correlation_distance_matrix(data)

    # Calculate silhouette using correlation-based distance
    sil <- cluster::silhouette(clusters, dist_matrix)
    silhouette_scores[i] <- mean(sil[, 3])
  }

  # Optimal K = maximum silhouette
  suggested_k <- k_range[which.max(silhouette_scores)]

  # Plot
  if (plot) {
    plot(k_range, silhouette_scores,
      type = "b", pch = 19, col = "steelblue",
      xlab = "Number of clusters (K)", ylab = "Average silhouette coefficient",
      main = "Silhouette Method",
      las = 1, lwd = 2, ylim = c(0, max(silhouette_scores) * 1.1)
    )

    # Mark the suggested K
    points(suggested_k, silhouette_scores[which(k_range == suggested_k)],
      pch = 19, col = "red", cex = 2
    )
    text(suggested_k, silhouette_scores[which(k_range == suggested_k)],
      labels = sprintf("K = %d", suggested_k),
      pos = 3, col = "red", font = 2
    )

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

#' Calinski-Harabasz Pseudo-F (Adapted for Variable Clustering)
#'
#' Calculates a Pseudo-F statistic based on the ratio of explained
#' to unexplained variance, penalized by the number of clusters.
#'
#' @param clusterer_class R6 class of the clusterer.
#' @param data Data frame to be clustered.
#' @param k_range Integer vector of K values to test.
#' @param plot Logical. Whether to display the plot.
#' @param ... Additional parameters for the clusterer.
#'
#' @return A list with the following components:
#' \item{k_values}{Numeric vector of the K values that were tested.}
#' \item{ch_scores}{Numeric vector of the Pseudo-F statistics
#'   corresponding to each K value. Higher values indicate better clustering.}
#' \item{suggested_k}{The K value with the highest Pseudo-F score.}
#'
#' @details
#' The Calinski-Harabasz index (Pseudo-F) balances clustering quality with
#' parsimony. It uses the ratio of explained to unexplained variance, penalized
#' by the number of clusters:
#'
#' \deqn{CH(k) = \frac{(H - 1)/(k-1)}{(n_{vars} - H)/(n_{vars} - k)}}
#'
#' where \eqn{H} is the total explained inertia (sum of within-cluster homogeneities)
#' and \eqn{n_{vars}} is the total number of variables.
#'
#' Higher values indicate better-defined, more separated clusters. The optimal K
#' typically corresponds to the maximum Pseudo-F value.
#'
#' @seealso
#' \code{\link{elbow_method}}, \code{\link{silhouette_method}},
#' \code{\link{compare_k_selection_methods}}
#'
#' @export
calinski_harabasz_method <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  if (length(k_range) < 2) stop("k_range must contain at least 2 values")

  ch_scores <- numeric(length(k_range))
  n_vars <- ncol(data)

  cat("Calculating Calinski-Harabasz (Pseudo-F)...\n")
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat(sprintf("  K = %d...\n", k))

    # 1. Fit the model
    clusterer <- clusterer_class$new(data = data, n_clusters = k, ...)
    clusterer$fit()

    # 2. Récupérer l'homogénéité moyenne
    # (Attention: get_homogeneity() renvoie une moyenne pondérée entre 0 et 1)
    avg_homogeneity <- clusterer$get_homogeneity()

    # 3. Convertir en "Somme des valeurs propres" (Inertie Expliquée Totale)
    # H = Somme(R^2 des variables avec leur centre)
    explained_inertia <- avg_homogeneity * n_vars

    # 4. Calcul du ratio Pseudo-F
    if (explained_inertia >= n_vars) {
      ch_scores[i] <- Inf
    } else {
      numerator <- (explained_inertia - 1) / (k - 1)
      denominator <- (n_vars - explained_inertia) / (n_vars - k)
      ch_scores[i] <- numerator / denominator
    }
  }

  # Optimal K = Maximal score (Recherche du pic)
  suggested_k <- k_range[which.max(ch_scores)]

  # Plot
  if (plot) {
    plot(k_range, ch_scores,
      type = "b", pch = 19, col = "forestgreen",
      xlab = "Number of clusters (K)",
      ylab = "Pseudo-F Statistic (higher is better)",
      main = "Calinski-Harabasz (Pseudo-F)",
      las = 1, lwd = 2
    )
    grid()
    points(suggested_k, ch_scores[which(k_range == suggested_k)],
      pch = 19, col = "red", cex = 2
    )
    text(suggested_k, ch_scores[which(k_range == suggested_k)],
      labels = sprintf("K = %d", suggested_k),
      pos = 3, col = "red", font = 2
    )
  }

  cat(sprintf("\nK suggested by Calinski-Harabasz: %d\n", suggested_k))
  cat(sprintf("Pseudo-F score: %.3f\n", ch_scores[which(k_range == suggested_k)]))

  return(list(
    k_values = k_range,
    ch_scores = ch_scores,
    suggested_k = suggested_k
  ))
}

#' Compare Multiple K Selection Methods
#'
#' Runs three complementary K selection methods (Elbow, Silhouette, and Homogeneity-Based)
#' and determines a consensus recommendation.
#'
#' @param clusterer_class R6 class of the clusterer (e.g., \code{KMeansClusterer}).
#' @param data Data frame or matrix to be clustered.
#' @param k_range Integer vector of K values to test (default: \code{2:10}).
#'   Must contain at least 2 values.
#' @param plot Logical. Whether to display comparison plots (default: \code{TRUE}).
#' @param ... Additional parameters passed to the clusterer constructor.
#'
#' @return A list with the following components:
#' \item{all_suggestions}{Named integer vector of K values suggested by each method
#'   (elbow, silhouette, homogeneity).}
#' \item{consensus_k}{The K value recommended by the majority of methods (mode).
#'   If all three methods disagree, returns the elbow method suggestion.}
#' \item{elbow_results}{Complete results from \code{\link{elbow_method}}.}
#' \item{silhouette_results}{Complete results from \code{\link{silhouette_method}}.}
#' \item{homogeneity_results}{Complete results from \code{\link{calinski_harabasz_method}}.}
#'
#' @details
#' This function provides a comparison of three complementary methods for
#' determining the optimal number of clusters:
#' \itemize{
#'   \item Elbow: Detects inflection point in quality criterion curve
#'   \item Silhouette: Validates clustering structure using correlation distance
#'   \item Calinski-Harabasz: Pseudo-F statistic balancing quality and parsimony
#' }
#'
#' The consensus is determined by majority vote (2 out of 3). If all three methods
#' suggest different K values, the elbow method result is used as default.
#'
#' If \code{plot = TRUE}, displays a 1x3 grid of comparison plots.
#'
#' Requires the \code{cluster} package for silhouette analysis.
#'
#' @seealso
#' \code{\link{elbow_method}}, \code{\link{silhouette_method}},
#' \code{\link{calinski_harabasz_method}}
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' results <- compare_k_selection_methods(
#'   KMeansClusterer,
#'   data,
#'   k_range = 2:6,
#'   standardize = TRUE
#' )
#' cat("Consensus K:", results$consensus_k, "\n")
#' print(results$all_suggestions)
#' }
#'
#' @export
compare_k_selection_methods <- function(clusterer_class, data, k_range = 2:10, plot = TRUE, ...) {
  cat("========================================\n")
  cat("COMPARISON OF K SELECTION METHODS\n")
  cat("========================================\n\n")

  # Plot configuration
  if (plot) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  }

  # 1. Elbow Method
  cat("\n1. ELBOW METHOD\n")
  cat("-------------------\n")
  elbow_res <- elbow_method(clusterer_class, data, k_range, plot = plot, ...)

  # 2. Silhouette Method
  cat("\n2. SILHOUETTE METHOD\n")
  cat("---------------------------\n")
  silhouette_res <- silhouette_method(clusterer_class, data, k_range, plot = plot, ...)

  # 3. Calinski-Harabasz Method (Pseudo-F)
  cat("\n3. CALINSKI-HARABASZ METHOD (PSEUDO-F)\n")
  cat("----------------------------------------\n")
  homogeneity_res <- calinski_harabasz_method(clusterer_class, data, k_range, plot = plot, ...)

  # Summary
  cat("\n========================================\n")
  cat("SUMMARY OF RECOMMENDATIONS\n")
  cat("========================================\n")
  cat(sprintf("Elbow Method:           K = %d\n", elbow_res$suggested_k))
  cat(sprintf("Silhouette Method:      K = %d\n", silhouette_res$suggested_k))
  cat(sprintf("Calinski-Harabasz:      K = %d\n", homogeneity_res$suggested_k))

  # Majority vote (2 out of 3)
  suggestions <- c(
    elbow_res$suggested_k, silhouette_res$suggested_k,
    homogeneity_res$suggested_k
  )
  vote_table <- table(suggestions)

  # If all three disagree, use elbow method as default
  if (max(vote_table) == 1) {
    consensus_k <- elbow_res$suggested_k
    cat(sprintf("\nNo consensus (all methods differ). Using Elbow method: K = %d\n", consensus_k))
  } else {
    consensus_k <- as.numeric(names(vote_table)[which.max(vote_table)])
    cat(sprintf("\nConsensus (majority vote): K = %d\n", consensus_k))
  }

  cat("========================================\n")

  return(list(
    elbow = elbow_res,
    silhouette = silhouette_res,
    homogeneity = homogeneity_res,
    consensus_k = consensus_k,
    all_suggestions = suggestions
  ))
}
