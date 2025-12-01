#' Visualization methods for the clustering classes
#'
#' This script contains visualization functions for the clustering results.
#' @name visualization
NULL


#' Plot Clustering Results in 2D PCA Space (Correlation Circle)
#'
#' Displays the clustering results by projecting variables as vectors onto the first two
#' principal components of a PCA. Ideally suited for variable clustering.
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}} (e.g., \code{\link{KMeansClusterer}}).
#' @param main Character string for the plot title. If \code{NULL} (default),
#'   a default title is used.
#' @param show_centers Logical. Whether to display cluster centers (default: \code{TRUE}).
#' @param show_labels Logical. Whether to show variable names as labels (default: \code{TRUE}).
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function creates a correlation circle where variables are represented as vectors
#' (arrows) starting from the origin. The length and direction of each arrow indicate
#' the strength and nature of the variable's representation in the 2D PCA space.
#' Variables are colored by their cluster assignment.
#'
#' The unit circle helps identify well-represented variables (arrows reaching near the circle).
#' The percentage of variance explained by each PC is shown in the axis labels.
#' The aspect ratio is fixed (asp=1) to preserve angular relationships.
#'
#' @seealso
#' \code{\link{KMeansClusterer}} for creating clustering objects.
#' \code{\link{plot_clustering_with_supp}} for plotting with supplementary variables.
#' \code{\link{plot_cluster_quality}} for quality metrics visualization.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_clustering_2d(clusterer)
#' }
#'
#' @export
plot_clustering_2d <- function(clusterer, main = NULL, show_centers = TRUE, show_labels = TRUE) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # data extraction via public method
  plot_data <- clusterer$get_plot_data()

  if (is.null(plot_data)) {
    stop("No plot data available. The clustering method may not support 2D visualization.")
  }

  coords <- plot_data$coords
  cluster_colors <- plot_data$colors
  pca <- plot_data$pca
  centers <- plot_data$centers

  # Default title
  if (is.null(main)) {
    main <- "Correlation Circle"
  }

  # % variance explained calculation
  var_exp_1 <- 0
  var_exp_2 <- 0
  if (!is.null(pca)) {
    if (inherits(pca, "prcomp")) {
      ve <- summary(pca)$importance[2, ] * 100
      var_exp_1 <- round(ve[1], 1)
      var_exp_2 <- round(ve[2], 1)
    } else { # PCAmix
      ve <- pca$eig[, 2]
      var_exp_1 <- round(ve[1], 1)
      var_exp_2 <- round(ve[2], 1)
    }
  }
  xlab <- paste0("Dim 1 (", var_exp_1, "%)")
  ylab <- paste0("Dim 2 (", var_exp_2, "%)")

  # --- INIT PLOT (Frame [-1, 1]) ---
  # asp=1 is CRUCIAL so that angles are visually correct
  plot(0, 0,
    type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
    xlab = xlab, ylab = ylab, main = main, las = 1, asp = 1
  )

  # Unit circle and axes
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray60")
  abline(h = 0, v = 0, col = "gray80", lty = 2)

  # Colors
  cols <- cluster_colors[coords$cluster]

  # --- DRAW ARROWS (Variables) ---
  arrows(
    x0 = 0, y0 = 0,
    x1 = coords$PC1, y1 = coords$PC2,
    col = cols, length = 0.1, lwd = 1.5
  )

  # --- LABELS ---
  if (show_labels && !is.null(coords$variable)) {
    # Slight offset for readability (x 1.15)
    text(coords$PC1 * 1.15, coords$PC2 * 1.15,
      labels = coords$variable,
      col = cols, cex = 0.8, font = 2
    )
  }

  # --- CLUSTER CENTERS ---
  if (show_centers && !is.null(centers)) {
    # Display centers as solid points
    # Note: Assumes centers is a matrix with PC1/PC2
    points(centers[, 1], centers[, 2],
      pch = 22, bg = "white", col = "black", cex = 2, lwd = 2
    )
    text(centers[, 1], centers[, 2],
      labels = gsub("C", "", rownames(centers)),
      col = "black", font = 2, cex = 0.8
    )
  }

  # Legend
  legend("topleft",
    legend = paste("Cluster", levels(coords$cluster)),
    col = cluster_colors[1:length(levels(coords$cluster))],
    lwd = 2, bty = "n", cex = 0.8, title = "Groups"
  )

  invisible(NULL)
}


#' Plot Clustering Results with Supplementary Variables (Correlation Circle)
#'
#' Displays clustering results distinguishing between active variables (used for
#' clustering) and supplementary variables (projected after clustering).
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}, with predictions already made.
#' @param main Character string for the plot title. If \code{NULL} (default),
#'   a default title is used.
#' @param show_centers Logical. Whether to display cluster centers (default: \code{TRUE}).
#' @param show_labels Logical. Whether to show variable names as labels (default: \code{TRUE}).
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function creates a correlation circle where active variables (solid arrows)
#' and supplementary variables (dashed arrows) are distinguished by line style.
#' All variables are colored by their cluster assignment.
#'
#' Active variables are displayed in bold font, supplementary variables in italic.
#' The aspect ratio is fixed (asp=1) to preserve angular relationships.
#'
#' Requires that \code{predict()} has been called on the clusterer object first.
#'
#' @seealso
#' \code{\link{plot_clustering_2d}} for plotting without supplementary variables.
#' \code{\link{KMeansClusterer}} for creating clustering objects.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' supp_data <- data.frame(matrix(rnorm(200), ncol = 2))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' clusterer$predict(supp_data)
#' plot_clustering_with_supp(clusterer)
#' }
#'
#' @export
plot_clustering_with_supp <- function(clusterer, main = NULL, show_centers = TRUE, show_labels = TRUE) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # data extraction via public method
  plot_data <- clusterer$get_plot_data_predict()

  if (is.null(plot_data)) {
    stop("No prediction data available. Call predict() first.")
  }

  coords_all <- plot_data$coords
  cluster_colors <- plot_data$colors
  pca <- plot_data$pca
  centers <- plot_data$centers

  # Default title
  if (is.null(main)) {
    main <- "Active & Supplementary Variables"
  }

  # % variance explained calculation
  var_exp_1 <- 0
  var_exp_2 <- 0
  if (!is.null(pca)) {
    if (inherits(pca, "prcomp")) {
      ve <- summary(pca)$importance[2, ] * 100
      var_exp_1 <- round(ve[1], 1)
      var_exp_2 <- round(ve[2], 1)
    } else { # PCAmix
      ve <- pca$eig[, 2]
      var_exp_1 <- round(ve[1], 1)
      var_exp_2 <- round(ve[2], 1)
    }
  }

  # --- INIT PLOT ---
  plot(0, 0,
    type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
    xlab = paste0("Dim 1 (", var_exp_1, "%)"),
    ylab = paste0("Dim 2 (", var_exp_2, "%)"),
    main = main, las = 1, asp = 1
  )

  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray60")
  abline(h = 0, v = 0, col = "gray80", lty = 2)

  # --- DRAWING ---
  # Separate active and supplementary for line style
  cols <- cluster_colors[coords_all$cluster]
  line_types <- ifelse(coords_all$type == "active", 1, 2) # 1=Solid, 2=Dashed
  line_widths <- ifelse(coords_all$type == "active", 1.5, 1.2)

  arrows(
    x0 = 0, y0 = 0,
    x1 = coords_all$PC1, y1 = coords_all$PC2,
    col = cols,
    lty = line_types, # Solid vs dashed
    lwd = line_widths,
    length = 0.1
  )

  if (show_labels) {
    text(coords_all$PC1 * 1.15, coords_all$PC2 * 1.15,
      labels = coords_all$variable,
      col = cols, cex = 0.8, font = ifelse(coords_all$type == "active", 2, 3)
    ) # Bold vs Italic
  }

  # Centers
  if (show_centers && !is.null(centers)) {
    points(centers[, 1], centers[, 2], pch = 22, bg = "white", col = "black", cex = 2, lwd = 2)
    text(centers[, 1], centers[, 2], labels = gsub("C", "", rownames(centers)), col = "black", font = 2, cex = 0.8)
  }

  # Enhanced legend
  legend("topleft",
    legend = c("Active (Training)", "Supplementary (Predict)"),
    lty = c(1, 2), lwd = 2, col = "black", bty = "n", cex = 0.8
  )

  legend("bottomleft",
    legend = paste("Cluster", levels(coords_all$cluster)),
    fill = cluster_colors[1:length(levels(coords_all$cluster))],
    bty = "n", cex = 0.8
  )

  invisible(NULL)
}


#' Plot Dendrogram for Hierarchical Clustering
#'
#' Creates a dendrogram visualization for hierarchical clustering methods.
#' For DivisiveClusterer, displays the split history as a top-down tree.
#'
#' @param clusterer A fitted hierarchical clustering object (e.g., \code{MCA_HClusterer}
#'   or \code{DivisiveClusterer}).
#' @param main Character string for the plot title (default: \code{"Dendrogram"}).
#' @param ... Additional parameters passed to \code{plot()} or the clusterer's
#'   specific dendrogram method.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function automatically detects the clusterer type:
#' \itemize{
#'   \item For \code{DivisiveClusterer}: Calls its \code{plot_split_dendrogram()} method
#'     which displays a top-down tree showing the split history.
#'   \item For other hierarchical methods: Will use standard dendrogram plotting
#'     (implementation depends on the specific clusterer structure).
#' }
#'
#' @seealso
#' \code{\link{DivisiveClusterer}} for divisive clustering.
#' \code{\link{MCA_HClusterer}} for hierarchical clustering on categorical data.
#'
#' @examples
#' \dontrun{
#' # For DivisiveClusterer
#' data <- data.frame(matrix(rnorm(1000), ncol = 10))
#' clusterer <- DivisiveClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_dendrogram(clusterer)
#' }
#'
#' @export
plot_dendrogram <- function(clusterer, main = "Dendrogram", ...) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Check if it's a DivisiveClusterer - use its specialized tree plot
  if (inherits(clusterer, "DivisiveClusterer")) {
    return(clusterer$plot_split_dendrogram(main = main, ...))
  }

  # Check if clusterer has an hclust object (for MCA_HClusterer or similar)
  if (!is.null(clusterer$hclust_result)) {
    plot(clusterer$hclust_result, main = main, ...)
    return(invisible(NULL))
  }

  # Fallback for other methods
  stop(
    "plot_dendrogram() not yet implemented for this clustering method. ",
    "The clusterer must be a DivisiveClusterer or have an hclust_result field."
  )
}


#' Plot Heatmap of Correlations by Cluster
#'
#' Creates a heatmap of the correlation matrix, optionally reordered by cluster
#' assignment to highlight within-cluster relationships.
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}.
#' @param main Character string for the plot title
#'   (default: \code{"Correlation Heatmap by Cluster"}).
#' @param reorder Logical. Whether to reorder variables by cluster assignment
#'   (default: \code{TRUE}).
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function computes the correlation matrix of all variables and displays it
#' as a heatmap. When \code{reorder = TRUE}, variables are sorted by cluster,
#' and black lines separate the clusters visually.
#'
#' Colors range from blue (negative correlation) through white (no correlation)
#' to red (positive correlation).
#'
#' @seealso
#' \code{\link{plot_network_graph}} for network-based correlation visualization.
#' \code{\link{plot_clustering_2d}} for PCA-based clustering visualization.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_correlation_heatmap(clusterer, reorder = TRUE)
#' }
#'
#' @export
plot_correlation_heatmap <- function(clusterer, main = "Correlation Heatmap by Cluster", reorder = TRUE) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Use mixed-data compatible distance matrix
  dist_obj <- get_correlation_distance_matrix(clusterer$data)
  dist_mat <- as.matrix(dist_obj)

  # Convert Distance back to Association (Similarity)
  # d = sqrt(2 * (1 - sim))  =>  sim = 1 - (d^2) / 2
  # Note: 'sim' here is on the squared scale (R², η², Cramer's V²)
  cor_matrix <- 1 - (dist_mat^2) / 2

  # Reorder by clusters
  if (reorder) {
    cluster_order <- order(clusterer$clusters)
    cor_matrix_ordered <- cor_matrix[cluster_order, cluster_order]
    var_names <- clusterer$variable_names[cluster_order]
  } else {
    cor_matrix_ordered <- cor_matrix
    var_names <- clusterer$variable_names
  }

  # Plot
  image(seq_len(ncol(cor_matrix_ordered)), seq_len(nrow(cor_matrix_ordered)),
    t(cor_matrix_ordered),
    col = colorRampPalette(c("blue", "white", "red"))(100),
    xlab = "", ylab = "",
    main = main,
    axes = FALSE
  )

  # Add names
  axis(1,
    at = seq_len(ncol(cor_matrix_ordered)),
    labels = var_names,
    las = 2, cex.axis = 0.7
  )
  axis(2,
    at = seq_len(nrow(cor_matrix_ordered)),
    labels = var_names,
    las = 2, cex.axis = 0.7
  )

  # Add separation lines between clusters
  if (reorder) {
    cumul_sizes <- cumsum(table(clusterer$clusters[cluster_order]))
    abline(v = cumul_sizes + 0.5, lwd = 2)
    abline(h = cumul_sizes + 0.5, lwd = 2)
  }

  invisible(NULL)
}


#' Plot Network Graph of Variable Correlations
#'
#' Creates a network graph where nodes represent variables and edges represent
#' correlations (or associations) above a specified threshold.
#' Supports quantitative, qualitative, and mixed data.
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}.
#' @param threshold Numeric. Minimum association value (0 to 1) for displaying
#'   edges (default: \code{0.3}). For mixed data, this applies to the square root
#'   of the association measure (R², η², Cramer's V²).
#' @param main Character string for the plot title
#'   (default: \code{"Variable Correlation Network"}).
#' @param layout Character string specifying the layout algorithm:
#'   \code{"fruchterman.reingold"} (default), \code{"circle"}, or \code{"kamada.kawai"}.
#'
#' @return Invisibly returns the \code{igraph} object if \code{igraph} is available,
#'   otherwise \code{NULL}.
#'
#' @details
#' This function requires the \code{igraph} package. If not available, it falls back
#' to a simple bar plot showing the number of connections per variable.
#'
#' Nodes are colored by cluster assignment. Edges are drawn only for associations
#' with value exceeding \code{threshold}.
#'
#' For mixed data, association is computed using:
#' \itemize{
#'   \item Pearson R² for numeric-numeric pairs
#'   \item η² (eta-squared) for numeric-factor pairs
#'   \item Cramer's V² for factor-factor pairs
#' }
#' The threshold applies to the square root of these measures (scale 0-1).
#'
#' @seealso
#' \code{\link{plot_correlation_heatmap}} for heatmap-based correlation visualization.
#' \code{\link{plot_clustering_2d}} for PCA-based clustering visualization.
#' \code{\link{get_correlation_distance_matrix}} for the underlying distance computation.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_network_graph(clusterer, threshold = 0.4, layout = "circle")
#' }
#'
#' @export
plot_network_graph <- function(clusterer, threshold = 0.3, main = "Variable Correlation Network",
                               layout = "fruchterman.reingold") {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Check if igraph is available
  if (!requireNamespace("igraph", quietly = TRUE)) {
    warning("Package 'igraph' required for network graphs. Falling back to simple plot.")
    return(plot_correlation_matrix_simple(clusterer, threshold, main))
  }

  # --- MIXED DATA SUPPORT ---
  # Calculate association matrix using get_correlation_distance_matrix (handles mixed types)
  # 1. Get distance matrix: d = sqrt(2 * (1 - sim))
  dist_obj <- get_correlation_distance_matrix(clusterer$data)
  dist_mat <- as.matrix(dist_obj)

  # 2. Convert distance back to association (similarity)
  # d = sqrt(2 * (1 - sim))  =>  sim = 1 - (d^2) / 2
  # Note: 'sim' here is on the squared scale (R², η², Cramer's V²)
  assoc_sq <- 1 - (dist_mat^2) / 2

  # 3. Take sqrt to make it comparable to a standard correlation (0-1 scale)
  # Handle potential negative zeros from float arithmetic WITHOUT FLATTENING MATRIX
  # Note: pmax() would flatten the matrix to a vector, so we use subsetting instead
  assoc_sq[assoc_sq < 0] <- 0
  assoc_matrix <- sqrt(assoc_sq)

  # 4. Create adjacency matrix (associations > threshold)
  adj_matrix <- assoc_matrix > threshold
  diag(adj_matrix) <- FALSE # No self-loops

  # Create graph
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = NULL)

  # Add attributes
  igraph::V(g)$cluster <- as.factor(clusterer$clusters)
  igraph::V(g)$name <- clusterer$variable_names

  # Colors by cluster
  cluster_colors <- rainbow(clusterer$n_clusters)
  vertex_colors <- cluster_colors[clusterer$clusters]

  # Layout
  if (layout == "circle") {
    layout_coords <- igraph::layout_in_circle(g)
  } else if (layout == "kamada.kawai") {
    layout_coords <- igraph::layout_with_kk(g)
  } else {
    layout_coords <- igraph::layout_with_fr(g)
  }

  # Plot
  plot(g,
    vertex.color = vertex_colors,
    vertex.label = igraph::V(g)$name,
    vertex.label.cex = 0.8,
    vertex.size = 15,
    edge.color = "gray70",
    edge.width = 1,
    layout = layout_coords,
    main = main
  )

  # Legend
  legend("topright",
    legend = paste("Cluster", 1:clusterer$n_clusters),
    fill = cluster_colors,
    cex = 0.8,
    title = "Clusters"
  )

  invisible(g)
}
#' Simple correlation matrix plot (fallback without igraph)
#' @keywords internal
plot_correlation_matrix_simple <- function(clusterer, threshold, main) {
  # Use mixed-data compatible method
  dist_obj <- get_correlation_distance_matrix(clusterer$data)
  dist_mat <- as.matrix(dist_obj)
  assoc_sq <- 1 - (dist_mat^2) / 2

  # Handle negative zeros carefully without destroying matrix dimensions
  assoc_sq[assoc_sq < 0] <- 0
  assoc_matrix <- sqrt(assoc_sq)

  # Create adjacency based on threshold
  adj <- assoc_matrix > threshold
  diag(adj) <- FALSE

  # Count connections per variable
  connections <- rowSums(adj)

  # Simple visualization
  barplot(connections[order(clusterer$clusters)],
    names.arg = clusterer$variable_names[order(clusterer$clusters)],
    col = rainbow(clusterer$n_clusters)[clusterer$clusters[order(clusterer$clusters)]],
    main = paste(main, "\n(Connections > threshold)"),
    las = 2,
    cex.names = 0.7,
    ylab = "Number of connections"
  )

  invisible(NULL)
}


#' Plot Variable Contributions to Clusters
#'
#' Displays the top contributing variables to each cluster based on PCA loadings.
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}.
#' @param cluster_id Integer. Specific cluster number to visualize, or \code{NULL}
#'   to display all clusters (default: \code{NULL}).
#' @param top_n Integer. Number of top contributing variables to show
#'   (default: \code{10}).
#' @param main Character string for the plot title
#'   (default: \code{"Variable Contributions"}).
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function displays bar plots showing which variables contribute most to
#' each cluster's first principal component. Contributions are measured by the
#' absolute value of PCA loadings on PC1.
#'
#' Requires that the clusterer has a \code{get_cluster_pca()} method and a
#' \code{get_cluster_homogeneity()} method.
#'
#' When \code{cluster_id = NULL}, creates a multi-panel plot showing all clusters.
#'
#' @seealso
#' \code{\link{plot_cluster_quality}} for cluster quality metrics.
#' \code{\link{plot_scree_by_cluster}} for variance explained per cluster.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_variable_contributions(clusterer, top_n = 5)
#' }
#'
#' @export
plot_variable_contributions <- function(clusterer, cluster_id = NULL, top_n = 10,
                                        main = "Variable Contributions") {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Get cluster homogeneity data if available
  if (is.function(clusterer$get_cluster_homogeneity)) {
    homog <- clusterer$get_cluster_homogeneity()
  } else {
    warning("Cluster homogeneity not available for this clustering method")
    return(invisible(NULL))
  }

  if (is.null(cluster_id)) {
    # Plot all clusters
    par(mfrow = c(ceiling(clusterer$n_clusters / 2), 2))
    for (k in 1:clusterer$n_clusters) {
      plot_single_cluster_contrib(clusterer, k, top_n, homog[k])
    }
    par(mfrow = c(1, 1))
  } else {
    # Plot specific cluster
    if (cluster_id < 1 || cluster_id > clusterer$n_clusters) {
      stop(sprintf("cluster_id must be between 1 and %d", clusterer$n_clusters))
    }
    plot_single_cluster_contrib(clusterer, cluster_id, top_n, homog[cluster_id])
  }

  invisible(NULL)
}


#' Helper function to plot contributions for a single cluster
#' @keywords internal
plot_single_cluster_contrib <- function(clusterer, k, top_n, homog) {
  vars_in_cluster <- which(clusterer$clusters == k)

  if (length(vars_in_cluster) == 0) {
    plot.new()
    text(0.5, 0.5, paste("Cluster", k, "is empty"), cex = 1.5)
    return(invisible(NULL))
  }

  # Get PCA if available
  if (is.function(clusterer$get_cluster_pca)) {
    # Use tryCatch to avoid crash if the index is empty (extra safety)
    cluster_pca <- tryCatch(clusterer$get_cluster_pca()[[k]], error = function(e) NULL)

    if (!is.null(cluster_pca) && length(vars_in_cluster) > 1) {
      # Handle PCAmix objects (mixed data) vs standard prcomp objects
      if (inherits(cluster_pca, "PCAmix")) {
        # For PCAmix (mixed data), use squared loadings (sqload)
        # sqload contains positive values (R² or correlation ratio)
        if (!is.null(cluster_pca$sqload)) {
          loadings <- cluster_pca$sqload[, 1] # Already positive
        } else {
          loadings <- numeric(0)
        }
      } else {
        # Standard case (prcomp or manual list from fast path)
        if (!is.null(cluster_pca$rotation)) {
          loadings <- abs(cluster_pca$rotation[, 1])
        } else {
          loadings <- numeric(0)
        }
      }

      if (length(loadings) > 0) {
        # Sort and select top N
        top_vars <- head(sort(loadings, decreasing = TRUE), min(top_n, length(loadings)))

        # Plot
        barplot(top_vars,
          main = sprintf("Cluster %d\n(Homogeneity: %.3f)", k, homog),
          ylab = "Contribution (Loading/R²)",
          las = 2,
          col = rainbow(clusterer$n_clusters)[k],
          cex.names = 0.8
        )
      } else {
        plot.new()
        text(0.5, 0.5, "No loadings available", cex = 1)
      }
    } else {
      # Single variable cluster
      barplot(1,
        names.arg = clusterer$variable_names[vars_in_cluster],
        main = sprintf("Cluster %d\n(Single variable)", k),
        col = rainbow(clusterer$n_clusters)[k],
        las = 2
      )
    }
  }

  invisible(NULL)
}


#' Plot Cluster Sizes and Quality Metrics
#'
#' Creates bar plots showing cluster sizes and, if available, cluster homogeneity
#' (variance explained by the first principal component).
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}.
#' @param main Character string for the plot title
#'   (default: \code{"Cluster Quality Metrics"}).
#'
#' @return Invisibly returns a list with two components:
#' \item{sizes}{Integer vector of cluster sizes (number of variables per cluster).}
#' \item{homogeneity}{Numeric vector of cluster homogeneity values, or \code{NULL}
#'   if not available.}
#'
#' @details
#' If the clusterer has a \code{get_cluster_homogeneity()} method, this function
#' creates a two-panel plot showing:
#' \itemize{
#'   \item Left panel: Number of variables per cluster
#'   \item Right panel: Homogeneity per cluster with mean line
#' }
#'
#' Otherwise, only cluster sizes are displayed.
#'
#' Homogeneity measures how well the first principal component explains variance
#' within each cluster (higher values indicate more homogeneous clusters).
#'
#' @seealso
#' \code{\link{plot_variable_contributions}} for within-cluster variable importance.
#' \code{\link{plot_scree_by_cluster}} for detailed variance decomposition.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' metrics <- plot_cluster_quality(clusterer)
#' print(metrics$sizes)
#' }
#'
#' @export
plot_cluster_quality <- function(clusterer, main = "Cluster Quality Metrics") {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Get cluster sizes using tabulate to handle potential missing cluster IDs
  # This ensures the vector has length = n_clusters
  cluster_sizes <- tabulate(clusterer$clusters, nbins = clusterer$n_clusters)

  # Get homogeneity if available
  if (is.function(clusterer$get_cluster_homogeneity)) {
    homog <- clusterer$get_cluster_homogeneity()
  } else {
    homog <- NULL
  }

  # Create layout
  if (!is.null(homog)) {
    par(mfrow = c(1, 2))

    # Plot 1: Cluster sizes
    barplot(cluster_sizes,
      names.arg = paste("C", 1:clusterer$n_clusters),
      col = rainbow(clusterer$n_clusters),
      main = "Cluster Sizes",
      ylab = "Number of variables",
      las = 1
    )

    # Plot 2: Homogeneity
    barplot(homog,
      names.arg = paste("C", 1:clusterer$n_clusters),
      col = rainbow(clusterer$n_clusters),
      main = "Cluster Homogeneity",
      ylab = "Homogeneity (PC1 variance explained)",
      las = 1,
      ylim = c(0, 1)
    )
    abline(h = mean(homog, na.rm = TRUE), lty = 2, col = "red", lwd = 2)
    legend("topright", legend = "Mean", lty = 2, col = "red", lwd = 2)

    par(mfrow = c(1, 1))
  } else {
    # Only cluster sizes
    barplot(cluster_sizes,
      names.arg = paste("Cluster", 1:clusterer$n_clusters),
      col = rainbow(clusterer$n_clusters),
      main = main,
      ylab = "Number of variables",
      las = 1
    )
  }

  invisible(list(sizes = cluster_sizes, homogeneity = homog))
}


#' Plot Scree Plot of Explained Variance by Cluster
#'
#' Creates scree plots showing the variance explained by each principal component
#' within each cluster.
#'
#' @param clusterer A fitted clustering object inheriting from
#'   \code{\link{BaseClusterer}}.
#' @param main Character string for the plot title
#'   (default: \code{"Scree Plot by Cluster"}).
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effect
#'   of creating a plot.
#'
#' @details
#' This function creates a multi-panel plot with one scree plot per cluster.
#' Each panel shows:
#' \itemize{
#'   \item Bar plot: Percentage of variance explained by each PC
#'   \item Red line: Cumulative percentage of variance explained
#' }
#'
#' Requires that the clusterer has a \code{get_cluster_pca()} method that returns
#' PCA results for each cluster.
#'
#' Useful for assessing cluster dimensionality and identifying whether a cluster
#' is well-represented by its first principal component.
#'
#' @seealso
#' \code{\link{plot_cluster_quality}} for cluster homogeneity metrics.
#' \code{\link{plot_variable_contributions}} for variable importance.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(500), ncol = 5))
#' clusterer <- KMeansClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' plot_scree_by_cluster(clusterer)
#' }
#'
#' @export
plot_scree_by_cluster <- function(clusterer, main = "Scree Plot by Cluster") {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # Get cluster PCA if available
  if (!is.function(clusterer$get_cluster_pca)) {
    stop("Cluster PCA not available for this clustering method")
  }

  cluster_pca <- clusterer$get_cluster_pca()

  # Setup multi-panel plot
  n_panels <- sum(sapply(cluster_pca, function(x) !is.null(x)))
  if (n_panels == 0) {
    stop("No PCA data available")
  }

  par(mfrow = c(ceiling(n_panels / 2), 2))

  for (k in 1:clusterer$n_clusters) {
    if (!is.null(cluster_pca[[k]])) {
      pca_obj <- cluster_pca[[k]]

      # Handle both prcomp and PCAmix objects
      if (inherits(pca_obj, "PCAmix")) {
        # PCAmix: eig[,1] are eigenvalues
        eigenvalues <- pca_obj$eig[, 1]
        var_exp <- (eigenvalues / sum(eigenvalues)) * 100
      } else {
        # prcomp: sdev^2 are eigenvalues
        eigenvalues <- pca_obj$sdev^2
        var_exp <- (eigenvalues / sum(eigenvalues)) * 100
      }

      barplot(var_exp,
        names.arg = paste0("PC", seq_along(var_exp)),
        main = sprintf("Cluster %d\n(%d variables)", k, sum(clusterer$clusters == k)),
        ylab = "% Variance Explained",
        col = rainbow(clusterer$n_clusters)[k],
        las = 1
      )

      # Add cumulative line
      cumsum_var <- cumsum(var_exp)
      points(seq_along(var_exp) * 1.2 - 0.5, cumsum_var, type = "b", pch = 19, col = "red")
      axis(4, las = 1)
      mtext("Cumulative %", side = 4, line = 3, col = "red")
    }
  }

  par(mfrow = c(1, 1))

  invisible(cluster_pca)
}
