#' Visualization methods for the clustering classes
#' 
#' This script contains visualization functions for the clustering results.
#' @name visualization
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom graphics abline axis barplot grid image legend lines matplot mtext par plot.new points text
#' @importFrom stats cor
#' @importFrom utils head
NULL


#' Plot clustering results in 2D PCA space
#' 
#' Plot clustering results in 2D PCA space for fitted model
#' 
#' @param clusterer A fitted clustering object (e.g., KMeansClusterer)
#' @param main Title of the plot
#' @param show_centers Logical, whether to show cluster centers
#' @param show_labels Logical, whether to show variable labels
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
  
  # default title
  if (is.null(main)) {
    main <- "Clustering des variables actives"
  }
  
  # % variance explained calculation
  if (!is.null(pca)) {
    var_exp_1 <- round(summary(pca)$importance[2,1]*100, 1)
    var_exp_2 <- round(summary(pca)$importance[2,2]*100, 1)
    xlab <- paste0("PC1 (", var_exp_1, "%)")
    ylab <- paste0("PC2 (", var_exp_2, "%)")
  } else {
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  }
  
  # Base plot
  plot(coords$PC1, coords$PC2,
       col = cluster_colors[coords$cluster],
       pch = 19,
       main = main,
       xlab = xlab,
       ylab = ylab,
       las = 1)
  
  # Labels
  if (show_labels && !is.null(coords$variable)) {
    text(coords$PC1, coords$PC2, 
         labels = coords$variable, 
         pos = 3, cex = 0.8)
  }
  
  # Cluster centers
  if (show_centers && !is.null(centers) && !is.null(pca)) {
    # Projection des centres dans l'espace PCA
    centers_pca <- as.matrix(centers) %*% pca$rotation[, 1:2]
    points(centers_pca[, 1], centers_pca[, 2], 
           pch = 8, col = "black", cex = 2, lwd = 2)
    text(centers_pca[, 1], centers_pca[, 2], 
         labels = paste0("C", seq_len(nrow(centers))), 
         pos = 1, cex = 0.9, col = "black", font = 2)
  }
  
  # LLegend
  legend("topright", 
         legend = c(paste("Cluster", seq_along(unique(coords$cluster))), 
                    if(show_centers) "Centers" else NULL),
         col = c(cluster_colors, if(show_centers) "black" else NULL),
         pch = c(rep(19, length(unique(coords$cluster))), 
                 if(show_centers) 8 else NULL),
         cex = 0.8)
  
  invisible(NULL)
}


#' Plot clustering results with supplementary variables
#' 
#' @param clusterer A fitted clustering object with prediction made
#' @param main Title of the plot
#' @param show_centers Logical, whether to show cluster centers
#' @param show_labels Logical, whether to show variable labels
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
    main <- "Clusters with supplementary variables"
  }
  
  # % variance explained calculation
  if (!is.null(pca)) {
    var_exp_1 <- round(summary(pca)$importance[2,1]*100, 1)
    var_exp_2 <- round(summary(pca)$importance[2,2]*100, 1)
    xlab <- paste0("PC1 (", var_exp_1, "%)")
    ylab <- paste0("PC2 (", var_exp_2, "%)")
  } else {
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  }
  
  # Plot with different symbols for active/supplementary variables
  plot(coords_all$PC1, coords_all$PC2,
       col = cluster_colors[coords_all$cluster],
       pch = ifelse(coords_all$type == "active", 19, 17),
       main = main,
       xlab = xlab,
       ylab = ylab,
       las = 1)
  
  # Labels
  if (show_labels && !is.null(coords_all$variable)) {
    text(coords_all$PC1, coords_all$PC2, 
         labels = coords_all$variable, 
         pos = 3, cex = 0.8)
  }
  
  # Centers
  if (show_centers && !is.null(centers) && !is.null(pca)) {
    centers_pca <- as.matrix(centers) %*% pca$rotation[, 1:2]
    points(centers_pca[, 1], centers_pca[, 2], 
           pch = 8, col = "black", cex = 2, lwd = 2)
    text(centers_pca[, 1], centers_pca[, 2], 
         labels = paste0("C", seq_len(nrow(centers))), 
         pos = 1, cex = 0.9, col = "black", font = 2)
  }
  
  # LLegend
  legend("topright", 
         legend = c("Active variables", "Supplementary variables", 
                    if(show_centers) "Centers" else NULL),
         pch = c(19, 17, if(show_centers) 8 else NULL), 
         col = c("black", "black", if(show_centers) "black" else NULL),
         cex = 0.8)
  
  invisible(NULL)
}


#' Plot dendrogram for hierarchical clustering
#' 
#' @param clusterer A fitted hierarchical clustering object
#' @param main Title of the plot
#' @param ... Additional parameters passed to plot()
#' @export
plot_dendrogram <- function(clusterer, main = "Dendrogram", ...) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }

  # TODO: Implement according to the structure of MCA_HClusterer
  # For now, placeholder
  stop("plot_dendrogram() not yet implemented for this clustering method")
}


#' Plot heatmap of correlations by cluster
#' 
#' @param clusterer A fitted clustering object
#' @param main Title of the plot
#' @param reorder Logical, whether to reorder variables by cluster
#' @export
plot_correlation_heatmap <- function(clusterer, main = "Correlation Heatmap by Cluster", reorder = TRUE) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }
  
  # correlation matrix calculation
  cor_matrix <- cor(clusterer$data)
  
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
        axes = FALSE)
  
  # Add names
  axis(1, at = seq_len(ncol(cor_matrix_ordered)), 
       labels = var_names,
       las = 2, cex.axis = 0.7)
  axis(2, at = seq_len(nrow(cor_matrix_ordered)), 
       labels = var_names,
       las = 2, cex.axis = 0.7)
  
  # Add separation lines between clusters
  if (reorder) {
    cumul_sizes <- cumsum(table(clusterer$clusters[cluster_order]))
    abline(v = cumul_sizes + 0.5, lwd = 2)
    abline(h = cumul_sizes + 0.5, lwd = 2)
  }
  
  invisible(NULL)
}


#' Plot network graph of variable correlations
#' 
#' @param clusterer A fitted clustering object
#' @param threshold Correlation threshold for displaying edges (default 0.3)
#' @param main Title of the plot
#' @param layout Layout algorithm: "fruchterman.reingold" (default), "circle", "kamada.kawai"
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
  
  # Calcul matrice de corrélation
  cor_matrix <- cor(clusterer$data)
  
  # Créer matrice d'adjacence (corrélations > threshold)
  adj_matrix <- abs(cor_matrix) > threshold
  diag(adj_matrix) <- FALSE  # Pas de self-loops
  
  # Créer graphe
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = NULL)
  
  # Ajouter attributs
  igraph::V(g)$cluster <- as.factor(clusterer$clusters)
  igraph::V(g)$name <- clusterer$variable_names
  
  # Couleurs par cluster
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
       vertex.size = 10,
       edge.color = "gray70",
       edge.width = 1,
       layout = layout_coords,
       main = main)
  
  # Légende
  legend("topright", 
         legend = paste("Cluster", 1:clusterer$n_clusters),
         fill = cluster_colors,
         cex = 0.8,
         title = "Clusters")
  
  invisible(g)
}


#' Simple correlation matrix plot (fallback without igraph)
#' @keywords internal
plot_correlation_matrix_simple <- function(clusterer, threshold, main) {
  cor_matrix <- cor(clusterer$data)
  
  # Create adjacency based on threshold
  adj <- abs(cor_matrix) > threshold
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
          ylab = "Number of connections")
  
  invisible(NULL)
}


#' Plot radar chart comparing clusters
#' 
#' @param clusterer A fitted clustering object
#' @param main Title of the plot
#' @param standardize Logical, whether to standardize variables before plotting
#' @export
plot_radar_chart <- function(clusterer, main = "Cluster Profiles (Radar Chart)", standardize = TRUE) {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }
  
  # Check if fmsb is available
  if (!requireNamespace("fmsb", quietly = TRUE)) {
    warning("Package 'fmsb' required for radar charts. Falling back to line plot.")
    return(plot_cluster_profiles_lines(clusterer, main, standardize))
  }
  
  # Calculer profils moyens par cluster
  cluster_profiles <- matrix(NA, nrow = clusterer$n_clusters, ncol = ncol(clusterer$data))
  colnames(cluster_profiles) <- clusterer$variable_names
  rownames(cluster_profiles) <- paste("Cluster", 1:clusterer$n_clusters)
  
  for (k in 1:clusterer$n_clusters) {
    vars_in_cluster <- which(clusterer$clusters == k)
    if (length(vars_in_cluster) > 0) {
      cluster_data <- clusterer$data[, vars_in_cluster, drop = FALSE]
      cluster_profiles[k, vars_in_cluster] <- colMeans(cluster_data)
    }
  }
  
  # Replace NA values with 0 (variables not in cluster)
  cluster_profiles[is.na(cluster_profiles)] <- 0
  
  # Standardize if requested
  if (standardize) {
    cluster_profiles <- scale(cluster_profiles)
  }
  
  # Prepare data for fmsb (need to add max/min rows)
  max_val <- max(cluster_profiles, na.rm = TRUE)
  min_val <- min(cluster_profiles, na.rm = TRUE)
  
  # Handle case where all values are NA
  if (!is.finite(max_val) || !is.finite(min_val)) {
    warning("All cluster profile values are NA. Cannot create radar chart.")
    return(invisible(NULL))
  }
  
  radar_data <- rbind(
    rep(max_val, ncol(cluster_profiles)),  # Max
    rep(min_val, ncol(cluster_profiles)),  # Min
    cluster_profiles
  )
  radar_data <- as.data.frame(radar_data)
  
  # Plot
  colors <- rainbow(clusterer$n_clusters, alpha = 0.3)
  line_colors <- rainbow(clusterer$n_clusters)
  
  fmsb::radarchart(
    radar_data,
    axistype = 1,
    pcol = line_colors,
    pfcol = colors,
    plwd = 2,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(min_val, max_val, length.out = 5),
    cglwd = 0.8,
    vlcex = 0.8,
    title = main
  )
  
  # Légende
  legend("topright",
         legend = rownames(cluster_profiles),
         col = line_colors,
         lty = 1,
         lwd = 2,
         cex = 0.8)
  
  invisible(cluster_profiles)
}


#' Fallback line plot for cluster profiles
#' @keywords internal
plot_cluster_profiles_lines <- function(clusterer, main, standardize) {
  # Calculer profils
  cluster_profiles <- matrix(NA, nrow = clusterer$n_clusters, ncol = ncol(clusterer$data))
  
  for (k in 1:clusterer$n_clusters) {
    vars_in_cluster <- which(clusterer$clusters == k)
    if (length(vars_in_cluster) > 0) {
      cluster_data <- clusterer$data[, vars_in_cluster, drop = FALSE]
      cluster_profiles[k, vars_in_cluster] <- colMeans(cluster_data)
    }
  }
  
  if (standardize) {
    cluster_profiles <- scale(cluster_profiles)
  }
  
  # Plot
  matplot(t(cluster_profiles),
          type = "l",
          lty = 1,
          lwd = 2,
          col = rainbow(clusterer$n_clusters),
          xlab = "Variables",
          ylab = "Mean value",
          main = main,
          xaxt = "n")
  
  axis(1, at = seq_len(ncol(clusterer$data)), labels = clusterer$variable_names, las = 2, cex.axis = 0.7)
  
  legend("topright",
         legend = paste("Cluster", 1:clusterer$n_clusters),
         col = rainbow(clusterer$n_clusters),
         lty = 1,
         lwd = 2,
         cex = 0.8)
  
  invisible(NULL)
}


#' Plot variable contributions to clusters
#' 
#' @param clusterer A fitted clustering object
#' @param cluster_id Cluster number to visualize (NULL for all clusters)
#' @param top_n Number of top contributing variables to show
#' @param main Title of the plot
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
    cluster_pca <- clusterer$get_cluster_pca()[[k]]
    
    if (!is.null(cluster_pca) && length(vars_in_cluster) > 1) {
      # Get loadings (contributions)
      loadings <- abs(cluster_pca$rotation[, 1])
      
      # Sort and select top N
      top_vars <- head(sort(loadings, decreasing = TRUE), min(top_n, length(loadings)))
      
      # Plot
      barplot(top_vars,
              main = sprintf("Cluster %d\n(Homogeneity: %.3f)", k, homog),
              ylab = "Absolute loading on PC1",
              las = 2,
              col = rainbow(clusterer$n_clusters)[k],
              cex.names = 0.8)
    } else {
      # Single variable cluster
      barplot(1,
              names.arg = clusterer$variable_names[vars_in_cluster],
              main = sprintf("Cluster %d\n(Single variable)", k),
              col = rainbow(clusterer$n_clusters)[k],
              las = 2)
    }
  }
  
  invisible(NULL)
}


#' Plot cluster sizes and quality metrics
#' 
#' @param clusterer A fitted clustering object
#' @param main Title of the plot
#' @export
plot_cluster_quality <- function(clusterer, main = "Cluster Quality Metrics") {
  # Validation
  if (!clusterer$fitted) {
    stop("Model must be fitted before plotting. Call fit() first.")
  }
  
  # Get cluster sizes
  cluster_sizes <- as.numeric(table(clusterer$clusters))
  
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
            las = 1)
    
    # Plot 2: Homogeneity
    barplot(homog,
            names.arg = paste("C", 1:clusterer$n_clusters),
            col = rainbow(clusterer$n_clusters),
            main = "Cluster Homogeneity",
            ylab = "Homogeneity (PC1 variance explained)",
            las = 1,
            ylim = c(0, 1))
    abline(h = mean(homog), lty = 2, col = "red", lwd = 2)
    legend("topright", legend = "Mean", lty = 2, col = "red", lwd = 2)
    
    par(mfrow = c(1, 1))
  } else {
    # Only cluster sizes
    barplot(cluster_sizes,
            names.arg = paste("Cluster", 1:clusterer$n_clusters),
            col = rainbow(clusterer$n_clusters),
            main = main,
            ylab = "Number of variables",
            las = 1)
  }
  
  invisible(list(sizes = cluster_sizes, homogeneity = homog))
}


#' Plot scree plot of explained variance by cluster
#' 
#' @param clusterer A fitted clustering object
#' @param main Title of the plot
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
      var_exp <- (cluster_pca[[k]]$sdev^2 / sum(cluster_pca[[k]]$sdev^2)) * 100
      
      barplot(var_exp,
              names.arg = paste0("PC", seq_along(var_exp)),
              main = sprintf("Cluster %d\n(%d variables)", k, sum(clusterer$clusters == k)),
              ylab = "% Variance Explained",
              col = rainbow(clusterer$n_clusters)[k],
              las = 1)
      
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