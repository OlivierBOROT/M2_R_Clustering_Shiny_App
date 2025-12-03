#' @title Divisive Clustering for Variables (VARCLUS-style) - HYBRID VERSION
#' @description R6 class that implements divisive (top-down) hierarchical clustering of variables
#' with enhanced robustness, multiple stopping criteria, and quality metrics.
#' This algorithm starts with all variables in one cluster and iteratively splits the most
#' heterogeneous cluster based on PCA eigenvalues and Varimax rotation.
#'
#' **Hybrid Architecture:** Automatically detects data type and uses:
#' - **Fast Path (numeric only):** eigen(cor(X)) - O(p^2) complexity, very fast
#' - **General Path (mixed data):** PCAmix via PCAmixdata package - handles factors
#'
#' Both paths yield mathematically equivalent results for numeric data.
#'
#' @details
#' Algorithm workflow:
#' 1. Initialize: All variables start in one cluster
#' 2. **Detect data type:** Pure numeric -> fast correlation path; Mixed -> PCAmix path
#' 3. Iterate until reaching k clusters or stopping criterion met:
#'    - Select cluster with highest 2nd eigenvalue (most heterogeneous)
#'    - Check stopping criteria (Kaiser, eigenvalue ratio)
#'    - Perform PCA on that cluster (method depends on data type)
#'    - Apply Varimax rotation to first 2 components
#'    - Split variables based on squared correlations with rotated components
#'    - Validate split quality (prevent empty clusters)
#' 4. Each cluster center is its 1st principal component
#'
#' Advantages:
#' - Deterministic (no random initialization)
#' - Interpretable hierarchy of splits (100% stable)
#' - **Hybrid speed:** Fast for numeric, flexible for mixed data
#' - Multiple stopping criteria (Kaiser, eigenvalue ratio)
#' - Split quality metrics for diagnostics
#' - Supports categorical variables via PCAmix (MCA + PCA fusion)
#'
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(1000), ncol = 10))
#' colnames(data) <- paste0("Var", 1:10)
#'
#' # Basic usage
#' clusterer <- DivisiveClusterer$new(data, n_clusters = 3)
#' clusterer$fit()
#' clusterer$print()
#'
#' # With custom parameters
#' clusterer <- DivisiveClusterer$new(
#'   data,
#'   n_clusters = 4,
#'   min_cluster_size = 2,
#'   rotation_method = "varimax",
#'   split_criterion = "eigenvalue2"
#' )
#' clusterer$fit()
#'
#' # With Kaiser stopping rule (auto-stop when lambda2 < 1)
#' clusterer <- DivisiveClusterer$new(
#'   data,
#'   n_clusters = 10, # Maximum clusters
#'   stop_at_kaiser = TRUE
#' )
#' clusterer$fit()
#'
#' # With eigenvalue ratio stopping rule (more flexible)
#' clusterer <- DivisiveClusterer$new(
#'   data,
#'   n_clusters = 10,
#'   min_eigenvalue_ratio = 0.15 # Stop if lambda2/lambda1 < 0.15
#' )
#' clusterer$fit()
#'
#' # Get interpretable hierarchy
#' history <- clusterer$get_split_history()
#' print(history)
#'
#' # Export results
#' assignments <- clusterer$get_cluster_assignments()
#' quality <- clusterer$get_split_quality_summary()
#'
#' # Visualize (shows lambda2/lambda1 ratio)
#' plot_clustering_2d(clusterer)
#' clusterer$plot_split_dendrogram()
#'
#' # Use with Elbow method (get_inertia returns 1 - homogeneity)
#' inertia <- clusterer$get_inertia()
#' }
#'
#' @export

DivisiveClusterer <- R6::R6Class(
  "DivisiveClusterer",
  inherit = BaseClusterer,
  public = list(
    #' @field split_history History of cluster splits (for interpretability)
    split_history = NULL,

    #' @field cluster_centers List of cluster centers (1st PC of each cluster)
    cluster_centers = NULL,

    #' @field cluster_eigenvalues Eigenvalues for each final cluster
    cluster_eigenvalues = NULL,

    #' @field cluster_pca List of PCA objects for each cluster (for compatibility)
    cluster_pca = NULL,

    #' @field cluster_homogeneity Homogeneity scores for each cluster
    cluster_homogeneity = NULL,

    #' @field min_cluster_size Minimum variables per cluster for splitting
    min_cluster_size = NULL,

    #' @field rotation_method Rotation method for PCA ("varimax", "promax", "none")
    rotation_method = NULL,

    #' @field split_criterion Criterion for selecting cluster to split
    split_criterion = NULL,

    #' @field stop_at_kaiser Whether to stop splitting when eigenvalue2 < 1.0 (Kaiser criterion)
    stop_at_kaiser = NULL,

    #' @field min_eigenvalue_ratio Minimum eigenvalue ratio (lambda2/lambda1) for splitting
    min_eigenvalue_ratio = NULL,

    #' @field promax_m Power parameter for Promax rotation (default 4)
    promax_m = NULL,

    #' @field cluster_eigenvalue2_cache Cache of 2nd eigenvalues for each cluster (performance optimization)
    cluster_eigenvalue2_cache = NULL,

    #' @field use_fast_path Logical flag indicating if fast numeric path is used (TRUE) or mixed PCAmix path (FALSE)
    use_fast_path = NULL,

    #' @field global_cor Pre-computed correlation matrix (only for fast numeric path)
    global_cor = NULL,

    #' @field data_mode Character indicating data type: "numeric" or "mixed"
    data_mode = NULL,

    #' @description
    #' Create a new DivisiveClusterer object.
    #' @param data The dataset to be clustered (variables in columns).
    #' @param n_clusters The number of clusters to form.
    #' @param standardize Whether to standardize the data (default TRUE).
    #' @param min_cluster_size Minimum number of variables required to split a cluster (default 3).
    #' @param rotation_method Rotation method: "varimax" (default), "promax", or "none".
    #' @param split_criterion Criterion for selecting cluster to split: "eigenvalue2" (default) or "homogeneity".
    #' @param stop_at_kaiser Logical. If TRUE, stop splitting when max 2nd eigenvalue < 1.0 (default FALSE).
    #' @param min_eigenvalue_ratio Numeric. Minimum lambda2/lambda1 ratio required to continue splitting (default 0.05).
    #' @param promax_m Numeric. Power parameter for Promax rotation (default 4). Only used when rotation_method = "promax".
    #' @return A new `DivisiveClusterer` object.
    initialize = function(data, n_clusters, standardize = TRUE,
                          min_cluster_size = 2, rotation_method = "varimax",
                          split_criterion = "eigenvalue2", stop_at_kaiser = FALSE,
                          min_eigenvalue_ratio = 0.05, promax_m = 4) {
      super$initialize(data, n_clusters, standardize = standardize)
      self$split_history <- list()
      self$cluster_centers <- list()
      self$cluster_eigenvalues <- list()
      self$cluster_pca <- list()
      self$cluster_homogeneity <- numeric(n_clusters)
      self$min_cluster_size <- min_cluster_size
      self$rotation_method <- match.arg(rotation_method, c("varimax", "promax", "none"))
      self$split_criterion <- match.arg(split_criterion, c("eigenvalue2", "homogeneity"))
      self$stop_at_kaiser <- stop_at_kaiser
      self$min_eigenvalue_ratio <- min_eigenvalue_ratio
      self$promax_m <- promax_m
      self$cluster_eigenvalue2_cache <- list()
    },

    #' @description
    #' Fit the divisive clustering algorithm to the data.
    #' Automatically detects data type and uses optimal computation path.
    #' @return The fitted divisive clustering model (self, invisibly).
    fit = function() {
      # ========== STEP 0: DETECT DATA TYPE ==========
      is_pure_numeric <- all(sapply(self$data, function(col) {
        is.numeric(col) || is.integer(col)
      }))

      if (is_pure_numeric) {
        # --- PATH A: FAST NUMERIC (Original optimized code) ---
        self$data_mode <- "numeric"
        self$use_fast_path <- TRUE

        # Standardize data if requested - store params for predict()
        if (self$standardize) {
          scaled_res <- standardize_data(self$data)
          data_to_use <- scaled_res
          private$centers <- attr(scaled_res, "centers")
          private$scales <- attr(scaled_res, "scales")
        } else {
          data_to_use <- self$data
          private$centers <- NULL
          private$scales <- NULL
        }

        # Pre-compute global correlation matrix (FAST: O(p^2) once)
        self$global_cor <- cor(data_to_use, use = "pairwise.complete.obs")

        cat("Data mode: NUMERIC (fast correlation path)\n")
      } else {
        # --- PATH B: MIXED DATA (PCAmix for categorical support) ---
        self$data_mode <- "mixed"
        self$use_fast_path <- FALSE
        self$global_cor <- NULL

        # Check PCAmixdata dependency
        if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
          stop(
            "Package 'PCAmixdata' is required for mixed (numeric + categorical) data.\n",
            "Install it with: install.packages('PCAmixdata')"
          )
        }

        cat("Data mode: MIXED (PCAmix path for categorical variables)\n")
      }

      # ========== STEP 1: INITIALIZE ==========
      n_vars <- ncol(self$data)
      current_clusters <- rep(1, n_vars)
      names(current_clusters) <- self$variable_names

      current_k <- 1
      iteration <- 0

      # Iteratively split until we reach n_clusters
      while (current_k < self$n_clusters) {
        iteration <- iteration + 1

        # Step A: Find cluster with highest 2nd eigenvalue
        # Uses fast path (correlation matrix) or mixed path (PCAmix) based on data_mode
        split_info <- private$find_cluster_to_split(current_clusters, current_k)
        cluster_to_split <- split_info$cluster_id
        max_eigenvalue_2 <- split_info$eigenvalue_2
        max_eigenvalue_1 <- split_info$eigenvalue_1

        if (is.null(cluster_to_split)) {
          warning(sprintf("Cannot split further. Stopping at %d clusters.", current_k))
          break
        }

        # Step A.2: Check Kaiser criterion (stop if max eigenvalue2 < 1.0)
        if (self$stop_at_kaiser && !is.na(max_eigenvalue_2) && max_eigenvalue_2 < 1.0) {
          cat(sprintf(
            "Kaiser criterion: Stopping at %d clusters (max lambda2 = %.3f < 1.0)\n",
            current_k, max_eigenvalue_2
          ))
          break
        }

        # Step A.3: Check eigenvalue ratio criterion (alternative to Kaiser)
        if (!is.na(max_eigenvalue_1) && !is.na(max_eigenvalue_2) && max_eigenvalue_1 > 0) {
          ratio <- max_eigenvalue_2 / max_eigenvalue_1
          if (ratio < self$min_eigenvalue_ratio) {
            cat(sprintf(
              "Eigenvalue ratio criterion: Stopping at %d clusters (lambda2/lambda1 = %.3f < %.3f)\n",
              current_k, ratio, self$min_eigenvalue_ratio
            ))
            break
          }
        }

        # Step B: Split this cluster (uses appropriate path based on data_mode)
        split_result <- private$split_cluster(current_clusters, cluster_to_split, current_k)

        # Step B.2: Validate split quality (prevent empty clusters)
        if (length(split_result$group1_vars) == 0 || length(split_result$group2_vars) == 0) {
          warning(sprintf("Split would create empty cluster. Stopping at %d clusters.", current_k))
          break
        }

        # Step C: Update cluster assignments
        current_clusters <- split_result$new_clusters
        current_k <- max(current_clusters)

        # Invalidate cache for split cluster (it changed) and mark new clusters as uncached
        self$cluster_eigenvalue2_cache[[as.character(cluster_to_split)]] <- NULL
        self$cluster_eigenvalue2_cache[[as.character(current_k)]] <- NULL

        # Record split history with enhanced metrics
        self$split_history[[iteration]] <- list(
          iteration = iteration,
          split_cluster = cluster_to_split,
          eigenvalue_1 = split_result$eigenvalue_1,
          eigenvalue_2 = split_result$eigenvalue_2,
          eigenvalue_ratio = if (!is.na(split_result$eigenvalue_1) && split_result$eigenvalue_1 > 0) {
            split_result$eigenvalue_2 / split_result$eigenvalue_1
          } else {
            NA
          },
          n_clusters_after = current_k,
          variables_group1 = split_result$group1_vars,
          variables_group2 = split_result$group2_vars,
          split_quality = split_result$split_quality
        )
      }

      # Store final clusters (FIX: Ensure integer type for compatibility with tests)
      self$clusters <- as.integer(current_clusters)

      # Initialize cluster_pca list with proper size to avoid index out of bounds
      self$cluster_pca <- vector("list", self$n_clusters)

      # Compute cluster centers and homogeneity (1st PC of each cluster)
      private$compute_cluster_centers()
      private$compute_cluster_homogeneity()

      self$fitted <- TRUE

      cat(sprintf(
        "Divisive clustering completed: %d clusters formed in %d iterations\n",
        max(self$clusters), iteration
      ))

      invisible(self)
    },

    #' @description
    #' Predict the cluster labels for new variables based on squared correlations with cluster centers.
    #' Uses hybrid logic: vectorized cor() for numeric data, eta-squared for categorical variables.
    #' @param new_data New data points (observations in rows, variables in columns).
    #' @return A vector of predicted cluster labels.
    predict = function(new_data) {
      private$check_fitted()

      # Local validation (don't use inherited validate_new_data which may reject factors)
      if (!is.data.frame(new_data)) {
        new_data <- as.data.frame(new_data)
      }
      if (nrow(new_data) != nrow(self$data)) {
        stop(sprintf(
          "New data must have the same number of observations as training data (%d vs %d)",
          nrow(new_data), nrow(self$data)
        ))
      }

      # FIX: Do not standardize new variables using training means/SDs.
      # Variable clustering adds *columns*. They should be standardized independently if needed.
      # Since we use correlation (which is scale-invariant), we can use raw data.
      data_to_use <- as.data.frame(new_data)

      # Ensure column names exist (fallback for raw matrices)
      if (is.null(colnames(data_to_use))) {
        colnames(data_to_use) <- paste0("V", seq_len(ncol(data_to_use)))
      }

      # Build centers matrix (observations x clusters)
      valid_clusters <- which(sapply(self$cluster_centers, function(x) !is.null(x) && length(x) > 0))

      if (length(valid_clusters) == 0) {
        stop("No valid cluster centers found.")
      }

      centers_matrix <- do.call(cbind, self$cluster_centers[valid_clusters])
      colnames(centers_matrix) <- valid_clusters

      n_vars <- ncol(data_to_use)
      n_clusters <- length(valid_clusters)
      squared_cor_matrix <- matrix(0, nrow = n_vars, ncol = n_clusters)
      rownames(squared_cor_matrix) <- colnames(data_to_use)

      # --- HYBRID PREDICTION LOGIC ---
      # Check if new_data is all numeric
      new_data_all_numeric <- all(sapply(data_to_use, is.numeric))

      if (self$use_fast_path && new_data_all_numeric) {
        # PATH A: FAST NUMERIC (Vectorized cor()) - only if new data is also numeric
        cor_mat <- cor(data_to_use, centers_matrix, use = "pairwise.complete.obs")
        squared_cor_matrix <- cor_mat^2
      } else {
        # PATH B: MIXED DATA (Handle factors with eta-squared)
        # Used when training was mixed OR when new data contains factors
        for (j in seq_len(n_vars)) {
          variable <- data_to_use[[j]]

          if (is.numeric(variable)) {
            # Pearson R^2 for numeric variable vs numeric center
            r <- cor(variable, centers_matrix, use = "pairwise.complete.obs")
            squared_cor_matrix[j, ] <- r^2
          } else {
            # Correlation Ratio (Eta-squared) for Factor vs numeric center
            # Eta^2 = R^2 from ANOVA: Center ~ Factor
            for (k in seq_len(n_clusters)) {
              center <- centers_matrix[, k]
              tryCatch(
                {
                  # Simple ANOVA model to get eta-squared
                  mod <- lm(center ~ variable)
                  squared_cor_matrix[j, k] <- summary(mod)$r.squared
                },
                error = function(e) {
                  # Handle edge cases (single level, all NAs, etc.)
                  squared_cor_matrix[j, k] <<- 0
                }
              )
            }
          }
        }
      }

      # Assign each variable to cluster with highest squared association
      best_cluster_idx <- apply(squared_cor_matrix, 1, which.max)
      predictions <- as.integer(valid_clusters[best_cluster_idx])
      names(predictions) <- colnames(data_to_use)

      return(predictions)
    },

    #' @description Print method for DivisiveClusterer
    print = function() {
      cat("=== Divisive Clusterer (VARCLUS-style) ===\n\n")
      cat(sprintf("Number of variables: %d\n", self$get_n_variables()))
      cat(sprintf("Number of clusters: %d\n", self$n_clusters))
      cat(sprintf("Standardization: %s\n", ifelse(self$standardize, "Yes", "No")))
      cat(sprintf("Fitted: %s\n", ifelse(self$fitted, "Yes", "No")))

      if (self$fitted) {
        cat("\nCluster sizes:\n")
        print(table(self$clusters))

        cat("\nSplit history:\n")
        for (i in seq_along(self$split_history)) {
          hist <- self$split_history[[i]]
          ratio_str <- if (!is.na(hist$eigenvalue_ratio)) sprintf(", ratio=%.3f", hist$eigenvalue_ratio) else ""
          cat(sprintf(
            "  Iteration %d: Split cluster %d (lambda1=%.3f, lambda2=%.3f%s) -> %d clusters\n",
            hist$iteration, hist$split_cluster,
            hist$eigenvalue_1, hist$eigenvalue_2, ratio_str,
            hist$n_clusters_after
          ))
        }

        cat(sprintf("\nGlobal homogeneity: %.3f\n", self$get_homogeneity()))
      }

      invisible(self)
    },

    #' @description Summary method for DivisiveClusterer
    #' @return A summary of the divisive clustering model.
    summary = function() {
      private$check_fitted()

      cat("=== Divisive Clusterer Summary ===\n\n")

      for (k in seq_len(self$n_clusters)) {
        members <- self$get_cluster_members(k)
        cat(sprintf("\nCluster %d (%d variables):\n", k, length(members)))
        cat("  Variables:", paste(members, collapse = ", "), "\n")

        # Safely check if eigenvalues exist for this cluster
        eigenvals <- NULL
        if (!is.null(self$cluster_eigenvalues) && 
            length(self$cluster_eigenvalues) >= k &&
            !is.null(self$cluster_eigenvalues[[k]])) {
          eigenvals <- self$cluster_eigenvalues[[k]]
        }

        if (!is.null(eigenvals) && length(eigenvals) > 0) {
          cat(sprintf("  1st eigenvalue: %.3f\n", eigenvals[1]))
          if (length(eigenvals) > 1) {
            cat(sprintf("  2nd eigenvalue: %.3f\n", eigenvals[2]))
            cat(sprintf(
              "  Eigenvalue ratio (lambda2/lambda1): %.3f\n",
              eigenvals[2] / eigenvals[1]
            ))
          }
          cat(sprintf("  Homogeneity: %.3f\n", self$cluster_homogeneity[k]))
        }
      }

      invisible(self)
    },

    #' @description
    #' Get cluster centers for visualization or further analysis
    #' @return List of cluster centers (1st PC scores for each cluster)
    get_centers = function() {
      private$check_fitted()
      return(self$cluster_centers)
    },

    #' @description
    #' Get the split history for interpretability
    #' @return List containing split history
    get_split_history = function() {
      private$check_fitted()
      return(self$split_history)
    },

    #' @description
    #' Get cluster homogeneity scores (compatibility with KMeansClusterer)
    #' @return Numeric vector of homogeneity scores
    get_cluster_homogeneity = function() {
      private$check_fitted()
      return(self$cluster_homogeneity)
    },

    #' @description
    #' Get global homogeneity score (compatibility with KMeansClusterer)
    #' @return Numeric value (weighted average homogeneity)
    get_homogeneity = function() {
      private$check_fitted()
      # FIX: Use tabulate to handle empty clusters correctly
      # table() drops levels with 0 count, causing a length mismatch with cluster_homogeneity
      cluster_sizes <- tabulate(self$clusters, nbins = self$n_clusters)
      return(weighted.mean(self$cluster_homogeneity, cluster_sizes))
    },

    #' @description
    #' Get unexplained variance metric for Elbow Method (Lower is better).
    #' Returns (1 - Global Homogeneity). This is the proportion of variance NOT
    #' explained by the first principal component of each cluster.
    #' Note: This differs from K-Means "inertia" (sum of squared distances).
    #' For variable clustering, we use correlation-based metrics instead.
    #' @return Numeric value in \code{[0, 1]} representing unexplained variance
    get_unexplained_variance = function() {
      private$check_fitted()
      return(1 - self$get_homogeneity())
    },

    #' @description
    #' Alias for get_unexplained_variance() for compatibility with validation tools.
    #' Note: Unlike K-Means inertia (SSE), this returns (1 - homogeneity).
    #' @return Numeric value representing unexplained variance
    get_inertia = function() {
      return(self$get_unexplained_variance())
    },

    #' @description
    #' Get PCA objects for each cluster (compatibility with KMeansClusterer)
    #' @return List of PCA-like objects
    get_cluster_pca = function() {
      private$check_fitted()
      return(self$cluster_pca)
    },

    #' @description
    #' Export cluster assignments as a tidy data frame
    #' @return Data frame with variable, cluster, and homogeneity columns
    get_cluster_assignments = function() {
      private$check_fitted()

      data.frame(
        variable = self$variable_names,
        cluster = self$clusters,
        homogeneity = self$cluster_homogeneity[self$clusters],
        stringsAsFactors = FALSE
      )
    },

    #' @description
    #' Get split quality summary as a data frame
    #' @return Data frame with detailed split metrics
    get_split_quality_summary = function() {
      private$check_fitted()

      if (length(self$split_history) == 0) {
        return(data.frame())
      }

      do.call(rbind, lapply(self$split_history, function(h) {
        data.frame(
          iteration = h$iteration,
          split_cluster = h$split_cluster,
          eigenvalue_1 = h$eigenvalue_1,
          eigenvalue_2 = h$eigenvalue_2,
          eigenvalue_ratio = h$eigenvalue_ratio,
          split_quality = h$split_quality,
          n_vars_group1 = length(h$variables_group1),
          n_vars_group2 = length(h$variables_group2),
          stringsAsFactors = FALSE
        )
      }))
    },

    #' @description
    #' Plot the history of splits as a tree diagram (top-down dendrogram)
    #' @param main Character string for plot title
    #' @param show_eigenvalues Whether to show eigenvalues on branches
    #' @param show_vars Whether to show variable counts in nodes
    plot_split_dendrogram = function(main = "Divisive Clustering Tree",
                                     show_eigenvalues = TRUE, show_vars = TRUE) {
      private$check_fitted()
      hist <- self$split_history

      if (length(hist) == 0) {
        cat("No splits to display (n_clusters = 1)\n")
        return(invisible(NULL))
      }

      n_iter <- length(hist)
      n_clust <- max(self$clusters)

      # Setup canvas (top-down: y=0 at top, y increases downward)
      par(mar = c(2, 4, 4, 2) + 0.1)
      plot(0, 0,
        type = "n",
        xlim = c(0, n_clust + 1),
        ylim = c(n_iter + 0.5, -0.5),
        axes = FALSE,
        xlab = "",
        ylab = "Split Iteration",
        main = main
      )
      axis(2, at = 0:n_iter, labels = c("Root", 1:n_iter), las = 1)

      # Track positions of clusters: List where index is cluster ID, value is x-pos
      # Initially, Cluster 1 is in the middle
      cluster_pos <- list()
      cluster_pos[[1]] <- (n_clust + 1) / 2

      # Draw Root node
      n_total <- length(self$clusters)
      root_label <- if (show_vars) sprintf("All\n(n=%d)", n_total) else "All"
      text(cluster_pos[[1]], 0, root_label, cex = 0.9, font = 2)
      points(cluster_pos[[1]], 0, pch = 21, bg = "lightgray", cex = 3)

      # Color palette for final clusters
      cluster_colors <- rainbow(n_clust, alpha = 0.7)

      # Iterate through split history
      for (h in hist) {
        parent <- h$split_cluster
        child_1 <- parent # Keeps original ID
        child_2 <- h$n_clusters_after # Gets new ID

        x_parent <- cluster_pos[[parent]]
        y_parent <- h$iteration - 1
        y_child <- h$iteration

        # Calculate new positions (spread children apart)
        spread <- (n_clust + 1) / (2^(h$iteration))
        x_child_1 <- x_parent - spread / 2
        x_child_2 <- x_parent + spread / 2

        # Update positions
        cluster_pos[[child_1]] <- x_child_1
        cluster_pos[[child_2]] <- x_child_2

        # Draw connecting lines
        segments(x_parent, y_parent + 0.15, x_parent, y_parent + 0.3, col = "gray40", lwd = 1.5)
        segments(x_parent, y_parent + 0.3, x_child_1, y_child - 0.15, col = "gray40", lwd = 1.5)
        segments(x_parent, y_parent + 0.3, x_child_2, y_child - 0.15, col = "gray40", lwd = 1.5)

        # Node info
        n_vars_1 <- length(h$variables_group1)
        n_vars_2 <- length(h$variables_group2)

        # Draw child nodes
        # Child 1 (keeps parent ID)
        label_1 <- if (show_vars) sprintf("C%d\n(n=%d)", child_1, n_vars_1) else paste0("C", child_1)
        points(x_child_1, y_child, pch = 21, bg = cluster_colors[child_1], cex = 3)
        text(x_child_1, y_child, label_1, cex = 0.7, font = 2)

        # Child 2 (new cluster)
        label_2 <- if (show_vars) sprintf("C%d\n(n=%d)", child_2, n_vars_2) else paste0("C", child_2)
        points(x_child_2, y_child, pch = 21, bg = cluster_colors[child_2], cex = 3)
        text(x_child_2, y_child, label_2, cex = 0.7, font = 2)

        # Show eigenvalue ratio on the split point (more informative than just lambda2)
        if (show_eigenvalues && !is.na(h$eigenvalue_ratio)) {
          text(x_parent, y_parent + 0.45,
            sprintf("L2/L1=%.2f", h$eigenvalue_ratio),
            cex = 0.6, col = "blue", font = 3
          )
        } else if (show_eigenvalues && !is.na(h$eigenvalue_2)) {
          text(x_parent, y_parent + 0.45,
            sprintf("L2=%.2f", h$eigenvalue_2),
            cex = 0.6, col = "blue", font = 3
          )
        }
      }

      # Legend for final clusters
      legend("bottomright",
        legend = paste("Cluster", 1:n_clust),
        fill = cluster_colors,
        cex = 0.7,
        title = "Final Clusters",
        bty = "n"
      )

      invisible(NULL)
    },

    #' @description
    #' Get detailed split information as data frame
    #' @return Data frame with split details
    get_split_details = function() {
      private$check_fitted()

      if (length(self$split_history) == 0) {
        return(data.frame())
      }

      details <- do.call(rbind, lapply(self$split_history, function(h) {
        data.frame(
          iteration = h$iteration,
          split_cluster = h$split_cluster,
          eigenvalue_1 = h$eigenvalue_1,
          eigenvalue_2 = h$eigenvalue_2,
          eigenvalue_ratio = h$eigenvalue_ratio,
          n_clusters_after = h$n_clusters_after,
          n_vars_group1 = length(h$variables_group1),
          n_vars_group2 = length(h$variables_group2),
          split_quality = h$split_quality,
          stringsAsFactors = FALSE
        )
      }))

      return(details)
    },

    #' @description
    #' Get hierarchy tree structure for external visualization
    #' @return List representing the split tree
    get_hierarchy_tree = function() {
      private$check_fitted()

      tree <- list(
        n_clusters = self$n_clusters,
        n_iterations = length(self$split_history),
        splits = self$split_history,
        final_clusters = as.list(table(self$clusters))
      )

      return(tree)
    },

    #' @description
    #' Prepare data for the 2D correlation circle plot (compatibility with plot_clustering_2d).
    #' @return A list with coords, colors, pca, and centers for visualization.
    get_plot_data = function() {
      private$check_fitted()

      # 1. Compute Global PCA (for visualization context)
      if (self$use_fast_path) {
        # Numeric path: use prcomp
        pca <- prcomp(self$data, scale. = self$standardize)

        # Calculate coordinates (correlations with PCA axes)
        # Loadings scaled by sqrt(eigenvalue) give correlations
        coords <- t(t(pca$rotation[, 1:2]) * pca$sdev[1:2])
      } else {
        # Mixed path: use PCAmix
        if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
          warning("PCAmixdata required for plotting mixed data")
          return(NULL)
        }

        split_res <- PCAmixdata::splitmix(self$data)
        pca <- PCAmixdata::PCAmix(
          X.quanti = split_res$X.quanti,
          X.quali = split_res$X.quali,
          ndim = 2,
          graph = FALSE
        )

        # For mixed data, use squared loadings for visualization
        if (!is.null(pca$sqload)) {
          # sqload contains squared correlations for all variables
          coords <- sqrt(pca$sqload[, 1:2, drop = FALSE])
          # Apply sign from quanti.cor if available
          if (!is.null(pca$quanti.cor)) {
            quanti_vars <- rownames(pca$quanti.cor)
            for (v in quanti_vars) {
              if (v %in% rownames(coords)) {
                coords[v, ] <- coords[v, ] * sign(pca$quanti.cor[v, 1:2])
              }
            }
          }
          # Normalize coordinates to stay within unit circle
          # (eta² for factors can exceed 1 before sqrt, causing coords > 1)
          for (i in seq_len(nrow(coords))) {
            norm_val <- sqrt(sum(coords[i, ]^2))
            if (norm_val > 1) {
              coords[i, ] <- coords[i, ] * 0.95 / norm_val
            }
          }
        } else if (!is.null(pca$quanti.cor)) {
          coords <- pca$quanti.cor[, 1:2, drop = FALSE]
        } else {
          # Fallback: zero coordinates
          coords <- matrix(0, nrow = ncol(self$data), ncol = 2)
          rownames(coords) <- self$variable_names
        }
      }

      # Build coordinates data frame
      coords_df <- as.data.frame(coords)
      colnames(coords_df) <- c("PC1", "PC2")
      coords_df$variable <- rownames(coords_df)
      if (is.null(coords_df$variable)) {
        coords_df$variable <- self$variable_names
      }
      coords_df$cluster <- factor(self$clusters[match(coords_df$variable, self$variable_names)])

      # Calculate cluster centers (centroids in PCA space)
      centers_list <- by(coords_df[, c("PC1", "PC2")], coords_df$cluster, colMeans)
      centers <- do.call(rbind, centers_list)
      rownames(centers) <- paste0("C", rownames(centers))

      # Colors for clusters
      n_actual_clusters <- length(unique(self$clusters))
      cluster_colors <- rainbow(n_actual_clusters)

      return(list(
        coords = coords_df,
        colors = cluster_colors,
        pca = pca,
        centers = centers
      ))
    },

    #' @description
    #' Prepare data for plotting with illustrative/supplementary variables.
    #' Projects new variables onto the global PCA space for visualization.
    #' @param new_data New data frame with same observations as training data.
    #' @param pred_result Optional prediction result (from predict()). If NULL, will call predict().
    #' @return Self (invisibly).
    prepare_plot_predict = function(new_data, pred_result = NULL) {
      private$check_fitted()

      # Get predictions if not provided
      if (is.null(pred_result)) {
        pred_result <- self$predict(new_data)
      }

      # Get base plot data (includes global PCA)
      base_plot <- self$get_plot_data()
      pca <- base_plot$pca

      n_new_vars <- ncol(new_data)
      new_coords <- matrix(0, nrow = n_new_vars, ncol = 2)

      # Project new variables onto PCA space
      if (self$use_fast_path) {
        # Numeric path: use prcomp
        if (inherits(pca, "prcomp")) {
          pca_scores <- pca$x[, 1:2, drop = FALSE]
        } else {
          pca_scores <- pca$ind$coord[, 1:2, drop = FALSE]
        }
      } else {
        # Mixed path: use PCAmix
        if (inherits(pca, "prcomp")) {
          pca_scores <- pca$x[, 1:2, drop = FALSE]
        } else {
          pca_scores <- pca$ind$coord[, 1:2, drop = FALSE]
        }
      }

      # Calculate coordinates for each new variable
      for (var_idx in seq_len(n_new_vars)) {
        var_data <- new_data[, var_idx]

        if (is.numeric(var_data)) {
          # Numeric: correlation with PCA axes
          new_coords[var_idx, 1] <- cor(var_data, pca_scores[, 1], use = "complete.obs")
          new_coords[var_idx, 2] <- cor(var_data, pca_scores[, 2], use = "complete.obs")
        } else {
          # Factor: eta² (correlation ratio)
          for (dim in 1:2) {
            tryCatch({
              fit <- suppressWarnings(lm(pca_scores[, dim] ~ var_data))
              eta2 <- summary(fit)$r.squared
              sign_val <- sign(mean(fit$fitted.values[var_data == levels(var_data)[1]], na.rm = TRUE))
              if (is.na(sign_val)) sign_val <- 1
              new_coords[var_idx, dim] <- sqrt(eta2) * sign_val
            }, error = function(e) {
              new_coords[var_idx, dim] <<- 0
            })
          }
        }
      }

      # Normalize coordinates to stay within unit circle
      for (i in seq_len(nrow(new_coords))) {
        coord_norm <- sqrt(new_coords[i, 1]^2 + new_coords[i, 2]^2)
        if (coord_norm > 1) {
          new_coords[i, ] <- new_coords[i, ] / coord_norm * 0.95
        }
      }

      # Create illustrative coordinates data frame
      coords_illus <- data.frame(
        PC1 = new_coords[, 1],
        PC2 = new_coords[, 2],
        variable = colnames(new_data),
        cluster = factor(pred_result),
        type = "illustrative",
        stringsAsFactors = FALSE
      )

      # Store for later retrieval
      private$coords_predict <- coords_illus

      invisible(self)
    },

    #' @description
    #' Get data for plotting with supplementary variables (compatibility with plot_clustering_with_supp).
    #' @return A list with coords (including illustrative), colors, pca, and centers for visualization.
    get_plot_data_predict = function() {
      private$check_fitted()

      # Get base plot data
      base_plot <- self$get_plot_data()

      # If no illustrative coords stored, return base
      if (is.null(private$coords_predict)) {
        return(base_plot)
      }

      # Add type to active coords
      coords_active <- base_plot$coords
      coords_active$type <- "active"

      # Combine active and illustrative
      coords_combined <- rbind(coords_active, private$coords_predict)

      return(list(
        coords = coords_combined,
        colors = base_plot$colors,
        pca = base_plot$pca,
        centers = base_plot$centers
      ))
    }
  ),
  private = list(
    # Stored standardization parameters from fit() for use in predict()
    centers = NULL,
    scales = NULL,
    # Stored coordinates for illustrative variables
    coords_predict = NULL,

    # Find the cluster with the highest 2nd eigenvalue (most heterogeneous)
    # Uses cached eigenvalues when available. Supports both numeric (fast) and mixed (PCAmix) paths.
    # @param current_clusters Current cluster assignments
    # @param current_k Current number of clusters
    # @return List with cluster_id, eigenvalue_1, eigenvalue_2, or NULL cluster_id if no valid split
    find_cluster_to_split = function(current_clusters, current_k) {
      max_criterion <- -Inf
      cluster_to_split <- NULL
      max_eigenvalue_1 <- NA
      max_eigenvalue_2 <- NA

      for (k in seq_len(current_k)) {
        vars_in_cluster <- which(current_clusters == k)

        # Need at least min_cluster_size variables to split
        if (length(vars_in_cluster) < self$min_cluster_size) {
          self$cluster_eigenvalue2_cache[[as.character(k)]] <- list(
            eigenvalue_1 = NA, eigenvalue_2 = NA, splittable = FALSE
          )
          next
        }

        # Check cache first (performance optimization)
        cache_key <- as.character(k)
        cached <- self$cluster_eigenvalue2_cache[[cache_key]]

        if (!is.null(cached) && isTRUE(cached$splittable)) {
          eigenvalue_1 <- cached$eigenvalue_1
          eigenvalue_2 <- cached$eigenvalue_2
        } else {
          # Compute eigenvalues based on data mode
          eigenvalues <- private$compute_cluster_eigenvalues(vars_in_cluster)

          eigenvalue_1 <- if (length(eigenvalues) >= 1) eigenvalues[1] else NA
          eigenvalue_2 <- if (length(eigenvalues) >= 2) eigenvalues[2] else NA

          # Store in cache
          self$cluster_eigenvalue2_cache[[cache_key]] <- list(
            eigenvalue_1 = eigenvalue_1,
            eigenvalue_2 = eigenvalue_2,
            splittable = TRUE
          )
        }

        # Select criterion based on split_criterion parameter
        if (self$split_criterion == "eigenvalue2") {
          criterion <- if (!is.na(eigenvalue_2)) eigenvalue_2 else -Inf
        } else if (self$split_criterion == "homogeneity") {
          if (!is.na(eigenvalue_1) && !is.na(eigenvalue_2)) {
            homogeneity <- eigenvalue_1 / (eigenvalue_1 + eigenvalue_2)
            criterion <- 1 - homogeneity
          } else {
            criterion <- -Inf
          }
        }

        if (criterion > max_criterion) {
          max_criterion <- criterion
          cluster_to_split <- k
          max_eigenvalue_1 <- eigenvalue_1
          max_eigenvalue_2 <- eigenvalue_2
        }
      }

      return(list(
        cluster_id = cluster_to_split,
        eigenvalue_1 = max_eigenvalue_1,
        eigenvalue_2 = max_eigenvalue_2
      ))
    },

    # Compute eigenvalues for a cluster (hybrid: fast path or PCAmix)
    # @param vars_in_cluster Indices of variables in the cluster
    # @return Vector of eigenvalues
    compute_cluster_eigenvalues = function(vars_in_cluster) {
      if (self$use_fast_path) {
        # --- FAST PATH: Slice pre-computed correlation matrix ---
        cor_sub <- self$global_cor[vars_in_cluster, vars_in_cluster, drop = FALSE]
        eigen_result <- eigen(cor_sub, symmetric = TRUE, only.values = TRUE)
        return(eigen_result$values)
      } else {
        # --- MIXED PATH: Use PCAmix ---
        sub_data <- self$data[, vars_in_cluster, drop = FALSE]

        # Split into numeric and categorical
        split_res <- PCAmixdata::splitmix(sub_data)

        X_quanti <- if (!is.null(split_res$X.quanti) && ncol(split_res$X.quanti) > 0) {
          split_res$X.quanti
        } else {
          NULL
        }

        X_quali <- if (!is.null(split_res$X.quali) && ncol(split_res$X.quali) > 0) {
          split_res$X.quali
        } else {
          NULL
        }

        # Need at least some data
        if (is.null(X_quanti) && is.null(X_quali)) {
          return(c(1, 0))
        }

        # Run PCAmix
        ndim <- min(2, length(vars_in_cluster))
        pca_res <- PCAmixdata::PCAmix(
          X.quanti = X_quanti,
          X.quali = X_quali,
          ndim = ndim,
          graph = FALSE
        )

        return(pca_res$eig[, 1])
      }
    },

    # Split a cluster into two groups using PCA + Varimax rotation (hybrid: fast or PCAmix)
    # @param current_clusters Current cluster assignments
    # @param cluster_id Cluster to split
    # @param current_k Current number of clusters
    # @return List with new_clusters, eigenvalue_1, eigenvalue_2, split_quality, and group variables
    split_cluster = function(current_clusters, cluster_id, current_k) {
      vars_in_cluster <- which(current_clusters == cluster_id)
      var_names <- names(current_clusters)[vars_in_cluster]

      if (self$use_fast_path) {
        # === FAST PATH: eigen() on correlation matrix ===
        cor_sub <- self$global_cor[vars_in_cluster, vars_in_cluster, drop = FALSE]

        eigen_result <- eigen(cor_sub, symmetric = TRUE)
        eigenvalues <- eigen_result$values
        eigenvectors <- eigen_result$vectors

        # Keep first 2 components
        n_comp <- min(2, length(eigenvalues))
        loadings <- eigenvectors[, 1:n_comp, drop = FALSE]

        # Apply rotation
        if (n_comp == 2 && self$rotation_method != "none") {
          if (self$rotation_method == "varimax") {
            rotation_result <- stats::varimax(loadings, normalize = TRUE)
            rotated_loadings <- rotation_result$loadings[, , drop = FALSE]
          } else if (self$rotation_method == "promax") {
            rotation_result <- stats::promax(loadings, m = self$promax_m)
            rotated_loadings <- rotation_result$loadings[, , drop = FALSE]
          }
        } else {
          rotated_loadings <- loadings
        }

        # Assign each variable to the component with highest squared loading
        if (n_comp == 2) {
          squared_loadings <- rotated_loadings^2
          assignments <- apply(squared_loadings, 1, which.max)
        } else {
          med <- median(rotated_loadings[, 1])
          assignments <- ifelse(rotated_loadings[, 1] >= med, 1, 2)
        }

        # Calculate split quality (only for fast path where we have cor_sub)
        split_quality <- private$calculate_split_quality(cor_sub, assignments)
      } else {
        # === MIXED PATH: PCAmix for mixed data ===
        sub_data <- self$data[, vars_in_cluster, drop = FALSE]

        # Split into numeric and categorical
        split_res <- PCAmixdata::splitmix(sub_data)

        X_quanti <- if (!is.null(split_res$X.quanti) && ncol(split_res$X.quanti) > 0) {
          split_res$X.quanti
        } else {
          NULL
        }

        X_quali <- if (!is.null(split_res$X.quali) && ncol(split_res$X.quali) > 0) {
          split_res$X.quali
        } else {
          NULL
        }

        # Run PCAmix with 2 components
        n_comp <- min(2, length(vars_in_cluster))
        pca_res <- PCAmixdata::PCAmix(
          X.quanti = X_quanti,
          X.quali = X_quali,
          ndim = n_comp,
          graph = FALSE
        )

        eigenvalues <- pca_res$eig[, 1]

        # Get squared loadings from PCAmix
        # For numeric vars: use coord.quanti
        # For categorical vars: use coord.quali (average over levels per variable)
        squared_loadings_list <- list()

        if (!is.null(X_quanti)) {
          sq_quanti <- pca_res$sqload[rownames(pca_res$sqload) %in% colnames(X_quanti), , drop = FALSE]
          squared_loadings_list <- c(squared_loadings_list, list(sq_quanti))
        }

        if (!is.null(X_quali)) {
          sq_quali <- pca_res$sqload[rownames(pca_res$sqload) %in% colnames(X_quali), , drop = FALSE]
          squared_loadings_list <- c(squared_loadings_list, list(sq_quali))
        }

        # Combine squared loadings in original variable order
        if (length(squared_loadings_list) > 0) {
          all_sqload <- do.call(rbind, squared_loadings_list)
          # Reorder to match vars_in_cluster order
          sqload_ordered <- all_sqload[match(colnames(sub_data), rownames(all_sqload)), , drop = FALSE]

          # Assign based on highest squared loading
          if (ncol(sqload_ordered) >= 2) {
            assignments <- apply(sqload_ordered[, 1:2, drop = FALSE], 1, which.max)
          } else {
            med <- median(sqload_ordered[, 1])
            assignments <- ifelse(sqload_ordered[, 1] >= med, 1, 2)
          }
        } else {
          # Fallback: random split
          assignments <- rep(1:2, length.out = length(vars_in_cluster))
        }

        # For mixed path, use a simplified quality metric
        split_quality <- 0 # No correlation-based quality for mixed data
      }

      # Create new cluster assignments
      new_clusters <- current_clusters

      # Variables assigned to group 1 keep current cluster_id
      # Variables assigned to group 2 get new cluster_id (current_k + 1)
      group1_indices <- vars_in_cluster[assignments == 1]
      group2_indices <- vars_in_cluster[assignments == 2]

      new_clusters[group2_indices] <- current_k + 1

      return(list(
        new_clusters = new_clusters,
        eigenvalue_1 = if (length(eigenvalues) >= 1) eigenvalues[1] else NA,
        eigenvalue_2 = if (length(eigenvalues) >= 2) eigenvalues[2] else NA,
        group1_vars = names(current_clusters)[group1_indices],
        group2_vars = names(current_clusters)[group2_indices],
        split_quality = split_quality
      ))
    },

    # Calculate split quality metric (silhouette-like measure)
    # @param cor_sub Correlation submatrix of cluster being split
    # @param assignments Vector of group assignments (1 or 2)
    # @return Quality score in [-1, 1], higher is better
    calculate_split_quality = function(cor_sub, assignments) {
      n <- nrow(cor_sub)

      if (n < 2) {
        return(0)
      }

      within_cor <- 0
      between_cor <- 0
      n_within <- 0
      n_between <- 0

      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          if (assignments[i] == assignments[j]) {
            within_cor <- within_cor + abs(cor_sub[i, j])
            n_within <- n_within + 1
          } else {
            between_cor <- between_cor + abs(cor_sub[i, j])
            n_between <- n_between + 1
          }
        }
      }

      avg_within <- if (n_within > 0) within_cor / n_within else 0
      avg_between <- if (n_between > 0) between_cor / n_between else 0

      # Quality = (within - between) / (within + between)
      # Range: [-1, 1], higher is better (high intra-group, low inter-group correlation)
      if (avg_within + avg_between > 0) {
        quality <- (avg_within - avg_between) / (avg_within + avg_between)
      } else {
        quality <- 0
      }

      return(quality)
    },

    # Compute cluster centers (1st PC of each cluster) - hybrid: fast or PCAmix
    compute_cluster_centers = function() {
      # Prepare data using stored standardization params from fit()
      data_to_use <- if (self$standardize && self$use_fast_path && !is.null(private$centers)) {
        apply_standardization(self$data, private$centers, private$scales)
      } else {
        self$data
      }

      for (k in seq_len(self$n_clusters)) {
        vars_in_cluster <- which(self$clusters == k)

        if (length(vars_in_cluster) == 0) next

        if (length(vars_in_cluster) == 1) {
          # Single variable: center is the variable itself (numeric) or factor codes (categorical)
          var_data <- data_to_use[, vars_in_cluster]
          if (is.numeric(var_data)) {
            self$cluster_centers[[k]] <- var_data
          } else {
            self$cluster_centers[[k]] <- as.numeric(factor(var_data))
          }
          self$cluster_eigenvalues[[k]] <- c(1.0)
        } else if (self$use_fast_path) {
          # === FAST PATH: eigen() on correlation submatrix ===
          cor_sub <- self$global_cor[vars_in_cluster, vars_in_cluster, drop = FALSE]
          eigen_result <- eigen(cor_sub, symmetric = TRUE)

          self$cluster_eigenvalues[[k]] <- eigen_result$values

          # Compute 1st PC scores
          data_sub <- data_to_use[, vars_in_cluster, drop = FALSE]
          first_pc_loadings <- eigen_result$vectors[, 1]

          pc_scores <- as.matrix(data_sub) %*% first_pc_loadings
          self$cluster_centers[[k]] <- as.vector(pc_scores)

          # Store PCA-like object for compatibility
          self$cluster_pca[[k]] <- list(
            sdev = sqrt(eigen_result$values),
            rotation = eigen_result$vectors,
            center = colMeans(data_sub),
            scale = apply(data_sub, 2, sd)
          )
        } else {
          # === MIXED PATH: PCAmix for mixed data ===
          sub_data <- self$data[, vars_in_cluster, drop = FALSE]

          split_res <- PCAmixdata::splitmix(sub_data)

          X_quanti <- if (!is.null(split_res$X.quanti) && ncol(split_res$X.quanti) > 0) {
            split_res$X.quanti
          } else {
            NULL
          }

          X_quali <- if (!is.null(split_res$X.quali) && ncol(split_res$X.quali) > 0) {
            split_res$X.quali
          } else {
            NULL
          }

          # Run PCAmix
          pca_res <- PCAmixdata::PCAmix(
            X.quanti = X_quanti,
            X.quali = X_quali,
            ndim = min(5, length(vars_in_cluster)),
            graph = FALSE
          )

          self$cluster_eigenvalues[[k]] <- pca_res$eig[, 1]

          # 1st PC scores from PCAmix
          self$cluster_centers[[k]] <- pca_res$ind$coord[, 1]

          # Store PCAmix object for compatibility
          self$cluster_pca[[k]] <- pca_res
        }
      }
    },

    # Compute homogeneity for each cluster (proportion of variance explained by PC1)
    compute_cluster_homogeneity = function() {
      for (k in seq_len(self$n_clusters)) {
        vars_in_cluster <- which(self$clusters == k)

        if (length(vars_in_cluster) == 0) {
          self$cluster_homogeneity[k] <- 0
        } else if (length(vars_in_cluster) == 1) {
          self$cluster_homogeneity[k] <- 1.0
        } else {
          eigenvalues <- self$cluster_eigenvalues[[k]]
          # Homogeneity = lambda1 / sum(lambda)
          self$cluster_homogeneity[k] <- eigenvalues[1] / sum(eigenvalues)
        }
      }
    }
  )
)
