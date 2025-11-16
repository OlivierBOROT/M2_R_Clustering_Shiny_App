#' @title Variable Clustering Based on Within-Cluster Homogeneity
#' @description R6 class implementing variable clustering using a ClustOfVar-like approach.
#' Variables are clustered to maximize within-cluster homogeneity, measured by the proportion
#' of variance explained by the first principal component of each cluster.
#' @export
KMeansClusterer <-  R6::R6Class("KMeansClusterer",
  inherit = BaseClusterer,
  
  private = list(
    cluster_pca = NULL,     # List of PCA objects for each cluster
    cluster_centers = NULL, # List of PC1 scores (centers) for each cluster
    cluster_homogeneity = NULL,  # Homogeneity score for each cluster
    global_homogeneity = NULL,   # Overall weighted homogeneity score
    pca_global = NULL,      # Global PCA for visualization
    coords_fit = NULL,      # Coordinates for plot_fit
    coords_predict = NULL,  # Coordinates for plot_predict
    cluster_colors = NULL,  # Fixed colors between fit and predict
    n_iterations = NULL,    # Actual number of iterations (per run)
    actual_runs = NULL,     # Actual number of initialization runs performed
    seed = NULL,            # Seed for reproducibility
    init_method = "random", # Initialization method
    n_init = 10,            # Maximum number of different initializations
    best_homogeneity = -Inf, # Best homogeneity achieved
    
    # Validate data for KMeansClusterer (requires numeric variables)
    # Overrides parent's validate_data to add numeric check
    validate_data = function() {
      # Call parent validation first
      super$validate_data()
      
      # Check all columns are numeric (specific to KMeansClusterer)
      if (!all(sapply(self$data, is.numeric))) {
        stop("All variables in data must be numeric")
      }
    },
    
    # Initialize clusters using hierarchical clustering on correlation matrix
    # Returns vector of cluster assignments
    initialize_correlation_based = function() {
      # Correlation matrix between variables
      cor_matrix <- cor(self$data)
      
      # Convert to distance: sqrt(2 * (1 - correlation))
      # This is a proper distance metric for correlations
      dist_matrix <- sqrt(2 * (1 - cor_matrix))
      dist_obj <- as.dist(dist_matrix)
      
      # Hierarchical clustering
      hc <- hclust(dist_obj, method = "ward.D2")
      
      # Cut tree to get K clusters
      clusters <- cutree(hc, k = self$n_clusters)
      
      return(clusters)
    },
    
    # Initialize clusters using homogeneity++ (similar to K-means++)
    # Selects initial cluster seeds to maximize diversity in homogeneity space
    # Returns vector of cluster assignments
    initialize_homogeneitypp = function() {
      n_vars <- ncol(self$data)
      clusters <- integer(n_vars)
      
      # Start with correlation-based initialization to get seed variables
      cor_matrix <- cor(self$data)
      dist_matrix <- sqrt(2 * (1 - cor_matrix))
      
      # Select first seed variable randomly
      seed_vars <- integer(self$n_clusters)
      seed_vars[1] <- sample(1:n_vars, 1)
      clusters[seed_vars[1]] <- 1
      
      # Select remaining seed variables (one per cluster)
      # Choose variables that are least correlated with already selected seeds
      for (k in 2:self$n_clusters) {
        # For unassigned variables, find min correlation to any seed
        unassigned <- which(clusters == 0)
        if (length(unassigned) == 0) break
        
        max_min_dist <- -Inf
        best_var <- unassigned[1]
        
        for (var in unassigned) {
          # Min correlation to any existing seed
          min_cor <- min(abs(cor_matrix[var, seed_vars[1:(k - 1)]]))
          # We want variables with low correlation (high distance)
          if ((1 - min_cor) > max_min_dist) {
            max_min_dist <- 1 - min_cor
            best_var <- var
          }
        }
        
        seed_vars[k] <- best_var
        clusters[best_var] <- k
      }
      
      # Assign remaining variables to cluster that maximizes homogeneity
      for (var in 1:n_vars) {
        if (clusters[var] == 0) {
          best_cluster <- 1
          best_homogeneity <- -Inf
          
          for (k in 1:self$n_clusters) {
            # Try adding variable to cluster k
            temp_clusters <- clusters
            temp_clusters[var] <- k
            
            # Calculate homogeneity for this cluster
            vars_in_k <- which(temp_clusters == k)
            if (length(vars_in_k) > 0) {
              data_k <- self$data[, vars_in_k, drop = FALSE]
              homog_k <- private$calculate_cluster_homogeneity(data_k)
              
              if (homog_k > best_homogeneity) {
                best_homogeneity <- homog_k
                best_cluster <- k
              }
            }
          }
          
          clusters[var] <- best_cluster
        }
      }
      
      # Ensure all clusters have at least one variable
      for (k in 1:self$n_clusters) {
        if (sum(clusters == k) == 0) {
          # Find the largest cluster and steal one variable
          cluster_sizes <- table(clusters)
          largest_cluster <- as.integer(names(cluster_sizes)[which.max(cluster_sizes)])
          vars_in_largest <- which(clusters == largest_cluster)
          # Reassign the first variable from largest cluster to empty cluster k
          if (length(vars_in_largest) > 0) {
            clusters[vars_in_largest[1]] <- k
          }
        }
      }
      
      return(clusters)
    },
    
    # Calculate homogeneity of a cluster (proportion of variance explained by PC1)
    # data_cluster: Data frame of variables in the cluster
    # Returns numeric value between 0 and 1
    calculate_cluster_homogeneity = function(data_cluster) {
      if (ncol(data_cluster) < 2) {
        return(1.0)  # Single variable cluster has perfect homogeneity
      }
      
      # PCA on cluster variables (observations in rows, variables in columns)
      pca_cluster <- prcomp(data_cluster, center = TRUE, scale. = self$standardize)
      
      # Variance explained by first component
      variance_explained <- pca_cluster$sdev^2
      total_variance <- sum(variance_explained)
      
      if (total_variance == 0) {
        return(0)
      }
      
      # Homogeneity = proportion of variance explained by PC1
      homogeneity <- variance_explained[1] / total_variance
      
      return(homogeneity)
    },
    
    # Calculate global homogeneity criterion (weighted average)
    # clusters: Vector of cluster assignments
    # Returns numeric value
    calculate_global_homogeneity = function(clusters) {
      total_homogeneity <- 0
      n_vars <- ncol(self$data)
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(clusters == k)
        if (length(vars_in_cluster) > 0) {
          data_cluster <- self$data[, vars_in_cluster, drop = FALSE]
          homogeneity_k <- private$calculate_cluster_homogeneity(data_cluster)
          weight <- length(vars_in_cluster) / n_vars
          total_homogeneity <- total_homogeneity + weight * homogeneity_k
        }
      }
      
      return(total_homogeneity)
    },
    
    # Update cluster centers (PC1 for each cluster)
    # clusters: Current cluster assignments
    # Returns list of PC1 scores for each cluster
    update_cluster_centers = function(clusters) {
      centers <- list()
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(clusters == k)
        
        if (length(vars_in_cluster) == 0) {
          # Empty cluster - assign NULL (will be handled in reassignment)
          centers[[k]] <- NULL
        } else if (length(vars_in_cluster) == 1) {
          # Single variable - PC1 is the variable itself (standardized)
          if (self$standardize) {
            centers[[k]] <- scale(self$data[, vars_in_cluster, drop = TRUE])
          } else {
            centers[[k]] <- scale(self$data[, vars_in_cluster, drop = TRUE], center = TRUE, scale = FALSE)
          }
        } else {
          # Multiple variables - compute PCA and extract PC1 scores
          data_cluster <- self$data[, vars_in_cluster, drop = FALSE]
          pca_cluster <- prcomp(data_cluster, center = TRUE, scale. = self$standardize)
          centers[[k]] <- pca_cluster$x[, 1]  # PC1 scores
        }
      }
      
      return(centers)
    },
    
    # Reassign variables to clusters based on correlation with centers (PC1)
    # This is the K-means-like reassignment step
    # clusters: Current cluster assignments
    # centers: List of PC1 scores for each cluster
    # Returns new cluster assignments
    reassign_variables = function(clusters, centers) {
      n_vars <- ncol(self$data)
      new_clusters <- integer(n_vars)
      
      # For each variable, assign to cluster with highest cor(var, PC1)^2
      for (var_idx in 1:n_vars) {
        var_data <- self$data[, var_idx]
        
        best_cluster <- 1
        best_cor_squared <- -Inf
        
        # Try each cluster
        for (k in 1:self$n_clusters) {
          if (is.null(centers[[k]])) {
            # Empty cluster - skip or use low correlation
            cor_squared <- -1
          } else {
            # Calculate correlation with PC1 of cluster k
            cor_val <- cor(var_data, centers[[k]])
            cor_squared <- cor_val^2  # Squared correlation
            
            if (cor_squared > best_cor_squared) {
              best_cor_squared <- cor_squared
              best_cluster <- k
            }
          }
        }
        
        new_clusters[var_idx] <- best_cluster
      }
      
      return(new_clusters)
    },
    
    # Run clustering optimization once with K-means-like algorithm
    # Alternates between: 1) Update centers (PC1), 2) Reassign variables
    # Returns list with clusters, iterations, and homogeneity
    run_clustering_once = function() {
      # Use initialization method
      if (private$init_method == "homogeneity++") {
        clusters <- private$initialize_homogeneitypp()
      } else if (private$init_method == "correlation") {
        clusters <- private$initialize_correlation_based()
      } else {
        # Random initialization (fallback)
        clusters <- sample(1:self$n_clusters, ncol(self$data), replace = TRUE)
        
        # Ensure each cluster has at least one variable
        for (k in 1:self$n_clusters) {
          if (sum(clusters == k) == 0) {
            clusters[k] <- k
          }
        }
      }
      
      old_homogeneity <- -Inf
      converged <- FALSE
      
      for (i in 1:self$max_iter) {
        # STEP 1: Update cluster centers (compute PC1 for each cluster)
        centers <- private$update_cluster_centers(clusters)
        
        # STEP 2: Reassign variables based on correlation with centers
        new_clusters <- private$reassign_variables(clusters, centers)
        
        # Calculate current homogeneity
        current_homogeneity <- private$calculate_global_homogeneity(new_clusters)
        
        # Check convergence based on homogeneity change
        if (abs(current_homogeneity - old_homogeneity) < self$tol) {
          clusters <- new_clusters
          converged <- TRUE
          break
        }
        
        # Also check if partition is stable (no variable changed cluster)
        if (all(clusters == new_clusters)) {
          converged <- TRUE
          break
        }
        
        old_homogeneity <- current_homogeneity
        clusters <- new_clusters
      }
      
      # Final homogeneity calculation
      final_homogeneity <- private$calculate_global_homogeneity(clusters)
      
      list(
        clusters = clusters,
        iterations = i,
        homogeneity = final_homogeneity,
        converged = converged
      )
    }
  ),
  
  public = list(
    #' @description
    #' Create a new KMeansClusterer object.
    #' @param data The dataset to be clustered (variables in columns, observations in rows).
    #' @param n_clusters The number of clusters to form.
    #' @param standardize Whether to standardize the data.
    #' @param max_iter Maximum number of iterations.
    #' @param tol Tolerance for convergence.
    #' @param seed Random seed for reproducibility.
    #' @param n_init Maximum number of initializations with early stopping heuristic.
    #'   Stops early if the same best score is found twice (likely global optimum).
    #'   Ignored if init_method = "correlation" (deterministic).
    #' @param init_method Initialization method: "homogeneity++" (smart), "correlation" (hierarchical), or "random".
    #' @return A new `KMeansClusterer` object.
    initialize = function(data, n_clusters = 2, standardize = TRUE, 
                          max_iter = 100, tol = 1e-4, seed = NULL, n_init = 10,
                          init_method = "homogeneity++") {
      super$initialize(data, n_clusters, standardize, max_iter, tol)
      
      private$seed <- seed
      
      if (!is.numeric(n_init) || n_init < 1) {
        stop("n_init must be a positive integer")
      }
      private$n_init <- as.integer(n_init)
      
      # Validate initialization method
      valid_methods <- c("homogeneity++", "correlation", "random")
      if (!init_method %in% valid_methods) {
        stop(sprintf("init_method must be one of: %s", paste(valid_methods, collapse = ", ")))
      }
      private$init_method <- init_method
      
      # For correlation method, n_init = 1 (deterministic)
      if (init_method == "correlation") {
        private$n_init <- 1
      }
    },
    
    #' @description
    #' Fit the clustering algorithm to maximize within-cluster homogeneity.
    #' Uses early stopping heuristic: stops if the same best score is found twice.
    #' @return The fitted clustering model (invisibly returns self).
    fit = function() {
      # Set seed for reproducibility
      if (!is.null(private$seed)) {
        set.seed(private$seed)
      }
      
      # Run clustering multiple times with different initializations
      # n_init is now the MAXIMUM number of attempts
      best_result <- NULL
      best_homogeneity <- -Inf
      actual_runs <- 0
      
      for (init_run in 1:private$n_init) {
        result <- private$run_clustering_once()
        actual_runs <- init_run
        
        # Case 1: First run OR found a NEW better score
        if (is.null(best_result) || result$homogeneity > best_homogeneity) {
          best_homogeneity <- result$homogeneity
          best_result <- result
          
        # Case 2: Found EQUIVALENT score to the best (within tolerance)
        # Early stopping: we've found the same optimum twice!
        } else if (abs(result$homogeneity - best_homogeneity) < self$tol) {
          # Same optimum found again - very likely to be global optimum
          break
        }
        
        # Case 3: Score is worse - continue searching
      }
      
      # Store best result
      self$clusters <- best_result$clusters
      private$n_iterations <- best_result$iterations
      private$actual_runs <- actual_runs
      private$best_homogeneity <- best_homogeneity
      
      # Calculate PCA for each cluster and store centers (PC1)
      private$cluster_pca <- list()
      private$cluster_centers <- list()
      private$cluster_homogeneity <- numeric(self$n_clusters)
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(self$clusters == k)
        if (length(vars_in_cluster) > 0) {
          data_cluster <- self$data[, vars_in_cluster, drop = FALSE]
          
          if (ncol(data_cluster) >= 2) {
            pca_cluster <- prcomp(data_cluster, center = TRUE, scale. = self$standardize)
            private$cluster_pca[[k]] <- pca_cluster
            private$cluster_centers[[k]] <- pca_cluster$x[, 1]  # Store PC1 scores as center
            
            variance_explained <- pca_cluster$sdev^2
            private$cluster_homogeneity[k] <- variance_explained[1] / sum(variance_explained)
          } else {
            # Single variable cluster
            private$cluster_pca[[k]] <- NULL
            if (self$standardize) {
              private$cluster_centers[[k]] <- scale(self$data[, vars_in_cluster, drop = TRUE])
            } else {
              private$cluster_centers[[k]] <- scale(self$data[, vars_in_cluster, drop = TRUE],
                                                   center = TRUE, scale = FALSE)
            }
            private$cluster_homogeneity[k] <- 1.0
          }
        }
      }
      
      # Global PCA for visualization
      private$pca_global <- prcomp(self$data, center = TRUE, scale. = self$standardize)
      
      # Store coordinates for plotting (using global PCA)
      # Handle case where PCA has <2 components (happens with 2 observations)
      n_comp <- ncol(private$pca_global$rotation)
      if (n_comp == 0) {
        # No components - create empty coords with all necessary columns
        coords <- data.frame(
          PC1 = numeric(0), 
          PC2 = numeric(0),
          cluster = factor(),
          variable = character(),
          stringsAsFactors = FALSE
        )
      } else {
        n_use <- min(2, n_comp)
        coords <- as.data.frame(private$pca_global$rotation[, seq_len(n_use), drop = FALSE])
        # Pad with zeros if only 1 component
        if (n_use == 1) {
          coords$PC2 <- 0
        }
        colnames(coords)[1:n_use] <- paste0("PC", 1:n_use)
        # Add cluster and variable columns when coords is not empty
        coords$cluster <- factor(self$clusters)
        coords$variable <- rownames(private$pca_global$rotation)
      }
      private$coords_fit <- coords
      
      # Fixed colors
      private$cluster_colors <- rainbow(self$n_clusters)
      
      private$global_homogeneity <- best_homogeneity
      self$fitted <- TRUE
      
      invisible(self)
    },
    
    #' @description
    #' Predict cluster labels for new variables (supplementary variables).
    #' Uses learned cluster centers (PC1) to assign new variables based on correlation.
    #' @param new_data New data with same observations as training data (variables in columns).
    #' @param return_scores Whether to return squared correlation scores.
    #' @return Data frame with cluster assignments (and scores if requested).
    predict = function(new_data, return_scores = TRUE) {
      private$check_fitted()
      
      # Validation
      if (!is.data.frame(new_data)) {
        stop("new_data must be a data.frame")
      }
      if (!all(sapply(new_data, is.numeric))) {
        stop("All variables must be numeric")
      }
      if (nrow(new_data) != nrow(self$data)) {
        stop("new_data must have the same number of observations as training data")
      }
      
      n_new_vars <- ncol(new_data)
      pred_clusters <- integer(n_new_vars)
      pred_scores_matrix <- matrix(0, nrow = n_new_vars, ncol = self$n_clusters)
      
      # For each new variable, assign to cluster with highest cor(var, center)^2
      # This is the SAME logic as reassign_variables() but for new variables
      for (var_idx in 1:n_new_vars) {
        var_data <- new_data[, var_idx]
        
        best_cluster <- 1
        best_cor_squared <- -Inf
        
        # Calculate correlation with each cluster center (PC1)
        for (k in 1:self$n_clusters) {
          if (is.null(private$cluster_centers[[k]])) {
            cor_squared <- -1
          } else {
            # Correlation with PC1 of cluster k
            cor_val <- cor(var_data, private$cluster_centers[[k]])
            cor_squared <- cor_val^2  # Squared correlation
            
            pred_scores_matrix[var_idx, k] <- cor_squared
            
            if (cor_squared > best_cor_squared) {
              best_cor_squared <- cor_squared
              best_cluster <- k
            }
          }
        }
        
        pred_clusters[var_idx] <- best_cluster
      }
      
      # Store coordinates for plot_predict
      n_comp <- ncol(private$pca_global$rotation)
      if (n_comp == 0) {
        # No PCA components - create empty coords
        coords_active <- data.frame(PC1 = numeric(0), PC2 = numeric(0))
      } else {
        n_use <- min(2, n_comp)
        coords_active <- as.data.frame(private$pca_global$rotation[, seq_len(n_use), drop = FALSE])
        
        # Ensure we have PC1 and PC2 columns (pad with 0 if needed)
        if (n_use == 1) {
          coords_active$PC2 <- 0
          colnames(coords_active)[1] <- "PC1"
        } else {
          colnames(coords_active) <- c("PC1", "PC2")
        }
      }
      
      coords_active$cluster <- self$clusters
      coords_active$type <- "active"
      coords_active$variable <- rownames(private$pca_global$rotation)
      
      # Project new variables (supplementary/illustrative) onto global PCA space
      # Note: new_data has variables in columns, observations in rows (same as training)
      # For visualization: we calculate loadings as correlation between new variables and PCA axes
      # Each new variable must be standardized (with its own parameters) to compute meaningful correlations
      if (self$standardize) {
        new_data_scaled <- scale(new_data, center = TRUE, scale = TRUE)
      } else {
        new_data_scaled <- scale(new_data, center = TRUE, scale = FALSE)
      }
      
      # Calculate loadings for supplementary variables as correlation with PCA scores
      # This gives the coordinates (PC1, PC2) for plotting the new variables on the same factorial plane
      n_pca_comp <- ncol(private$pca_global$x)
      if (n_pca_comp == 0) {
        # No PCA scores - create empty loadings
        new_loadings <- matrix(0, nrow = ncol(new_data), ncol = 2)
        colnames(new_loadings) <- c("PC1", "PC2")
      } else {
        n_use <- min(2, n_pca_comp)
        new_loadings <- cor(new_data_scaled, private$pca_global$x[, seq_len(n_use), drop = FALSE])
        
        # Pad with zeros if only 1 component
        if (n_use == 1) {
          new_loadings <- cbind(new_loadings, PC2 = 0)
        }
        colnames(new_loadings) <- c("PC1", "PC2")
      }
      
      coords_illus <- as.data.frame(new_loadings)
      coords_illus$cluster <- pred_clusters
      coords_illus$type <- "illustrative"
      coords_illus$variable <- colnames(new_data)
      
      private$coords_predict <- rbind(coords_active, coords_illus)
      
      # Return
      if (return_scores) {
        # Create result data frame with all scores
        result <- data.frame(
          variable = colnames(new_data),
          cluster = pred_clusters,
          row.names = NULL
        )
        
        # Add score columns for each cluster
        for (k in 1:self$n_clusters) {
          result[[paste0("score_", k)]] <- pred_scores_matrix[, k]
        }
        
        return(result)
      } else {
        return(data.frame(
          variable = colnames(new_data),
          cluster = pred_clusters,
          row.names = NULL
        ))
      }
    },
    
    #' @description Get PCA objects for each cluster.
    #' @return List of PCA objects.
    get_cluster_pca = function() {
      private$check_fitted()
      return(private$cluster_pca)
    },
    
    #' @description Get homogeneity scores for each cluster.
    #' @return Numeric vector of homogeneity scores.
    get_cluster_homogeneity = function() {
      private$check_fitted()
      return(private$cluster_homogeneity)
    },
    
    #' @description Get global homogeneity score.
    #' @return Numeric value (weighted average homogeneity).
    get_homogeneity = function() {
      private$check_fitted()
      return(private$global_homogeneity)
    },
    
    #' @description Get best homogeneity achieved (alias for get_homogeneity).
    #' @return Numeric value.
    get_inertia = function() {
      # For compatibility with K selection methods that expect get_inertia()
      # We return -homogeneity so that minimization still works
      private$check_fitted()
      return(-private$best_homogeneity)
    },
    
    #' @description Get number of iterations until convergence.
    #' @return Integer.
    get_iterations = function() {
      private$check_fitted()
      return(private$n_iterations)
    },
    
    #' @description Get maximum number of initializations configured.
    #' @return Integer.
    get_n_init = function() {
      return(private$n_init)
    },
    
    #' @description Get actual number of initialization runs performed (with early stopping).
    #' @return Integer, or NULL if not fitted yet.
    get_actual_runs = function() {
      return(private$actual_runs)
    },
    
    #' @description Get initialization method used.
    #' @return Character string.
    get_init_method = function() {
      return(private$init_method)
    },
    
    #' @description Get global PCA object (for visualization).
    #' @return PCA object from prcomp.
    get_pca = function() {
      private$check_fitted()
      return(private$pca_global)
    },
    
    #' @description Get PCA loadings for visualization (global PCA).
    #' @return Data frame of loadings.
    get_loadings = function() {
      private$check_fitted()
      return(as.data.frame(private$pca_global$rotation))
    },
    
    #' @description Get cluster centers in loadings space (for K selection methods).
    #' @return Matrix of cluster centers (k rows x 2 columns for PC1 and PC2).
    get_centers = function() {
      private$check_fitted()
      
      # Calculate mean loadings for each cluster
      # Handle case where PCA might have fewer than 2 components
      n_components <- ncol(private$pca_global$rotation)
      
      if (n_components == 0) {
        # No PCA components available - return zero matrix
        return(matrix(0, nrow = self$n_clusters, ncol = 2))
      }
      
      # Take at most 2 components
      n_use <- min(2, n_components)
      loadings <- private$pca_global$rotation[, seq_len(n_use), drop = FALSE]
      centers <- matrix(0, nrow = self$n_clusters, ncol = n_use)
      
      for (k in 1:self$n_clusters) {
        cluster_vars <- which(self$clusters == k)
        if (length(cluster_vars) > 0) {
          centers[k, ] <- colMeans(loadings[cluster_vars, , drop = FALSE])
        }
      }
      
      # Ensure we always return 2 columns (pad with 0 if needed)
      if (n_use < 2) {
        centers <- cbind(centers, matrix(0, nrow = self$n_clusters, ncol = 2 - n_use))
      }
      
      return(centers)
    },
    
    #' @description Get plot data for fitted model (internal use by visualization functions).
    #' @return List with coords, colors, pca.
    get_plot_data = function() {
      if (!self$fitted) return(NULL)
      
      list(
        coords = private$coords_fit,
        colors = private$cluster_colors,
        pca = private$pca_global,
        centers = NULL  # No centers in homogeneity-based approach
      )
    },
    
    #' @description Get plot data for predictions (internal use by visualization functions).
    #' @return List with coords, colors, pca.
    get_plot_data_predict = function() {
      if (is.null(private$coords_predict)) return(NULL)
      
      list(
        coords = private$coords_predict,
        colors = private$cluster_colors,
        pca = private$pca_global,
        centers = NULL  # No centers in homogeneity-based approach
      )
    },
    
    #' @description Plot fitted clustering results in 2D loadings space.
    #' @param main Plot title.
    #' @param show_centers Logical, show cluster centers.
    #' @param show_labels Logical, show variable labels.
    #' @return Self (invisibly).
    plot_fit = function(main = NULL, show_centers = TRUE, show_labels = TRUE) {
      plot_clustering_2d(self, main, show_centers, show_labels)
      invisible(self)
    },
    
    #' @description Plot predictions with supplementary variables.
    #' @param main Plot title.
    #' @param show_centers Logical, show cluster centers.
    #' @param show_labels Logical, show variable labels.
    #' @return Self (invisibly).
    plot_predict = function(main = NULL, show_centers = TRUE, show_labels = TRUE) {
      plot_clustering_with_supp(self, main, show_centers, show_labels)
      invisible(self)
    },
    
    #' @description Display model information.
    #' @return Self (invisibly).
    print = function() {
      if (!self$fitted) {
        cat("Variable Clusterer (not fitted)\n")
        cat(sprintf("  Number of clusters: %d\n", self$n_clusters))
        cat(sprintf("  Number of variables: %d\n", ncol(self$data)))
        cat(sprintf("  Standardize: %s\n", ifelse(self$standardize, "YES", "NO")))
        cat(sprintf("  n_init: %d\n", private$n_init))
        return(invisible(self))
      }
      
      cat("Variable Clusterer (ClustOfVar-like approach)\n")
      cat(sprintf("  %d clusters (sizes: %s)\n",
                  self$n_clusters,
                  paste(table(self$clusters), collapse = ", ")))
      
      # Show early stopping info
      if (private$actual_runs < private$n_init) {
        cat(sprintf("  Convergence after %d iterations (init: %s, runs=%d/%d - early stop)\n", 
                    private$n_iterations, private$init_method, 
                    private$actual_runs, private$n_init))
      } else {
        cat(sprintf("  Convergence after %d iterations (init: %s, runs=%d)\n", 
                    private$n_iterations, private$init_method, private$actual_runs))
      }
      
      cat(sprintf("  Global homogeneity: %.3f\n\n", private$global_homogeneity))
      
      cat("Variable assignments:\n")
      assignation <- data.frame(
        variable = colnames(self$data),
        cluster = unname(self$clusters)
      )
      print(assignation, row.names = FALSE)
      
      invisible(self)
    },
    
    #' @description Detailed summary of the clustering model.
    #' @return Self (invisibly).
    summary = function() {
      if (!self$fitted) {
        cat("Model not fitted yet.\n")
        return(invisible(self))
      }
      
      cat("=================================================\n")
      cat("Variable Clustering - ClustOfVar-like Approach\n")
      cat("=================================================\n\n")
      
      cat("Configuration:\n")
      cat(sprintf("  - Number of clusters: %d\n", self$n_clusters))
      cat(sprintf("  - Number of variables: %d\n", ncol(self$data)))
      cat(sprintf("  - Number of observations: %d\n", nrow(self$data)))
      cat(sprintf("  - Standardize: %s\n", ifelse(self$standardize, "YES", "NO")))
      cat(sprintf("  - Initialization method: %s\n", private$init_method))
      
      # Show early stopping info
      if (private$actual_runs < private$n_init) {
        cat(sprintf("  - Initialization runs: %d / %d (early stopped)\n", 
                    private$actual_runs, private$n_init))
      } else {
        cat(sprintf("  - Initialization runs: %d\n", private$actual_runs))
      }
      
      cat(sprintf("  - Iterations (best run): %d / %d\n", private$n_iterations, self$max_iter))
      cat(sprintf("  - Tolerance: %.2e\n\n", self$tol))
      
      cat("Clustering Quality (Homogeneity Criterion):\n")
      cat(sprintf("  - Global homogeneity: %.3f\n", private$global_homogeneity))
      cat("  - Interpretation: Proportion of variance explained by PC1 within clusters\n")
      cat("  - Range: [0, 1] where 1 = perfect homogeneity\n\n")
      
      cat("Cluster Sizes:\n")
      cluster_sizes <- as.numeric(table(self$clusters))
      for (i in 1:self$n_clusters) {
        cat(sprintf("  Cluster %d: %d variables\n", i, cluster_sizes[i]))
      }
      cat("\n")
      
      cat("Within-Cluster Homogeneity:\n")
      for (j in 1:self$n_clusters) {
        homog <- private$cluster_homogeneity[j]
        n_vars <- sum(self$clusters == j)
        cat(sprintf("  Cluster %d: %.3f (n=%d vars)\n", j, homog, n_vars))
        
        if (!is.null(private$cluster_pca[[j]])) {
          var_exp <- private$cluster_pca[[j]]$sdev^2
          total_var <- sum(var_exp)
          cat(sprintf("    PC1 explains %.1f%% of cluster variance\n", 
                     (var_exp[1] / total_var) * 100))
        }
      }
      
      cat("\n=================================================\n")
      
      invisible(self)
    }
  )
)
