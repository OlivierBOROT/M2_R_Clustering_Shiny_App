#' @title Variable Clustering Based on Within-Cluster Homogeneity (Mixed Data)
#' @description R6 class implementing variable clustering by maximizing
#'   within-cluster homogeneity for quantitative, qualitative, and mixed data.
#'
#' @details
#' This algorithm clusters variables (not observations) into a pre-specified
#' number of clusters. It supports numeric, factor, and mixed data types.
#'
#' The homogeneity of a cluster is measured by the proportion of variance
#' explained by its first latent component. The \code{fit} method
#' iteratively reassigns variables to the cluster with whose center (latent component)
#' it has the highest association (R² for quantitative, η² for qualitative).
#'
#' Multiple initialization strategies are supported: "homogeneity++"
#' (similar to k-means++), "correlation" (hierarchical clustering), or
#' "random". The algorithm performs multiple runs with different
#' initializations and keeps the best solution.
#'
#' @examples
#' \dontrun{
#' # 1. Create sample data (10 variables, 100 observations)
#' set.seed(123)
#' my_data <- as.data.frame(matrix(rnorm(1000), ncol = 10))
#' colnames(my_data) <- paste0("Var", 1:10)
#'
#' # 2. Initialize the clusterer
#' clusterer <- KMeansClusterer$new(
#'   data = my_data,
#'   n_clusters = 3,
#'   standardize = TRUE,
#'   seed = 42
#' )
#'
#' # 3. Fit the model
#' clusterer$fit()
#'
#' # 4. Examine results
#' clusterer$print()
#' clusterer$summary()
#' results <- clusterer$get_results()
#' print(results)
#'
#' # 5. Predict on new variables
#' supp_data <- as.data.frame(matrix(rnorm(200), ncol = 2))
#' colnames(supp_data) <- c("Supp1", "Supp2")
#' predictions <- clusterer$predict(new_data = supp_data)
#' print(predictions)
#'
#' # 6. Visualize
#' plot_clustering_2d(clusterer)
#' }
#'
#' @seealso
#' Base class: \code{\link{BaseClusterer}}
#'
#' K selection methods: \code{\link{elbow_method}},
#'   \code{\link{silhouette_method}}, \code{\link{calinski_harabasz_method}}
#'
#' Visualization: \code{\link{plot_clustering_2d}},
#'   \code{\link{plot_cluster_quality}}
#'
#' @export
KMeansClusterer <-  R6::R6Class("KMeansClusterer",
  inherit = BaseClusterer,
  
  private = list(
    cluster_pca = NULL,     # List of PCA objects for each cluster (LOCAL - required for algorithm)
    cluster_centers = NULL, # List of PC1 scores (centers) for each cluster (LOCAL - required)
    cluster_homogeneity = NULL,  # Homogeneity score for each cluster
    global_homogeneity = NULL,   # Overall weighted homogeneity score
    
    pca_global = NULL,            # Global PCA for visualization (computed on demand)
    pca_global_computed = FALSE,  # Flag: has global PCA been computed?
    coords_fit = NULL,            # Coordinates for plot_fit (computed on demand)
    coords_predict = NULL,        # Coordinates for plot_predict (computed on demand)
    cluster_colors = NULL,        # Fixed colors (computed on demand)
    last_new_data = NULL,         # Cache for last predict() data (enables plot_predict without args)
    
    n_iterations = NULL,    # Actual number of iterations (per run)
    actual_runs = NULL,     # Actual number of initialization runs performed
    seed = NULL,            # Seed for reproducibility
    init_method = "random", # Initialization method
    n_init = 10,            # Maximum number of different initializations
    best_homogeneity = -Inf, # Best homogeneity achieved
    
    # Validate data for KMeansClusterer (accepts numeric and factor variables)
    # Overrides parent's validate_data to accept mixed data
    validate_data = function() {
      # Call parent validation first
      super$validate_data()
      
      # Convert character and integer appropriately
      for (col_name in names(self$data)) {
        col_data <- self$data[[col_name]]
        
        if (is.character(col_data)) {
          self$data[[col_name]] <- as.factor(col_data)
        } else if (is.integer(col_data)) {
          # Keep integers as numeric for continuous treatment
          self$data[[col_name]] <- as.numeric(col_data)
        }
        
        # Check for invalid types (dates, lists, etc.)
        if (!is.numeric(self$data[[col_name]]) && !is.factor(self$data[[col_name]])) {
          stop(sprintf("Variable '%s' has unsupported type '%s'. Only numeric and factor are allowed.",
                      col_name, class(self$data[[col_name]])[1]))
        }
        
        # Check for missing values
        if (anyNA(self$data[[col_name]])) {
          stop(sprintf("Variable '%s' contains missing values. Please impute them before clustering.", col_name))
        }
      }
    },
    
    # Initialize clusters using hierarchical clustering on mixed dissimilarity matrix
    # Returns vector of cluster assignments
    # Uses: Pearson² (quanti-quanti), Cramer's V² (quali-quali), η² (quanti-quali)
    initialize_correlation_based = function() {
      n_vars <- ncol(self$data)
      dist_mat <- matrix(0, n_vars, n_vars)
      
      # Pre-calculate variable types
      is_num <- sapply(self$data, is.numeric)
      
      # Compute pairwise similarities
      for (i in 1:n_vars) {
        for (j in i:n_vars) {
          if (i == j) {
            sim <- 1
          } else {
            v1 <- self$data[, i]
            v2 <- self$data[, j]
            type1 <- is_num[i]
            type2 <- is_num[j]
            
            if (type1 && type2) {
              # Quanti vs Quanti: Pearson correlation squared
              sim <- cor(v1, v2)^2
            } else if (!type1 && !type2) {
              # Quali vs Quali: Cramer's V squared
              suppressWarnings({
                chisq_result <- chisq.test(table(v1, v2))
                chisq_stat <- chisq_result$statistic
              })
              n <- length(v1)
              min_dim <- min(length(levels(v1)), length(levels(v2))) - 1
              if (min_dim > 0) {
                sim <- min(1, chisq_stat / (n * min_dim))  # Cramer's V²
              } else {
                sim <- 0
              }
            } else {
              # Quanti vs Quali: Correlation Ratio (η²)
              if (type1) {
                # v1 numeric, v2 factor
                sim <- summary(lm(v1 ~ v2))$r.squared
              } else {
                # v1 factor, v2 numeric
                sim <- summary(lm(v2 ~ v1))$r.squared
              }
            }
          }
          # Convert similarity to distance: d = sqrt(1 - sim)
          dist_mat[i, j] <- dist_mat[j, i] <- sqrt(1 - sim)
        }
      }
      
      # Hierarchical clustering
      hc <- hclust(as.dist(dist_mat), method = "ward.D2")
      clusters <- cutree(hc, k = self$n_clusters)
      
      return(clusters)
    },
    
    # Initialize clusters using homogeneity++ (similar to K-means++)
    # Selects initial cluster seeds to maximize diversity in homogeneity space
    # Returns vector of cluster assignments
    initialize_homogeneitypp = function() {
      n_vars <- ncol(self$data)
      clusters <- integer(n_vars)
      
      # Build mixed distance matrix (same as correlation init)
      dist_matrix <- matrix(0, n_vars, n_vars)
      is_num <- sapply(self$data, is.numeric)
      
      for (i in 1:n_vars) {
        for (j in i:n_vars) {
          if (i == j) {
            sim <- 1
          } else {
            v1 <- self$data[, i]
            v2 <- self$data[, j]
            type1 <- is_num[i]
            type2 <- is_num[j]
            
            if (type1 && type2) {
              sim <- cor(v1, v2)^2
            } else if (!type1 && !type2) {
              chisq_stat <- suppressWarnings(chisq.test(table(v1, v2))$statistic)
              n <- length(v1)
              min_dim <- min(length(levels(v1)), length(levels(v2))) - 1
              sim <- min(1, chisq_stat / (n * min_dim))
            } else {
              if (type1) {
                sim <- summary(lm(v1 ~ v2))$r.squared
              } else {
                sim <- summary(lm(v2 ~ v1))$r.squared
              }
            }
          }
          
          dist_matrix[i, j] <- dist_matrix[j, i] <- sqrt(1 - sim)
        }
      }
      
      # Select first seed variable randomly
      seed_vars <- integer(self$n_clusters)
      seed_vars[1] <- sample(1:n_vars, 1)
      clusters[seed_vars[1]] <- 1
      
      # Select remaining seed variables (one per cluster)
      # Choose variables that are most distant from already selected seeds
      for (k in 2:self$n_clusters) {
        unassigned <- which(clusters == 0)
        if (length(unassigned) == 0) break
        
        max_min_dist <- -Inf
        best_var <- unassigned[1]
        
        for (var in unassigned) {
          # Min distance to any existing seed
          min_dist <- min(dist_matrix[var, seed_vars[1:(k - 1)]])
          # We want variables with high min distance
          if (min_dist > max_min_dist) {
            max_min_dist <- min_dist
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
    
    # Compute global PCA only if needed (lazy loading)
    # This method is called by visualization functions, not by fit()
    compute_global_pca_if_needed = function() {
      if (private$pca_global_computed) return(invisible(NULL))
      
      # Determine data types
      all_numeric <- all(sapply(self$data, is.numeric))
      all_factor <- all(sapply(self$data, is.factor))
      
      if (all_numeric) {
        # Pure quantitative: use prcomp (faster and simpler, no PCAmixdata needed)
        private$pca_global <- prcomp(self$data, center = TRUE, scale. = self$standardize)
      } else {
        # Mixed or qualitative: use PCAmix
        if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
          stop("PCAmixdata package required for mixed/qualitative data visualization. Install with: install.packages('PCAmixdata')")
        }
        
        split_global <- PCAmixdata::splitmix(self$data)
        
        # Run PCAmix with ndim=2 for 2D visualization
        private$pca_global <- suppressWarnings(PCAmixdata::PCAmix(
          X.quanti = split_global$X.quanti,
          X.quali = split_global$X.quali,
          ndim = 2,
          graph = FALSE,
          rename.level = TRUE
        ))
      }
      
      private$pca_global_computed <- TRUE
      
      # Compute coordinates and colors
      private$compute_coords_fit()
      private$cluster_colors <- rainbow(self$n_clusters)
      
      invisible(NULL)
    },
    
    # Compute coordinates for plot_fit (lazy)
    # Requires pca_global to be computed first
    compute_coords_fit = function() {
      if (!private$pca_global_computed) {
        stop("Global PCA not computed. Call compute_global_pca_if_needed() first.")
      }
      
      all_numeric <- all(sapply(self$data, is.numeric))
      
      if (all_numeric && inherits(private$pca_global, "prcomp")) {
        # Pure quantitative with prcomp: extract loadings as coordinates
        loadings <- private$pca_global$rotation[, 1:2, drop = FALSE]
        coords <- data.frame(
          PC1 = loadings[, 1],
          PC2 = loadings[, 2],
          variable = rownames(loadings),
          stringsAsFactors = FALSE
        )
      } else {
        # Mixed or qualitative with PCAmix
        # Quantitative variables: use correlations with axes
        coords_quant <- if (!is.null(private$pca_global$quanti.cor)) {
          data.frame(
            PC1 = private$pca_global$quanti.cor[, 1],
            PC2 = private$pca_global$quanti.cor[, 2],
            variable = rownames(private$pca_global$quanti.cor),
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
        
        # Qualitative variables: use eta² (correlation ratio with axes)
        coords_qual <- if (!is.null(private$pca_global$quali.eta2)) {
          # Extract eta² square roots
          eta_sqrt_pc1 <- sqrt(private$pca_global$quali.eta2[, 1])
          eta_sqrt_pc2 <- sqrt(private$pca_global$quali.eta2[, 2])
          
          # Get signs from categ.coord if available
          if (!is.null(private$pca_global$categ.coord) && is.data.frame(private$pca_global$categ.coord)) {
            var_names_qual <- rownames(private$pca_global$quali.eta2)
            signs_pc1 <- sign(private$pca_global$categ.coord[match(var_names_qual, private$pca_global$categ.coord$variable), 1])
            signs_pc2 <- sign(private$pca_global$categ.coord[match(var_names_qual, private$pca_global$categ.coord$variable), 2])
            signs_pc1[is.na(signs_pc1)] <- 1
            signs_pc2[is.na(signs_pc2)] <- 1
          } else {
            signs_pc1 <- rep(1, length(eta_sqrt_pc1))
            signs_pc2 <- rep(1, length(eta_sqrt_pc2))
          }
          
          data.frame(
            PC1 = eta_sqrt_pc1 * signs_pc1,
            PC2 = eta_sqrt_pc2 * signs_pc2,
            variable = rownames(private$pca_global$quali.eta2),
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
        
        # Combine coordinates
        coords <- rbind(coords_quant, coords_qual)
      }
      
      if (!is.null(coords) && nrow(coords) > 0) {
        # Add cluster assignments
        var_names <- colnames(self$data)
        coords$cluster <- factor(self$clusters[match(coords$variable, var_names)])
      } else {
        # Fallback: empty coords
        coords <- data.frame(
          PC1 = numeric(0),
          PC2 = numeric(0),
          variable = character(0),
          cluster = factor(),
          stringsAsFactors = FALSE
        )
      }
      
      private$coords_fit <- coords
      invisible(NULL)
    },
    
    # Calculate homogeneity of a cluster (proportion of variance explained by first component)
    # data_cluster: Data frame of variables in the cluster (mixed or pure)
    # Returns numeric value between 0 and 1
    calculate_cluster_homogeneity = function(data_cluster) {
      if (ncol(data_cluster) < 2) {
        return(1.0)  # Single variable cluster has perfect homogeneity
      }
      
      # Check if data is purely quantitative (numeric)
      all_numeric <- all(sapply(data_cluster, is.numeric))
      
      if (all_numeric) {
        # Pure quantitative: use prcomp (no PCAmixdata needed)
        pca_result <- prcomp(data_cluster, center = TRUE, scale. = self$standardize)
        variance_explained <- summary(pca_result)$importance[2, 1]  # Proportion of variance for PC1
        return(variance_explained)
      }
      
      # Mixed or qualitative data: require PCAmixdata
      if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
        stop("PCAmixdata package required for mixed data clustering. Install with: install.packages('PCAmixdata')")
      }
      
      # Use splitmix to automatically separate quanti and quali
      split <- PCAmixdata::splitmix(data_cluster)
      
      # Run PCAmix with ndim=2 (minimum required by PCAmix)
      # We only need first dimension but must request at least 2
      ndim_requested <- min(2, ncol(data_cluster))
      pca_mix <- suppressWarnings(PCAmixdata::PCAmix(
        X.quanti = split$X.quanti,
        X.quali = split$X.quali,
        ndim = ndim_requested,
        graph = FALSE,
        rename.level = TRUE
      ))
      
      # Extract percentage of variance from eig table (column 2)
      # Homogeneity = percentage explained by first dimension / 100
      homogeneity <- pca_mix$eig[1, 2] / 100
      
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
    
    # Update cluster centers (first latent component for each cluster)
    # clusters: Current cluster assignments
    # Returns list of latent component scores for each cluster (polymorphic: PCA/MCA/PCAmix)
    update_cluster_centers = function(clusters) {
      centers <- list()
      pcas <- list()  # Store PCA/PCAmix objects for visualization
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(clusters == k)
        
        if (length(vars_in_cluster) == 0) {
          centers[[k]] <- NULL
          pcas[[k]] <- NULL
        } else if (length(vars_in_cluster) == 1) {
          # Single variable case
          var_data <- self$data[, vars_in_cluster, drop = FALSE]
          
          if (is.numeric(var_data[[1]])) {
            # Numeric: use prcomp for consistency with visualization
            res <- prcomp(var_data, center = TRUE, scale. = self$standardize)
            centers[[k]] <- res$x[, 1]
            pcas[[k]] <- res
          } else {
            # Factor: use PCAmix on single variable (MCA with 1 variable)
            if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
              stop("PCAmixdata package required for qualitative data. Install with: install.packages('PCAmixdata')")
            }
            res <- suppressWarnings(PCAmixdata::PCAmix(
              X.quali = var_data,
              ndim = 2,
              graph = FALSE,
              rename.level = TRUE
            ))
            centers[[k]] <- res$ind$coord[, 1]
            pcas[[k]] <- res
          }
        } else {
          # Multiple variables: check if all quantitative first
          sub_data <- self$data[, vars_in_cluster, drop = FALSE]
          all_numeric <- all(sapply(sub_data, is.numeric))
          
          if (all_numeric) {
            # 100% quantitative: use prcomp (no PCAmixdata needed)
            res <- prcomp(sub_data, center = TRUE, scale. = self$standardize)
            centers[[k]] <- res$x[, 1]
            pcas[[k]] <- res
          } else {
            # Mixed or qualitative: use PCAmix
            if (!requireNamespace("PCAmixdata", quietly = TRUE)) {
              stop("PCAmixdata package required for mixed/qualitative data. Install with: install.packages('PCAmixdata')")
            }
            
            split <- PCAmixdata::splitmix(sub_data)
            ndim_requested <- min(2, ncol(sub_data))
            res <- suppressWarnings(PCAmixdata::PCAmix(
              X.quanti = split$X.quanti,
              X.quali = split$X.quali,
              ndim = ndim_requested,
              graph = FALSE,
              rename.level = TRUE
            ))
            centers[[k]] <- res$ind$coord[, 1]
            pcas[[k]] <- res
          }
        }
      }
      
      # Store PCA objects for visualization (THIS WAS MISSING!)
      private$cluster_pca <- pcas
      
      return(centers)
    },
    
    # Reassign variables to clusters based on association with centers (latent component)
    # This is the K-means-like reassignment step (polymorphic: R² for quanti, η² for quali)
    # clusters: Current cluster assignments
    # centers: List of latent component scores for each cluster
    # Returns new cluster assignments
    reassign_variables = function(clusters, centers) {
      n_vars <- ncol(self$data)
      new_clusters <- integer(n_vars)
      
      # Pre-calculate variable types for efficiency
      is_num_vec <- sapply(self$data, is.numeric)
      
      # For each variable, assign to cluster with highest association (R² or η²)
      for (var_idx in 1:n_vars) {
        var_data <- self$data[, var_idx]
        is_numeric <- is_num_vec[var_idx]
        
        best_cluster <- 1
        best_assoc <- -Inf
        
        # Try each cluster
        for (k in 1:self$n_clusters) {
          if (is.null(centers[[k]])) next
          
          center_scores <- centers[[k]]
          
          # Calculate squared correlation or correlation ratio
          if (is_numeric) {
            # R² for quantitative
            cor_val <- cor(var_data, center_scores)
            assoc <- cor_val^2
          } else {
            # η² for qualitative (Correlation Ratio)
            # Equivalent to R² of linear model: Center ~ Factor
            tryCatch({
              fit <- suppressWarnings(lm(center_scores ~ var_data))
              assoc <- summary(fit)$r.squared
            }, error = function(e) {
              assoc <<- 0
            })
          }
          
          if (assoc > best_assoc) {
            best_assoc <- assoc
            best_cluster <- k
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
      
      # Reset visualization and prediction caches
      private$pca_global <- NULL
      private$pca_global_computed <- FALSE
      private$coords_fit <- NULL
      private$coords_predict <- NULL
      private$last_new_data <- NULL
      
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
      
      # Calculate centers and homogeneity for each cluster using final assignments
      private$cluster_pca <- list()
      private$cluster_centers <- list()
      private$cluster_homogeneity <- numeric(self$n_clusters)
      
      # Recalculate centers using the update_cluster_centers method
      final_centers <- private$update_cluster_centers(self$clusters)
      private$cluster_centers <- final_centers
      
      # Calculate homogeneity for each cluster
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(self$clusters == k)
        if (length(vars_in_cluster) > 0) {
          data_cluster <- self$data[, vars_in_cluster, drop = FALSE]
          private$cluster_homogeneity[k] <- private$calculate_cluster_homogeneity(data_cluster)
        } else {
          private$cluster_homogeneity[k] <- 0
        }
      }
      
      # Store final results
      private$global_homogeneity <- best_homogeneity
      self$fitted <- TRUE
      
      # NOTE: Global PCA for visualization is NOT computed here (lazy loading)
      # It will be computed on-demand when get_plot_data() is called
      
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
      
      # Convert character to factor
      for (col_name in names(new_data)) {
        if (is.character(new_data[[col_name]])) {
          new_data[[col_name]] <- as.factor(new_data[[col_name]])
        }
      }
      
      # Check valid types
      for (col_name in names(new_data)) {
        if (!is.numeric(new_data[[col_name]]) && !is.factor(new_data[[col_name]])) {
          stop(sprintf("Variable '%s' has unsupported type '%s'. Only numeric and factor are allowed.",
                      col_name, class(new_data[[col_name]])[1]))
        }
      }
      
      if (nrow(new_data) != nrow(self$data)) {
        stop("new_data must have the same number of observations as training data")
      }
      
      # Cache data for lazy visualization (enables plot_predict without args)
      private$last_new_data <- new_data
      private$coords_predict <- NULL  # Reset coords since data changed
      
      n_new_vars <- ncol(new_data)
      pred_clusters <- integer(n_new_vars)
      pred_scores_matrix <- matrix(0, nrow = n_new_vars, ncol = self$n_clusters)
      
      # CORE PREDICTION LOGIC: Assign to cluster with highest association (R² or η²)
      for (var_idx in 1:n_new_vars) {
        var_data <- new_data[, var_idx]
        is_var_numeric <- is.numeric(var_data)
        
        best_cluster <- 1
        best_assoc <- -Inf
        
        # Calculate association with each cluster center (local PC1)
        for (k in 1:self$n_clusters) {
          if (is.null(private$cluster_centers[[k]])) {
            assoc <- -1
          } else {
            center_scores <- private$cluster_centers[[k]]
            
            # Calculate association depending on variable type
            if (is_var_numeric) {
              # Quantitative: R² (squared correlation)
              cor_val <- cor(var_data, center_scores, use = "complete.obs")
              assoc <- cor_val^2
            } else {
              # Qualitative: η² (correlation ratio)
              tryCatch({
                model <- suppressWarnings(lm(center_scores ~ var_data))
                assoc <- summary(model)$r.squared
              }, error = function(e) {
                assoc <<- 0
              })
            }
            
            pred_scores_matrix[var_idx, k] <- assoc
            
            if (assoc > best_assoc) {
              best_assoc <- assoc
              best_cluster <- k
            }
          }
        }
        
        pred_clusters[var_idx] <- best_cluster
      }
      
      # Return prediction results
      result <- data.frame(
        variable = colnames(new_data),
        cluster = pred_clusters,
        row.names = NULL
      )
      
      if (return_scores) {
        # Add score columns for each cluster
        for (k in 1:self$n_clusters) {
          result[[paste0("score_", k)]] <- pred_scores_matrix[, k]
        }
      }
      
      return(result)
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
    #' @return PCA object from prcomp or PCAmix.
    get_pca = function() {
      private$check_fitted()
      private$compute_global_pca_if_needed()  # Trigger lazy loading
      return(private$pca_global)
    },
    
    #' @description Get PCA loadings for visualization (global PCA).
    #' @return Data frame of loadings (for prcomp) or correlations (for PCAmix).
    get_loadings = function() {
      private$check_fitted()
      private$compute_global_pca_if_needed()  # Trigger lazy loading
      
      if (is.null(private$pca_global)) return(data.frame())
      
      # Handle both prcomp and PCAmix objects
      if (inherits(private$pca_global, "prcomp")) {
        return(as.data.frame(private$pca_global$rotation))
      } else {
        # PCAmix: return correlations for quantitative variables
        if (!is.null(private$pca_global$quanti.cor)) {
          return(as.data.frame(private$pca_global$quanti.cor))
        } else {
          return(data.frame())
        }
      }
    },
    
    #' @description Get cluster centers in loadings space (for K selection methods).
    #' @return Matrix of cluster centers (k rows x 2 columns for PC1 and PC2).
    get_centers = function() {
      private$check_fitted()
      private$compute_global_pca_if_needed()  # Trigger lazy loading
      
      if (is.null(private$pca_global)) return(matrix(0, nrow = self$n_clusters, ncol = 2))
      
      # Calculate mean loadings for each cluster
      # Handle both prcomp and PCAmix objects
      if (inherits(private$pca_global, "prcomp")) {
        n_components <- ncol(private$pca_global$rotation)
        
        if (n_components == 0) {
          return(matrix(0, nrow = self$n_clusters, ncol = 2))
        }
        
        # Take at most 2 components
        n_use <- min(2, n_components)
        loadings <- private$pca_global$rotation[, seq_len(n_use), drop = FALSE]
      } else {
        # PCAmix: use correlations for quantitative variables
        if (!is.null(private$pca_global$quanti.cor)) {
          n_components <- ncol(private$pca_global$quanti.cor)
          n_use <- min(2, n_components)
          loadings <- private$pca_global$quanti.cor[, seq_len(n_use), drop = FALSE]
        } else {
          return(matrix(0, nrow = self$n_clusters, ncol = 2))
        }
      }
      
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
    #' Uses lazy loading: computes global PCA only when first requested.
    #' @return List with coords, colors, pca, centers (geometric barycenters).
    get_plot_data = function() {
      if (!self$fitted) return(NULL)
      
      # Trigger lazy loading of global PCA if needed
      private$compute_global_pca_if_needed()
      
      # Compute geometric barycenters of each cluster in PCA space
      # These represent the "average direction" of variables in each cluster
      centers_pca <- matrix(NA, nrow = self$n_clusters, ncol = 2)
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(self$clusters == k)
        if (length(vars_in_cluster) > 0) {
          # Average of PC1 and PC2 coordinates for variables in this cluster
          centers_pca[k, 1] <- mean(private$coords_fit$PC1[vars_in_cluster])
          centers_pca[k, 2] <- mean(private$coords_fit$PC2[vars_in_cluster])
        }
      }
      
      colnames(centers_pca) <- c("PC1", "PC2")
      rownames(centers_pca) <- paste0("C", 1:self$n_clusters)
      
      list(
        coords = private$coords_fit,
        colors = private$cluster_colors,
        pca = private$pca_global,
        centers = centers_pca  # Geometric barycenters (visual approximation of latent centers)
      )
    },
    
    #' @description Get plot data for predictions (internal use by visualization functions).
    #' Computes coords_predict on demand if not already available.
    #' @param new_data New data (optional, only needed if coords_predict not computed yet).
    #' @return List with coords, colors, pca, centers.
    get_plot_data_predict = function(new_data = NULL) {
      if (!self$fitted) return(NULL)
      
      # Trigger lazy loading of global PCA if needed
      private$compute_global_pca_if_needed()
      
      # Determine which data to use: explicit arg > cached data
      data_to_use <- if (!is.null(new_data)) new_data else private$last_new_data
      
      # If coords not computed yet but we have data, compute them now
      if (is.null(private$coords_predict) && !is.null(data_to_use)) {
        self$prepare_plot_predict(data_to_use)
      }
      
      # Final check
      if (is.null(private$coords_predict)) {
        warning("No prediction coordinates available. Call predict(new_data) or prepare_plot_predict(new_data) first.")
        return(NULL)
      }
      
      # Compute geometric barycenters using ALL variables (active + supplementary)
      centers_pca <- matrix(NA, nrow = self$n_clusters, ncol = 2)
      
      for (k in 1:self$n_clusters) {
        vars_in_cluster <- which(private$coords_predict$cluster == k)
        if (length(vars_in_cluster) > 0) {
          centers_pca[k, 1] <- mean(private$coords_predict$PC1[vars_in_cluster])
          centers_pca[k, 2] <- mean(private$coords_predict$PC2[vars_in_cluster])
        }
      }
      
      colnames(centers_pca) <- c("PC1", "PC2")
      rownames(centers_pca) <- paste0("C", 1:self$n_clusters)
      
      list(
        coords = private$coords_predict,
        colors = private$cluster_colors,
        pca = private$pca_global,
        centers = centers_pca  # Geometric barycenters (updated with supplementary variables)
      )
    },
    
    #' @description Prepare plot coordinates for predicted variables (supplementary).
    #' This method computes visualization coordinates for new variables.
    #' Call this after predict() if you want to visualize predictions.
    #' @param new_data New data frame with same observations as training data.
    #' @param pred_result Optional prediction result (from predict()). If NULL, will call predict().
    #' @return Self (invisibly).
    prepare_plot_predict = function(new_data, pred_result = NULL) {
      private$check_fitted()
      
      # Ensure global PCA is computed
      private$compute_global_pca_if_needed()
      
      # Get predictions if not provided
      if (is.null(pred_result)) {
        pred_result <- self$predict(new_data, return_scores = FALSE)
      }
      
      pred_clusters <- pred_result$cluster
      n_new_vars <- ncol(new_data)
      
      # Start with active variables (from fit)
      coords_active <- private$coords_fit
      coords_active$type <- "active"
      
      # Project new variables onto global PCA space
      all_numeric <- all(sapply(self$data, is.numeric))
      
      if (all_numeric && inherits(private$pca_global, "prcomp")) {
        # Case 1: Pure quantitative with prcomp
        if (self$standardize) {
          new_data_scaled <- scale(new_data, center = TRUE, scale = TRUE)
        } else {
          new_data_scaled <- scale(new_data, center = TRUE, scale = FALSE)
        }
        
        # Calculate loadings as correlation with PCA scores
        pca_scores <- private$pca_global$x[, 1:2, drop = FALSE]
        new_coords <- cor(new_data_scaled, pca_scores)
        
      } else {
        # Case 2: Mixed data with PCAmix
        pca_scores <- private$pca_global$ind$coord[, 1:2, drop = FALSE]
        new_coords <- matrix(0, nrow = n_new_vars, ncol = 2)
        
        for (var_idx in 1:n_new_vars) {
          var_data <- new_data[, var_idx]
          
          if (is.numeric(var_data)) {
            # Numeric: correlation with axes
            new_coords[var_idx, 1] <- cor(var_data, pca_scores[, 1], use = "complete.obs")
            new_coords[var_idx, 2] <- cor(var_data, pca_scores[, 2], use = "complete.obs")
          } else {
            # Factor: eta² (correlation ratio)
            for (dim in 1:2) {
              tryCatch({
                fit <- suppressWarnings(lm(pca_scores[, dim] ~ var_data))
                eta2 <- summary(fit)$r.squared
                sign_val <- sign(mean(fit$fitted.values[var_data == levels(var_data)[1]]))
                new_coords[var_idx, dim] <- sqrt(eta2) * sign_val
              }, error = function(e) {
                new_coords[var_idx, dim] <<- 0
              })
            }
          }
        }
      }
      
      # Create supplementary coordinates
      coords_illus <- data.frame(
        PC1 = new_coords[, 1],
        PC2 = new_coords[, 2],
        variable = colnames(new_data),
        cluster = factor(pred_clusters),
        type = "illustrative",
        stringsAsFactors = FALSE
      )
      
      # Combine active and supplementary
      private$coords_predict <- rbind(coords_active, coords_illus)
      
      invisible(self)
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
      
      cat("Variable Clusterer \n")
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
      cat("Variable Clustering \n")
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
        
        # Homogeneity already represents PC1 variance proportion
        cat(sprintf("    First dimension explains %.1f%% of cluster variance\n", 
                   homog * 100))
      }
      
      cat("\n=================================================\n")
      
      invisible(self)
    }
  )
)
