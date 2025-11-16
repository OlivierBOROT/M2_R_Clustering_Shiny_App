#' @title Base Clustering class for Clustering Algorithms
#' @description R6 abstract class that serves as a base for all clustering algorithm implementations.
#' it defines common properties and methods that all clustering algorithms should have.
#' @export

BaseClusterer <- R6::R6Class(
  "BaseClusterer",

  public = list(
    #' @field data The dataset to be clustered.
    data = NULL,

    #' @field n_clusters The number of clusters to form.
    n_clusters = NULL,

    #' @field fitted Logical flag indicating whether the model has been fitted.
    fitted = FALSE,

    #' @field clusters The resulting clusters after fitting the model.
    clusters = NULL,

    #' @field standardize Logical flag indicating whether to standardize the data before clustering.
    standardize = TRUE,

    #' @field max_iter The maximum number of iterations for the clustering algorithm.
    max_iter = 100,

    #' @field tol The tolerance to declare convergence.
    tol = 1e-4,

    #' @field variable_names The names of the variables being clustered.
    variable_names = NULL,



    #' @description
    #' Create a new BaseClusterer object.
    #' @param data The dataset to be clustered (variables in columns, observations in rows).
    #' @param n_clusters The number of clusters to form.
    #' @param standardize Whether to standardize the data.
    #' @param max_iter Maximum number of iterations.
    #' @param tol Tolerance for convergence.
    #' @param ... Additional parameters for specific clusterer implementations.
    #' @return A new `BaseClusterer` object.
    initialize = function(data, n_clusters, standardize = FALSE, max_iter = 100, tol = 1e-4, ...) {
      self$data <- data
      self$n_clusters <- n_clusters
      self$standardize <- standardize
      self$max_iter <- max_iter
      self$tol <- tol

      # Validate data first (before accessing ncol)
      private$validate_data()
      
      # Store variable names
      self$variable_names <- colnames(data)
      if (is.null(self$variable_names)) {
        self$variable_names <- paste0("Var", seq_len(ncol(data)))
        colnames(self$data) <- self$variable_names
      }

      # Parameter validation
      private$validate_params()
    },

    
    #' @description
    #' Fit the clustering algorithm to the data.
    #' @return The fitted clustering model.
    fit = function() {
      # fit method
    },

    #' @description
    #' Predict the cluster labels for new data points.
    #' @param new_data New data points to predict cluster labels for.
    #' @return A vector of predicted cluster labels.
    predict = function(new_data) {
      # predict method
    },

    #' @description model display
    #' @details This method should be overridden in derived classes
    print = function() {
      cat("BaseClusterer (abstract class)\n")
      cat("This method should be implemented in derived classes.\n")
      invisible(self)
    },

    #' @description summary of the model
    #' @details This method should be overridden in derived classes
    #' @return A summary of the clustering model.
    summary = function() {
      cat("BaseClusterer (abstract class)\n")
      cat("This method should be implemented in derived classes.\n")
      invisible(self)
    },
    #' @description
    #' Get the number of variables in the dataset
    #' @return Integer
    get_n_variables = function() {
      return(ncol(self$data))
    },
    
    #' @description
    #' Get the number of observations in the dataset
    #' @return Integer
    get_n_observations = function() {
      return(nrow(self$data))
    },
    
    #' @description
    #' Get cluster sizes
    #' @return Named vector of cluster sizes
    get_cluster_sizes = function() {
      private$check_fitted()
      return(table(self$clusters))
    },
    
    #' @description
    #' Get variables in a specific cluster
    #' @param cluster_id Integer, cluster number
    #' @return Character vector of variable names
    get_cluster_members = function(cluster_id) {
      private$check_fitted()
      if (cluster_id < 1 || cluster_id > self$n_clusters) {
        stop(sprintf("cluster_id must be between 1 and %d", self$n_clusters))
      }
      return(colnames(self$data)[self$clusters == cluster_id])
    },

    #' @description
    #' Reset the model to unfitted state
    reset = function() {
      self$fitted <- FALSE
      self$clusters <- NULL
      cat("Model reset to unfitted state\n")
      invisible(self)
    },

    #' @description
    #' Export clustering results as data.frame
    #' @return Data frame with variable names and cluster assignments
    get_results = function() {
      private$check_fitted()
    
      results <- data.frame(
        variable = self$variable_names,
        cluster = self$clusters,
        stringsAsFactors = FALSE
      )
    
      return(results)
    },
  
    #' @description
    #' Export results to CSV file
    #' @param file_path Path to save the CSV file
    save_results = function(file_path) {
      results <- self$get_results()
      write.csv(results, file_path, row.names = FALSE)
      cat(sprintf("Results saved to: %s\n", file_path))
      invisible(self)
    },
    
    #' @description
    #' Get cluster centers (for compatibility with K selection methods)
    #' @return Matrix of cluster centers or NULL for base class
    get_centers = function() {
      warning("get_centers() not implemented for BaseClusterer. Returning NULL.")
      return(NULL)
    }
  ),
  
  private = list(
    # Validate parameters
    # Validates n_clusters, max_iter, and tol parameters
    validate_params = function() {
      # Validation of n_clusters
      if (!is.numeric(self$n_clusters) || self$n_clusters < 2) {
        stop("n_clusters must be an integer >= 2")
      }
    
      # Validation of max_iter
      if (!is.numeric(self$max_iter) || self$max_iter < 1) {
        stop("max_iter must be a positive integer")
      }
    
      # Validation of tol
      if (!is.numeric(self$tol) || self$tol <= 0) {
        stop("tol must be a positive number")
      }
    
      # Check that n_clusters is not greater than or equal to number of variables
      if (self$n_clusters >= ncol(self$data)) {
        stop(sprintf(
          "n_clusters (%d) must be strictly less than number of variables (%d)",
          self$n_clusters, ncol(self$data)
        ))
      }
    },
    
    # Validate data
    # Checks that data is a data.frame or matrix with minimum required dimensions
    validate_data = function() {
      if (!is.data.frame(self$data) && !is.matrix(self$data)) {
        stop("data must be a data.frame or matrix")
      }
      
      # Minimum size validation
      if (ncol(self$data) < 2) {
        stop("data must have at least 2 variables")
      }
      
      if (nrow(self$data) < 2) {
        stop("data must have at least 2 observations")
      }
    },

    # Check if fitted
    # Throws an error if the model hasn't been fitted yet
    check_fitted = function() {
      if (!self$fitted) {
        stop("Model is not fitted yet. Please call the fit() method first.")
      }
    },

    # Validate new data for prediction
    # new_data: Data frame to validate for prediction
    # Checks that new_data has same structure as training data
    validate_new_data = function(new_data) {
      if (!is.data.frame(new_data)) {
        stop("new_data must be a data.frame")
      }
      if (nrow(new_data) != nrow(self$data)) {
        stop(sprintf(
          "new_data must have the same number of observations (%d) as training data",
          nrow(self$data)
        ))
      }
    
      if (!all(sapply(new_data, is.numeric))) {
        stop("All variables in new_data must be numeric")
      }
    }  
  )
)
