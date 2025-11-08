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

    #' @field max_iter The maximum number of iterations for the clustering algorithm.
    max_iter = 100,

    #' @field tol The tolerance to declare convergence.
    tol = 1e-4,

    #' @description
    #' Create a new BaseClusterer object.
    #' @param data The dataset to be clustered.
    #' @param n_clusters The number of clusters to form.
    #' @return A new `BaseClusterer` object.
    initialize = function(data, n_clusters) {
        # constructor
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
    print = function() {
      # print method to display information about the model
      invisible(self)
    },

    #' @description summary of the model
    #' @return A summary of the clustering model.
    summary = function() {
      # summary method to provide details about the model
      invisible(self)
    },
  ),
    private = list(
    #' @description validate parameters
    validate_params = function() {
      if (!is.numeric(self$n_clusters) || self$n_clusters < 2) {
        stop("n_clusters must be an integer >= 2")
        }
    }
  )
)
