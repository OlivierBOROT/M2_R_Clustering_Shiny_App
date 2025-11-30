#' @title Clustering using MCA followed by HAC (Hierarchical Agglomerative Clustering) for qualitative data
#' @description R6 class that implements clustering by first performing Multiple Correspondence Analysis (MCA)
#' on qualitative data, followed by Hierarchical Agglomerative Clustering (HAC) on the MCA results.
#' @export

MCA_HClusterer <- R6::R6Class(
  "MCA_HClusterer",
  inherit = BaseClusterer,
  public = list(
    #' @field mca_result The result of the MCA analysis.
    mca_result = NULL,

    #' @field hclust_result The result of the hierarchical clustering.
    hclust_result = NULL,

    #' @description
    #' Create a new MCA_HClusterer object.
    #' @param data The dataset to be clustered.
    #' @param n_clusters The number of clusters to form.
    #' @return A new `MCA_HClusterer` object.
    initialize = function(data, n_clusters) {
      super$initialize(data, n_clusters) # call to BaseClusterer initialize
      # additional initialization for MCA + HAC
    },

    #' @description
    #' Fit the MCA followed by HAC clustering algorithm to the data.
    #' @return The fitted MCA + HAC clustering model.
    fit = function() {
      # implementation of the MCA + HAC fitting algorithm
    },

    #' @description
    #' Predict the cluster labels for new data points using the fitted MCA + HAC model.
    #' @param new_data New data points to predict cluster labels for.
    #' @return A vector of predicted cluster labels.
    predict = function(new_data) {
      # implementation of the MCA + HAC prediction algorithm
    },

    #' @description model display
    print = function() {
      # print method specific to MCA + HAC
      invisible(self)
    },

    #' @description summary of the MCA + HAC model
    #' @return A summary of the MCA + HAC clustering model.
    summary = function() {
      # summary method specific to MCA + HAC
      invisible(self)
    },

    #' @description plot dendrogram of the hierarchical clustering
    #' @return A dendrogram plot.
    plot_dendrogram = function() {
      plot_dendrogram(self) # wrapper function calling external plotting function to keep class clean
      invisible(self)
    }
  ),
  private = list(
    # private methods and fields specific to MCA + HAC can be added here
  )
)
