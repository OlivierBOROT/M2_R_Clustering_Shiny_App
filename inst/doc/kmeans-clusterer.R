## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----load-package-------------------------------------------------------------
library(M2RClust)

## ----data-exploration---------------------------------------------------------
data(mtcars)
str(mtcars)

# Correlation overview (numeric columns only)
round(cor(mtcars[, sapply(mtcars, is.numeric)]), 2)

## ----basic-usage--------------------------------------------------------------
# Create the clusterer
clusterer <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  standardize = TRUE,
  seed = 42
)

# Fit the model
clusterer$fit()

# Display results
clusterer$print()

## ----summary------------------------------------------------------------------
clusterer$summary()

## ----get-results--------------------------------------------------------------
results <- clusterer$get_results()
print(results)

# Table by cluster
for (k in 1:clusterer$n_clusters) {
  vars <- results$variable[results$cluster == k]
  cat(sprintf("\nCluster %d: %s\n", k, paste(vars, collapse = ", ")))
}

## ----init-homogeneitypp-------------------------------------------------------
clust_hpp <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  init_method = "homogeneity++",
  seed = 42
)
clust_hpp$fit()
cat("Homogeneity (homogeneity++):", round(clust_hpp$get_homogeneity(), 4), "\n")

## ----init-correlation---------------------------------------------------------
clust_cor <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  init_method = "correlation",
  seed = 42
)
clust_cor$fit()
cat("Homogeneity (correlation):", round(clust_cor$get_homogeneity(), 4), "\n")

## ----init-random--------------------------------------------------------------
clust_rand <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  init_method = "random",
  n_init = 20,  # More attempts since less stable
  seed = 42
)
clust_rand$fit()
cat("Homogeneity (random):", round(clust_rand$get_homogeneity(), 4), "\n")

## ----compare-init-------------------------------------------------------------
cat("\n=== Initialization Methods Comparison ===\n")
cat(sprintf("homogeneity++ : %.4f (runs: %d)\n", 
            clust_hpp$get_homogeneity(), clust_hpp$get_actual_runs()))
cat(sprintf("correlation   : %.4f (runs: %d)\n", 
            clust_cor$get_homogeneity(), clust_cor$get_actual_runs()))
cat(sprintf("random        : %.4f (runs: %d)\n", 
            clust_rand$get_homogeneity(), clust_rand$get_actual_runs()))

## ----plot-2d, fig.cap="Correlation circle with clusters"----------------------
plot_clustering_2d(clusterer, 
                   main = "Variable Clustering - mtcars",
                   show_centers = TRUE,
                   show_labels = TRUE)

## ----plot-heatmap, fig.cap="Correlation heatmap ordered by cluster"-----------
plot_correlation_heatmap(clusterer, reorder = TRUE)

## ----plot-quality, fig.cap="Cluster size and homogeneity"---------------------
quality <- plot_cluster_quality(clusterer)
print(quality)

## ----plot-network, fig.cap="Correlation network between variables"------------
# Requires the igraph package
if (requireNamespace("igraph", quietly = TRUE)) {
  plot_network_graph(clusterer, threshold = 0.5, layout = "fruchterman.reingold")
}

## ----plot-scree, fig.cap="Variance explained by component in each cluster"----
plot_scree_by_cluster(clusterer)

## ----predict------------------------------------------------------------------
# Create new variables (simulated)
set.seed(123)
new_vars <- data.frame(
  NewVar1 = mtcars$mpg + rnorm(32, sd = 2),  # Correlated with mpg
  NewVar2 = mtcars$hp * 0.8 + rnorm(32, sd = 10)  # Correlated with hp
)

# Predict clusters
predictions <- clusterer$predict(new_vars, return_scores = TRUE)
print(predictions)

## ----plot-predict, fig.cap="Correlation circle with supplementary variables"----
plot_clustering_with_supp(clusterer)

## ----elbow, fig.cap="Elbow method"--------------------------------------------
elbow_res <- elbow_method(
  KMeansClusterer,
  mtcars,
  k_range = 2:8,
  plot = TRUE,
  standardize = TRUE,
  seed = 42
)
cat("\nSuggested K (Elbow):", elbow_res$suggested_k, "\n")

## ----silhouette, fig.cap="Silhouette method"----------------------------------
sil_res <- silhouette_method(
  KMeansClusterer,
  mtcars,
  k_range = 2:8,
  plot = TRUE,
  standardize = TRUE,
  seed = 42
)
cat("\nSuggested K (Silhouette):", sil_res$suggested_k, "\n")

## ----calinski, fig.cap="Calinski-Harabasz index (Pseudo-F)"-------------------
ch_res <- calinski_harabasz_method(
  KMeansClusterer,
  mtcars,
  k_range = 2:8,
  plot = TRUE,
  standardize = TRUE,
  seed = 42
)
cat("\nSuggested K (Calinski-Harabasz):", ch_res$suggested_k, "\n")

## ----compare-k, fig.width=12, fig.height=4, fig.cap="Comparison of K selection methods"----
comparison <- compare_k_selection_methods(
  KMeansClusterer,
  mtcars,
  k_range = 2:8,
  plot = TRUE,
  standardize = TRUE,
  seed = 42
)
cat("\n=== Consensus ===\n")
cat("Recommended K:", comparison$consensus_k, "\n")

## ----mixed-data---------------------------------------------------------------
# Create mixed data
mtcars_mixed <- mtcars[, c("mpg", "cyl", "hp", "wt", "qsec")]
mtcars_mixed$cyl <- as.factor(mtcars_mixed$cyl)
mtcars_mixed$gear <- as.factor(mtcars$gear)
mtcars_mixed$am <- as.factor(mtcars$am)

str(mtcars_mixed)

# Clustering on mixed data
if (requireNamespace("PCAmixdata", quietly = TRUE)) {
  clusterer_mixed <- KMeansClusterer$new(
    data = mtcars_mixed,
    n_clusters = 3,
    standardize = TRUE,
    seed = 42
  )
  clusterer_mixed$fit()
  clusterer_mixed$print()
  
  # Visualization
  plot_clustering_2d(clusterer_mixed, main = "Clustering - Mixed Data")
}

## ----advanced-params----------------------------------------------------------
clusterer_advanced <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  standardize = TRUE,
  max_iter = 200,      # Maximum iterations per run
  tol = 1e-6,          # Stricter convergence tolerance
  n_init = 30,         # More initializations
  init_method = "homogeneity++",
  seed = 42
)
clusterer_advanced$fit()

cat("Iterations:", clusterer_advanced$get_iterations(), "\n")
cat("Runs performed:", clusterer_advanced$get_actual_runs(), "/", 
    clusterer_advanced$get_n_init(), "\n")
cat("Homogeneity:", round(clusterer_advanced$get_homogeneity(), 4), "\n")

## ----early-stop---------------------------------------------------------------
# With many initializations, early stopping is often triggered
clusterer_es <- KMeansClusterer$new(
  data = mtcars,
  n_clusters = 3,
  n_init = 50,
  seed = 42
)
clusterer_es$fit()

if (clusterer_es$get_actual_runs() < 50) {
  cat("Early stopping triggered after", clusterer_es$get_actual_runs(), "runs\n")
}

## ----interpret-homogeneity----------------------------------------------------
cat("Homogeneity by cluster:\n")
homog <- clusterer$get_cluster_homogeneity()
for (k in 1:length(homog)) {
  cat(sprintf("  Cluster %d: %.3f\n", k, homog[k]))
}
cat(sprintf("\nGlobal homogeneity (weighted average): %.3f\n", 
            clusterer$get_homogeneity()))

## ----session-info-------------------------------------------------------------
sessionInfo()

