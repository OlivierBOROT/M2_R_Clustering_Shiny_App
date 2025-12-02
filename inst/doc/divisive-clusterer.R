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

## ----data---------------------------------------------------------------------
data(mtcars)

# Larger data to better illustrate the hierarchy
set.seed(123)
n <- 100
p <- 12

# Create data with known cluster structure
data_struct <- data.frame(
  # Group 1: Correlated variables (performance)
  V1 = rnorm(n),
  V2 = NA, V3 = NA,
  # Group 2: Correlated variables (dimensions)
  V4 = rnorm(n),
  V5 = NA, V6 = NA,
  # Group 3: Correlated variables (economy)
  V7 = rnorm(n),
  V8 = NA, V9 = NA,
  # Group 4: Independent variables
  V10 = rnorm(n),
  V11 = rnorm(n),
  V12 = rnorm(n)
)

# Add within-group correlations
data_struct$V2 <- data_struct$V1 * 0.9 + rnorm(n, sd = 0.3)
data_struct$V3 <- data_struct$V1 * 0.85 + rnorm(n, sd = 0.4)
data_struct$V5 <- data_struct$V4 * 0.88 + rnorm(n, sd = 0.35)
data_struct$V6 <- data_struct$V4 * 0.82 + rnorm(n, sd = 0.45)
data_struct$V8 <- data_struct$V7 * 0.87 + rnorm(n, sd = 0.38)
data_struct$V9 <- data_struct$V7 * 0.80 + rnorm(n, sd = 0.5)

cat("Data dimensions:", dim(data_struct), "\n")

## ----basic-usage--------------------------------------------------------------
# Create the divisive clusterer
clusterer <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  standardize = TRUE
)

# Fit
clusterer$fit()

# Display
clusterer$print()

## ----summary------------------------------------------------------------------
clusterer$summary()

## ----split-history------------------------------------------------------------
history <- clusterer$get_split_history()

cat("=== Split History ===\n\n")
for (h in history) {
  cat(sprintf("Iteration %d:\n", h$iteration))
  cat(sprintf("  Split cluster: %d\n", h$split_cluster))
  cat(sprintf("  λ₁ = %.3f, λ₂ = %.3f\n", h$eigenvalue_1, h$eigenvalue_2))
  cat(sprintf("  Ratio λ₂/λ₁ = %.3f\n", h$eigenvalue_ratio))
  cat(sprintf("  Split quality: %.3f\n", h$split_quality))
  cat(sprintf("  Group 1: %s\n", paste(h$variables_group1, collapse = ", ")))
  cat(sprintf("  Group 2: %s\n", paste(h$variables_group2, collapse = ", ")))
  cat("\n")
}

## ----split-details------------------------------------------------------------
details <- clusterer$get_split_details()
print(details)

## ----dendrogram, fig.cap="Split tree (top-down dendrogram)", fig.height=6-----
clusterer$plot_split_dendrogram(
  main = "Divisive Clustering Tree",
  show_eigenvalues = TRUE,
  show_vars = TRUE
)

## ----kaiser-------------------------------------------------------------------
clusterer_kaiser <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 10,  # Theoretical maximum
  stop_at_kaiser = TRUE
)
clusterer_kaiser$fit()

cat("Clusters formed with Kaiser:", max(clusterer_kaiser$clusters), "\n")
clusterer_kaiser$print()

## ----eigenvalue-ratio---------------------------------------------------------
clusterer_ratio <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 10,
  min_eigenvalue_ratio = 0.2  # Stop if λ₂/λ₁ < 0.2
)
clusterer_ratio$fit()

cat("Clusters formed with ratio 0.2:", max(clusterer_ratio$clusters), "\n")

## ----min-size-----------------------------------------------------------------
clusterer_minsize <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 6,
  min_cluster_size = 3  # At least 3 variables to split
)
clusterer_minsize$fit()
clusterer_minsize$print()

## ----rotation-varimax---------------------------------------------------------
clust_varimax <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  rotation_method = "varimax"
)
clust_varimax$fit()
cat("Homogeneity (Varimax):", round(clust_varimax$get_homogeneity(), 4), "\n")

## ----rotation-promax----------------------------------------------------------
clust_promax <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  rotation_method = "promax",
  promax_m = 4  # Power parameter
)
clust_promax$fit()
cat("Homogeneity (Promax):", round(clust_promax$get_homogeneity(), 4), "\n")

## ----rotation-none------------------------------------------------------------
clust_none <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  rotation_method = "none"
)
clust_none$fit()
cat("Homogeneity (No rotation):", round(clust_none$get_homogeneity(), 4), "\n")

## ----split-eigenvalue2--------------------------------------------------------
clust_ev2 <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  split_criterion = "eigenvalue2"
)
clust_ev2$fit()

## ----split-homogeneity--------------------------------------------------------
clust_homog <- DivisiveClusterer$new(
  data = data_struct,
  n_clusters = 4,
  split_criterion = "homogeneity"
)
clust_homog$fit()

## ----plot-heatmap, fig.cap="Correlation heatmap showing cluster structure", fig.height=6----
plot_correlation_heatmap(clusterer, reorder = TRUE)

## ----plot-quality, fig.cap="Cluster sizes and homogeneity"--------------------
plot_cluster_quality(clusterer)

## ----plot-network, fig.cap="Variable correlation network", eval=requireNamespace("igraph", quietly=TRUE)----
plot_network_graph(clusterer, threshold = 0.5)

## ----predict------------------------------------------------------------------
# New variables
new_vars <- data.frame(
  NewV1 = data_struct$V1 * 0.7 + rnorm(n, sd = 0.5),  # Close to group 1
  NewV2 = data_struct$V4 * 0.6 + rnorm(n, sd = 0.6),  # Close to group 2
  NewV3 = rnorm(n)  # Independent
)

predictions <- clusterer$predict(new_vars)
print(predictions)

## ----elbow-divisive, fig.cap="Elbow method - DivisiveClusterer"---------------
elbow_res <- elbow_method(
  DivisiveClusterer,
  data_struct,
  k_range = 2:8,
  plot = TRUE
)
cat("Suggested K:", elbow_res$suggested_k, "\n")

## ----compare-k-divisive, fig.width=12, fig.height=4---------------------------
comparison <- compare_k_selection_methods(
  DivisiveClusterer,
  data_struct,
  k_range = 2:7,
  plot = TRUE
)
cat("Consensus K:", comparison$consensus_k, "\n")

## ----mixed-data---------------------------------------------------------------
# Mixed data
data_mixed <- data_struct[, 1:6]
data_mixed$Cat1 <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
data_mixed$Cat2 <- factor(sample(c("X", "Y"), n, replace = TRUE))

str(data_mixed)

if (requireNamespace("PCAmixdata", quietly = TRUE)) {
  clusterer_mixed <- DivisiveClusterer$new(
    data = data_mixed,
    n_clusters = 3
  )
  clusterer_mixed$fit()
  clusterer_mixed$print()
}

## ----compare-methods----------------------------------------------------------
# Same data, same parameters (use numeric columns only)
data(mtcars)
data_test <- mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "qsec")]

# DivisiveClusterer
div_clust <- DivisiveClusterer$new(data = data_test, n_clusters = 3)
div_clust$fit()

# KMeansClusterer
km_clust <- KMeansClusterer$new(data = data_test, n_clusters = 3, seed = 42)
km_clust$fit()

cat("=== Comparison ===\n")
cat(sprintf("DivisiveClusterer - Homogeneity: %.4f\n", div_clust$get_homogeneity()))
cat(sprintf("KMeansClusterer   - Homogeneity: %.4f\n", km_clust$get_homogeneity()))

# Compare assignments
cat("\nDivisiveClusterer assignments:\n")
print(table(div_clust$clusters))

cat("\nKMeansClusterer assignments:\n")
print(table(km_clust$clusters))

## ----compare-viz, fig.width=12, fig.height=5----------------------------------
par(mfrow = c(1, 2))
plot_clustering_2d(div_clust, main = "DivisiveClusterer")
plot_clustering_2d(km_clust, main = "KMeansClusterer")
par(mfrow = c(1, 1))

## ----export-assignments-------------------------------------------------------
assignments <- clusterer$get_cluster_assignments()
head(assignments)

## ----export-tree--------------------------------------------------------------
tree <- clusterer$get_hierarchy_tree()
str(tree, max.level = 2)

## ----export-centers-----------------------------------------------------------
centers <- clusterer$get_centers()
cat("Number of centers:", length(centers), "\n")
cat("Length of first center:", length(centers[[1]]), "observations\n")

## ----session-info-------------------------------------------------------------
sessionInfo()

