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

## ----data-creation------------------------------------------------------------
set.seed(123)
n <- 200

# Simulate survey data with correlated responses
survey_data <- data.frame(
  # Satisfaction group (correlated responses)
  satisfaction = factor(sample(c("Very Satisfied", "Satisfied", "Neutral", 
                                  "Dissatisfied", "Very Dissatisfied"), 
                               n, replace = TRUE, 
                               prob = c(0.2, 0.3, 0.25, 0.15, 0.1))),
  
  # Recommendation (correlated with satisfaction)
  recommend = factor(sample(c("Definitely", "Probably", "Maybe", 
                              "Probably Not", "Definitely Not"), 
                            n, replace = TRUE)),
  
  # Usage frequency
  usage = factor(sample(c("Daily", "Weekly", "Monthly", "Rarely"), 
                        n, replace = TRUE)),
  
  # Age group
  age_group = factor(sample(c("18-25", "26-35", "36-45", "46-55", "55+"), 
                            n, replace = TRUE)),
  
  # Region
  region = factor(sample(c("North", "South", "East", "West"), 
                         n, replace = TRUE))
)

str(survey_data)
cat("\nSample data:\n")
head(survey_data)

## ----basic-usage--------------------------------------------------------------
# Create the modality clusterer
clusterer <- ModalitiesDiceClusterer$new(
  n_groups = 4,
  linkage = "ward.D2",
  dissimilarity = "dice"
)

# Fit the model
clusterer$fit(survey_data)

# Display basic results
clusterer$print()

## ----cluster-table------------------------------------------------------------
# Get detailed cluster table
cluster_table <- clusterer$get_cluster_table()
print(cluster_table)

## ----summary------------------------------------------------------------------
# Full summary with inertia decomposition
summary_result <- clusterer$summary()

## ----dice---------------------------------------------------------------------
# Already fitted with Dice (default)
cat("Dissimilarity method:", clusterer$dissimilarity, "\n")

# View dissimilarity matrix
d_matrix <- clusterer$get_dice_matrix()
cat("\nDissimilarity matrix (first 5x5):\n")
print(round(d_matrix[1:5, 1:5], 3))

## ----cramer-------------------------------------------------------------------
clusterer_cramer <- ModalitiesDiceClusterer$new(
  n_groups = 4,
  linkage = "ward.D2",
  dissimilarity = "cramer"
)
clusterer_cramer$fit(survey_data)

cat("Cramer's V clustering:\n")
print(clusterer_cramer$get_cluster_table())

## ----compare-dissim-----------------------------------------------------------
cat("=== Comparison of Dissimilarity Measures ===\n\n")

# Inertia decomposition for Dice
dice_summary <- clusterer$summary()

cat("\n--- Cramer's V ---\n")
cramer_summary <- clusterer_cramer$summary()

## ----linkage-comparison-------------------------------------------------------
linkage_methods <- c("ward.D2", "complete", "average", "single")

cat("=== Linkage Methods Comparison ===\n\n")

for (method in linkage_methods) {
  clust <- ModalitiesDiceClusterer$new(
    n_groups = 4,
    linkage = method,
    dissimilarity = "dice"
  )
  clust$fit(survey_data)
  
  # Get inertia
  summary_res <- clust$summary()
  cat(sprintf("\n%s: Between/Total = %.2f%%\n", 
              method, 
              100 * summary_res$inertia["between"] / summary_res$inertia["total"]))
}

## ----dendrogram, fig.cap="Hierarchical clustering dendrogram of modalities"----
clusterer$plot_dendrogram()

## ----mca-plot, fig.cap="Modalities in MCA factorial space"--------------------
clusterer$plot_mca(show_labels = TRUE)

## ----cluster-plot, fig.cap="Modalities colored by cluster with confidence ellipses"----
clusterer$plot_clusters(add_ellipses = TRUE)

## ----mixed-data---------------------------------------------------------------
# Create mixed data
mixed_data <- survey_data
mixed_data$income <- rnorm(n, mean = 50000, sd = 15000)
mixed_data$age <- sample(18:70, n, replace = TRUE)
mixed_data$score <- runif(n, 0, 100)

str(mixed_data)

# Fit with auto-discretization
clusterer_mixed <- ModalitiesDiceClusterer$new(
  n_groups = 5,
  linkage = "ward.D2",
  dissimilarity = "dice",
  auto_discretize = TRUE,
  n_bins = 4  # Quartiles
)
clusterer_mixed$fit(mixed_data)

cat("Modalities after discretization:\n")
print(clusterer_mixed$get_modalities())

cat("\nCluster assignments:\n")
print(clusterer_mixed$get_cluster_table())

## ----discretization-bins------------------------------------------------------
# Compare different bin counts
for (bins in c(3, 4, 5)) {
  clust <- ModalitiesDiceClusterer$new(
    n_groups = 4,
    auto_discretize = TRUE,
    n_bins = bins
  )
  clust$fit(mixed_data)
  cat(sprintf("n_bins=%d: %d modalities\n", bins, length(clust$get_modalities())))
}

## ----cut-tree-----------------------------------------------------------------
cat("=== Exploring Different k Values ===\n\n")

# Original clustering (k=4)
cat("Original (k=4):\n")
print(table(clusterer$groups))

# Re-cut to k=3
clusterer$cut_tree(k = 3)
cat("\nAfter cut_tree(k=3):\n")
print(table(clusterer$groups))

# Re-cut to k=6
clusterer$cut_tree(k = 6)
cat("\nAfter cut_tree(k=6):\n")
print(table(clusterer$groups))

# Restore original
clusterer$cut_tree(k = 4)

## ----illustrative-------------------------------------------------------------
# Create an illustrative variable
set.seed(456)
illus_var <- factor(sample(c("Segment A", "Segment B", "Segment C"), 
                           n, replace = TRUE))

# Compute distances to clusters
illus_result <- clusterer$predict_illustrative(illus_var)

cat("Distances from illustrative modalities to clustered modalities:\n")
print(round(illus_result$distances, 3))

cat("\nMean distances by cluster:\n")
print(round(illus_result$by_group, 3))

## ----plot-illustrative, fig.cap="MCA projection with illustrative variable"----
clusterer$plot_with_illustrative(illus_var)

## ----mca-data-----------------------------------------------------------------
mca_coords <- clusterer$get_mca_data(n_dims = 3)
head(mca_coords)

## ----cluster-data-------------------------------------------------------------
cluster_data <- clusterer$get_cluster_data()
head(cluster_data)

## ----cluster-stats------------------------------------------------------------
cluster_stats <- clusterer$get_cluster_stats()
print(cluster_stats[, c("Cluster", "Size", "TotalFrequency", "AvgFrequency", "WithinVariance")])

## ----silhouette-plot, fig.cap="Silhouette analysis for optimal k"-------------
clusterer$plot_silhouette(min_k = 2, max_k = 8)

## ----silhouette-data----------------------------------------------------------
sil_data <- clusterer$get_silhouette_data()
head(sil_data)

# Average silhouette by cluster
cat("\nAverage silhouette by cluster:\n")
print(aggregate(Silhouette ~ Cluster, data = sil_data, FUN = mean))

# Poorly clustered modalities
poorly_clustered <- sil_data[sil_data$Silhouette < 0, ]
if (nrow(poorly_clustered) > 0) {
  cat("\nPoorly clustered modalities (silhouette < 0):\n")
  print(poorly_clustered)
} else {
  cat("\nNo poorly clustered modalities found.\n")
}

## ----comparison-table, echo=FALSE---------------------------------------------
comparison <- data.frame(
  Aspect = c("Unit of analysis", "Input data", "Output", 
             "Dissimilarity", "Best for", "Hierarchy"),
  ModalitiesDiceClusterer = c("Factor levels", "Categorical (mixed OK)", 
                               "Groups of modalities", "Dice / Cramer's V",
                               "Survey analysis, response patterns", "Yes (hclust)"),
  KMeansClusterer = c("Variables", "Numeric (mixed OK)", 
                      "Groups of variables", "Correlation-based",
                      "Dimensionality reduction", "No"),
  DivisiveClusterer = c("Variables", "Numeric (mixed OK)", 
                        "Groups of variables", "PCA eigenvalues",
                        "Interpretable hierarchy", "Yes (top-down)")
)
knitr::kable(comparison, caption = "Comparison of Clustering Algorithms")

## ----practical-comparison-----------------------------------------------------
cat("=== Variable Clustering (KMeansClusterer) ===\n")
# Clusters variables together
km <- KMeansClusterer$new(data = survey_data, n_clusters = 3, seed = 42)
km$fit()
cat("Variable clusters:\n")
print(km$get_results())

cat("\n=== Modality Clustering (ModalitiesDiceClusterer) ===\n")
# Clusters modalities (factor levels) together
cat("Modality clusters:\n")
print(clusterer$get_cluster_table())

## ----chaining-----------------------------------------------------------------
# Method chaining for concise code
result <- ModalitiesDiceClusterer$new(n_groups = 4)$
  fit(survey_data)$
  cut_tree(5)$
  get_cluster_table()

print(result)

## ----pipeline-----------------------------------------------------------------
# Complete analysis pipeline
analyze_modalities <- function(data, k_range = 2:8) {
  # Fit initial model
  clust <- ModalitiesDiceClusterer$new(n_groups = max(k_range))
  clust$fit(data)
  
  # Compute silhouette for each k
  results <- data.frame(k = k_range, avg_silhouette = NA, between_ratio = NA)
  
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    clust$cut_tree(k)
    
    # Silhouette
    sil <- clust$get_silhouette_data()
    results$avg_silhouette[i] <- mean(sil$Silhouette)
    
    # Between/Total inertia
    summary_res <- clust$summary()
    results$between_ratio[i] <- summary_res$inertia["between"] / summary_res$inertia["total"]
  }
  
  return(results)
}

# Run analysis
analysis <- analyze_modalities(survey_data)
print(analysis)

# Recommend k
best_k <- analysis$k[which.max(analysis$avg_silhouette)]
cat(sprintf("\nRecommended k (by silhouette): %d\n", best_k))

## ----session-info-------------------------------------------------------------
sessionInfo()

