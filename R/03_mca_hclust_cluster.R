#' @title Modalities Dice Clusterer
#' @name ModalitiesDiceClusterer-class
#' @aliases ModalitiesDiceClusterer
#' @description
#' Comprehensive R6 class for clustering variable modalities using a Dice-style
#' dissimilarity computed on a disjunctive (one-hot) representation of categorical
#' data. This implementation includes features inspired by `HClustVar`: automatic
#' discretization of numeric variables, optional Cramér's V dissimilarity, cached
#' group-level statistics (centers, inertia decomposition, modality contributions),
#' and flexible hierarchical linkage and re-cutting.
#'
#' @details
#' Each modality (variable level) is represented as a binary column in a
#' disjunctive matrix. Dissimilarities between modalities are computed either
#' using a Dice-based squared distance (default) or a Cramér's V-based measure
#' transformed into a squared dissimilarity. After computing the modality-by-modality
#' dissimilarity matrix, hierarchical clustering (`hclust`) is applied and
#' modalities are assigned to `n_groups` clusters. The class also computes and
#' caches group statistics (centers, inertia decomposition, modality contributions)
#' to support quick summaries and diagnostics.
#'
#' @section Fields:
#' \describe{
#'   \item{n_groups}{Integer. Target number of modality groups (clusters).}
#'   \item{linkage}{Character. Linkage method passed to `hclust()` (e.g. `'ward.D2'`).}
#'   \item{dissimilarity}{Character. Dissimilarity measure used ('dice' or 'cramer').}
#'   \item{auto_discretize}{Logical. Whether numeric variables are automatically discretized.}
#'   \item{n_bins}{Integer. Number of bins for discretization.}
#'   \item{data}{Data.frame. The processed input data after discretization.}
#'   \item{disj}{Matrix. Disjunctive (one-hot) matrix of the input data (observations x modalities).}
#'   \item{d2}{Matrix. Squared-distance matrix between modalities (modalities x modalities).}
#'   \item{hclust}{Object. `stats::hclust()` result after clustering modalities.}
#'   \item{groups}{Integer vector. Named vector giving cluster id for each modality (names = modality labels).}
#'   \item{fitted}{Logical. TRUE after successful `fit()`.}
#'   \item{modality_counts}{Numeric vector. Frequency (counts) for each modality.}
#'   \item{modality_relcontrib}{Numeric vector. Relative contribution of each modality to total inertia.}
#'   \item{group_centers}{Matrix. Center (mean profile) for each modality group.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize}{Create a new object. Main arguments: `n_groups`, `linkage`,
#'     `dissimilarity` (`'dice'` or `'cramer'`), `auto_discretize` (logical), and `n_bins` (integer).}
#'   \item{fit}{Compute the disjunctive table, modality dissimilarities, run `hclust`,
#'     cut the tree and compute cached group statistics. Call `fit(data)` with a
#'     `data.frame` of factor columns (categorical variables) or a mixed dataset
#'     when `auto_discretize = TRUE`.}
#'   \item{get_dice_matrix}{Return the modality distance matrix (square-rooted from `d2`).}
#'   \item{cut_tree}{Re-cut the hierarchical tree to a different number of clusters and refresh caches.}
#'   \item{get_cluster_table}{Return a `data.frame` with modality names, assigned cluster and frequency.}
#'   \item{predict_illustrative}{Compute distances between modalities and an illustrative variable
#'     (factor or single-column `data.frame`) and aggregate results by modality group.}
#'   \item{measure_dissimilarity, measure_similarity}{Convenience accessors to the computed
#'     dissimilarity / similarity matrices.}
#'   \item{plot_dendrogram}{Plot hierarchical dendrogram with cluster rectangles.}
#'   \item{plot_mca}{Plot modalities in 2D MCA factorial space.}
#'   \item{plot_clusters}{Plot modalities colored by cluster with optional ellipses.}
#'   \item{plot_with_illustrative}{Plot modalities and illustrative variable in MCA space.}
#'   \item{get_mca_data}{Extract MCA coordinates as a data.frame for export/analysis.}
#'   \item{get_cluster_data}{Extract clustering visualization data with coordinates and assignments.}
#'   \item{get_illustrative_data}{Extract projection data for active and illustrative modalities.}
#'   \item{get_cluster_stats}{Generate comprehensive statistics table for each cluster.}
#'   \item{summary}{Prints a concise summary and returns cached structured statistics
#'     (`group_centers`, `inertia`, `modality_contribution`, `modality_relcontrib`).}
#' }
#'
#' @field n_groups Integer. Target number of modality groups (clusters).
#' @field linkage Character. Linkage method passed to `hclust()`.
#' @field dissimilarity Character. Dissimilarity measure used ('dice' or 'cramer').
#' @field auto_discretize Logical. Whether numeric variables are automatically discretized.
#' @field n_bins Integer. Number of bins for discretization.
#' @field data Data.frame. The processed input data after discretization.
#' @field disj Matrix. Disjunctive (one-hot) matrix of the input data.
#' @field d2 Matrix. Squared-distance matrix between modalities.
#' @field hclust Object. `stats::hclust()` result after clustering modalities.
#' @field groups Integer vector. Named vector giving cluster id for each modality.
#' @field fitted Logical. TRUE after successful `fit()`.
#' @field modality_counts Numeric vector. Frequency (counts) for each modality.
#' @field modality_relcontrib Numeric vector. Relative contribution of each modality to total inertia.
#' @field group_centers Matrix. Center (mean profile) for each modality group.
#'
#' @param n_groups Integer. See `initialize()`.
#' @param linkage Character. See `initialize()`.
#' @param dissimilarity Character. One of `'dice'` (default) or `'cramer'`.
#' @param auto_discretize Logical. If TRUE, numeric columns in `fit()` input
#'   will be discretized into `n_bins` quantile categories before creating the
#'   disjunctive matrix. Useful for mixed datasets.
#' @param n_bins Integer. Number of quantile bins used when `auto_discretize = TRUE`.
#'
#' @return An R6 object (modalities clusterer). Most methods return `invisible(self)`
#'   to allow chaining. `summary()` returns a list with `group_centers`, `inertia`,
#'   `modality_contribution` and `modality_relcontrib` (and is printed for convenience).
#'
#' @examples
#' \dontrun{
#' source("CAH/new_CAH.R")
#' df <- data.frame(
#'   A = factor(sample(c("a", "b", "c"), 200, TRUE)),
#'   B = factor(sample(c("x", "y"), 200, TRUE)),
#'   C = rnorm(200)
#' )
#' m <- ModalitiesDiceClusterer$new(n_groups = 3, dissimilarity = "dice", auto_discretize = TRUE)
#' m$fit(df)
#' m$plot_dendrogram()
#' print(m$get_cluster_table())
#' res <- m$summary()
#' }
#'
#' @export
ModalitiesDiceClusterer <- R6::R6Class(
  "ModalitiesDiceClusterer",
  public = list(
    # fields
    n_groups = NULL,
    linkage = NULL,
    dissimilarity = NULL,
    auto_discretize = NULL,
    n_bins = NULL,
    data = NULL,
    disj = NULL, # disjunctive matrix (observations x modalities)
    d2 = NULL, # squared-distance matrix (modalities x modalities)
    hclust = NULL, # hclust object
    groups = NULL, # grouping for modalities
    fitted = FALSE,
    modality_counts = NULL,
    modality_relcontrib = NULL,
    group_centers = NULL,

    #' @description Create a new ModalitiesDiceClusterer
    #' @details
    #' This constructor initializes a new clustering object with specified parameters.
    #' The object is not yet fitted to data - use the `fit()` method after construction.
    #'
    #' @param n_groups Integer. The target number of modality clusters (groups) to form.
    #'   Must be a positive integer. Default is 3.
    #' @param linkage Character. The agglomeration method to be used in hierarchical
    #'   clustering. Must be one of:
    #'   \itemize{
    #'     \item "ward.D2" (default): Ward's minimum variance method using squared distances
    #'     \item "single": Single linkage (nearest neighbor)
    #'     \item "complete": Complete linkage (furthest neighbor)
    #'     \item "average": Average linkage (UPGMA)
    #'     \item "ward.D": Ward's method with d instead of d²
    #'   }
    #' @param dissimilarity Character. The dissimilarity measure between modalities.
    #'   Must be one of:
    #'   \itemize{
    #'     \item "dice" (default): Dice dissimilarity based on binary vectors
    #'     \item "cramer": Cramér's V association measure transformed to dissimilarity
    #'   }
    #' @param auto_discretize Logical. If TRUE, numeric columns in the input data
    #'   will be automatically discretized into categorical bins before clustering.
    #'   Default is TRUE. Useful for mixed-type datasets.
    #' @param n_bins Integer. The number of quantile-based bins to use when
    #'   discretizing numeric variables (only used if auto_discretize = TRUE).
    #'   Must be >= 2. Default is 4 (quartiles).
    #'
    #' @return A new ModalitiesDiceClusterer R6 object (invisible for chaining)
    #'
    #' @examples
    #' \dontrun{
    #' # Basic initialization with defaults
    #' clusterer <- ModalitiesDiceClusterer$new()
    #'
    #' # Custom initialization
    #' clusterer <- ModalitiesDiceClusterer$new(
    #'   n_groups = 5,
    #'   linkage = "complete",
    #'   dissimilarity = "cramer",
    #'   auto_discretize = FALSE
    #' )
    #' }
    initialize = function(n_groups = 3, linkage = "ward.D2",
                          dissimilarity = c("dice", "cramer"),
                          auto_discretize = TRUE,
                          n_bins = 4) {
      # Store parameters as instance fields for later use
      self$n_groups <- as.integer(n_groups)
      self$linkage <- linkage
      # match.arg validates that dissimilarity is one of the allowed values
      self$dissimilarity <- match.arg(dissimilarity)
      # isTRUE ensures robust logical conversion (handles NA, NULL)
      self$auto_discretize <- isTRUE(auto_discretize)
      self$n_bins <- as.integer(n_bins)

      # Validate all parameters immediately to catch errors early
      # Delegates to private method to keep public interface clean
      private$verify_entries()

      # Return self invisibly to allow method chaining
      invisible(self)
    },

    #' @description Fit the model to categorical data
    #' @details
    #' This is the main fitting method that performs the complete clustering pipeline:
    #' \enumerate{
    #'   \item Validates and converts input data to data.frame format
    #'   \item Optionally discretizes numeric variables into categorical bins
    #'   \item Creates a disjunctive (one-hot encoded) matrix from categorical data
    #'   \item Computes pairwise dissimilarities between all modalities
    #'   \item Performs hierarchical clustering on the dissimilarity matrix
    #'   \item Cuts the dendrogram into n_groups clusters
    #'   \item Computes and caches group statistics (inertia, contributions, etc.)
    #' }
    #'
    #' The disjunctive matrix represents each modality (factor level) as a binary column,
    #' where 1 indicates the modality is present for that observation and 0 indicates absence.
    #' For example, a factor variable "color" with levels {red, blue, green} becomes
    #' three binary columns: color.red, color.blue, color.green.
    #'
    #' @param data A data.frame or matrix containing the data to cluster. Each column
    #'   represents a variable (feature), and each row represents an observation.
    #'   Variables should be factors or characters (categorical). If numeric variables
    #'   are present and auto_discretize = TRUE, they will be automatically binned.
    #'   Cannot be NULL or empty.
    #'
    #' @return The fitted ModalitiesDiceClusterer object (invisible for chaining).
    #'   The object's fields are updated with clustering results:
    #'   \itemize{
    #'     \item data: Processed data (after discretization if applicable)
    #'     \item disj: Disjunctive matrix (observations × modalities)
    #'     \item d2: Squared dissimilarity matrix (modalities × modalities)
    #'     \item hclust: Hierarchical clustering result
    #'     \item groups: Cluster assignments for each modality
    #'     \item modality_counts: Frequency of each modality
    #'     \item fitted: Set to TRUE
    #'   }
    #'
    #' @examples
    #' \dontrun{
    #' # Create sample categorical data
    #' df <- data.frame(
    #'   gender = factor(c("M", "F", "M", "F", "M")),
    #'   region = factor(c("North", "South", "North", "West", "South"))
    #' )
    #'
    #' # Fit the clusterer
    #' clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
    #' clusterer$fit(df)
    #'
    #' # Chain method calls
    #' ModalitiesDiceClusterer$new()$fit(df)$plot_dendrogram()
    #' }
    fit = function(data) {
      # Check for missing or NULL data - fail fast with clear error
      if (missing(data) || is.null(data)) stop("data must be provided to fit()")

      # Validate data type BEFORE attempting conversion
      # This prevents silent failures from as.data.frame() converting invalid types
      if (!is.data.frame(data) && !is.matrix(data)) {
        stop("data must be a data.frame or matrix of categorical variables")
      }

      # Convert to data.frame for consistent handling
      # as.data.frame() preserves column names and handles matrices properly
      df <- as.data.frame(data)

      # Perform comprehensive parameter and data validation
      # This checks dimensions, valid parameter ranges, etc.
      private$verify_entries(data = df)

      # Optionally discretize numeric variables (for mixed datasets)
      # Converts continuous variables into categorical bins using quantiles
      # Only processes numeric columns, leaves factors unchanged
      if (self$auto_discretize) df <- private$discretize_numeric(df, self$n_bins)

      # Store processed data for future reference
      # Used for validation, prediction, and user inspection
      self$data <- df

      # Build disjunctive (one-hot) matrix: model.matrix(~ . -1)
      # Formula ~ . -1 means: use all columns, suppress intercept
      # Result: binary matrix where each column = one modality (factor level)
      # Example: var1={A,B,C} becomes var1A, var1B, var1C columns
      disj_mat <- stats::model.matrix(~ . - 1, data = df)
      # Ensure matrix format (model.matrix sometimes returns other classes)
      disj_mat <- as.matrix(disj_mat)

      # Rebuild modality names in format variable.level for clarity
      # model.matrix() creates names like "var1B", we want "var1.B"
      # This makes output more readable and preserves variable structure
      var_names <- colnames(df)
      new_names <- character(0) # Initialize empty character vector

      # Loop through each variable to construct new column names
      for (vn in var_names) {
        # Get all levels of this variable (as factor)
        levs <- levels(as.factor(df[[vn]]))
        # Create names: variable.level (e.g., "color.red")
        new_names <- c(new_names, paste(vn, levs, sep = "."))
      }

      # Apply new names if count matches (sanity check)
      # model.matrix might drop unused factor levels, so verify length
      if (length(new_names) == ncol(disj_mat)) colnames(disj_mat) <- new_names
      # Remove row names for cleaner output
      rownames(disj_mat) <- NULL
      # Store the disjunctive matrix as a public field
      self$disj <- disj_mat

      # Compute modality counts (frequency of each modality across observations)
      # colSums gives total occurrences of each binary modality column
      # These are the marginal frequencies used in various calculations
      self$modality_counts <- colSums(self$disj)

      # Get number of modalities (columns in disjunctive matrix)
      p <- ncol(disj_mat)

      # Compute squared distances d2 between all pairs of modalities
      # Initialize symmetric matrix filled with zeros
      d2 <- matrix(0, nrow = p, ncol = p)
      # Set row and column names to modality names for interpretability
      colnames(d2) <- colnames(disj_mat)
      rownames(d2) <- colnames(disj_mat)

      # Nested loop to compute all pairwise dissimilarities
      # Could be optimized with vectorization but kept explicit for clarity
      for (j in seq_len(p)) {
        for (jprim in seq_len(p)) {
          # Branch based on chosen dissimilarity measure
          if (self$dissimilarity == "dice") {
            # Dice dissimilarity: 0.5 * sum((m1 - m2)^2)
            # Measures squared Euclidean distance between binary vectors
            # Ranges from 0 (identical) to higher values (different)
            d2[j, jprim] <- private$dice(disj_mat[, j], disj_mat[, jprim])
          } else {
            # Cramér's V based dissimilarity between two modalities
            # Treats modalities as binary vectors and computes association

            # Create contingency table of the two binary modality columns
            tbl <- table(disj_mat[, j], disj_mat[, jprim])

            # Handle degenerate cases (empty table, all zeros)
            if (sum(tbl) == 0) {
              v <- 0 # No association possible
            } else {
              # Compute chi-square statistic from contingency table
              # suppressWarnings prevents errors on small/sparse tables
              chi <- suppressWarnings(stats::chisq.test(tbl, simulate.p.value = FALSE)$statistic)

              # Sample size for normalization
              n <- sum(tbl)
              # Table dimensions (rows and columns)
              r <- nrow(tbl)
              c <- ncol(tbl)

              # Cramér's V formula: sqrt(chi² / (n * min(r-1, c-1)))
              # Measures strength of association, ranges [0, 1]
              v <- as.numeric(sqrt(as.numeric(chi) / (n * pmin(r - 1, c - 1))))

              # Handle NA results (can occur with sparse tables)
              if (is.na(v)) v <- 0
            }

            # Transform similarity (Cramér's V) to dissimilarity
            # (1 - V)² gives squared dissimilarity, 0 = identical
            d2[j, jprim] <- (1 - v)^2
          }
        }
      }
      # Store squared dissimilarity matrix
      self$d2 <- d2

      # Convert squared dissimilarity matrix to distance object for hclust
      # Take square root to get proper distances (not squared)
      # as.dist() extracts lower triangle and creates dist object
      d <- stats::as.dist(sqrt(d2))

      # Perform hierarchical clustering using specified linkage method
      # hclust builds a dendrogram by successively merging closest clusters
      # Returns: merge matrix, heights, cluster tree structure
      self$hclust <- stats::hclust(d, method = self$linkage)

      # Cut dendrogram into k groups (n_groups clusters)
      # cutree() assigns each modality to one of k clusters
      # Returns: named integer vector (names = modalities, values = cluster IDs)
      self$groups <- stats::cutree(self$hclust, k = self$n_groups)

      # Compute and cache group-level statistics
      # Calculates: group centers, inertia decomposition, modality contributions
      # Stores results in private$.summary_results for efficient retrieval
      private$compute_group_stats()

      # Mark model as fitted (enables other methods like predict, summary)
      self$fitted <- TRUE

      # Return self invisibly to allow method chaining
      invisible(self)
    },

    #' @description Return the Dice distance matrix between modalities
    #' @details
    #' Extracts the dissimilarity matrix computed during the fit() process.
    #' The matrix contains pairwise distances between all modalities (factor levels).
    #' Distances are computed using either Dice dissimilarity or Cramér's V,
    #' depending on the dissimilarity parameter set during initialization.
    #'
    #' The returned matrix is symmetric with zeros on the diagonal (each modality
    #' has zero distance to itself). The values represent how dissimilar two
    #' modalities are in terms of their co-occurrence patterns across observations.
    #'
    #' @param as_dist Logical. If TRUE, returns a dist object (lower triangle only).
    #'   If FALSE (default), returns a full symmetric matrix. The dist format is
    #'   useful for compatibility with clustering functions, while the matrix format
    #'   is easier for inspection and custom analyses.
    #'
    #' @return Either a numeric matrix (modalities × modalities) or a dist object,
    #'   depending on the as_dist parameter. Matrix elements represent dissimilarities:
    #'   smaller values indicate more similar modalities.
    #'
    #' @examples
    #' \dontrun{
    #' # Get full distance matrix
    #' d_matrix <- clusterer$get_dice_matrix(as_dist = FALSE)
    #' print(d_matrix[1:3, 1:3])  # View subset
    #'
    #' # Get dist object for use with other clustering functions
    #' d_dist <- clusterer$get_dice_matrix(as_dist = TRUE)
    #' hc <- hclust(d_dist, method = "complete")
    #' }
    get_dice_matrix = function(as_dist = FALSE) {
      # Check if model has been fitted - fail fast if not
      if (is.null(self$d2)) stop("Distance matrix not computed. Run $fit(data) first")

      # Take square root of squared dissimilarity matrix (self$d2)
      # This converts d² back to d (proper metric distance)
      dmat <- sqrt(self$d2)

      # Return in requested format: dist object or full matrix
      # as.dist() extracts lower triangle for efficient storage
      if (as_dist) {
        return(stats::as.dist(dmat))
      }

      # Default: return full symmetric matrix for easy inspection
      return(dmat)
    },

    #' @description Cut the hierarchical tree into a different number of clusters
    #' @details
    #' Re-partitions the modalities into k groups by cutting the dendrogram at a
    #' different level, without re-computing distances or re-running the clustering.
    #' This is much faster than calling fit() again with a different n_groups.
    #'
    #' After cutting, all cached statistics (inertia decomposition, group centers,
    #' modality contributions) are automatically recomputed for the new grouping.
    #' The original hierarchical tree structure remains unchanged.
    #'
    #' This method is useful for exploring different numbers of clusters to find
    #' the optimal balance between interpretability and cluster quality.
    #'
    #' @param k Integer. The new number of clusters to form. Must be a positive
    #'   integer and should be less than or equal to the number of modalities.
    #'   Typical values range from 2 to 10 depending on the application.
    #'
    #' @return The ModalitiesDiceClusterer object (invisible for chaining).
    #'   The following fields are updated:
    #'   \itemize{
    #'     \item groups: New cluster assignments for each modality
    #'     \item n_groups: Updated to the new k value
    #'     \item Cached statistics: Recomputed for new grouping
    #'   }
    #'
    #' @examples
    #' \dontrun{
    #' # Fit with initial k=3
    #' clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
    #' clusterer$fit(data)
    #'
    #' # Try k=5 without re-fitting
    #' clusterer$cut_tree(5)
    #' clusterer$summary()  # View new clustering
    #'
    #' # Compare different k values
    #' for (k in 2:6) {
    #'   clusterer$cut_tree(k)
    #'   print(clusterer$summary()$inertia)
    #' }
    #' }
    cut_tree = function(k) {
      # Verify model has been fitted (need existing dendrogram)
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate k parameter: must be provided and valid
      if (missing(k) || is.null(k)) stop("k must be provided")
      if (!is.numeric(k) || k < 1) stop("k must be a positive integer")

      # Cut the existing dendrogram at new k level
      # cutree() operates on the stored hclust object
      # Returns: named vector of cluster assignments
      self$groups <- stats::cutree(self$hclust, k = as.integer(k))

      # Update n_groups to reflect new clustering
      self$n_groups <- as.integer(k)

      # Recompute all cached statistics for the new grouping
      # Includes: inertia, group centers, modality contributions
      private$compute_group_stats()

      # Return self invisibly for method chaining
      invisible(self)
    },

    #' @description Return a data.frame with modality, cluster and frequency
    get_cluster_table = function() {
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")
      df <- data.frame(
        Modality = colnames(self$disj),
        Cluster = as.integer(self$groups),
        Frequency = as.numeric(self$modality_counts),
        stringsAsFactors = FALSE
      )
      df <- df[order(df$Cluster, -df$Frequency), , drop = FALSE]
      rownames(df) <- NULL
      return(df)
    },

    #' @description Return modality names
    get_modalities = function() {
      if (is.null(self$disj)) {
        return(NULL)
      }
      colnames(self$disj)
    },

    #' @description Return dissimilarity matrix between modalities (Dice)
    #' @param as_dist Logical. If TRUE, returns a dist object. Default is FALSE.
    #' @return Dissimilarity matrix or dist object
    measure_dissimilarity = function(as_dist = FALSE) {
      if (is.null(self$d2)) stop("Dissimilarity not computed. Run $fit(data) first")
      # return sqrt(d2) as the (raw) distance matrix
      d <- sqrt(self$d2)
      if (as_dist) {
        return(stats::as.dist(d))
      }
      return(d)
    },

    #' @description Return similarity matrix derived from dissimilarity
    #' @param normalize Logical. If TRUE, normalizes to \[0,1\] range. Default is TRUE.
    #' @return Similarity matrix
    measure_similarity = function(normalize = TRUE) {
      if (is.null(self$d2)) stop("Dissimilarity not computed. Run $fit(data) first")
      d <- sqrt(self$d2)
      # normalize to [0,1] if requested
      maxd <- max(d, na.rm = TRUE)
      if (normalize && maxd > 0) d_norm <- d / maxd else d_norm <- d
      sim <- 1 - d_norm
      return(sim)
    },

    #' @description Compute distances from modalities to an illustrative variable
    #' @param illus factor or one-column data.frame representing illustrative variable
    #' @return list with `distances` (vector per modality) and `by_group` (mean per modality group)
    predict_illustrative = function(illus) {
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")
      # Accept factor / character vector or one-column data.frame
      if (is.data.frame(illus)) {
        if (ncol(illus) != 1) stop("illus data.frame must have a single column")
        illus_vec <- illus[[1]]
      } else {
        illus_vec <- illus
      }
      illus_vec <- as.factor(illus_vec)

      # Build disjunctive for illustrative variable
      illus_disj <- stats::model.matrix(~ illus_vec - 1)
      # ensure columns named after levels
      colnames(illus_disj) <- sub("illus_vec", "", colnames(illus_disj))

      # compute dice distance between each modality and each illustrative level
      dice <- function(m1, m2) {
        0.5 * sum((m1 - m2)^2)
      }

      # if multiple illustrative levels, compute distances to each level and return a matrix
      dist_mat <- sapply(seq_len(ncol(illus_disj)), function(k) {
        v <- illus_disj[, k]
        apply(self$disj, 2, function(col) dice(col, v))
      })
      colnames(dist_mat) <- colnames(illus_disj)

      # aggregate distances by modality group (mean)
      by_group <- apply(dist_mat, 2, function(distances) tapply(distances, self$groups, mean))

      list(distances = dist_mat, by_group = by_group)
    },

    #' @description Plot dendrogram of modalities
    plot_dendrogram = function() {
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")
      plot(self$hclust, main = sprintf("Modalities clustering (k=%d)", self$n_groups))
      rect.hclust(self$hclust, k = self$n_groups, border = 2:(self$n_groups + 1))
      invisible(self)
    },

    #' @description Print method for the clusterer
    #' @return The ModalitiesDiceClusterer object (invisible)
    print = function() {
      cat("ModalitiesDiceClusterer\n")
      if (self$fitted) {
        cat(sprintf("Fitted with %d modality-groups\n", self$n_groups))
        print(table(self$groups))
      } else {
        cat("(not fitted)\n")
      }
      invisible(self)
    },

    #' @description Generate comprehensive summary statistics
    #' @details
    #' Prints a detailed summary including model configuration, data information,
    #' cluster assignments, inertia decomposition, modality contributions, distance
    #' statistics, and hierarchical clustering details.
    #' @return A list containing group_centers, inertia, modality_contribution,
    #'   and modality_relcontrib (returned invisibly)
    summary = function() {
      # Return a comprehensive structured summary
      cat("================================================================================\n")
      cat("                    MODALITIES DICE CLUSTERER SUMMARY\n")
      cat("================================================================================\n\n")

      if (!self$fitted) {
        cat("Model not fitted yet. Call $fit(data) first.\n")
        return(invisible(NULL))
      }

      # If cached summary exists, reuse it
      if (!is.null(private$.summary_results)) {
        res <- private$.summary_results
      } else {
        res <- private$compute_group_stats()
      }

      # 1. Model Configuration
      cat("--- MODEL CONFIGURATION ---\n")
      cat(sprintf("Dissimilarity measure: %s\n", toupper(self$dissimilarity)))
      cat(sprintf("Linkage method: %s\n", self$linkage))
      cat(sprintf("Number of clusters: %d\n", self$n_groups))
      cat(sprintf("Auto-discretization: %s\n", ifelse(self$auto_discretize, sprintf("Yes (n_bins=%d)", self$n_bins), "No")))
      cat("\n")

      # 2. Data Information
      cat("--- DATA INFORMATION ---\n")
      cat(sprintf("Number of observations: %d\n", nrow(self$data)))
      cat(sprintf("Number of variables: %d\n", ncol(self$data)))
      cat(sprintf("Number of modalities: %d\n", length(self$get_modalities())))
      cat(sprintf("Modalities: %s\n", paste(self$get_modalities(), collapse = ", ")))
      cat("\n")

      # 3. Cluster Assignments
      cat("--- CLUSTER ASSIGNMENTS ---\n")
      cluster_table <- self$get_cluster_table()
      cat(sprintf("Cluster sizes (n_modalities): %s\n", paste(as.numeric(table(self$groups)), collapse = ", ")))
      cat("\nDetailed cluster composition:\n")
      for (i in sort(unique(cluster_table$Cluster))) {
        clust_mods <- cluster_table[cluster_table$Cluster == i, ]
        cat(sprintf(
          "\nCluster %d (%d modalities, total freq: %d):\n",
          i, nrow(clust_mods), sum(clust_mods$Frequency)
        ))
        for (j in 1:nrow(clust_mods)) {
          cat(sprintf(
            "  - %s (freq: %d, %.1f%%)\n",
            clust_mods$Modality[j],
            clust_mods$Frequency[j],
            100 * clust_mods$Frequency[j] / nrow(self$data)
          ))
        }
      }
      cat("\n")

      # 4. Inertia Decomposition
      cat("--- INERTIA DECOMPOSITION ---\n")
      cat(sprintf("Total inertia: %.4f\n", res$inertia["total"]))
      cat(sprintf(
        "Between-cluster inertia: %.4f (%.2f%%)\n",
        res$inertia["between"],
        100 * res$inertia["between"] / res$inertia["total"]
      ))
      cat(sprintf(
        "Within-cluster inertia: %.4f (%.2f%%)\n",
        res$inertia["within"],
        100 * res$inertia["within"] / res$inertia["total"]
      ))
      cat("\nInterpretation:\n")
      inertia_ratio <- res$inertia["between"] / res$inertia["total"]
      if (inertia_ratio > 0.5) {
        cat("  ✓ Good separation (>50% between-cluster variance)\n")
      } else if (inertia_ratio > 0.3) {
        cat("  ⚠ Moderate separation (30-50% between-cluster variance)\n")
      } else {
        cat("  ⚠ Weak separation (<30% between-cluster variance)\n")
      }
      cat("\n")

      # 5. Modality Contributions
      cat("--- TOP MODALITY CONTRIBUTIONS TO TOTAL INERTIA ---\n")
      contrib_df <- data.frame(
        Modality = names(res$modality_contribution),
        Contribution = as.numeric(res$modality_contribution),
        RelativeContrib = as.numeric(res$modality_relcontrib) * 100,
        Cluster = as.integer(self$groups)
      )
      contrib_df <- contrib_df[order(-contrib_df$Contribution), ]
      top_n <- min(10, nrow(contrib_df))
      for (i in 1:top_n) {
        cat(sprintf(
          "%2d. %s (cluster %d): %.4f (%.2f%%)\n",
          i,
          contrib_df$Modality[i],
          contrib_df$Cluster[i],
          contrib_df$Contribution[i],
          contrib_df$RelativeContrib[i]
        ))
      }
      cat("\n")

      # 6. Distance Statistics
      cat("--- DISTANCE MATRIX STATISTICS ---\n")
      d_mat <- self$get_dice_matrix()
      diag(d_mat) <- NA # exclude diagonal
      cat(sprintf("Distance range: [%.4f, %.4f]\n", min(d_mat, na.rm = TRUE), max(d_mat, na.rm = TRUE)))
      cat(sprintf("Mean distance: %.4f\n", mean(d_mat, na.rm = TRUE)))
      cat(sprintf("Median distance: %.4f\n", median(d_mat, na.rm = TRUE)))
      cat("\n")

      # 7. Hierarchical Clustering Details
      cat("--- HIERARCHICAL CLUSTERING DETAILS ---\n")
      cat(sprintf("Height range: [%.4f, %.4f]\n", min(self$hclust$height), max(self$hclust$height)))
      cat(sprintf("Number of merges: %d\n", length(self$hclust$height)))
      cat("\n")

      cat("================================================================================\n")

      # Return full results invisibly
      invisible(list(
        config = list(
          dissimilarity = self$dissimilarity,
          linkage = self$linkage,
          n_groups = self$n_groups,
          auto_discretize = self$auto_discretize,
          n_bins = self$n_bins
        ),
        data_info = list(
          n_obs = nrow(self$data),
          n_vars = ncol(self$data),
          n_modalities = length(self$get_modalities()),
          modalities = self$get_modalities()
        ),
        clusters = cluster_table,
        inertia = res$inertia,
        modality_contribution = contrib_df,
        group_centers = res$group_centers,
        distance_stats = list(
          min = min(d_mat, na.rm = TRUE),
          max = max(d_mat, na.rm = TRUE),
          mean = mean(d_mat, na.rm = TRUE),
          median = median(d_mat, na.rm = TRUE)
        )
      ))
    },

    #' @description Plot modalities in 2D MCA space
    #' @details
    #' Performs Multiple Correspondence Analysis (MCA) on the disjunctive matrix
    #' to project modalities into a 2-dimensional space that preserves their
    #' dissimilarity structure. The first two factorial dimensions (those explaining
    #' the most variance) are used for visualization.
    #'
    #' This plot helps visualize:
    #' \itemize{
    #'   \item Spatial relationships between modalities
    #'   \item Overall structure and patterns in the data
    #'   \item Distance between modalities (closer = more similar)
    #'   \item Quality of the 2D representation (via explained variance)
    #' }
    #'
    #' @param dims Integer vector of length 2. Which factorial dimensions to plot.
    #'   Default is c(1, 2) for the first two dimensions (explaining most variance).
    #'   Can be changed to explore other dimension pairs, e.g., c(1, 3) or c(2, 3).
    #' @param point_size Numeric. Size of the points. Default is 3.
    #' @param label_size Numeric. Size of the modality labels. Default is 3.
    #' @param show_labels Logical. If TRUE (default), displays modality names next
    #'   to points. Set to FALSE for cleaner plots with many modalities.
    #'
    #' @return The ModalitiesDiceClusterer object (invisible for chaining).
    #'   A plot is displayed showing modalities as points in 2D MCA space.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #' clusterer$plot_mca()
    #'
    #' # Without labels for cleaner visualization
    #' clusterer$plot_mca(show_labels = FALSE)
    #'
    #' # Explore different dimensions
    #' clusterer$plot_mca(dims = c(2, 3))
    #' }
    plot_mca = function(dims = c(1, 2), point_size = 3, label_size = 3, show_labels = TRUE) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate dims parameter
      if (length(dims) != 2 || !is.numeric(dims)) {
        stop("dims must be a numeric vector of length 2")
      }

      # Perform MCA if not already cached
      if (is.null(private$.mca_result)) {
        private$.mca_result <- private$compute_mca()
      }

      mca <- private$.mca_result

      # Extract coordinates for the requested dimensions
      coords <- mca$coord[, dims, drop = FALSE]

      # Create plot data frame
      plot_df <- data.frame(
        Dim1 = coords[, 1],
        Dim2 = coords[, 2],
        Modality = rownames(coords),
        stringsAsFactors = FALSE
      )

      # Create base plot
      plot(plot_df$Dim1, plot_df$Dim2,
        pch = 19, cex = point_size,
        col = "steelblue",
        xlab = sprintf("Dim %d (%.1f%%)", dims[1], mca$eig[dims[1]]),
        ylab = sprintf("Dim %d (%.1f%%)", dims[2], mca$eig[dims[2]]),
        main = "MCA Projection of Modalities"
      )

      # Add grid for reference
      abline(h = 0, v = 0, col = "gray70", lty = 2)
      grid(col = "gray90", lty = 1)

      # Add labels if requested
      if (show_labels) {
        text(plot_df$Dim1, plot_df$Dim2,
          labels = plot_df$Modality,
          pos = 4, cex = label_size * 0.7, col = "gray30"
        )
      }

      # Return invisibly for chaining
      invisible(self)
    },

    #' @description Plot modalities colored by cluster assignment
    #' @details
    #' Projects modalities into 2D MCA space and colors them according to their
    #' cluster assignments. This visualization helps assess:
    #' \itemize{
    #'   \item Cluster separation: Are clusters spatially distinct?
    #'   \item Cluster cohesion: Are modalities within a cluster close together?
    #'   \item Clustering quality: Does the clustering match the natural structure?
    #'   \item Potential outliers or misclassifications
    #' }
    #'
    #' Well-separated clusters in the MCA space indicate good clustering quality.
    #' Overlapping clusters may suggest the need for fewer groups or different
    #' linkage methods.
    #'
    #' @param dims Integer vector of length 2. Which factorial dimensions to plot.
    #'   Default is c(1, 2).
    #' @param point_size Numeric. Size of the points. Default is 3.
    #' @param label_size Numeric. Size of the modality labels. Default is 3.
    #' @param show_labels Logical. If TRUE (default), displays modality names.
    #' @param colors Character vector. Custom colors for clusters. If NULL,
    #'   uses a default color palette. Length should match number of clusters.
    #' @param add_ellipses Logical. If TRUE (default), adds confidence ellipses
    #'   around each cluster to visualize cluster boundaries.
    #'
    #' @return The ModalitiesDiceClusterer object (invisible for chaining).
    #'   A plot is displayed with modalities colored by cluster.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #' clusterer$plot_clusters()
    #'
    #' # Custom colors
    #' clusterer$plot_clusters(colors = c("red", "blue", "green"))
    #'
    #' # Without ellipses
    #' clusterer$plot_clusters(add_ellipses = FALSE)
    #' }
    plot_clusters = function(dims = c(1, 2), point_size = 3, label_size = 3,
                             show_labels = TRUE, colors = NULL, add_ellipses = TRUE) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate dims parameter
      if (length(dims) != 2 || !is.numeric(dims)) {
        stop("dims must be a numeric vector of length 2")
      }

      # Perform MCA if not already cached
      if (is.null(private$.mca_result)) {
        private$.mca_result <- private$compute_mca()
      }

      mca <- private$.mca_result

      # Extract coordinates for the requested dimensions
      coords <- mca$coord[, dims, drop = FALSE]

      # Get cluster assignments
      clusters <- self$groups[rownames(coords)]

      # Default color palette if not provided
      if (is.null(colors)) {
        # Use a colorblind-friendly palette
        colors <- c(
          "#E69F00", "#56B4E9", "#009E73", "#F0E442",
          "#0072B2", "#D55E00", "#CC79A7", "#999999"
        )
        colors <- rep(colors, length.out = self$n_groups)
      }

      # Create plot data frame
      plot_df <- data.frame(
        Dim1 = coords[, 1],
        Dim2 = coords[, 2],
        Modality = rownames(coords),
        Cluster = as.factor(clusters),
        stringsAsFactors = FALSE
      )

      # Create base plot
      plot(plot_df$Dim1, plot_df$Dim2,
        pch = 19, cex = point_size,
        col = colors[as.integer(plot_df$Cluster)],
        xlab = sprintf("Dim %d (%.1f%%)", dims[1], mca$eig[dims[1]]),
        ylab = sprintf("Dim %d (%.1f%%)", dims[2], mca$eig[dims[2]]),
        main = sprintf("Modality Clustering (k=%d)", self$n_groups)
      )

      # Add grid for reference
      abline(h = 0, v = 0, col = "gray70", lty = 2)
      grid(col = "gray90", lty = 1)

      # Add confidence ellipses around clusters
      if (add_ellipses) {
        for (k in sort(unique(clusters))) {
          cluster_points <- plot_df[plot_df$Cluster == k, ]
          if (nrow(cluster_points) > 2) {
            # Compute ellipse using covariance
            tryCatch(
              {
                center <- c(mean(cluster_points$Dim1), mean(cluster_points$Dim2))
                cov_mat <- cov(cbind(cluster_points$Dim1, cluster_points$Dim2))

                # Eigendecomposition for ellipse parameters
                eig <- eigen(cov_mat)
                angle <- atan2(eig$vectors[2, 1], eig$vectors[1, 1])

                # 95% confidence ellipse (chi-square quantile)
                radius <- sqrt(qchisq(0.95, df = 2))

                # Generate ellipse points
                theta <- seq(0, 2 * pi, length.out = 100)
                ellipse <- cbind(
                  center[1] + radius * sqrt(eig$values[1]) * cos(theta) * cos(angle) -
                    radius * sqrt(eig$values[2]) * sin(theta) * sin(angle),
                  center[2] + radius * sqrt(eig$values[1]) * cos(theta) * sin(angle) +
                    radius * sqrt(eig$values[2]) * sin(theta) * cos(angle)
                )

                # Draw ellipse
                lines(ellipse, col = colors[k], lwd = 2, lty = 2)
              },
              error = function(e) {
                # Skip ellipse if computation fails (e.g., singular covariance)
                invisible(NULL)
              }
            )
          }
        }
      }

      # Add labels if requested
      if (show_labels) {
        text(plot_df$Dim1, plot_df$Dim2,
          labels = plot_df$Modality,
          pos = 4, cex = label_size * 0.7, col = "gray30"
        )
      }

      # Add legend
      legend("topright",
        legend = paste("Cluster", sort(unique(clusters))),
        col = colors[sort(unique(clusters))],
        pch = 19, cex = 0.8, bg = "white"
      )

      # Return invisibly for chaining
      invisible(self)
    },

    #' @description Plot modalities with illustrative variable projection
    #' @details
    #' Projects both the clustered modalities and an illustrative (supplementary)
    #' variable into the same 2D MCA space. This helps understand:
    #' \itemize{
    #'   \item How the illustrative variable relates to existing clusters
    #'   \item Which clusters the illustrative modalities are closest to
    #'   \item Predictive relationships between variables
    #'   \item Whether the illustrative variable would cluster similarly
    #' }
    #'
    #' The illustrative variable is projected passively (without affecting the
    #' MCA solution) and shown with different symbols (triangles) to distinguish
    #' from the active modalities (circles).
    #'
    #' @param illus Factor or one-column data.frame. The illustrative variable
    #'   to project onto the MCA space. Must have the same number of observations
    #'   as the original fitted data.
    #' @param dims Integer vector of length 2. Which factorial dimensions to plot.
    #'   Default is c(1, 2).
    #' @param point_size Numeric. Size of the points. Default is 3.
    #' @param label_size Numeric. Size of the labels. Default is 3.
    #' @param show_labels Logical. If TRUE (default), displays modality names.
    #' @param colors Character vector. Custom colors for clusters. If NULL,
    #'   uses default palette.
    #' @param illus_color Character. Color for illustrative variable points.
    #'   Default is "red".
    #'
    #' @return The ModalitiesDiceClusterer object (invisible for chaining).
    #'   A plot is displayed with both clustered modalities and illustrative
    #'   variable projections.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Project an illustrative variable
    #' illus_var <- factor(sample(c("Low", "High"), nrow(data), replace = TRUE))
    #' clusterer$plot_with_illustrative(illus_var)
    #'
    #' # Custom styling
    #' clusterer$plot_with_illustrative(illus_var,
    #'                                  illus_color = "purple",
    #'                                  show_labels = FALSE)
    #' }
    plot_with_illustrative = function(illus, dims = c(1, 2), point_size = 3,
                                      label_size = 3, show_labels = TRUE,
                                      colors = NULL, illus_color = "red") {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate dims parameter
      if (length(dims) != 2 || !is.numeric(dims)) {
        stop("dims must be a numeric vector of length 2")
      }

      # Process illustrative variable
      if (is.data.frame(illus)) {
        if (ncol(illus) != 1) stop("illus data.frame must have a single column")
        illus_vec <- illus[[1]]
        illus_name <- names(illus)[1]
      } else {
        illus_vec <- illus
        illus_name <- "Illustrative"
      }
      illus_vec <- as.factor(illus_vec)

      # Validate length
      if (length(illus_vec) != nrow(self$data)) {
        stop(sprintf(
          "illus must have same length as fitted data (%d observations)",
          nrow(self$data)
        ))
      }

      # Perform MCA if not already cached
      if (is.null(private$.mca_result)) {
        private$.mca_result <- private$compute_mca()
      }

      mca <- private$.mca_result

      # Extract coordinates for active modalities
      coords <- mca$coord[, dims, drop = FALSE]
      clusters <- self$groups[rownames(coords)]

      # Project illustrative variable onto MCA space
      illus_coords <- private$project_illustrative(illus_vec, dims)

      # Default color palette if not provided
      if (is.null(colors)) {
        colors <- c(
          "#E69F00", "#56B4E9", "#009E73", "#F0E442",
          "#0072B2", "#D55E00", "#CC79A7", "#999999"
        )
        colors <- rep(colors, length.out = self$n_groups)
      }

      # Create plot data frame for active modalities
      plot_df <- data.frame(
        Dim1 = coords[, 1],
        Dim2 = coords[, 2],
        Modality = rownames(coords),
        Cluster = as.factor(clusters),
        Type = "Active",
        stringsAsFactors = FALSE
      )

      # Create plot data frame for illustrative modalities
      illus_df <- data.frame(
        Dim1 = illus_coords[, 1],
        Dim2 = illus_coords[, 2],
        Modality = rownames(illus_coords),
        Cluster = NA,
        Type = "Illustrative",
        stringsAsFactors = FALSE
      )

      # Combine data frames
      all_df <- rbind(plot_df, illus_df)

      # Determine plot range
      xlim <- range(all_df$Dim1)
      ylim <- range(all_df$Dim2)

      # Create base plot
      plot(xlim, ylim,
        type = "n",
        xlab = sprintf("Dim %d (%.1f%%)", dims[1], mca$eig[dims[1]]),
        ylab = sprintf("Dim %d (%.1f%%)", dims[2], mca$eig[dims[2]]),
        main = "Modality Clustering with Illustrative Variable"
      )

      # Add grid for reference
      abline(h = 0, v = 0, col = "gray70", lty = 2)
      grid(col = "gray90", lty = 1)

      # Plot active modalities (circles)
      points(plot_df$Dim1, plot_df$Dim2,
        pch = 19, cex = point_size,
        col = colors[as.integer(plot_df$Cluster)]
      )

      # Plot illustrative modalities (triangles)
      points(illus_df$Dim1, illus_df$Dim2,
        pch = 17, cex = point_size * 1.2,
        col = illus_color
      )

      # Add labels if requested
      if (show_labels) {
        # Active modality labels
        text(plot_df$Dim1, plot_df$Dim2,
          labels = plot_df$Modality,
          pos = 4, cex = label_size * 0.7, col = "gray30"
        )

        # Illustrative modality labels
        text(illus_df$Dim1, illus_df$Dim2,
          labels = illus_df$Modality,
          pos = 4, cex = label_size * 0.7, col = illus_color, font = 2
        )
      }

      # Add legend
      legend("topright",
        legend = c(paste("Cluster", sort(unique(clusters))), "Illustrative"),
        col = c(colors[sort(unique(clusters))], illus_color),
        pch = c(rep(19, self$n_groups), 17),
        cex = 0.8, bg = "white"
      )

      # Return invisibly for chaining
      invisible(self)
    },

    #' @description Get MCA coordinates as a data.frame
    #' @details
    #' Extracts the factorial coordinates from the MCA projection that are used
    #' in the plot_mca() visualization. This allows you to:
    #' \itemize{
    #'   \item Export the projection data for custom visualizations
    #'   \item Perform additional statistical analyses on the coordinates
    #'   \item Identify modalities with extreme coordinates
    #'   \item Create custom tables and reports
    #' }
    #'
    #' Each row represents one modality, and columns represent factorial dimensions.
    #' The number of dimensions returned matches the number of modalities minus 1
    #' (theoretical maximum for MCA).
    #'
    #' @param n_dims Integer. Number of dimensions to return. If NULL (default),
    #'   returns all available dimensions. Common choices are 2 (for plotting),
    #'   5-10 (for analysis), or NULL (for complete data).
    #'
    #' @return A data.frame with the following columns:
    #'   \itemize{
    #'     \item Modality: Character. Name of the modality (e.g., "var1.A")
    #'     \item Dim1, Dim2, ..., DimN: Numeric. Factorial coordinates
    #'     \item Variable: Character. Source variable name (extracted from modality)
    #'     \item Level: Character. Factor level (extracted from modality)
    #'   }
    #'   Rows are ordered by modality name for consistency.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Get first 2 dimensions (for plotting)
    #' coords_2d <- clusterer$get_mca_data(n_dims = 2)
    #' head(coords_2d)
    #'
    #' # Get all dimensions
    #' coords_all <- clusterer$get_mca_data()
    #'
    #' # Export to CSV
    #' write.csv(coords_2d, "mca_coordinates.csv", row.names = FALSE)
    #'
    #' # Find extreme modalities on first axis
    #' coords_2d[order(abs(coords_2d$Dim1), decreasing = TRUE), ][1:5, ]
    #' }
    get_mca_data = function(n_dims = NULL) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Perform MCA if not already cached
      if (is.null(private$.mca_result)) {
        private$.mca_result <- private$compute_mca()
      }

      mca <- private$.mca_result

      # Determine number of dimensions to return
      if (is.null(n_dims)) {
        n_dims <- ncol(mca$coord)
      } else {
        n_dims <- min(n_dims, ncol(mca$coord))
      }

      # Extract coordinates
      coords <- mca$coord[, 1:n_dims, drop = FALSE]

      # Create data frame
      result <- data.frame(
        Modality = rownames(coords),
        coords,
        stringsAsFactors = FALSE
      )

      # Extract variable and level from modality names
      # Modality names from model.matrix are like "var1A", "var2X", etc.
      # We need to find the variable name prefix
      result$Variable <- NA_character_
      result$Level <- NA_character_

      # Get original variable names
      var_names <- colnames(self$data)

      # For each modality, find which variable it belongs to
      for (i in 1:nrow(result)) {
        mod_name <- result$Modality[i]
        matched <- FALSE

        # Try each variable name as a prefix
        for (vn in var_names) {
          if (startsWith(mod_name, vn)) {
            result$Variable[i] <- vn
            # Level is everything after the variable name
            result$Level[i] <- substring(mod_name, nchar(vn) + 1)
            matched <- TRUE
            break
          }
        }

        # If no match found, use the whole name for both
        if (!matched) {
          result$Variable[i] <- mod_name
          result$Level[i] <- mod_name
        }
      }

      # Reorder columns: Modality, Variable, Level, then dimensions
      dim_cols <- grep("^Dim", names(result), value = TRUE)
      result <- result[, c("Modality", "Variable", "Level", dim_cols)]

      rownames(result) <- NULL
      return(result)
    },

    #' @description Get cluster visualization data as a data.frame
    #' @details
    #' Extracts the complete data behind the plot_clusters() visualization,
    #' including MCA coordinates, cluster assignments, and variance explained.
    #' This combines information from multiple sources:
    #' \itemize{
    #'   \item MCA factorial coordinates
    #'   \item Cluster assignments from hierarchical clustering
    #'   \item Modality frequencies
    #'   \item Variance explained by each dimension
    #' }
    #'
    #' @param n_dims Integer. Number of MCA dimensions to include. Default is 2
    #'   (matching the typical 2D visualization). Can be increased to include
    #'   more dimensions for analysis.
    #'
    #' @return A data.frame with the following columns:
    #'   \itemize{
    #'     \item Modality: Character. Modality name
    #'     \item Variable: Character. Source variable
    #'     \item Level: Character. Factor level
    #'     \item Cluster: Integer. Assigned cluster ID (1 to n_groups)
    #'     \item Frequency: Integer. Number of observations with this modality
    #'     \item Dim1, Dim2, ...: Numeric. MCA coordinates
    #'     \item VarExplained_Dim1, VarExplained_Dim2, ...: Numeric. % variance
    #'       explained by each dimension (repeated for each row)
    #'   }
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Get clustering visualization data
    #' cluster_data <- clusterer$get_cluster_data()
    #' head(cluster_data)
    #'
    #' # Analyze cluster composition
    #' table(cluster_data$Cluster, cluster_data$Variable)
    #'
    #' # Find modalities far from cluster centers
    #' library(dplyr)
    #' cluster_data %>%
    #'   group_by(Cluster) %>%
    #'   mutate(
    #'     dist_from_center = sqrt((Dim1 - mean(Dim1))^2 + (Dim2 - mean(Dim2))^2)
    #'   ) %>%
    #'   arrange(desc(dist_from_center))
    #'
    #' # Export for Tableau/PowerBI
    #' write.csv(cluster_data, "cluster_visualization.csv", row.names = FALSE)
    #' }
    get_cluster_data = function(n_dims = 2) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Get MCA coordinates
      mca_data <- self$get_mca_data(n_dims = n_dims)

      # Add cluster assignments (remove names)
      mca_data$Cluster <- as.integer(unname(self$groups[mca_data$Modality]))

      # Add frequencies (remove names)
      mca_data$Frequency <- as.integer(unname(self$modality_counts[mca_data$Modality]))

      # Add variance explained for each dimension
      if (is.null(private$.mca_result)) {
        private$.mca_result <- private$compute_mca()
      }
      mca <- private$.mca_result

      for (i in 1:n_dims) {
        var_col_name <- paste0("VarExplained_Dim", i)
        mca_data[[var_col_name]] <- mca$eig[i]
      }

      # Reorder columns logically
      dim_cols <- grep("^Dim[0-9]+$", names(mca_data), value = TRUE)
      var_cols <- grep("^VarExplained", names(mca_data), value = TRUE)
      other_cols <- setdiff(names(mca_data), c(dim_cols, var_cols))

      mca_data <- mca_data[, c(other_cols, dim_cols, var_cols)]

      # Sort by cluster, then frequency (descending)
      mca_data <- mca_data[order(mca_data$Cluster, -mca_data$Frequency), ]
      rownames(mca_data) <- NULL

      return(mca_data)
    },

    #' @description Get illustrative variable projection data as a data.frame
    #' @details
    #' Extracts the complete data behind the plot_with_illustrative() visualization,
    #' including both active (clustered) modalities and illustrative (supplementary)
    #' modalities projected into the same MCA space.
    #'
    #' This method is useful for:
    #' \itemize{
    #'   \item Comparing illustrative variables to existing clusters
    #'   \item Computing custom distance metrics
    #'   \item Creating overlay visualizations
    #'   \item Generating detailed reports
    #' }
    #'
    #' @param illus Factor or one-column data.frame. The illustrative variable
    #'   to project. Must have the same length as the fitted data.
    #' @param n_dims Integer. Number of MCA dimensions to include. Default is 2.
    #'
    #' @return A data.frame with the following columns:
    #'   \itemize{
    #'     \item Modality: Character. Modality name
    #'     \item Type: Character. Either "Active" (original clustered modalities)
    #'       or "Illustrative" (supplementary variable)
    #'     \item Variable: Character. Source variable name
    #'     \item Level: Character. Factor level
    #'     \item Cluster: Integer. Cluster ID (NA for illustrative modalities)
    #'     \item Frequency: Integer. Observation count (for active modalities only)
    #'     \item Dim1, Dim2, ...: Numeric. MCA coordinates
    #'     \item DistToActive_Min: Numeric. Minimum distance to any active modality
    #'       (only for illustrative)
    #'     \item ClosestCluster: Integer. ID of nearest cluster (only for illustrative)
    #'   }
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Create illustrative variable
    #' illus_var <- factor(sample(c("Low", "High"), nrow(data), replace = TRUE))
    #'
    #' # Get projection data
    #' illus_data <- clusterer$get_illustrative_data(illus_var)
    #'
    #' # View illustrative modalities only
    #' illus_data[illus_data$Type == "Illustrative", ]
    #'
    #' # Find which cluster each illustrative level is closest to
    #' subset(illus_data, Type == "Illustrative",
    #'        select = c(Modality, ClosestCluster, DistToActive_Min))
    #'
    #' # Compute custom distances
    #' active <- illus_data[illus_data$Type == "Active", ]
    #' illustrative <- illus_data[illus_data$Type == "Illustrative", ]
    #' dist_matrix <- as.matrix(dist(rbind(
    #'   active[, c("Dim1", "Dim2")],
    #'   illustrative[, c("Dim1", "Dim2")]
    #' )))
    #' }
    get_illustrative_data = function(illus, n_dims = 2) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate n_dims
      if (!is.numeric(n_dims) || n_dims < 1) {
        stop("n_dims must be a positive integer")
      }

      # Process illustrative variable
      if (is.data.frame(illus)) {
        if (ncol(illus) != 1) stop("illus data.frame must have a single column")
        illus_vec <- illus[[1]]
        illus_name <- names(illus)[1]
      } else {
        illus_vec <- illus
        illus_name <- "Illustrative"
      }
      illus_vec <- as.factor(illus_vec)

      # Validate length
      if (length(illus_vec) != nrow(self$data)) {
        stop(sprintf(
          "illus must have same length as fitted data (%d observations)",
          nrow(self$data)
        ))
      }

      # Get active modalities data
      active_data <- self$get_cluster_data(n_dims = n_dims)
      active_data$Type <- "Active"

      # Get illustrative projection
      illus_coords <- private$project_illustrative(illus_vec, 1:n_dims)

      # Create illustrative data frame
      illus_data <- data.frame(
        Modality = rownames(illus_coords),
        Type = "Illustrative",
        Variable = illus_name,
        Level = rownames(illus_coords),
        Cluster = NA_integer_,
        Frequency = NA_integer_,
        stringsAsFactors = FALSE
      )

      # Add coordinates
      for (i in 1:n_dims) {
        illus_data[[paste0("Dim", i)]] <- illus_coords[, i]
      }

      # Add variance explained columns (same as active)
      var_cols <- grep("^VarExplained", names(active_data), value = TRUE)
      for (vc in var_cols) {
        illus_data[[vc]] <- active_data[[vc]][1] # Same for all rows
      }

      # Compute distances from illustrative to active modalities
      dim_cols <- paste0("Dim", 1:n_dims)

      illus_data$DistToActive_Min <- NA_real_
      illus_data$ClosestCluster <- NA_integer_

      for (i in 1:nrow(illus_data)) {
        # Compute Euclidean distances to all active modalities
        illus_point <- as.numeric(illus_data[i, dim_cols])

        distances <- apply(active_data[, dim_cols], 1, function(active_point) {
          sqrt(sum((active_point - illus_point)^2))
        })

        # Find minimum distance and corresponding cluster
        min_idx <- which.min(distances)
        illus_data$DistToActive_Min[i] <- distances[min_idx]
        illus_data$ClosestCluster[i] <- active_data$Cluster[min_idx]
      }

      # Combine active and illustrative data
      # Ensure column order matches
      common_cols <- intersect(names(active_data), names(illus_data))
      extra_active_cols <- setdiff(names(active_data), names(illus_data))
      extra_illus_cols <- setdiff(names(illus_data), names(active_data))

      # Add missing columns to each
      for (col in extra_illus_cols) {
        active_data[[col]] <- NA
      }
      for (col in extra_active_cols) {
        illus_data[[col]] <- NA
      }

      # Reorder columns to match
      all_cols <- c(
        "Modality", "Type", "Variable", "Level", "Cluster", "Frequency",
        dim_cols, var_cols, "DistToActive_Min", "ClosestCluster"
      )
      all_cols <- all_cols[all_cols %in% c(names(active_data), names(illus_data))]

      result <- rbind(
        active_data[, all_cols],
        illus_data[, all_cols]
      )

      rownames(result) <- NULL
      return(result)
    },

    #' @description Get cluster statistics table
    #' @details
    #' Generates a comprehensive summary table for each cluster, including:
    #' \itemize{
    #'   \item Cluster size (number of modalities)
    #'   \item Total frequency (sum of modality frequencies)
    #'   \item Average frequency per modality
    #'   \item Coordinates of cluster centroid in MCA space
    #'   \item Within-cluster variance
    #'   \item List of modalities in the cluster
    #' }
    #'
    #' This table is useful for:
    #' \itemize{
    #'   \item Cluster interpretation and labeling
    #'   \item Quality assessment (balanced sizes, low variance)
    #'   \item Reporting and documentation
    #'   \item Comparing different clustering solutions
    #' }
    #'
    #' @param n_dims Integer. Number of MCA dimensions to include in centroid
    #'   coordinates. Default is 2.
    #'
    #' @return A data.frame with one row per cluster and the following columns:
    #'   \itemize{
    #'     \item Cluster: Integer. Cluster ID
    #'     \item Size: Integer. Number of modalities in cluster
    #'     \item TotalFrequency: Integer. Sum of frequencies of all modalities
    #'     \item AvgFrequency: Numeric. Mean frequency per modality
    #'     \item Centroid_Dim1, Centroid_Dim2, ...: Numeric. Cluster center coordinates
    #'     \item WithinVariance: Numeric. Average squared distance from centroid
    #'     \item Modalities: Character. Comma-separated list of modality names
    #'   }
    #'   Rows are ordered by cluster ID.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Get cluster statistics
    #' stats <- clusterer$get_cluster_stats()
    #' print(stats)
    #'
    #' # Identify largest cluster
    #' stats[which.max(stats$Size), ]
    #'
    #' # Find most cohesive cluster (lowest variance)
    #' stats[which.min(stats$WithinVariance), ]
    #'
    #' # Compare cluster qualities
    #' stats$QualityScore <- stats$TotalFrequency / stats$WithinVariance
    #' stats[order(-stats$QualityScore), ]
    #' }
    get_cluster_stats = function(n_dims = 2) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Get cluster data with coordinates
      cluster_data <- self$get_cluster_data(n_dims = n_dims)

      dim_cols <- paste0("Dim", 1:n_dims)

      # Compute statistics for each cluster
      stats_list <- lapply(sort(unique(cluster_data$Cluster)), function(k) {
        cluster_mods <- cluster_data[cluster_data$Cluster == k, ]

        # Basic counts
        size <- nrow(cluster_mods)
        total_freq <- sum(cluster_mods$Frequency, na.rm = TRUE)
        avg_freq <- mean(cluster_mods$Frequency, na.rm = TRUE)

        # Centroid (mean coordinates)
        centroid <- colMeans(cluster_mods[, dim_cols, drop = FALSE])

        # Within-cluster variance (average squared distance from centroid)
        distances <- apply(cluster_mods[, dim_cols, drop = FALSE], 1, function(point) {
          sum((point - centroid)^2)
        })
        within_var <- mean(distances)

        # Modality list
        modality_list <- paste(cluster_mods$Modality, collapse = ", ")

        # Build result row
        result <- data.frame(
          Cluster = k,
          Size = size,
          TotalFrequency = total_freq,
          AvgFrequency = avg_freq,
          stringsAsFactors = FALSE
        )

        # Add centroid coordinates
        for (i in 1:n_dims) {
          result[[paste0("Centroid_Dim", i)]] <- centroid[i]
        }

        result$WithinVariance <- within_var
        result$Modalities <- modality_list

        return(result)
      })

      # Combine into single data frame
      stats_df <- do.call(rbind, stats_list)
      rownames(stats_df) <- NULL

      return(stats_df)
    },

    #' @description Plot average silhouette scores across different k values (elbow plot)
    #' @details
    #' Computes and visualizes average silhouette coefficients for different numbers
    #' of clusters (k) to help identify the optimal clustering solution. This creates
    #' an "elbow plot" using silhouette scores as the quality metric.
    #'
    #' The silhouette coefficient measures how similar a modality is to its own cluster
    #' compared to other clusters. Values range from -1 to 1, with higher values
    #' indicating better clustering quality.
    #'
    #' The plot displays:
    #' \itemize{
    #'   \item A line/curve showing average silhouette vs. number of clusters
    #'   \item Points marking each k value tested
    #'   \item The optimal k (highest silhouette) highlighted in red
    #'   \item Grid lines for easier reading
    #' }
    #'
    #' Use this plot to:
    #' \itemize{
    #'   \item Identify the optimal number of clusters
    #'   \item Compare clustering quality across different k values
    #'   \item Detect when adding more clusters stops improving quality
    #' }
    #'
    #' @param min_k Integer. Minimum number of clusters to test. Default is 2.
    #'   Must be >= 2.
    #' @param max_k Integer. Maximum number of clusters to test. Default is 10.
    #'   If NULL, uses the number of modalities minus 1.
    #' @param line_color Character. Color for the main line. Default is "steelblue".
    #' @param point_color Character. Color for the points. Default is "steelblue".
    #' @param optimal_color Character. Color for highlighting optimal k. Default is "red".
    #' @param mark_optimal Logical. If TRUE (default), marks the optimal k with
    #'   a larger red point and vertical line.
    #'
    #' @return The ModalitiesDiceClusterer object (invisible for chaining).
    #'   A plot is displayed showing average silhouette scores across k values.
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Basic silhouette comparison plot
    #' clusterer$plot_silhouette()
    #'
    #' # Test specific range of k values
    #' clusterer$plot_silhouette(min_k = 2, max_k = 8)
    #'
    #' # Custom styling
    #' clusterer$plot_silhouette(
    #'   line_color = "darkblue",
    #'   optimal_color = "darkred"
    #' )
    #'
    #' # Without marking optimal
    #' clusterer$plot_silhouette(mark_optimal = FALSE)
    #' }
    plot_silhouette = function(min_k = 2, max_k = 10,
                               line_color = "steelblue",
                               point_color = "steelblue",
                               optimal_color = "red",
                               mark_optimal = TRUE) {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Validate parameters
      if (!is.numeric(min_k) || min_k < 2) stop("min_k must be >= 2")

      # Determine max_k
      n_modalities <- ncol(self$disj)
      if (is.null(max_k)) {
        max_k <- min(n_modalities - 1, 10)
      } else {
        if (!is.numeric(max_k) || max_k < min_k) {
          stop("max_k must be >= min_k")
        }
        if (max_k >= n_modalities) {
          warning(sprintf(
            "max_k (%d) >= number of modalities (%d). Setting max_k to %d",
            max_k, n_modalities, n_modalities - 1
          ))
          max_k <- n_modalities - 1
        }
      }

      # Store current state
      original_k <- self$n_groups
      original_groups <- self$groups

      # Compute silhouette for each k
      k_values <- min_k:max_k
      avg_silhouettes <- numeric(length(k_values))

      cat(sprintf("Computing silhouette scores for k = %d to %d...\n", min_k, max_k))

      for (i in seq_along(k_values)) {
        k <- k_values[i]

        # Cut tree to k clusters
        self$groups <- stats::cutree(self$hclust, k = k)
        self$n_groups <- k

        # Compute silhouette
        sil_result <- private$compute_silhouette()
        avg_silhouettes[i] <- sil_result$avg_silhouette

        cat(sprintf("  k=%d: avg silhouette = %.4f\n", k, avg_silhouettes[i]))
      }

      # Restore original state
      self$n_groups <- original_k
      self$groups <- original_groups
      private$compute_group_stats()

      # Find optimal k
      optimal_idx <- which.max(avg_silhouettes)
      optimal_k <- k_values[optimal_idx]
      optimal_sil <- avg_silhouettes[optimal_idx]

      cat(sprintf("\nOptimal k = %d (avg silhouette = %.4f)\n", optimal_k, optimal_sil))

      # Create plot
      par(mar = c(5, 4, 4, 2) + 0.1)

      plot(k_values, avg_silhouettes,
        type = "b",
        col = line_color,
        pch = 19,
        lwd = 2,
        xlab = "Number of Clusters (k)",
        ylab = "Average Silhouette Coefficient",
        main = "Silhouette Analysis for Optimal k",
        xaxt = "n",
        ylim = c(min(0, min(avg_silhouettes)), max(avg_silhouettes) * 1.1)
      )

      # Add x-axis with integer values
      axis(1, at = k_values, labels = k_values)

      # Add grid
      grid(col = "gray90", lty = 1)

      # Add horizontal line at 0
      abline(h = 0, col = "gray70", lty = 2)

      # Mark optimal k
      if (mark_optimal) {
        # Vertical line at optimal k
        abline(v = optimal_k, col = optimal_color, lty = 2, lwd = 1.5)

        # Larger point at optimal k
        points(optimal_k, optimal_sil,
          col = optimal_color,
          pch = 19,
          cex = 2
        )

        # Add text label
        text(optimal_k, optimal_sil,
          labels = sprintf("Optimal\nk=%d", optimal_k),
          pos = 3,
          col = optimal_color,
          font = 2,
          cex = 0.9
        )
      }

      # Add legend with interpretation
      legend_text <- c(
        sprintf("Optimal k: %d", optimal_k),
        sprintf("Silhouette: %.3f", optimal_sil),
        "",
        "Interpretation:",
        "  > 0.5: Strong structure",
        "  0.25-0.5: Weak structure",
        "  < 0.25: No structure"
      )

      legend("topright",
        legend = legend_text,
        bg = "white",
        cex = 0.8
      )

      # Return invisibly for chaining
      invisible(self)
    },

    #' @description Get silhouette data as a data.frame
    #' @details
    #' Computes and returns silhouette coefficients for all modalities.
    #' The silhouette coefficient for modality i is:
    #' s(i) = (b(i) - a(i)) / max(a(i), b(i))
    #' where:
    #' \itemize{
    #'   \item a(i) = average distance to modalities in the same cluster
    #'   \item b(i) = minimum average distance to modalities in other clusters
    #' }
    #'
    #' @return A data.frame with the following columns:
    #'   \itemize{
    #'     \item Modality: Character. Modality name
    #'     \item Cluster: Integer. Assigned cluster ID
    #'     \item Silhouette: Numeric. Silhouette coefficient (-1 to 1)
    #'     \item AvgDistWithin: Numeric. Average distance within cluster (a(i))
    #'     \item MinAvgDistOther: Numeric. Min average distance to other clusters (b(i))
    #'     \item NearestCluster: Integer. ID of nearest other cluster
    #'   }
    #'   Rows are sorted by cluster and silhouette coefficient (descending).
    #'
    #' @examples
    #' \dontrun{
    #' clusterer$fit(data)
    #'
    #' # Get silhouette data
    #' sil_data <- clusterer$get_silhouette_data()
    #' head(sil_data)
    #'
    #' # Find poorly clustered modalities
    #' sil_data[sil_data$Silhouette < 0, ]
    #'
    #' # Summary statistics
    #' aggregate(Silhouette ~ Cluster, data = sil_data, FUN = mean)
    #' }
    get_silhouette_data = function() {
      # Verify model has been fitted
      if (!self$fitted) stop("Model not fitted. Call $fit(data) first")

      # Compute silhouette coefficients
      sil_result <- private$compute_silhouette()

      # Create data frame
      result <- data.frame(
        Modality = names(sil_result$silhouette),
        Cluster = sil_result$cluster,
        Silhouette = sil_result$silhouette,
        AvgDistWithin = sil_result$avg_dist_within,
        MinAvgDistOther = sil_result$min_avg_dist_other,
        NearestCluster = sil_result$nearest_cluster,
        stringsAsFactors = FALSE
      )

      # Sort by cluster and silhouette (descending)
      result <- result[order(result$Cluster, -result$Silhouette), ]
      rownames(result) <- NULL

      return(result)
    }
  ),
  private = list(
    .summary_results = NULL,
    # Cache for MCA result
    .mca_result = NULL,

    # verify initialization entries and data sanity
    verify_entries = function(data = NULL) {
      if (is.null(self$n_groups) || !is.numeric(self$n_groups) || length(self$n_groups) != 1 || self$n_groups < 1) {
        stop("n_groups must be a single positive integer")
      }
      allowed_linkage <- c("ward.D2", "single", "complete", "average", "ward.D")
      if (!is.character(self$linkage) || !(self$linkage %in% allowed_linkage)) {
        stop("invalid linkage; choose one of: ", paste(allowed_linkage, collapse = ", "))
      }
      allowed_dissim <- c("dice", "cramer")
      if (!is.character(self$dissimilarity) || !(self$dissimilarity %in% allowed_dissim)) {
        stop("invalid dissimilarity; choose one of: ", paste(allowed_dissim, collapse = ", "))
      }
      if (!is.numeric(self$n_bins) || self$n_bins < 2) stop("n_bins must be an integer >= 2")
      if (!is.null(data)) {
        if (!is.data.frame(data) && !is.matrix(data)) stop("data must be a data.frame or matrix of categorical variables")
        if (nrow(data) < 1) stop("data must have at least one observation")
        if (ncol(data) < 1) stop("data must have at least one variable")
      }
      invisible(TRUE)
    },

    # basic dice distance (squared-type) between two numeric vectors
    dice = function(m1, m2) {
      # ensure numeric
      m1 <- as.numeric(m1)
      m2 <- as.numeric(m2)
      return(0.5 * sum((m1 - m2)^2))
    },

    # Discretize numeric columns into quantile bins (returns a copy)
    discretize_numeric = function(df, n_bins) {
      df_copy <- as.data.frame(df)
      is_num <- sapply(df_copy, is.numeric)
      if (any(is_num)) {
        for (nm in names(df_copy)[is_num]) {
          v <- df_copy[[nm]]
          # if binary 0/1, keep as is but coerce to factor
          uniq <- unique(na.omit(v))
          if (length(uniq) <= 2 && all(uniq %in% c(0, 1))) {
            df_copy[[nm]] <- factor(ifelse(is.na(v), NA, v))
            next
          }
          # use quantile cuts, handle duplicates gracefully
          qs <- unique(stats::quantile(v, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
          if (length(qs) <= 2) {
            df_copy[[nm]] <- factor(ifelse(is.na(v), NA, as.character(round(v, 3))))
          } else {
            # cut with include.lowest to ensure proper binning
            df_copy[[nm]] <- cut(v, breaks = qs, include.lowest = TRUE, labels = paste0("Q", seq_len(length(qs) - 1)))
          }
        }
      }
      return(df_copy)
    },

    # compute group-centered statistics: centers, inertia decomposition, contributions
    compute_group_stats = function() {
      if (is.null(self$disj)) {
        return(NULL)
      }
      n <- nrow(self$disj)
      p <- ncol(self$disj) # number of modalities

      # Grand center: mean profile across all observations
      # For disjunctive table, this is the relative frequency of each modality
      grand_center <- colSums(self$disj) / n

      # Group centers: for each cluster of modalities, compute the mean profile
      # across observations (average of the disjunctive columns in that cluster)
      groups <- self$groups
      uniq_g <- sort(unique(groups))
      group_centers <- lapply(uniq_g, function(g) {
        cols <- names(groups)[groups == g]
        if (length(cols) == 0) {
          return(rep(0, ncol(self$disj)))
        }
        rowMeans(self$disj[, cols, drop = FALSE])
      })
      names(group_centers) <- as.character(uniq_g)

      # ===== INERTIA DECOMPOSITION =====
      # Total inertia: sum of squared distances from each modality column to grand center
      # This measures the total variability in the disjunctive table
      # Formula: sum over all modalities j of: sum over all observations i of: (X[i,j]/n - p_j)^2
      # where p_j = grand_center[j] is the marginal frequency of modality j

      total_inertia <- 0
      for (j in seq_len(p)) {
        # For each modality, compute squared deviation of its profile from expected frequency
        # self$disj[, j] is binary (0/1), grand_center[j] is its mean
        total_inertia <- total_inertia + sum((self$disj[, j] - grand_center[j])^2) / n
      }

      # Between-group inertia: measures separation between clusters
      # For each cluster, compute the centroid (mean of modality profiles in that cluster)
      # Then measure squared distance from each cluster centroid to the grand center
      # Weight by the number of modalities in each cluster
      between_inertia <- 0
      for (g in uniq_g) {
        # Get modalities in this cluster
        cols <- names(groups)[groups == g]
        n_modalities_in_cluster <- length(cols)

        if (n_modalities_in_cluster > 0) {
          # Cluster centroid: mean of the profiles of modalities in this cluster
          # For each observation, average the values across modalities in this cluster
          cluster_centroid <- rowMeans(self$disj[, cols, drop = FALSE])

          # Squared distance from cluster centroid to grand center
          # Sum across all modalities that belong to this cluster
          for (col in cols) {
            between_inertia <- between_inertia + sum((cluster_centroid - grand_center[col])^2) / n
          }
        }
      }

      # Within-group inertia: measures compactness within clusters
      # For each cluster, measure how much modalities vary around their cluster centroid
      within_inertia <- 0
      for (g in uniq_g) {
        cols <- names(groups)[groups == g]

        if (length(cols) > 0) {
          # Cluster centroid (same as above)
          cluster_centroid <- rowMeans(self$disj[, cols, drop = FALSE])

          # For each modality in this cluster, compute squared distance to centroid
          for (col in cols) {
            within_inertia <- within_inertia + sum((self$disj[, col] - cluster_centroid)^2) / n
          }
        }
      }

      # Verify decomposition: total = between + within (within numerical precision)
      # If this doesn't hold, there's a mathematical error
      inertia <- c(
        total = as.numeric(total_inertia),
        between = as.numeric(between_inertia),
        within = as.numeric(within_inertia)
      )

      # Modality contributions: how much each modality contributes to total inertia
      # This helps identify which modalities drive the clustering structure
      contrib <- numeric(p)
      names(contrib) <- colnames(self$disj)
      for (j in seq_len(p)) {
        # Contribution = squared deviation from grand center
        contrib[j] <- sum((self$disj[, j] - grand_center[j])^2) / n
      }

      # Relative contributions (as percentages of total inertia)
      contrib_rel <- contrib / total_inertia

      # Store results
      res <- list(
        group_centers = group_centers,
        inertia = inertia,
        modality_contribution = contrib,
        modality_relcontrib = contrib_rel
      )
      private$.summary_results <- res
      return(res)
    },
    validate_new_data = function(new_data) {
      if (!is.data.frame(new_data) && !is.matrix(new_data)) {
        stop("new_data must be a data.frame or matrix")
      }
      if (ncol(new_data) != ncol(self$data)) {
        stop(sprintf("new_data must have the same number of variables (%d) as the fitted data (%d)", ncol(self$data), ncol(new_data)))
      }
    },

    # Compute Multiple Correspondence Analysis
    # Performs MCA on the disjunctive matrix to reduce dimensionality and
    # enable 2D visualization. MCA is essentially PCA on the disjunctive table.
    compute_mca = function() {
      # Center the disjunctive matrix (mean-center each column)
      # This is required for PCA/MCA
      n <- nrow(self$disj)
      p <- ncol(self$disj)

      # Column means (modal frequency)
      col_means <- colSums(self$disj) / n

      # Centered matrix
      X_centered <- scale(self$disj, center = col_means, scale = FALSE)

      # Compute SVD (Singular Value Decomposition)
      # This is computationally efficient for MCA
      svd_result <- svd(X_centered / sqrt(n))

      # Factorial coordinates for modalities (V matrix from SVD)
      coord <- svd_result$v %*% diag(svd_result$d)
      rownames(coord) <- colnames(self$disj)
      colnames(coord) <- paste0("Dim", 1:ncol(coord))

      # Eigenvalues (squared singular values)
      eigenvalues <- svd_result$d^2

      # Percentage of variance explained
      total_var <- sum(eigenvalues)
      var_explained <- 100 * eigenvalues / total_var

      # Return MCA result
      list(
        coord = coord,
        eig = var_explained,
        values = eigenvalues
      )
    },

    # Project illustrative variable onto MCA space
    # Projects an illustrative (supplementary) variable onto the existing MCA
    # factorial space without modifying the MCA solution.
    project_illustrative = function(illus_vec, dims) {
      # Build disjunctive matrix for illustrative variable
      illus_disj <- stats::model.matrix(~ illus_vec - 1)
      colnames(illus_disj) <- levels(illus_vec)

      # Get MCA coordinates for observations (U matrix from SVD)
      # These are the row coordinates in factorial space
      n <- nrow(self$disj)
      col_means <- colSums(self$disj) / n
      X_centered <- scale(self$disj, center = col_means, scale = FALSE)
      svd_result <- svd(X_centered / sqrt(n))

      # Observation coordinates
      obs_coord <- svd_result$u %*% diag(svd_result$d)

      # For each level of illustrative variable, compute barycenter
      # (weighted mean of observation coordinates)
      illus_levels <- levels(illus_vec)
      illus_coord <- matrix(0, nrow = length(illus_levels), ncol = length(dims))
      rownames(illus_coord) <- illus_levels
      colnames(illus_coord) <- paste0("Dim", dims)

      for (i in seq_along(illus_levels)) {
        # Find observations with this level
        idx <- which(illus_vec == illus_levels[i])

        # Compute barycenter (mean position in factorial space)
        if (length(idx) > 0) {
          illus_coord[i, ] <- colMeans(obs_coord[idx, dims, drop = FALSE])
        }
      }

      return(illus_coord)
    },

    # Compute silhouette coefficients
    # Computes silhouette coefficients for all modalities based on the
    # dissimilarity matrix. The silhouette measures how well each modality
    # fits within its assigned cluster.
    compute_silhouette = function() {
      # Get distance matrix (sqrt of squared distances)
      d_mat <- sqrt(self$d2)
      n <- nrow(d_mat)
      modalities <- colnames(d_mat)
      clusters <- self$groups

      # Initialize result vectors
      silhouette <- numeric(n)
      avg_dist_within <- numeric(n)
      min_avg_dist_other <- numeric(n)
      nearest_cluster <- integer(n)
      names(silhouette) <- modalities
      names(avg_dist_within) <- modalities
      names(min_avg_dist_other) <- modalities
      names(nearest_cluster) <- modalities

      # Compute silhouette for each modality
      for (i in 1:n) {
        mod_name <- modalities[i]
        current_cluster <- clusters[mod_name]

        # Indices of modalities in same cluster
        same_cluster <- which(clusters == current_cluster)

        # a(i): average distance to other modalities in same cluster
        if (length(same_cluster) == 1) {
          # Singleton cluster: silhouette = 0 by convention
          a_i <- 0
          b_i <- 0
          silhouette[i] <- 0
          avg_dist_within[i] <- 0
          min_avg_dist_other[i] <- 0
          nearest_cluster[i] <- NA
          next
        } else {
          # Exclude self from average
          same_cluster_other <- same_cluster[same_cluster != i]
          a_i <- mean(d_mat[i, same_cluster_other])
        }

        # b(i): minimum average distance to modalities in other clusters
        other_clusters <- setdiff(unique(clusters), current_cluster)

        if (length(other_clusters) == 0) {
          # Only one cluster exists
          b_i <- 0
          silhouette[i] <- 0
          nearest_clust <- NA
        } else {
          avg_dist_to_others <- sapply(other_clusters, function(k) {
            other_cluster_idx <- which(clusters == k)
            mean(d_mat[i, other_cluster_idx])
          })

          min_idx <- which.min(avg_dist_to_others)
          b_i <- avg_dist_to_others[min_idx]
          nearest_clust <- other_clusters[min_idx]

          # Compute silhouette coefficient
          silhouette[i] <- (b_i - a_i) / max(a_i, b_i)
        }

        avg_dist_within[i] <- a_i
        min_avg_dist_other[i] <- b_i
        nearest_cluster[i] <- nearest_clust
      }

      # Compute overall average silhouette
      avg_sil <- mean(silhouette[!is.na(silhouette)])

      return(list(
        silhouette = silhouette,
        cluster = clusters,
        avg_silhouette = avg_sil,
        avg_dist_within = avg_dist_within,
        min_avg_dist_other = min_avg_dist_other,
        nearest_cluster = nearest_cluster
      ))
    }
  )
)
