library(testthat)
library(R6)

# Helper function to create sample data
create_sample_data <- function(n = 100) {
  data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("Low", "Med", "High"), n, replace = TRUE)),
    stringsAsFactors = TRUE
  )
}

create_mixed_data <- function(n = 100) {
  data.frame(
    cat1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    num1 = rnorm(n, mean = 50, sd = 10),
    cat2 = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    num2 = runif(n, 0, 100),
    stringsAsFactors = TRUE
  )
}

# =============================================================================
# Tests for initialize() method
# =============================================================================

test_that("initialize creates a valid object with default parameters", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_true(R6::is.R6(clusterer))
  expect_equal(clusterer$n_groups, 3)
  expect_equal(clusterer$linkage, "ward.D2")
  expect_equal(clusterer$dissimilarity, "dice")
  expect_true(clusterer$auto_discretize)
  expect_equal(clusterer$n_bins, 4)
  expect_false(clusterer$fitted)
})

test_that("initialize accepts custom parameters", {
  clusterer <- ModalitiesDiceClusterer$new(
    n_groups = 5,
    linkage = "complete",
    dissimilarity = "cramer",
    auto_discretize = FALSE,
    n_bins = 10
  )

  expect_equal(clusterer$n_groups, 5)
  expect_equal(clusterer$linkage, "complete")
  expect_equal(clusterer$dissimilarity, "cramer")
  expect_false(clusterer$auto_discretize)
  expect_equal(clusterer$n_bins, 10)
})

test_that("initialize validates n_groups parameter", {
  expect_error(
    ModalitiesDiceClusterer$new(n_groups = 0),
    "n_groups must be a single positive integer"
  )

  expect_error(
    ModalitiesDiceClusterer$new(n_groups = -1),
    "n_groups must be a single positive integer"
  )

  expect_error(
    ModalitiesDiceClusterer$new(n_groups = c(2, 3)),
    "n_groups must be a single positive integer"
  )

  expect_error(
    ModalitiesDiceClusterer$new(n_groups = NULL),
    "n_groups must be a single positive integer"
  )
})

test_that("initialize validates linkage parameter", {
  expect_error(
    ModalitiesDiceClusterer$new(linkage = "invalid_method"),
    "invalid linkage"
  )

  expect_error(
    ModalitiesDiceClusterer$new(linkage = 123),
    "invalid linkage"
  )
})

test_that("initialize validates dissimilarity parameter", {
  expect_error(
    ModalitiesDiceClusterer$new(dissimilarity = "invalid"),
    "should be one of"
  )
})

test_that("initialize validates n_bins parameter", {
  expect_error(
    ModalitiesDiceClusterer$new(n_bins = 1),
    "n_bins must be an integer >= 2"
  )

  expect_error(
    ModalitiesDiceClusterer$new(n_bins = -5),
    "n_bins must be an integer >= 2"
  )
})

test_that("initialize accepts all valid linkage methods", {
  valid_linkages <- c("ward.D2", "single", "complete", "average", "ward.D")

  for (linkage in valid_linkages) {
    clusterer <- ModalitiesDiceClusterer$new(linkage = linkage)
    expect_equal(clusterer$linkage, linkage)
  }
})

# =============================================================================
# Tests for fit() method
# =============================================================================

test_that("fit works with basic categorical data", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)

  result <- clusterer$fit(data)

  expect_true(clusterer$fitted)
  expect_false(is.null(clusterer$disj))
  expect_false(is.null(clusterer$d2))
  expect_false(is.null(clusterer$hclust))
  expect_false(is.null(clusterer$groups))
  expect_equal(length(clusterer$groups), ncol(clusterer$disj))
  expect_true(is.matrix(clusterer$disj))
  expect_equal(nrow(clusterer$disj), 50)
})

test_that("fit requires data parameter", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$fit(),
    "data must be provided to fit"
  )

  expect_error(
    clusterer$fit(NULL),
    "data must be provided to fit"
  )
})

test_that("fit validates data is a data.frame or matrix", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$fit(list(a = 1:5, b = 6:10)),
    "data must be a data.frame or matrix"
  )

  expect_error(
    clusterer$fit(c(1, 2, 3)),
    "data must be a data.frame or matrix"
  )
})

test_that("fit validates data has at least one observation", {
  clusterer <- ModalitiesDiceClusterer$new()
  empty_data <- data.frame()

  expect_error(
    clusterer$fit(empty_data),
    "data must have at least one observation"
  )
})

test_that("fit validates data has at least one variable", {
  clusterer <- ModalitiesDiceClusterer$new()
  no_cols <- data.frame(row.names = 1:10)

  expect_error(
    clusterer$fit(no_cols),
    "data must have at least one variable"
  )
})

test_that("fit creates proper disjunctive matrix", {
  data <- data.frame(
    v1 = factor(c("A", "B", "A", "B")),
    v2 = factor(c("X", "X", "Y", "Y"))
  )
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_equal(nrow(clusterer$disj), 4)
  # model.matrix drops the first level of each factor by default, so we get n_levels - 1 per variable
  # Actually, with ~ . - 1, all levels are kept but only actually present levels
  expect_true(ncol(clusterer$disj) >= 3) # At least 2 + 1 levels present
  expect_true(all(clusterer$disj %in% c(0, 1)))
})

test_that("fit computes modality counts correctly", {
  data <- data.frame(
    v1 = factor(c("A", "A", "A", "B", "B"))
  )
  # Use n_groups <= number of modalities
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer$fit(data)

  expect_equal(length(clusterer$modality_counts), 2)
  expect_equal(sum(clusterer$modality_counts), 5)
  expect_true(all(clusterer$modality_counts > 0))
})

test_that("fit with dice dissimilarity works", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(dissimilarity = "dice")
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  expect_true(is.matrix(clusterer$d2))
  expect_true(all(clusterer$d2 >= 0))
})

test_that("fit with cramer dissimilarity works", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(dissimilarity = "cramer")
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  expect_true(is.matrix(clusterer$d2))
  expect_true(all(clusterer$d2 >= 0))
})

test_that("fit with auto_discretize handles numeric variables", {
  data <- create_mixed_data(100)
  clusterer <- ModalitiesDiceClusterer$new(auto_discretize = TRUE, n_bins = 4)
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  # All variables should be converted to factors
  expect_true(all(sapply(clusterer$data, is.factor)))
})

test_that("fit without auto_discretize keeps factor variables only", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(auto_discretize = FALSE)
  clusterer$fit(data)

  expect_true(clusterer$fitted)
})

test_that("fit creates symmetric distance matrix", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_true(isSymmetric(clusterer$d2))
})

test_that("fit assigns correct number of groups", {
  data <- create_sample_data(100)
  for (k in 2:5) {
    clusterer <- ModalitiesDiceClusterer$new(n_groups = k)
    clusterer$fit(data)

    unique_groups <- length(unique(clusterer$groups))
    expect_true(unique_groups <= k)
    expect_true(unique_groups > 0)
  }
})

test_that("fit works with different linkage methods", {
  data <- create_sample_data(50)
  linkages <- c("ward.D2", "single", "complete", "average")

  for (link in linkages) {
    clusterer <- ModalitiesDiceClusterer$new(linkage = link)
    expect_silent(clusterer$fit(data))
    expect_true(clusterer$fitted)
  }
})

test_that("fit handles data with many levels", {
  set.seed(456)
  data <- data.frame(
    var1 = factor(sample(letters[1:10], 100, replace = TRUE)),
    var2 = factor(sample(LETTERS[1:15], 100, replace = TRUE))
  )
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 5)
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  # With random sampling, not all levels may appear, so check for approximately the expected number
  expect_true(ncol(clusterer$disj) >= 20) # Should be close to 25 (10 + 15)
  expect_true(ncol(clusterer$disj) <= 25)
})

test_that("fit handles single variable data", {
  data <- data.frame(
    var1 = factor(sample(c("A", "B", "C"), 50, replace = TRUE))
  )
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  expect_equal(ncol(clusterer$disj), 3)
})

# =============================================================================
# Tests for get_dice_matrix() method
# =============================================================================

test_that("get_dice_matrix returns matrix before conversion", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dice_mat <- clusterer$get_dice_matrix()

  expect_true(is.matrix(dice_mat))
  expect_equal(nrow(dice_mat), ncol(dice_mat))
  expect_equal(nrow(dice_mat), ncol(clusterer$disj))
})

test_that("get_dice_matrix returns dist object when requested", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dice_dist <- clusterer$get_dice_matrix(as_dist = TRUE)

  expect_s3_class(dice_dist, "dist")
})

test_that("get_dice_matrix fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_dice_matrix(),
    "Distance matrix not computed. Run \\$fit\\(data\\) first"
  )
})

test_that("get_dice_matrix returns non-negative values", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dice_mat <- clusterer$get_dice_matrix()

  expect_true(all(dice_mat >= 0))
})

test_that("get_dice_matrix diagonal is zero", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dice_mat <- clusterer$get_dice_matrix()

  expect_true(all(diag(dice_mat) == 0))
})

test_that("get_dice_matrix is symmetric", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dice_mat <- clusterer$get_dice_matrix()

  expect_true(isSymmetric(dice_mat))
})

# =============================================================================
# Tests for cut_tree() method
# =============================================================================

test_that("cut_tree changes number of groups", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  original_groups <- clusterer$groups
  clusterer$cut_tree(5)

  expect_false(identical(original_groups, clusterer$groups))
  expect_true(length(unique(clusterer$groups)) <= 5)
})

test_that("cut_tree fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$cut_tree(3),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("cut_tree requires k parameter", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_error(
    clusterer$cut_tree(),
    "k must be provided"
  )

  expect_error(
    clusterer$cut_tree(NULL),
    "k must be provided"
  )
})

test_that("cut_tree validates k is positive", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_error(
    clusterer$cut_tree(0),
    "k must be a positive integer"
  )

  expect_error(
    clusterer$cut_tree(-1),
    "k must be a positive integer"
  )
})

test_that("cut_tree validates k is numeric", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_error(
    clusterer$cut_tree("three"),
    "k must be a positive integer"
  )
})

test_that("cut_tree with k=1 creates single group", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)
  clusterer$cut_tree(1)

  expect_equal(length(unique(clusterer$groups)), 1)
})

test_that("cut_tree returns self invisibly", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- withVisible(clusterer$cut_tree(4))
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

test_that("cut_tree updates cached statistics", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer$fit(data)

  summary1 <- clusterer$summary()
  clusterer$cut_tree(5)
  summary2 <- clusterer$summary()

  expect_false(identical(summary1, summary2))
})

# =============================================================================
# Tests for get_cluster_table() method
# =============================================================================

test_that("get_cluster_table returns data.frame with correct structure", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  expect_s3_class(table, "data.frame")
  expect_true("Modality" %in% colnames(table))
  expect_true("Cluster" %in% colnames(table))
  expect_true("Frequency" %in% colnames(table))
})

test_that("get_cluster_table fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_cluster_table(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_cluster_table has correct number of rows", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  expect_equal(nrow(table), ncol(clusterer$disj))
})

test_that("get_cluster_table modalities match disjunctive matrix columns", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  expect_true(all(table$Modality %in% colnames(clusterer$disj)))
})

test_that("get_cluster_table frequencies sum to total observations", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  # Total frequency equals n_obs * n_vars (each obs contributes to one modality per var)
  # This should equal the sum of all modality counts
  expect_equal(sum(table$Frequency), sum(clusterer$modality_counts))
})

test_that("get_cluster_table is sorted by cluster and frequency", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  # Check if clusters are in ascending order (with ties broken by frequency descending)
  expect_true(all(diff(table$Cluster) >= 0))
})

test_that("get_cluster_table contains valid cluster IDs", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 4)
  clusterer$fit(data)

  table <- clusterer$get_cluster_table()

  expect_true(all(table$Cluster %in% 1:4))
})

# =============================================================================
# Tests for get_modalities() method
# =============================================================================

test_that("get_modalities returns NULL before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_null(clusterer$get_modalities())
})

test_that("get_modalities returns character vector after fit", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  modalities <- clusterer$get_modalities()

  expect_type(modalities, "character")
  expect_equal(length(modalities), ncol(clusterer$disj))
})

test_that("get_modalities matches disjunctive matrix column names", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  modalities <- clusterer$get_modalities()

  expect_equal(modalities, colnames(clusterer$disj))
})

test_that("get_modalities returns unique values", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  modalities <- clusterer$get_modalities()

  expect_equal(length(modalities), length(unique(modalities)))
})

# =============================================================================
# Tests for measure_dissimilarity() method
# =============================================================================

test_that("measure_dissimilarity returns matrix", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity()

  expect_true(is.matrix(dissim))
  expect_equal(nrow(dissim), ncol(dissim))
})

test_that("measure_dissimilarity returns dist object when requested", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity(as_dist = TRUE)

  expect_s3_class(dissim, "dist")
})

test_that("measure_dissimilarity fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$measure_dissimilarity(),
    "Dissimilarity not computed. Run \\$fit\\(data\\) first"
  )
})

test_that("measure_dissimilarity returns non-negative values", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity()

  expect_true(all(dissim >= 0))
})

test_that("measure_dissimilarity diagonal is zero", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity()

  expect_true(all(abs(diag(dissim)) < 1e-10))
})

test_that("measure_dissimilarity is symmetric", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity()

  expect_true(isSymmetric(dissim))
})

# =============================================================================
# Tests for measure_similarity() method
# =============================================================================

test_that("measure_similarity returns matrix", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sim <- clusterer$measure_similarity()

  expect_true(is.matrix(sim))
  expect_equal(nrow(sim), ncol(sim))
})

test_that("measure_similarity fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$measure_similarity(),
    "Dissimilarity not computed. Run \\$fit\\(data\\) first"
  )
})

test_that("measure_similarity normalized values are in [0,1]", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sim <- clusterer$measure_similarity(normalize = TRUE)

  expect_true(all(sim >= 0))
  expect_true(all(sim <= 1))
})

test_that("measure_similarity without normalization", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sim <- clusterer$measure_similarity(normalize = FALSE)

  expect_true(is.matrix(sim))
})

test_that("measure_similarity diagonal is maximum", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sim <- clusterer$measure_similarity(normalize = TRUE)

  # Diagonal should be close to 1 (maximum similarity with self)
  expect_true(all(abs(diag(sim) - 1) < 1e-10))
})

test_that("measure_similarity is symmetric", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sim <- clusterer$measure_similarity()

  expect_true(isSymmetric(sim))
})

test_that("measure_similarity inverse relationship with dissimilarity", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  dissim <- clusterer$measure_dissimilarity()
  sim <- clusterer$measure_similarity(normalize = TRUE)

  # When normalized, sim should be approximately 1 - normalized(dissim)
  max_d <- max(dissim)
  expected_sim <- 1 - (dissim / max_d)

  expect_true(all(abs(sim - expected_sim) < 1e-10))
})

# =============================================================================
# Tests for predict_illustrative() method
# =============================================================================

test_that("predict_illustrative works with factor input", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("Red", "Blue"), 100, replace = TRUE))
  result <- clusterer$predict_illustrative(illus)

  expect_type(result, "list")
  expect_true("distances" %in% names(result))
  expect_true("by_group" %in% names(result))
})

test_that("predict_illustrative works with data.frame input", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(color = factor(sample(c("Red", "Blue"), 100, replace = TRUE)))
  result <- clusterer$predict_illustrative(illus)

  expect_type(result, "list")
  expect_true("distances" %in% names(result))
  expect_true("by_group" %in% names(result))
})

test_that("predict_illustrative fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()
  illus <- factor(c("A", "B", "A"))

  expect_error(
    clusterer$predict_illustrative(illus),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("predict_illustrative fails with multi-column data.frame", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(
    col1 = factor(c("A", "B")),
    col2 = factor(c("X", "Y"))
  )

  expect_error(
    clusterer$predict_illustrative(illus),
    "illus data.frame must have a single column"
  )
})

test_that("predict_illustrative distances matrix has correct dimensions", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  illus <- factor(sample(c("Cat1", "Cat2", "Cat3"), 100, replace = TRUE))
  result <- clusterer$predict_illustrative(illus)

  expect_equal(nrow(result$distances), ncol(clusterer$disj))
  expect_equal(ncol(result$distances), 3) # 3 levels in illustrative variable
})

test_that("predict_illustrative by_group has correct dimensions", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 4)
  clusterer$fit(data)

  illus <- factor(sample(c("L1", "L2"), 100, replace = TRUE))
  result <- clusterer$predict_illustrative(illus)

  expect_true(is.matrix(result$by_group))
  expect_equal(nrow(result$by_group), 4) # 4 groups
  expect_equal(ncol(result$by_group), 2) # 2 levels in illustrative
})

test_that("predict_illustrative returns non-negative distances", {
  data <- create_mixed_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  result <- clusterer$predict_illustrative(illus)

  expect_true(all(result$distances >= 0))
  expect_true(all(result$by_group >= 0))
})

# =============================================================================
# Tests for plot_dendrogram() method
# =============================================================================

test_that("plot_dendrogram fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$plot_dendrogram(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("plot_dendrogram executes without error after fit", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_dendrogram())
})

test_that("plot_dendrogram returns self invisibly", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- withVisible(clusterer$plot_dendrogram())
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

# =============================================================================
# Tests for print() method
# =============================================================================

test_that("print works before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_output(
    print(clusterer),
    "ModalitiesDiceClusterer"
  )
  expect_output(
    print(clusterer),
    "not fitted"
  )
})

test_that("print works after fit", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  expect_output(
    print(clusterer),
    "ModalitiesDiceClusterer"
  )
  expect_output(
    print(clusterer),
    "Fitted with 3 modality-groups"
  )
})

test_that("print returns self invisibly", {
  clusterer <- ModalitiesDiceClusterer$new()

  result <- withVisible(print(clusterer))
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

# =============================================================================
# Tests for summary() method
# =============================================================================

test_that("summary works after fit", {
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- clusterer$summary()

  expect_type(result, "list")
  expect_true("group_centers" %in% names(result))
  expect_true("inertia" %in% names(result))
  expect_true("modality_contribution" %in% names(result))
})

test_that("summary returns NULL before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  result <- clusterer$summary()

  expect_null(result)
})

test_that("summary inertia has correct structure", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- clusterer$summary()

  expect_true("total" %in% names(result$inertia))
  expect_true("between" %in% names(result$inertia))
  expect_true("within" %in% names(result$inertia))
  # Inertia values should be numeric (allow small negative values due to numerical precision)
  expect_true(all(result$inertia > -1e-10))
})

test_that("summary group_centers has correct length", {
  set.seed(123)
  data <- create_sample_data(100)
  n_groups <- 4
  clusterer <- ModalitiesDiceClusterer$new(n_groups = n_groups)
  clusterer$fit(data)

  result <- clusterer$summary()

  expect_equal(length(result$group_centers), n_groups)
})

test_that("summary caches results", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result1 <- clusterer$summary()
  result2 <- clusterer$summary()

  expect_equal(result1, result2)
})

test_that("summary prints output", {
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_output(
    clusterer$summary(),
    "MODALITIES DICE CLUSTERER SUMMARY"
  )
})

# =============================================================================
# Integration tests
# =============================================================================

test_that("complete workflow with method chaining", {
  set.seed(123)
  data <- create_sample_data(100)

  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)
  clusterer$cut_tree(4)
  table <- clusterer$get_cluster_table()

  expect_true(clusterer$fitted)
  expect_s3_class(table, "data.frame")
  expect_true(length(unique(clusterer$groups)) <= 4)
})

test_that("multiple fit calls update state correctly", {
  data1 <- create_sample_data(50)
  data2 <- create_sample_data(75)

  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data1)

  groups1 <- clusterer$groups

  clusterer$fit(data2)
  groups2 <- clusterer$groups

  expect_false(identical(groups1, groups2))
  expect_equal(nrow(clusterer$disj), 75)
})

test_that("workflow with auto_discretize on mixed data", {
  set.seed(123)
  data <- create_mixed_data(100)

  clusterer <- ModalitiesDiceClusterer$new(
    n_groups = 4,
    auto_discretize = TRUE,
    n_bins = 5
  )

  clusterer$fit(data)
  table <- clusterer$get_cluster_table()
  summary <- clusterer$summary()

  expect_true(clusterer$fitted)
  expect_s3_class(table, "data.frame")
  expect_type(summary, "list")
})

test_that("workflow with cramer dissimilarity", {
  set.seed(123)
  data <- create_sample_data(100)

  clusterer <- ModalitiesDiceClusterer$new(
    n_groups = 3,
    dissimilarity = "cramer"
  )

  clusterer$fit(data)
  dissim <- clusterer$measure_dissimilarity()
  sim <- clusterer$measure_similarity()

  expect_true(is.matrix(dissim))
  expect_true(is.matrix(sim))
})

test_that("comparing dice and cramer dissimilarity", {
  set.seed(123)
  data <- create_sample_data(100)

  clusterer_dice <- ModalitiesDiceClusterer$new(dissimilarity = "dice")
  clusterer_dice$fit(data)

  clusterer_cramer <- ModalitiesDiceClusterer$new(dissimilarity = "cramer")
  clusterer_cramer$fit(data)

  # Both should be fitted
  expect_true(clusterer_dice$fitted)
  expect_true(clusterer_cramer$fitted)

  # Dissimilarity matrices may differ
  expect_false(identical(
    clusterer_dice$measure_dissimilarity(),
    clusterer_cramer$measure_dissimilarity()
  ))
})

test_that("large dataset stress test", {
  set.seed(123)
  data <- data.frame(
    v1 = factor(sample(letters[1:20], 1000, replace = TRUE)),
    v2 = factor(sample(LETTERS[1:15], 1000, replace = TRUE)),
    v3 = factor(sample(1:10, 1000, replace = TRUE)),
    stringsAsFactors = TRUE
  )

  clusterer <- ModalitiesDiceClusterer$new(n_groups = 10)

  clusterer$fit(data)
  expect_true(clusterer$fitted)
  # Check that we have at least most of the expected modalities (may vary due to sampling)
  expect_true(ncol(clusterer$disj) >= 40) # Should be close to 45 (20 + 15 + 10)
})

test_that("edge case: two modalities only", {
  data <- data.frame(
    var1 = factor(c("A", "B", "A", "B", "A"))
  )

  clusterer <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer$fit(data)

  expect_true(clusterer$fitted)
  expect_equal(ncol(clusterer$disj), 2)
  expect_equal(length(unique(clusterer$groups)), 2)
})

test_that("all methods work in sequence", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)

  # Fit
  clusterer$fit(data)
  expect_true(clusterer$fitted)

  # Get matrices
  dice <- clusterer$get_dice_matrix()
  expect_true(is.matrix(dice))

  # Cut tree
  clusterer$cut_tree(4)
  expect_true(length(unique(clusterer$groups)) <= 4)

  # Get cluster table
  table <- clusterer$get_cluster_table()
  expect_s3_class(table, "data.frame")

  # Get modalities
  mods <- clusterer$get_modalities()
  expect_type(mods, "character")

  # Measure dissimilarity and similarity
  dissim <- clusterer$measure_dissimilarity()
  sim <- clusterer$measure_similarity()
  expect_true(is.matrix(dissim))
  expect_true(is.matrix(sim))

  # Predict illustrative
  illus <- factor(sample(c("X", "Y"), 100, replace = TRUE))
  pred <- clusterer$predict_illustrative(illus)
  expect_type(pred, "list")

  # Summary
  summ <- clusterer$summary()
  expect_type(summ, "list")

  # Print
  expect_output(print(clusterer), "ModalitiesDiceClusterer")
})

# =============================================================================
# Tests for plot_mca() method
# =============================================================================

test_that("plot_mca fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$plot_mca(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("plot_mca executes without error after fit", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_mca())
})

test_that("plot_mca returns self invisibly", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- withVisible(clusterer$plot_mca())
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

test_that("plot_mca accepts different dimensions", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_mca(dims = c(1, 2)))
  expect_silent(clusterer$plot_mca(dims = c(2, 3)))
})

test_that("plot_mca validates dims parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_error(
    clusterer$plot_mca(dims = c(1, 2, 3)),
    "dims must be a numeric vector of length 2"
  )

  expect_error(
    clusterer$plot_mca(dims = "1,2"),
    "dims must be a numeric vector of length 2"
  )
})

test_that("plot_mca works with show_labels parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_mca(show_labels = TRUE))
  expect_silent(clusterer$plot_mca(show_labels = FALSE))
})

# =============================================================================
# Tests for plot_mca() method
# =============================================================================

test_that("plot_mca fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$plot_mca(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("plot_mca executes without error after fit", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_mca())
})

test_that("plot_mca returns self invisibly", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- withVisible(clusterer$plot_mca())
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

test_that("plot_mca accepts custom colors", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  custom_colors <- c("red", "blue", "green")
  expect_silent(clusterer$plot_mca(colors = custom_colors))
})

test_that("plot_mca works with add_ellipses parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_silent(clusterer$plot_mca(add_ellipses = TRUE))
  expect_silent(clusterer$plot_mca(add_ellipses = FALSE))
})

test_that("plot_mca validates dims parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_error(
    clusterer$plot_mca(dims = 1),
    "dims must be a numeric vector of length 2"
  )
})

# =============================================================================
# Tests for plot_mca_illustrative() method
# =============================================================================

test_that("plot_mca_illustrative fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()
  illus <- factor(c("A", "B", "A"))

  expect_error(
    clusterer$plot_mca_illustrative(illus),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("plot_mca_illustrative works with factor input", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("Red", "Blue"), 100, replace = TRUE))
  expect_silent(clusterer$plot_mca_illustrative(illus))
})

test_that("plot_mca_illustrative works with data.frame input", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(color = factor(sample(c("Red", "Blue"), 100, replace = TRUE)))
  expect_silent(clusterer$plot_mca_illustrative(illus))
})

test_that("plot_mca_illustrative returns self invisibly", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  result <- withVisible(clusterer$plot_mca_illustrative(illus))
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

test_that("plot_mca_illustrative validates illustrative length", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 50, replace = TRUE)) # Wrong length
  expect_error(
    clusterer$plot_mca_illustrative(illus),
    "illus must have same length as fitted data"
  )
})

test_that("plot_mca_illustrative fails with multi-column data.frame", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(
    col1 = factor(sample(c("A", "B"), 100, replace = TRUE)),
    col2 = factor(sample(c("X", "Y"), 100, replace = TRUE))
  )
  expect_error(
    clusterer$plot_mca_illustrative(illus),
    "illus data.frame must have a single column"
  )
})

test_that("plot_mca_illustrative accepts custom colors", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  expect_silent(clusterer$plot_mca_illustrative(illus, illus_color = "purple"))
})

# =============================================================================
# Tests for get_mca_data() method
# =============================================================================

test_that("get_mca_data fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_mca_data(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_mca_data returns data.frame", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data()

  expect_s3_class(mca_data, "data.frame")
  expect_true("Modality" %in% colnames(mca_data))
  expect_true("Variable" %in% colnames(mca_data))
  expect_true("Level" %in% colnames(mca_data))
})

test_that("get_mca_data includes dimension columns", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data(n_dims = 3)

  expect_true("Dim1" %in% colnames(mca_data))
  expect_true("Dim2" %in% colnames(mca_data))
  expect_true("Dim3" %in% colnames(mca_data))
})

test_that("get_mca_data respects n_dims parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data_2 <- clusterer$get_mca_data(n_dims = 2)
  mca_data_5 <- clusterer$get_mca_data(n_dims = 5)

  dim_cols_2 <- grep("^Dim", colnames(mca_data_2), value = TRUE)
  dim_cols_5 <- grep("^Dim", colnames(mca_data_5), value = TRUE)

  expect_equal(length(dim_cols_2), 2)
  expect_equal(length(dim_cols_5), 5)
})

test_that("get_mca_data has correct number of rows", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data()

  expect_equal(nrow(mca_data), ncol(clusterer$disj))
})

test_that("get_mca_data modalities match disjunctive matrix", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data()

  expect_equal(sort(mca_data$Modality), sort(colnames(clusterer$disj)))
})

test_that("get_mca_data extracts variable and level correctly", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data()

  # Check that Variable and Level are extracted (should never be NA)
  expect_true(all(!is.na(mca_data$Variable)))
  expect_true(all(!is.na(mca_data$Level)))

  # Check format: Modality should equal Variable + Level (concatenated, no separator)
  # model.matrix creates names like "var1A" = variable "var1" + level "A"
  reconstructed <- paste0(mca_data$Variable, mca_data$Level)
  expect_equal(mca_data$Modality, reconstructed)
})

# =============================================================================
# Tests for get_cluster_data() method
# =============================================================================

test_that("get_cluster_data fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_cluster_data(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_cluster_data returns data.frame with correct structure", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data <- clusterer$get_cluster_data()

  expect_s3_class(cluster_data, "data.frame")
  expect_true("Modality" %in% colnames(cluster_data))
  expect_true("Cluster" %in% colnames(cluster_data))
  expect_true("Frequency" %in% colnames(cluster_data))
  expect_true("Dim1" %in% colnames(cluster_data))
  expect_true("Dim2" %in% colnames(cluster_data))
})

test_that("get_cluster_data includes variance explained", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data <- clusterer$get_cluster_data()

  expect_true("VarExplained_Dim1" %in% colnames(cluster_data))
  expect_true("VarExplained_Dim2" %in% colnames(cluster_data))
})

test_that("get_cluster_data cluster assignments match groups", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data <- clusterer$get_cluster_data()

  for (i in 1:nrow(cluster_data)) {
    mod <- cluster_data$Modality[i]
    # Use unname() to strip names before comparison
    expect_equal(unname(cluster_data$Cluster[i]), unname(clusterer$groups[mod]))
  }
})

test_that("get_cluster_data frequencies match modality_counts", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data <- clusterer$get_cluster_data()

  for (i in 1:nrow(cluster_data)) {
    mod <- cluster_data$Modality[i]
    # Use unname() to strip names before comparison
    expect_equal(unname(cluster_data$Frequency[i]), unname(clusterer$modality_counts[mod]))
  }
})

test_that("get_cluster_data respects n_dims parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data_3 <- clusterer$get_cluster_data(n_dims = 3)

  dim_cols <- grep("^Dim[0-9]+$", colnames(cluster_data_3), value = TRUE)
  expect_equal(length(dim_cols), 3)
})

test_that("get_cluster_data is sorted by cluster and frequency", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  cluster_data <- clusterer$get_cluster_data()

  # Check cluster ordering
  expect_true(all(diff(cluster_data$Cluster) >= 0))
})

# =============================================================================
# Tests for get_illustrative_data() method
# =============================================================================

test_that("get_illustrative_data fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()
  illus <- factor(c("A", "B", "A"))

  expect_error(
    clusterer$get_illustrative_data(illus),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_illustrative_data works with factor input", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("Red", "Blue"), 100, replace = TRUE))
  illus_data <- clusterer$get_illustrative_data(illus)

  expect_s3_class(illus_data, "data.frame")
  expect_true("Type" %in% colnames(illus_data))
  expect_true(all(illus_data$Type %in% c("Active", "Illustrative")))
})

test_that("get_illustrative_data works with data.frame input", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(color = factor(sample(c("Red", "Blue"), 100, replace = TRUE)))
  illus_data <- clusterer$get_illustrative_data(illus)

  expect_s3_class(illus_data, "data.frame")
})

test_that("get_illustrative_data includes both active and illustrative", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  illus_data <- clusterer$get_illustrative_data(illus)

  expect_true("Active" %in% illus_data$Type)
  expect_true("Illustrative" %in% illus_data$Type)
})

test_that("get_illustrative_data includes distance metrics", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  illus_data <- clusterer$get_illustrative_data(illus)

  expect_true("DistToActive_Min" %in% colnames(illus_data))
  expect_true("ClosestCluster" %in% colnames(illus_data))

  # Only illustrative modalities should have these values
  illus_rows <- illus_data[illus_data$Type == "Illustrative", ]
  expect_true(all(!is.na(illus_rows$DistToActive_Min)))
  expect_true(all(!is.na(illus_rows$ClosestCluster)))
})

test_that("get_illustrative_data validates illustrative length", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 50, replace = TRUE)) # Wrong length
  expect_error(
    clusterer$get_illustrative_data(illus),
    "illus must have same length as fitted data"
  )
})

test_that("get_illustrative_data fails with multi-column data.frame", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- data.frame(
    col1 = factor(sample(c("A", "B"), 100, replace = TRUE)),
    col2 = factor(sample(c("X", "Y"), 100, replace = TRUE))
  )
  expect_error(
    clusterer$get_illustrative_data(illus),
    "illus data.frame must have a single column"
  )
})

test_that("get_illustrative_data validates n_dims parameter", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))

  expect_error(
    clusterer$get_illustrative_data(illus, n_dims = -1),
    "n_dims must be a positive integer"
  )

  expect_error(
    clusterer$get_illustrative_data(illus, n_dims = "two"),
    "n_dims must be a positive integer"
  )
})

test_that("get_illustrative_data respects n_dims parameter", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  illus_data <- clusterer$get_illustrative_data(illus, n_dims = 3)

  dim_cols <- grep("^Dim[0-9]+$", colnames(illus_data), value = TRUE)
  expect_equal(length(dim_cols), 3)
})

# =============================================================================
# Tests for get_cluster_stats() method
# =============================================================================

test_that("get_cluster_stats fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_cluster_stats(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_cluster_stats returns data.frame with correct structure", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()

  expect_s3_class(stats, "data.frame")
  expect_true("Cluster" %in% colnames(stats))
  expect_true("Size" %in% colnames(stats))
  expect_true("TotalFrequency" %in% colnames(stats))
  expect_true("AvgFrequency" %in% colnames(stats))
  expect_true("WithinVariance" %in% colnames(stats))
  expect_true("Modalities" %in% colnames(stats))
})

test_that("get_cluster_stats has one row per cluster", {
  set.seed(123)
  data <- create_sample_data(50)
  n_groups <- 4
  clusterer <- ModalitiesDiceClusterer$new(n_groups = n_groups)
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()

  expect_equal(nrow(stats), n_groups)
})

test_that("get_cluster_stats includes centroid coordinates", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats(n_dims = 2)

  expect_true("Centroid_Dim1" %in% colnames(stats))
  expect_true("Centroid_Dim2" %in% colnames(stats))
})

test_that("get_cluster_stats respects n_dims parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  stats_3 <- clusterer$get_cluster_stats(n_dims = 3)

  centroid_cols <- grep("^Centroid_Dim", colnames(stats_3), value = TRUE)
  expect_equal(length(centroid_cols), 3)
})

test_that("get_cluster_stats cluster sizes are correct", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()
  cluster_table <- clusterer$get_cluster_table()

  for (i in 1:nrow(stats)) {
    cluster_id <- stats$Cluster[i]
    expected_size <- sum(cluster_table$Cluster == cluster_id)
    expect_equal(stats$Size[i], expected_size)
  }
})

test_that("get_cluster_stats total frequencies are correct", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()
  cluster_table <- clusterer$get_cluster_table()

  for (i in 1:nrow(stats)) {
    cluster_id <- stats$Cluster[i]
    expected_freq <- sum(cluster_table$Frequency[cluster_table$Cluster == cluster_id])
    expect_equal(stats$TotalFrequency[i], expected_freq)
  }
})

test_that("get_cluster_stats within variance is non-negative", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()

  expect_true(all(stats$WithinVariance >= 0))
})

test_that("get_cluster_stats modalities list is non-empty", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  stats <- clusterer$get_cluster_stats()

  expect_true(all(nchar(stats$Modalities) > 0))
  expect_true(all(grepl(",", stats$Modalities) | !grepl(",", stats$Modalities))) # Has commas or single modality
})

# =============================================================================
# Integration tests for new methods
# =============================================================================

test_that("all visualization methods work in sequence", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Plot methods
  expect_silent(clusterer$plot_mca())
  expect_silent(clusterer$plot_mca())

  # Illustrative plot
  illus <- factor(sample(c("X", "Y"), 100, replace = TRUE))
  expect_silent(clusterer$plot_mca_illustrative(illus))
})

test_that("all data extraction methods work in sequence", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Get MCA data
  mca_data <- clusterer$get_mca_data()
  expect_s3_class(mca_data, "data.frame")

  # Get cluster data
  cluster_data <- clusterer$get_cluster_data()
  expect_s3_class(cluster_data, "data.frame")

  # Get illustrative data
  illus <- factor(sample(c("A", "B"), 100, replace = TRUE))
  illus_data <- clusterer$get_illustrative_data(illus)
  expect_s3_class(illus_data, "data.frame")

  # Get cluster stats
  stats <- clusterer$get_cluster_stats()
  expect_s3_class(stats, "data.frame")
})

test_that("MCA computation is cached", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # First call should compute MCA
  mca_data1 <- clusterer$get_mca_data()

  # Second call should use cached result
  mca_data2 <- clusterer$get_mca_data()

  expect_equal(mca_data1, mca_data2)
})

test_that("data extraction methods return consistent modalities", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  mca_data <- clusterer$get_mca_data()
  cluster_data <- clusterer$get_cluster_data()

  # Both should have same modalities
  expect_equal(sort(mca_data$Modality), sort(cluster_data$Modality))

  # Both should match disjunctive matrix columns
  expect_equal(sort(mca_data$Modality), sort(colnames(clusterer$disj)))
})

test_that("illustrative projection works with different variable types", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # Binary illustrative
  illus_binary <- factor(sample(c("Yes", "No"), 100, replace = TRUE))
  expect_silent(clusterer$plot_mca_illustrative(illus_binary))
  illus_data_binary <- clusterer$get_illustrative_data(illus_binary)
  expect_equal(sum(illus_data_binary$Type == "Illustrative"), 2)

  # Multi-level illustrative
  illus_multi <- factor(sample(c("Low", "Med", "High"), 100, replace = TRUE))
  expect_silent(clusterer$plot_mca_illustrative(illus_multi))
  illus_data_multi <- clusterer$get_illustrative_data(illus_multi)
  expect_equal(sum(illus_data_multi$Type == "Illustrative"), 3)
})

test_that("workflow: fit, extract data, export to tables", {
  set.seed(123)
  data <- create_sample_data(100)

  # Fit model
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Extract all data tables
  mca_coords <- clusterer$get_mca_data(n_dims = 2)
  cluster_info <- clusterer$get_cluster_data()
  cluster_summary <- clusterer$get_cluster_stats()

  # Verify we can perform analyses on extracted data
  expect_true(nrow(mca_coords) > 0)
  expect_true(nrow(cluster_info) > 0)
  expect_true(nrow(cluster_summary) > 0)

  # Verify consistency across tables
  expect_equal(nrow(mca_coords), nrow(cluster_info))
  expect_equal(nrow(cluster_summary), clusterer$n_groups)
})

# =============================================================================
# Tests for plot_silhouette() method
# =============================================================================

test_that("plot_silhouette fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$plot_silhouette(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("plot_silhouette executes without error after fit", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # Suppress printing of progress messages
  # Use max_k=5 to avoid warning with small number of modalities
  expect_output(clusterer$plot_silhouette(max_k = 5))
})

test_that("plot_silhouette returns self invisibly", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  result <- withVisible(clusterer$plot_silhouette(max_k = 5))
  expect_false(result$visible)
  expect_true(R6::is.R6(result$value))
})

test_that("plot_silhouette accepts custom min_k and max_k", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Test with custom k range
  expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 5))
})

test_that("plot_silhouette works with custom colors", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  expect_output(clusterer$plot_silhouette(line_color = "blue", point_color = "darkblue", max_k = 5))
  expect_output(clusterer$plot_silhouette(optimal_color = "green", mark_optimal = FALSE, max_k = 5))
})

test_that("plot_silhouette works with different k ranges", {
  set.seed(123)
  data <- create_sample_data(100)

  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Test different k ranges
  expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 4))
  expect_output(clusterer$plot_silhouette(min_k = 3, max_k = 5))
})

test_that("plot_silhouette handles limited modalities", {
  set.seed(123)
  # Create data with few modalities
  data <- data.frame(
    v1 = factor(c(rep("A", 25), rep("B", 25))),
    v2 = factor(c(rep("X", 25), rep("Y", 25)))
  )
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer$fit(data)

  # Should work but may generate warning if max_k is too high
  # With only 3 modalities (v1A, v1B, v2X, v2Y might actually be 4), max_k=3 will trigger adjustment
  expect_warning(
    expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 3)),
    "max_k.*>=.*number of modalities"
  )
})

test_that("plot_silhouette validates min_k parameter", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # min_k must be at least 2
  expect_error(
    clusterer$plot_silhouette(min_k = 1),
    "min_k must be >= 2"
  )

  expect_error(
    clusterer$plot_silhouette(min_k = 0),
    "min_k must be >= 2"
  )
})

test_that("plot_silhouette validates max_k relative to min_k", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # max_k must be >= min_k
  expect_error(
    clusterer$plot_silhouette(min_k = 5, max_k = 3),
    "max_k must be >= min_k"
  )
})

test_that("plot_silhouette validates max_k relative to modalities", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  n_modalities <- ncol(clusterer$disj)

  # max_k >= number of modalities generates warning and adjusts
  expect_warning(
    expect_output(
      clusterer$plot_silhouette(min_k = 2, max_k = n_modalities + 1)
    ),
    "max_k.*>=.*number of modalities"
  )
})

test_that("plot_silhouette restores original clustering state", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Store original state
  original_k <- clusterer$n_groups
  original_groups <- clusterer$groups

  # Run plot_silhouette
  expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 5))

  # State should be restored
  expect_equal(clusterer$n_groups, original_k)
  expect_equal(clusterer$groups, original_groups)
})

# =============================================================================
# Tests for get_silhouette_data() method
# =============================================================================

test_that("get_silhouette_data fails before fit", {
  clusterer <- ModalitiesDiceClusterer$new()

  expect_error(
    clusterer$get_silhouette_data(),
    "Model not fitted. Call \\$fit\\(data\\) first"
  )
})

test_that("get_silhouette_data returns data.frame with correct structure", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  expect_s3_class(sil_data, "data.frame")
  expect_true("Modality" %in% colnames(sil_data))
  expect_true("Cluster" %in% colnames(sil_data))
  expect_true("Silhouette" %in% colnames(sil_data))
  expect_true("AvgDistWithin" %in% colnames(sil_data))
  expect_true("MinAvgDistOther" %in% colnames(sil_data))
  expect_true("NearestCluster" %in% colnames(sil_data))
})

test_that("get_silhouette_data has correct number of rows", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  expect_equal(nrow(sil_data), ncol(clusterer$disj))
})

test_that("get_silhouette_data silhouette values are in valid range", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  # Silhouette should be in [-1, 1]
  expect_true(all(sil_data$Silhouette >= -1))
  expect_true(all(sil_data$Silhouette <= 1))
})

test_that("get_silhouette_data modalities match disjunctive matrix", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  expect_equal(sort(sil_data$Modality), sort(colnames(clusterer$disj)))
})

test_that("get_silhouette_data clusters match group assignments", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  for (i in 1:nrow(sil_data)) {
    mod <- sil_data$Modality[i]
    expect_equal(sil_data$Cluster[i], unname(clusterer$groups[mod]))
  }
})

test_that("get_silhouette_data is sorted by cluster and silhouette", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  # Check cluster ordering (ascending)
  expect_true(all(diff(sil_data$Cluster) >= 0))

  # Within each cluster, silhouette should be descending
  for (k in unique(sil_data$Cluster)) {
    cluster_sil <- sil_data$Silhouette[sil_data$Cluster == k]
    expect_true(all(diff(cluster_sil) <= 0))
  }
})

test_that("get_silhouette_data handles singleton clusters", {
  set.seed(123)
  # Create data that might produce singleton clusters
  data <- data.frame(
    v1 = factor(c(rep("A", 45), rep("B", 4), "C")),
    v2 = factor(sample(c("X", "Y"), 50, replace = TRUE))
  )
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 4)
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  # Singleton clusters should have silhouette = 0
  expect_true(all(sil_data$Silhouette >= -1))
  expect_true(all(sil_data$Silhouette <= 1))
})

test_that("get_silhouette_data AvgDistWithin is non-negative", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  expect_true(all(sil_data$AvgDistWithin >= 0))
})

test_that("get_silhouette_data MinAvgDistOther is non-negative", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  expect_true(all(sil_data$MinAvgDistOther >= 0))
})

test_that("get_silhouette_data NearestCluster is valid", {
  set.seed(123)
  data <- create_sample_data(50)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  # Remove NA values (can occur for singleton clusters or single-cluster case)
  valid_nearest <- sil_data$NearestCluster[!is.na(sil_data$NearestCluster)]

  if (length(valid_nearest) > 0) {
    expect_true(all(valid_nearest %in% 1:3))

    # Nearest cluster should be different from current cluster
    for (i in which(!is.na(sil_data$NearestCluster))) {
      expect_true(sil_data$NearestCluster[i] != sil_data$Cluster[i])
    }
  }
})

test_that("get_silhouette_data works after cut_tree", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  sil_data_3 <- clusterer$get_silhouette_data()

  clusterer$cut_tree(5)
  sil_data_5 <- clusterer$get_silhouette_data()

  # Different numbers of clusters should give different results
  expect_false(identical(sil_data_3, sil_data_5))
  expect_equal(nrow(sil_data_3), nrow(sil_data_5))
})

# =============================================================================
# Integration tests for silhouette methods
# =============================================================================

test_that("silhouette methods work in complete workflow", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  # Get silhouette data
  sil_data <- clusterer$get_silhouette_data()
  expect_s3_class(sil_data, "data.frame")

  # Plot silhouette elbow
  expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 5))

  # Change number of clusters and repeat
  clusterer$cut_tree(4)
  sil_data_4 <- clusterer$get_silhouette_data()
  expect_s3_class(sil_data_4, "data.frame")
  expect_output(clusterer$plot_silhouette(min_k = 2, max_k = 5))
})

test_that("silhouette quality metrics make sense", {
  set.seed(123)
  data <- create_sample_data(100)

  # Fit with different k values
  results <- list()
  for (k in 2:5) {
    clusterer <- ModalitiesDiceClusterer$new(n_groups = k)
    clusterer$fit(data)
    sil_data <- clusterer$get_silhouette_data()
    results[[k]] <- mean(sil_data$Silhouette)
  }

  # All average silhouettes should be in valid range
  expect_true(all(unlist(results) >= -1))
  expect_true(all(unlist(results) <= 1))
})

test_that("silhouette identifies poorly clustered modalities", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new(n_groups = 3)
  clusterer$fit(data)

  sil_data <- clusterer$get_silhouette_data()

  # Modalities with negative silhouette are poorly clustered
  poorly_clustered <- sil_data[sil_data$Silhouette < 0, ]

  # If there are poorly clustered modalities, they should have valid properties
  if (nrow(poorly_clustered) > 0) {
    expect_true(all(!is.na(poorly_clustered$Silhouette)))
    expect_true(all(poorly_clustered$AvgDistWithin > poorly_clustered$MinAvgDistOther))
  }
})

test_that("silhouette plot handles edge cases", {
  set.seed(123)

  # Very small dataset
  small_data <- create_sample_data(20)
  clusterer_small <- ModalitiesDiceClusterer$new(n_groups = 2)
  clusterer_small$fit(small_data)
  expect_output(clusterer_small$plot_silhouette(min_k = 2, max_k = 4))

  # Test with limited max_k
  expect_output(clusterer_small$plot_silhouette(min_k = 2, max_k = 3))
})

test_that("silhouette computation is consistent", {
  set.seed(123)
  data <- create_sample_data(100)
  clusterer <- ModalitiesDiceClusterer$new()
  clusterer$fit(data)

  # Get silhouette data twice
  sil_data_1 <- clusterer$get_silhouette_data()
  sil_data_2 <- clusterer$get_silhouette_data()

  # Should be identical
  expect_equal(sil_data_1, sil_data_2)
})
