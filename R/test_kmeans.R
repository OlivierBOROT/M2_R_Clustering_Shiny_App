#' simple kmeans clustering to test devtools and testing setup
#'
#' @param x matrix or data.frame
#' @param centers number of centers
#' @return vector of cluster assignments
#' @export
kmeans_simple <- function(x, centers = 3) {
  x <- as.matrix(x)
  stats::kmeans(x, centers = centers)$cluster
}