#' Run the Shiny application
#'
#' @export
run_app <- function() {
  app_dir <- system.file("shinyR", package = "M2RClust")
  shiny::runApp(app_dir)
}
