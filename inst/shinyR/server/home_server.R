
output$home_content <- shiny::renderUI({
  md_file_name <- if (current_language() == "fr") {
    "home_fr.md"
  } else {
    "home_en.md"
  }
  md_file <- file.path("texts", "markdowns", md_file_name)

  if (file.exists(md_file)) {
    shiny::HTML(markdown::renderMarkdown(file = md_file))
  } else {
    shiny::tags$div(
      class = "text-muted text-center",
      paste("Home page file not found:", md_file_name)
    )
  }
})