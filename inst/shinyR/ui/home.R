home_page <- shiny::div(
  class = "container-fluid",
  style = "margin-top:12px;",
  shiny::uiOutput("home_content")
)