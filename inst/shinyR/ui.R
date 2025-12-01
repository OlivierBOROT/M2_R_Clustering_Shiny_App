# PAGES
source(file.path("ui", "home.R"), local = TRUE)
source(file.path("ui", "upload.R"), local = TRUE)
source(file.path("ui", "cluster.R"), local = TRUE)

# ===============================
#            UI
# ===============================
ui <- shiny::fluidPage(
  theme = themes$light,

  shiny::tags$header(
    class = "app-header",
    style = "
      display:flex;
      align-items:center;
      justify-content:space-between;
      padding:10px;
    ",

    # Title
    shiny::h1(
      shiny::uiOutput("txt_APP_TITLE"),
      style = "margin:0;font-size:20px;"
    ),

    # Buttons
    shiny::div(
      style = "display:flex;align-items:center;gap:12px;",
      shiny::uiOutput("txt_DATASET_INFO"),
      shiny::actionButton(
        "BUTTON_toggle_theme",
        shiny::uiOutput("txt_BTN_THEME")
      ),
      shiny::actionButton(
        "BUTTON_language",
        shiny::uiOutput("txt_BTN_LANG")
      )
    )
  ),


  # ------------------------------
  # TABS
  # ------------------------------
  shiny::tabsetPanel(
    id = "tabs",

    shiny::tabPanel(
      title = shiny::uiOutput("txt_TAB_HOME"),
      home_page
    ),

    shiny::tabPanel(
      title = shiny::uiOutput("txt_TAB_UPLOAD"),
      upload_page
    ),

    shiny::tabPanel(
      title = shiny::uiOutput("txt_TAB_CLUSTER"),
      cluster_page
    )
  )
)