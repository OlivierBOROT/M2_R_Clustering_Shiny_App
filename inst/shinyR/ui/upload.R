# Upload page UI ------------------------------------------------------------
# Container holds the whole upload page layout
upload_page <- shiny::div(
  class = "container-fluid",
  style = "margin-top:12px;",

  # Row: File input / Upload card -------------------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(class = "card shadow-sm", style = "margin-bottom:12px;",
        shiny::div(class = "card-header", shiny::uiOutput("txt_UPLOAD_CARD_TITLE")),

        # Card body: file input, help text, metadata and actions
        shiny::div(class = "card-body",
          # dynamic file input UI rendered from server
          shiny::div(class = "mb-3",
            shiny::uiOutput("upload_input_ui")
          ),

          # file metadata / preview area (server provides content)
          shiny::uiOutput("upload_file_info"),

          # small actions (reload)
          shiny::div(class = "d-flex gap-2 mt-2",
            shiny::actionButton("BUTTON_reload_data",
              shiny::uiOutput("txt_UPLOAD_BTN_RELOAD_DATA"),
              class = "btn-sm btn-secondary"
            )
          )
        )
      )
    )
  ),

  # Row: Columns card (types / roles / save) --------------------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(class = "card", style = "margin-bottom:12px;",
        shiny::div(class = "card-header", shiny::uiOutput("txt_UPLOAD_COLUMNS_CARD_TITLE")),

        # Card body: help text, inline controls and dynamic columns UI
        shiny::div(class = "card-body",
          shiny::helpText(shiny::uiOutput("txt_UPLOAD_COLUMNS_CARD_HELP")),

          # Inline control row: edit names checkbox + save button
          shiny::div(class = "d-flex align-items-center gap-2 mt-2",
            shiny::checkboxInput(
              "UPLOAD_EDIT_NAMES",
              label = shiny::uiOutput("txt_UPLOAD_EDIT_NAMES"),
              value = TRUE
            ),
            shiny::actionButton(
              "BUTTON_SAVE_COLUMNS",
              shiny::uiOutput("txt_UPLOAD_BTN_SAVE"),
              class = "btn-sm btn-primary ms-auto"
            )
          ),

          # Dynamic columns UI rendered server-side
          shiny::uiOutput("columns_card")
        )
      )
    )
  ),

  # Row: Data preview card (table + optional message) -----------------------
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::div(class = "card", style = "margin-bottom:12px; overflow:auto;",
        shiny::div(class = "card-header", shiny::uiOutput("txt_UPLOAD_PREVIEW_TITLE")),

        # Card body: table responsive wrapper and optional no-data message
        shiny::div(class = "card-body",
          shiny::div(class = "table-responsive", style = "overflow-x:auto;",
            shiny::tableOutput("upload_preview")
          ),
          shiny::uiOutput("upload_preview_no_data")
        )
      )
    )
  )
)
