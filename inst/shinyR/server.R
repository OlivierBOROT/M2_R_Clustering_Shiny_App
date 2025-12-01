# ===============================
#          SERVER
# ===============================
server <- function(input, output, session) {

  # ----------------------------
  # REACTIVE VALUES AND HELPERS
  # ----------------------------

  cached <- shiny::reactiveValues(path = NULL, mtime = NULL, data = NULL)
  file_input_index <- shiny::reactiveVal(1)
  current_file_input_id <- shiny::reactive({
    paste0("UPLOAD_file_", file_input_index())
  })

  data_store <- shiny::reactiveValues(
    dataset = NULL,
    illustrative_dataset = NULL,
    clustering_vars = list(active = character(0), illustrative = character(0), excluded = character(0))
  )

  # function to load and cache data
  get_data <- function(force = FALSE) {
    # dynamic id read
    fid <- current_file_input_id()
    file_input_val <- input[[fid]]

    if (is.null(file_input_val) || !nzchar(file_input_val$datapath)) return(NULL)

    path <- file_input_val$datapath
    info <- file.info(path)
    mtime <- if (!is.na(info$mtime)) info$mtime else Sys.time()

    needs_reload <- force ||
      is.null(cached$data) ||
      is.null(cached$path) ||
      cached$path != path ||
      is.null(cached$mtime) ||
      !identical(cached$mtime, mtime)

    if (needs_reload) {
      ext <- tolower(tools::file_ext(file_input_val$name))
      dat <- NULL
      try({
        if (ext %in% c("csv")) {
          dat <- readr::read_csv(path, show_col_types = FALSE)
        } else if (ext %in% c("xls", "xlsx")) {
          dat <- readxl::read_excel(path)
        } else {
          dat <- read.csv(path, stringsAsFactors = FALSE)
        }
      }, silent = TRUE)

      # Clean up the data after loading
      if (!is.null(dat) && is.data.frame(dat)) {
        # Convert tibble to data.frame first
        dat <- as.data.frame(dat)

        # Check for row names column: first column with empty name or "...1"
        first_col_name <- names(dat)[1]
        if (first_col_name == "" || grepl("^\\.\\.\\.\\d+$", first_col_name)) {
          # Use first column as row names if it contains unique values
          first_col <- dat[[1]]
          if (is.character(first_col) && !anyDuplicated(first_col) && !any(is.na(first_col))) {
            rownames(dat) <- first_col
            dat <- dat[, -1, drop = FALSE]
          } else {
            # Just remove the auto-generated column
            dat <- dat[, -1, drop = FALSE]
          }
        }

        # Convert list columns to atomic vectors (readr sometimes creates these)
        for (col_name in names(dat)) {
          col_data <- dat[[col_name]]
          if (is.list(col_data) && !is.data.frame(col_data)) {
            # Try to unlist; if mixed types, convert to character
            tryCatch({
              unlisted <- unlist(col_data)
              if (length(unlisted) == nrow(dat)) {
                dat[[col_name]] <- unlisted
              } else {
                dat[[col_name]] <- as.character(col_data)
              }
            }, error = function(e) {
              dat[[col_name]] <<- as.character(col_data)
            })
          }
        }
      }

      cached$path <- path
      cached$mtime <- mtime
      cached$data <- dat
    }

    cached$data
  }



  # ----------------------------
  # THEME TOGGLE
  # ----------------------------
  current_theme <- shiny::reactiveVal("light")

  shiny::observeEvent(input$BUTTON_toggle_theme, {
    new_theme <- if (current_theme() == "light") "dark" else "light"
    current_theme(new_theme)

    # Apply the selected bs_theme object from the `themes` list
    session$setCurrentTheme(themes[[new_theme]])
  })


  # ----------------------------
  # LANGUAGE TOGGLE
  # ----------------------------
  current_language <- shiny::reactiveVal("en")

  shiny::observeEvent(input$BUTTON_language, {
    if (current_language() == "en") {
      current_language("fr")
    } else {
      current_language("en")
    }
  })


  # ----------------------------
  # TRANSLATION FUNCTION
  # ----------------------------
  tr <- shiny::reactive({
    lang <- current_language()

    function(key) {
      if (!key %in% names(translation)) {
        return(paste0("[Missing key: ", key, "]"))
      }
      if (!lang %in% names(translation[[key]])) {
        return(paste0("[Missing lang: ", lang, "]"))
      }
      translation[[key]][[lang]]
    }
  })


  # ----------------------------
  # DATASET INFO (shows filename if uploaded)
  # ----------------------------
  output$txt_DATASET_INFO <- shiny::renderUI({
    translate <- tr()
    fid <- current_file_input_id()
    fileval <- input[[fid]]
    if (!is.null(fileval) && nzchar(fileval$name)) {
      shiny::span(style = "margin-right:15px;",
           glue::glue("data : {fileval$name}"))
    } else {
      shiny::span(style = "margin-right:15px;", translate("DATASET_INFO"))
    }
  })

  # ==========================================================
  # PATTERN 3 — AUTO-GENERATE ALL TRANSLATED UI ELEMENTS
  # (skip DATASET_INFO because it is handled above)
  # ==========================================================
  shiny::observe({
    translate <- tr()

    for (key in names(translation)) {

      if (key == "DATASET_INFO") next  # skip custom output

      output_id <- paste0("txt_", key)

      local({
        my_key <- key
        my_id  <- output_id

        output[[my_id]] <- shiny::renderUI({
          shiny::span(translate(my_key))
        })
      })

    }
  })

  # SERVERS
  source(file.path("server", "upload_server.R"), local = TRUE)
  source(file.path("server", "home_server.R"), local = TRUE)
  source(file.path("server", "cluster_server.R"), local = TRUE)
}
