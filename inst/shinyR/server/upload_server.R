# server logic for the upload page (expects get_data(), cached, tr(), etc. in scope)

# helper to make safe input ids from column names
col_id_safe <- function(colname) {
  paste0("col_role_", gsub("[^A-Za-z0-9_]", "_", colname))
}
# new helpers for name & type inputs
col_name_id <- function(colname) {
  paste0("col_name_", gsub("[^A-Za-z0-9_]", "_", colname))
}
col_type_id <- function(colname) {
  paste0("col_type_", gsub("[^A-Za-z0-9_]", "_", colname))
}

# helper to get translated text or fallback
txt_or_default <- function(key, fallback) {
  translate <- tr()
  val <- tryCatch( translate(key), error = function(e) fallback )
  if (is.null(val) || grepl("^\\[Missing", val)) fallback else as.character(val)[1]
}

editable_names <- shiny::reactive({
  isTRUE(input$UPLOAD_EDIT_NAMES)
})

# update data_store when raw data is (re)loaded
shiny::observe({
  df <- get_data()
  data_store$dataset <- df
  if (is.null(df)) {
    data_store$illustrative_dataset <- NULL
  } else {
    # same number of rows, zero columns
    data_store$illustrative_dataset <- df[ , FALSE, drop = FALSE]
  }
})

# helper operator: default if NULL
`%||%` <- function(a, b) if (is.null(a)) b else a

# ensure get_column_meta exists (if not, define a fallback)
if (!exists("get_column_meta")) {
  get_column_meta <- shiny::reactive({
    df <- get_data()
    if (is.null(df)) return(NULL)
    cols <- names(df)
    meta <- lapply(cols, function(cn) {
      name_val <- input[[col_name_id(cn)]]
      type_val <- input[[col_type_id(cn)]]
      role_val <- input[[col_id_safe(cn)]]
      list(
        name = name_val %||% cn,
        type = type_val %||% (if (is.numeric(df[[cn]])) txt_or_default("UPLOAD_TYPE_QUANTITATIVE", "quantitative") else txt_or_default("UPLOAD_TYPE_QUALITATIVE", "qualitative")),
        role = role_val %||% txt_or_default("UPLOAD_ROLE_ACTIVE", "Active")
      )
    })
    names(meta) <- cols
    meta
  })
}
shiny::observeEvent(input$BUTTON_clear_upload, {
  # increment index so the fileInput is re-rendered with a new id (this clears the browser selection)
  file_input_index(file_input_index() + 1)

  # clear cache and stored data
  cached$path <- NULL
  cached$mtime <- NULL
  cached$data <- NULL

  data_store$dataset <- NULL
  data_store$illustrative_dataset <- NULL
  data_store$clustering_vars <- list(active = character(0), illustrative = character(0), excluded = character(0))

  # reset preview output and columns outputs to "no data"
  output$upload_preview <- shiny::renderTable({ NULL })
  output$upload_preview_no_data <- shiny::renderUI({
    shiny::tags$div(class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded"))
  })
  output$columns_card <- shiny::renderUI({
    shiny::tags$div(class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded"))
  })
  output$columns_table_rows <- shiny::renderUI({
    shiny::tags$tr(shiny::tags$td(colspan = 3, class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded")))
  })

  # update top DATASET_INFO output
  output$txt_DATASET_INFO <- shiny::renderText({
    translation[["DATASET_INFO"]][[current_language()]]
  })

  shiny::showNotification(txt_or_default("UPLOAD_BTN_CLEAR_UPLOAD", "Upload cleared"), type = "message")
})

# Save: apply edited names and populate clustering_vars according to current role selections.
shiny::observeEvent(input$BUTTON_SAVE_COLUMNS, {
  meta <- get_column_meta()
  if (is.null(meta) || length(meta) == 0) {
    shiny::showNotification(txt_or_default("DATASET_INFO", "No dataset loaded"), type = "warning")
    return()
  }

  # apply new names to data_store$dataset if present
  df <- data_store$dataset
  if (!is.null(df)) {
    new_names <- vapply(meta, function(x) x$name %||% "", FUN.VALUE = character(1))
    if (length(new_names) == ncol(df)) {
      names(df) <- new_names
      data_store$dataset <- df
    } else {
      shiny::showNotification("Column count mismatch, cannot rename", type = "error")
    }
  }

  # populate clustering_vars lists based on roles
  roles <- vapply(meta, function(x) x$role %||% "", FUN.VALUE = character(1))
  display_names <- vapply(meta, function(x) x$name %||% "", FUN.VALUE = character(1))

  data_store$clustering_vars <- list(
    active = display_names[which(roles == txt_or_default("UPLOAD_ROLE_ACTIVE", "Active"))],
    illustrative = display_names[which(roles == txt_or_default("UPLOAD_ROLE_ILLUSTRATIVE", "Illustrative"))],
    excluded = display_names[which(roles == txt_or_default("UPLOAD_ROLE_EXCLUDED", "Excluded"))]
  )

  shiny::showNotification(txt_or_default("UPLOAD_SAVE_DONE", "Saved"), type = "message")
})

# ----------------------------
# Columns compact card (first columns card)
# ----------------------------
output$columns_card <- shiny::renderUI({
  df <- get_data()
  if (is.null(df) || ncol(df) == 0) {
    return(shiny::tags$div(class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded")))
  }

  cols <- names(df)
  col_w <- "160px"
  row_padding <- "6px"
  col_padding <- "14px"
  row_gap <- "12px"

  # Build per-column controls (no extra margin)
  name_row_controls <- lapply(cols, function(cn) {
    if (editable_names()) {
      shiny::textInput(inputId = col_name_id(cn), label = NULL, value = cn, width = "100%")
    } else {
      shiny::tags$div(class = "form-control-plaintext", style = "padding:6px 8px; background:transparent; border:0; width:100%;", cn)
    }
  })

  type_row_controls <- lapply(cols, function(cn) {
    vec <- df[[cn]]
    estimated_label <- if (is.numeric(vec)) txt_or_default("UPLOAD_TYPE_QUANTITATIVE", "quantitative") else txt_or_default("UPLOAD_TYPE_QUALITATIVE", "qualitative")
    shiny::selectizeInput(
      inputId = col_type_id(cn), label = NULL,
      choices = c(
        txt_or_default("UPLOAD_TYPE_QUANTITATIVE", "quantitative"),
        txt_or_default("UPLOAD_TYPE_QUALITATIVE", "qualitative")
      ),
      selected = estimated_label,
      width = "100%",
      options = list(dropdownParent = "body")
    )
  })

  role_row_controls <- lapply(cols, function(cn) {
    vec <- df[[cn]]
    default_role <- txt_or_default("UPLOAD_ROLE_ACTIVE", "Active")
    shiny::selectizeInput(
      inputId = col_id_safe(cn), label = NULL,
      choices = c(
        txt_or_default("UPLOAD_ROLE_ACTIVE", "Active"),
        txt_or_default("UPLOAD_ROLE_ILLUSTRATIVE", "Illustrative"),
        txt_or_default("UPLOAD_ROLE_EXCLUDED", "Excluded")
      ),
      selected = default_role,
      width = "100%",
      options = list(dropdownParent = "body")
    )
  })

  # helper to build a horizontal set of column controls for a row
  build_row_block <- function(controls) {
    shiny::tags$div(style = paste0("display:flex; gap:", row_gap, ";"),
             lapply(seq_along(controls), function(i) {
               shiny::tags$div(style = paste0("flex:0 0 ", col_w, "; min-width:", col_w, "; padding:", row_padding, ";"), controls[[i]])
             })
    )
  }

  # labels column: FIXED - use same gap as rows (12px instead of 8px)
  labels_col <- shiny::tags$div(
    class = "d-flex flex-column",
    style = paste0("gap:", row_gap, "; flex:0 0 180px;"),
    # use Bootstrap plaintext so theming (dark/light, font sizing) is respected
    shiny::tags$div(class = "form-control-plaintext fw-semibold", style = paste0("padding:", col_padding, ";"), txt_or_default("UPLOAD_COL_NAME", "Name")),
    shiny::tags$div(class = "form-control-plaintext fw-semibold", style = paste0("padding:", col_padding, ";"), txt_or_default("UPLOAD_COL_TYPE", "Type")),
    shiny::tags$div(class = "form-control-plaintext fw-semibold", style = paste0("padding:", col_padding, ";"), txt_or_default("UPLOAD_COL_ROLE", "Role"))
  )

  # single scroll wrapper containing the three stacked rows
  columns_scroll <- shiny::tags$div(
    style = "overflow-x:auto; width:100%;",
    shiny::tags$div(style = "display:inline-block;",
             build_row_block(name_row_controls),
             build_row_block(type_row_controls),
             build_row_block(role_row_controls),
    )
  )

  shiny::tagList(
    shiny::tags$div(class = "mb-2 fw-semibold", paste(length(cols), "columns")),
    shiny::tags$div(class = "d-flex align-items-start gap-3",
      labels_col,
      columns_scroll
    ),
  )
})

# render the fileInput UI with the dynamic id
output$upload_input_ui <- shiny::renderUI({
  translate <- tr()
  fid <- current_file_input_id()
  shiny::fileInput(
    inputId = fid,
    label = translate("UPLOAD_LABEL"),
    multiple = FALSE,
    accept = c(".csv", ".xlsx", ".xls")
  )
})

# Columns table rows (detailed table) — use selectize to avoid clipping
output$columns_table_rows <- shiny::renderUI({
  df <- get_data()
  if (is.null(df) || ncol(df) == 0) {
    return(
      shiny::tags$tr(
        shiny::tags$td(colspan = 3, class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded"))
      )
    )
  }

  cols <- names(df)
  rows <- lapply(cols, function(cn) {
    vec <- df[[cn]]
    estimated_label <- if (is.numeric(vec)) txt_or_default("UPLOAD_TYPE_QUANTITATIVE", "quantitative") else txt_or_default("UPLOAD_TYPE_QUALITATIVE", "qualitative")
    sel_role_id <- col_id_safe(cn)
    sel_type_id <- col_type_id(cn)
    name_id <- col_name_id(cn)

    shiny::tags$tr(
      # name cell
      shiny::tags$td(
        if (editable_names()) {
          shiny::textInput(inputId = name_id, label = NULL, value = cn, width = "100%")
        } else {
          shiny::tags$div(style = "padding:6px 8px;", cn)
        }
      ),
      # type selector cell
      shiny::tags$td(
        shiny::selectizeInput(
          inputId = sel_type_id, label = NULL,
          choices = c(
            txt_or_default("UPLOAD_TYPE_QUANTITATIVE", "quantitative"),
            txt_or_default("UPLOAD_TYPE_QUALITATIVE", "qualitative")
          ),
          selected = estimated_label,
          width = "100%",
          options = list(dropdownParent = "body")
        )
      ),
      # role selector cell
      shiny::tags$td(
        shiny::selectizeInput(
          inputId = sel_role_id, label = NULL,
          choices = c(
            txt_or_default("UPLOAD_ROLE_ACTIVE", "Active"),
            txt_or_default("UPLOAD_ROLE_ILLUSTRATIVE", "Illustrative"),
            txt_or_default("UPLOAD_ROLE_EXCLUDED", "Excluded")
          ),
          selected = txt_or_default("UPLOAD_ROLE_ACTIVE", "Active"),
          width = "100%",
          options = list(dropdownParent = "body")
        )
      )
    )
  })

  do.call(shiny::tagList, rows)
})

output$upload_preview <- shiny::renderTable({
  df <- get_data()
  if (is.null(df)) return(NULL)
  head(df, 5)
}, striped = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

# Optional message when no data (keeps card body tidy)
output$upload_preview_no_data <- shiny::renderUI({
  if (is.null(get_data())) {
    shiny::tags$div(class = "text-muted", txt_or_default("DATASET_INFO", "No dataset loaded"))
  } else {
    NULL
  }
})