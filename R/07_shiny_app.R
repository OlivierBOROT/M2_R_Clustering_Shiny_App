#' @title Shiny App for Variable Clustering Analysis
#' @description Interactive Shiny application to demonstrate the M2RClust package
#' capabilities for variable clustering analysis.
#'
#' @details
#' This application allows users to:
#' \itemize{
#'   \item Load data files (CSV, TSV, Excel)
#'   \item Select active and supplementary variables
#'   \item Choose clustering algorithm (KMeansClusterer or DivisiveClusterer)
#'   \item Configure algorithm parameters
#'   \item Visualize results with multiple plot types
#'   \item Evaluate cluster quality with various metrics
#'   \item Export results
#' }
#'
#' @return A Shiny app object
#'
#' @examples
#' \dontrun{
#' # Launch the application
#' run_clustering_app()
#' }
#'
#' @importFrom utils read.csv read.delim write.csv
#' @export
run_clustering_app <- function() {
  # Check for required packages

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with: install.packages('shiny')")
  }
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("Package 'shinydashboard' is required. Install it with: install.packages('shinydashboard')")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required. Install it with: install.packages('DT')")
  }

  shiny::shinyApp(ui = app_ui(), server = app_server)
}

#' @title Application UI
#' @description Builds the user interface for the clustering application
#' @return A shinydashboard page
#' @keywords internal
app_ui <- function() {
  shinydashboard::dashboardPage(
    skin = "blue",

    # =========================================================================
    # HEADER
    # =========================================================================
    shinydashboard::dashboardHeader(
      title = "M2RClust - Variable Clustering",
      titleWidth = 300
    ),

    # =========================================================================
    # SIDEBAR
    # =========================================================================
    shinydashboard::dashboardSidebar(
      width = 300,
      shinydashboard::sidebarMenu(
        id = "tabs",

        # --- Data Import ---
        shinydashboard::menuItem("1. Data Import",
          tabName = "data_import",
          icon = shiny::icon("file-import")
        ),

        # --- Variable Selection ---
        shinydashboard::menuItem("2. Variable Selection",
          tabName = "var_selection",
          icon = shiny::icon("list-check")
        ),

        # --- Algorithm Configuration ---
        shinydashboard::menuItem("3. Algorithm",
          tabName = "algorithm",
          icon = shiny::icon("cogs")
        ),

        # --- Results ---
        shinydashboard::menuItem("4. Results",
          tabName = "results",
          icon = shiny::icon("chart-pie")
        ),

        # --- Visualizations ---
        shinydashboard::menuItem("5. Visualizations",
          tabName = "visualizations",
          icon = shiny::icon("chart-line")
        ),

        # --- K Selection ---
        shinydashboard::menuItem("6. Optimal K",
          tabName = "k_selection",
          icon = shiny::icon("search")
        ),

        # --- Export ---
        shinydashboard::menuItem("7. Export",
          tabName = "export",
          icon = shiny::icon("download")
        ),

        shiny::hr(),

        # --- About ---
        shinydashboard::menuItem("About",
          tabName = "about",
          icon = shiny::icon("info-circle")
        )
      )
    ),

    # =========================================================================
    # BODY
    # =========================================================================
    shinydashboard::dashboardBody(
      # Custom CSS
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .content-wrapper { background-color: #f4f6f9; }
          .box-header { background-color: #3c8dbc; color: white; }
          .info-box { min-height: 90px; }
          .nav-tabs-custom > .tab-content { padding: 15px; }
          .variable-list { max-height: 300px; overflow-y: auto; }
          .result-text { font-family: monospace; white-space: pre-wrap; }
          .plot-container { min-height: 500px; }
        "))
      ),

      shinydashboard::tabItems(
        # =====================================================================
        # TAB 1: DATA IMPORT
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "data_import",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Import Data File",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              shiny::fileInput("file_input",
                label = "Choose a data file:",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values",
                  "text/tab-separated-values",
                  ".csv",
                  ".tsv",
                  ".txt",
                  ".xlsx",
                  ".xls"
                ),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),

              shiny::hr(),

              shiny::h4("File Options"),

              shiny::radioButtons("file_type",
                label = "File type:",
                choices = c(
                  "CSV (comma separated)" = "csv",
                  "TSV (tab separated)" = "tsv",
                  "Excel (.xlsx)" = "xlsx"
                ),
                selected = "csv"
              ),

              shiny::conditionalPanel(
                condition = "input.file_type == 'csv' || input.file_type == 'tsv'",
                shiny::checkboxInput("header", "First row is header", value = TRUE),
                shiny::radioButtons("decimal",
                  label = "Decimal separator:",
                  choices = c("Point (.)" = ".", "Comma (,)" = ","),
                  selected = ".",
                  inline = TRUE
                )
              ),

              shiny::conditionalPanel(
                condition = "input.file_type == 'xlsx'",
                shiny::numericInput("sheet_number",
                  label = "Sheet number:",
                  value = 1,
                  min = 1,
                  max = 10
                )
              ),

              shiny::hr(),
              shiny::actionButton("load_data",
                label = "Load Data",
                icon = shiny::icon("upload"),
                class = "btn-primary btn-lg"
              )
            ),

            shinydashboard::box(
              title = "Demo Datasets",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              shiny::p("You can also use built-in demo datasets:"),

              shiny::actionButton("load_mtcars",
                label = "Load mtcars (numeric)",
                icon = shiny::icon("car"),
                class = "btn-info"
              ),
              shiny::br(), shiny::br(),

              shiny::actionButton("load_iris",
                label = "Load iris (numeric + factor)",
                icon = shiny::icon("leaf"),
                class = "btn-info"
              ),
              shiny::br(), shiny::br(),

              shiny::p(shiny::em("mtcars: 32 observations, 11 numeric variables")),
              shiny::p(shiny::em("iris: 150 observations, 4 numeric + 1 factor"))
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Data Preview",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,

              shiny::verbatimTextOutput("data_summary"),
              shiny::hr(),
              DT::dataTableOutput("data_preview")
            )
          )
        ),

        # =====================================================================
        # TAB 2: VARIABLE SELECTION
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "var_selection",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Active Variables (for clustering)",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              shiny::p("Select variables to include in the clustering analysis:"),

              shiny::checkboxGroupInput("active_vars",
                label = NULL,
                choices = NULL
              ),

              shiny::hr(),

              shiny::actionButton("select_all_active",
                label = "Select All Numeric",
                icon = shiny::icon("check-double"),
                class = "btn-sm btn-default"
              ),

              shiny::actionButton("deselect_all_active",
                label = "Deselect All",
                icon = shiny::icon("times"),
                class = "btn-sm btn-default"
              )
            ),

            shinydashboard::box(
              title = "Illustrative Variables (not used in clustering)",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              shiny::p("Select illustrative variables to visualize alongside clustering results:"),

              shiny::checkboxGroupInput("illustrative_vars",
                label = NULL,
                choices = NULL
              ),

              shiny::hr(),

              shiny::p(shiny::em(
                "Illustrative variables are not used in clustering but are projected onto the visualization to aid interpretation."
              ))
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Variable Summary",
              status = "success",
              solidHeader = TRUE,
              width = 12,

              shiny::fluidRow(
                shiny::column(4,
                  shinydashboard::infoBoxOutput("n_active_vars", width = 12)
                ),
                shiny::column(4,
                  shinydashboard::infoBoxOutput("n_supp_vars", width = 12)
                ),
                shiny::column(4,
                  shinydashboard::infoBoxOutput("n_observations", width = 12)
                )
              ),

              shiny::hr(),

              shiny::h4("Variable Types"),
              shiny::verbatimTextOutput("var_types_summary")
            )
          )
        ),

        # =====================================================================
        # TAB 3: ALGORITHM CONFIGURATION
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "algorithm",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Choose Algorithm",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              shiny::radioButtons("algorithm_choice",
                label = "Clustering Algorithm:",
                choices = c(
                  "KMeansClusterer (iterative, multiple initializations)" = "kmeans",
                  "DivisiveClusterer (hierarchical, deterministic)" = "divisive"
                ),
                selected = "kmeans"
              ),

              shiny::hr(),

              shiny::h4("Common Parameters"),

              shiny::numericInput("n_clusters",
                label = "Number of clusters (K):",
                value = 3,
                min = 2,
                max = 20,
                step = 1
              ),

              shiny::checkboxInput("standardize",
                label = "Standardize data (recommended)",
                value = TRUE
              )
            ),

            shinydashboard::box(
              title = "Algorithm-Specific Parameters",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              # KMeansClusterer parameters
              shiny::conditionalPanel(
                condition = "input.algorithm_choice == 'kmeans'",

                shiny::selectInput("init_method",
                  label = "Initialization method:",
                  choices = c(
                    "homogeneity++ (recommended)" = "homogeneity++",
                    "correlation (hierarchical)" = "correlation",
                    "random" = "random"
                  ),
                  selected = "homogeneity++"
                ),

                shiny::numericInput("n_init",
                  label = "Number of initializations:",
                  value = 10,
                  min = 1,
                  max = 50
                ),

                shiny::numericInput("max_iter",
                  label = "Maximum iterations:",
                  value = 100,
                  min = 10,
                  max = 500
                ),

                shiny::numericInput("seed",
                  label = "Random seed (for reproducibility):",
                  value = 42,
                  min = 1
                )
              ),

              # DivisiveClusterer parameters
              shiny::conditionalPanel(
                condition = "input.algorithm_choice == 'divisive'",

                shiny::selectInput("rotation_method",
                  label = "Rotation method:",
                  choices = c(
                    "varimax (orthogonal)" = "varimax",
                    "promax (oblique)" = "promax",
                    "none" = "none"
                  ),
                  selected = "varimax"
                ),

                shiny::selectInput("split_criterion",
                  label = "Split criterion:",
                  choices = c(
                    "eigenvalue2 (largest λ₂)" = "eigenvalue2",
                    "homogeneity (lowest homogeneity)" = "homogeneity"
                  ),
                  selected = "eigenvalue2"
                ),

                shiny::checkboxInput("stop_at_kaiser",
                  label = "Stop at Kaiser criterion (λ₂ < 1)",
                  value = FALSE
                ),

                shiny::numericInput("min_cluster_size",
                  label = "Minimum cluster size:",
                  value = 2,
                  min = 2,
                  max = 10
                )
              )
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Run Clustering",
              status = "success",
              solidHeader = TRUE,
              width = 12,

              shiny::actionButton("run_clustering",
                label = "Run Clustering Algorithm",
                icon = shiny::icon("play"),
                class = "btn-success btn-lg"
              ),

              shiny::hr(),

              shiny::verbatimTextOutput("clustering_log")
            )
          )
        ),

        # =====================================================================
        # TAB 4: RESULTS
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "results",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Clustering Summary",
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              shiny::verbatimTextOutput("clustering_summary", placeholder = TRUE)
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Cluster Assignments",
              status = "success",
              solidHeader = TRUE,
              width = 6,

              DT::dataTableOutput("cluster_assignments")
            ),

            shinydashboard::box(
              title = "Cluster Statistics",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              shiny::fluidRow(
                shiny::column(6,
                  shinydashboard::valueBoxOutput("global_homogeneity", width = 12)
                ),
                shiny::column(6,
                  shinydashboard::valueBoxOutput("n_clusters_found", width = 12)
                )
              ),

              shiny::hr(),

              shiny::h4("Homogeneity by Cluster"),
              shiny::plotOutput("homogeneity_barplot", height = "250px")
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Variables by Cluster",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,

              shiny::uiOutput("cluster_members_ui")
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Illustrative Variables (projected)",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,

              shiny::uiOutput("illustrative_vars_ui")
            )
          )
        ),

        # =====================================================================
        # TAB 5: VISUALIZATIONS
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "visualizations",
          shiny::fluidRow(
            shinydashboard::tabBox(
              title = "Visualizations",
              width = 12,

              # Correlation Circle
              shiny::tabPanel(
                title = "Correlation Circle",
                icon = shiny::icon("circle"),

                shiny::fluidRow(
                  shiny::column(3,
                    shiny::checkboxInput("show_centers", "Show cluster centers", value = TRUE),
                    shiny::checkboxInput("show_labels", "Show variable labels", value = TRUE),
                    shiny::hr(),
                    shiny::h5("Additional variables:"),
                    shiny::checkboxInput("show_illustrative_vars", "Show illustrative variables", value = FALSE),
                    shiny::helpText(shiny::em("(dashed lines - from step 2)"))
                  ),
                  shiny::column(9,
                    shiny::plotOutput("correlation_circle", height = "600px")
                  )
                )
              ),

              # Correlation Heatmap
              shiny::tabPanel(
                title = "Correlation Heatmap",
                icon = shiny::icon("th"),

                shiny::fluidRow(
                  shiny::column(3,
                    shiny::checkboxInput("reorder_heatmap",
                      "Reorder by cluster",
                      value = TRUE
                    )
                  ),
                  shiny::column(9,
                    shiny::plotOutput("correlation_heatmap", height = "600px")
                  )
                )
              ),

              # Cluster Quality
              shiny::tabPanel(
                title = "Cluster Quality",
                icon = shiny::icon("chart-bar"),

                shiny::plotOutput("cluster_quality_plot", height = "400px")
              ),

              # Scree Plot
              shiny::tabPanel(
                title = "Scree Plot by Cluster",
                icon = shiny::icon("chart-area"),

                shiny::plotOutput("scree_by_cluster", height = "500px")
              ),

              # Variable Contributions
              shiny::tabPanel(
                title = "Variable Contributions",
                icon = shiny::icon("arrows-alt"),

                shiny::fluidRow(
                  shiny::column(3,
                    shiny::selectInput("contrib_cluster",
                      label = "Select cluster:",
                      choices = NULL
                    ),
                    shiny::numericInput("contrib_top_n",
                      label = "Top N variables:",
                      value = 5,
                      min = 1,
                      max = 20
                    )
                  ),
                  shiny::column(9,
                    shiny::plotOutput("variable_contributions", height = "400px")
                  )
                )
              ),

              # Network Graph
              shiny::tabPanel(
                title = "Network Graph",
                icon = shiny::icon("project-diagram"),

                shiny::fluidRow(
                  shiny::column(3,
                    shiny::sliderInput("network_threshold",
                      label = "Correlation threshold:",
                      min = 0.1,
                      max = 0.9,
                      value = 0.5,
                      step = 0.1
                    ),
                    shiny::selectInput("network_layout",
                      label = "Layout:",
                      choices = c(
                        "Fruchterman-Reingold" = "fruchterman.reingold",
                        "Circle" = "circle",
                        "Kamada-Kawai" = "kamada.kawai"
                      )
                    )
                  ),
                  shiny::column(9,
                    shiny::plotOutput("network_graph", height = "500px")
                  )
                )
              ),

              # Dendrogram (for DivisiveClusterer)
              shiny::tabPanel(
                title = "Split Dendrogram",
                icon = shiny::icon("sitemap"),

                shiny::conditionalPanel(
                  condition = "input.algorithm_choice == 'divisive'",
                  shiny::plotOutput("split_dendrogram", height = "500px")
                ),

                shiny::conditionalPanel(
                  condition = "input.algorithm_choice == 'kmeans'",
                  shiny::p(shiny::em("Dendrogram is only available for DivisiveClusterer."))
                )
              )
            )
          )
        ),

        # =====================================================================
        # TAB 6: OPTIMAL K SELECTION
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "k_selection",
          shiny::fluidRow(
            shinydashboard::box(
              title = "K Selection Parameters",
              status = "primary",
              solidHeader = TRUE,
              width = 4,

              shiny::sliderInput("k_range",
                label = "Range of K to test:",
                min = 2,
                max = 15,
                value = c(2, 8),
                step = 1
              ),

              shiny::hr(),

              shiny::actionButton("run_k_selection",
                label = "Evaluate K Values",
                icon = shiny::icon("search"),
                class = "btn-primary btn-lg"
              ),

              shiny::hr(),

              shiny::h4("Recommended K"),
              shiny::verbatimTextOutput("k_recommendation")
            ),

            shinydashboard::box(
              title = "K Selection Methods Comparison",
              status = "success",
              solidHeader = TRUE,
              width = 8,

              shiny::plotOutput("k_selection_plots", height = "400px")
            )
          ),

          shiny::fluidRow(
            shinydashboard::box(
              title = "Method Details",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = TRUE,

              shiny::fluidRow(
                shiny::column(4,
                  shiny::h4("Elbow Method"),
                  shiny::p("Looks for the 'elbow' point where adding more clusters provides diminishing returns."),
                  shiny::plotOutput("elbow_plot", height = "250px")
                ),
                shiny::column(4,
                  shiny::h4("Silhouette Method"),
                  shiny::p("Measures how similar variables are to their own cluster vs. other clusters."),
                  shiny::plotOutput("silhouette_plot", height = "250px")
                ),
                shiny::column(4,
                  shiny::h4("Calinski-Harabasz"),
                  shiny::p("Ratio of between-cluster to within-cluster variance (Pseudo-F)."),
                  shiny::plotOutput("calinski_plot", height = "250px")
                )
              )
            )
          )
        ),

        # =====================================================================
        # TAB 7: EXPORT
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "export",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Export Cluster Assignments",
              status = "primary",
              solidHeader = TRUE,
              width = 6,

              shiny::p("Download the cluster assignments as a CSV file:"),

              shiny::downloadButton("download_assignments",
                label = "Download Assignments (CSV)",
                class = "btn-primary"
              ),

              shiny::hr(),

              shiny::h4("Preview"),
              DT::dataTableOutput("export_preview")
            ),

            shinydashboard::box(
              title = "Export Full Report",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              shiny::p("Generate a comprehensive HTML report:"),

              shiny::downloadButton("download_report",
                label = "Download Report (HTML)",
                class = "btn-info"
              ),

              shiny::hr(),

              shiny::h4("Report Contents"),
              shiny::tags$ul(
                shiny::tags$li("Data summary"),
                shiny::tags$li("Algorithm configuration"),
                shiny::tags$li("Cluster assignments"),
                shiny::tags$li("Homogeneity metrics"),
                shiny::tags$li("Visualizations")
              )
            )
          )
        ),

        # =====================================================================
        # TAB: ABOUT
        # =====================================================================
        shinydashboard::tabItem(
          tabName = "about",
          shiny::fluidRow(
            shinydashboard::box(
              title = "About M2RClust",
              status = "primary",
              solidHeader = TRUE,
              width = 12,

              shiny::h3("M2RClust: Variable Clustering Package"),

              shiny::p("This package provides tools for clustering variables (not observations)
                       based on their correlations and associations."),

              shiny::hr(),

              shiny::h4("Available Algorithms"),

              shiny::tags$ul(
                shiny::tags$li(
                  shiny::strong("KMeansClusterer:"),
                  "Iterative algorithm that maximizes within-cluster homogeneity.
                  Supports multiple initialization methods (homogeneity++, correlation, random)."
                ),
                shiny::tags$li(
                  shiny::strong("DivisiveClusterer:"),
                  "Hierarchical divisive algorithm (VARCLUS-style). Deterministic results,
                  provides interpretable split history."
                )
              ),

              shiny::hr(),

              shiny::h4("Key Features"),

              shiny::tags$ul(
                shiny::tags$li("Support for numeric and mixed (numeric + categorical) data"),
                shiny::tags$li("Multiple visualization options (correlation circle, heatmap, network)"),
                shiny::tags$li("Automatic K selection methods (Elbow, Silhouette, Calinski-Harabasz)"),
                shiny::tags$li("Prediction on supplementary variables"),
                shiny::tags$li("Export results and reports")
              ),

              shiny::hr(),

              shiny::h4("Authors"),

              shiny::p("Developed as part of the R Programming course (Master 2 SISE),
                       Université Lumière Lyon 2."),

              shiny::tags$ul(
                shiny::tags$li("Léo-Paul Knoepffler"),
                shiny::tags$li("Olivier Borot"),
                shiny::tags$li("Perrine Ibouroi")
              ),

              shiny::hr(),

              shiny::h4("References"),

              shiny::tags$ul(
                shiny::tags$li("Chavent, M., et al. (2012). ClustOfVar: An R Package for the Clustering of Variables."),
                shiny::tags$li("Sarle, W. S. (1990). The VARCLUS Procedure. SAS/STAT User's Guide.")
              )
            )
          )
        )
      )
    )
  )
}

#' @title Application Server Logic
#' @description Defines the server-side logic for the clustering application
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @keywords internal
app_server <- function(input, output, session) {
  # ===========================================================================
  # REACTIVE VALUES
  # ===========================================================================

  rv <- shiny::reactiveValues(
    data = NULL, # Loaded data
    data_name = NULL, # Name of the dataset
    active_vars = NULL, # Selected active variables
    illustrative_vars = NULL, # Selected illustrative variables (not used in clustering)
    clusterer = NULL, # Fitted clusterer object
    illustrative_results = NULL, # Illustrative variables projection results
    k_results = NULL # K selection results
  )

  # ===========================================================================
  # DATA IMPORT
  # ===========================================================================

  # Load data from file
  shiny::observeEvent(input$load_data, {
    shiny::req(input$file_input)

    tryCatch(
      {
        file_path <- input$file_input$datapath
        file_name <- input$file_input$name

        # Read based on file type
        if (input$file_type == "csv") {
          rv$data <- utils::read.csv(file_path,
            header = input$header,
            dec = input$decimal,
            stringsAsFactors = TRUE
          )
        } else if (input$file_type == "tsv") {
          rv$data <- utils::read.delim(file_path,
            header = input$header,
            dec = input$decimal,
            stringsAsFactors = TRUE
          )
        } else if (input$file_type == "xlsx") {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            shiny::showNotification("Package 'readxl' required for Excel files.",
              type = "error"
            )
            return()
          }
          rv$data <- as.data.frame(readxl::read_excel(file_path,
            sheet = input$sheet_number
          ))
        }

        rv$data_name <- file_name
        rv$clusterer <- NULL # Reset clusterer

        # Update variable selection
        update_var_choices()

        shiny::showNotification(
          paste("Data loaded successfully:", nrow(rv$data), "rows,", ncol(rv$data), "columns"),
          type = "message"
        )
      },
      error = function(e) {
        shiny::showNotification(paste("Error loading file:", e$message),
          type = "error"
        )
      }
    )
  })

  # Load mtcars demo dataset
shiny::observeEvent(input$load_mtcars, {
    rv$data <- datasets::mtcars
    rv$data_name <- "mtcars"
    rv$clusterer <- NULL
    update_var_choices()
    shiny::showNotification("mtcars dataset loaded (32 obs, 11 variables)",
      type = "message"
    )
  })

  # Load iris demo dataset
  shiny::observeEvent(input$load_iris, {
    rv$data <- datasets::iris
    rv$data_name <- "iris"
    rv$clusterer <- NULL
    update_var_choices()
    shiny::showNotification("iris dataset loaded (150 obs, 5 variables)",
      type = "message"
    )
  })

  # Update variable choices based on loaded data
  update_var_choices <- function() {
    shiny::req(rv$data)

    var_names <- colnames(rv$data)
    var_types <- sapply(rv$data, function(x) {
      if (is.numeric(x)) {
        "numeric"
      } else if (is.factor(x)) {
        "factor"
      } else {
        "other"
      }
    })

    # Create labeled choices
    choices <- setNames(var_names, paste0(var_names, " [", var_types, "]"))

    # Default: select all numeric variables as active
    numeric_vars <- var_names[var_types == "numeric"]

    shiny::updateCheckboxGroupInput(session, "active_vars",
      choices = choices,
      selected = numeric_vars
    )

    shiny::updateCheckboxGroupInput(session, "illustrative_vars",
      choices = choices,
      selected = NULL
    )
  }

  # Select all numeric variables
  shiny::observeEvent(input$select_all_active, {
    shiny::req(rv$data)
    numeric_vars <- colnames(rv$data)[sapply(rv$data, is.numeric)]
    var_types <- sapply(rv$data, function(x) {
      if (is.numeric(x)) {
        "numeric"
      } else if (is.factor(x)) {
        "factor"
      } else {
        "other"
      }
    })
    choices <- setNames(colnames(rv$data), paste0(colnames(rv$data), " [", var_types, "]"))
    shiny::updateCheckboxGroupInput(session, "active_vars",
      choices = choices,
      selected = numeric_vars
    )
  })

  # Deselect all active variables
  shiny::observeEvent(input$deselect_all_active, {
    shiny::req(rv$data)
    var_types <- sapply(rv$data, function(x) {
      if (is.numeric(x)) {
        "numeric"
      } else if (is.factor(x)) {
        "factor"
      } else {
        "other"
      }
    })
    choices <- setNames(colnames(rv$data), paste0(colnames(rv$data), " [", var_types, "]"))
    shiny::updateCheckboxGroupInput(session, "active_vars",
      choices = choices,
      selected = character(0)
    )
  })

  # Data preview outputs
  output$data_summary <- shiny::renderPrint({
    shiny::req(rv$data)
    cat("Dataset:", rv$data_name, "\n")
    cat("Dimensions:", nrow(rv$data), "observations x", ncol(rv$data), "variables\n\n")
    cat("Variable types:\n")
    print(sapply(rv$data, class))
  })

  output$data_preview <- DT::renderDataTable({
    shiny::req(rv$data)
    DT::datatable(rv$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # ===========================================================================
  # VARIABLE SELECTION
  # ===========================================================================

  output$n_active_vars <- shinydashboard::renderInfoBox({
    n <- length(input$active_vars)
    shinydashboard::infoBox(
      title = "Active Variables",
      value = n,
      icon = shiny::icon("list"),
      color = if (n >= 2) "green" else "red"
    )
  })

  output$n_supp_vars <- shinydashboard::renderInfoBox({
    n <- length(input$illustrative_vars)
    shinydashboard::infoBox(
      title = "Illustrative Variables",
      value = n,
      icon = shiny::icon("eye"),
      color = "blue"
    )
  })

  output$n_observations <- shinydashboard::renderInfoBox({
    n <- if (!is.null(rv$data)) nrow(rv$data) else 0
    shinydashboard::infoBox(
      title = "Observations",
      value = n,
      icon = shiny::icon("users"),
      color = "purple"
    )
  })

  output$var_types_summary <- shiny::renderPrint({
    shiny::req(rv$data, input$active_vars)

    active_data <- rv$data[, input$active_vars, drop = FALSE]

    cat("Active variables types:\n")
    types <- sapply(active_data, function(x) {
      if (is.numeric(x)) "numeric" else if (is.factor(x)) "factor" else class(x)[1]
    })
    print(table(types))

    if (length(input$illustrative_vars) > 0) {
      cat("\nIllustrative variables types:\n")
      illus_data <- rv$data[, input$illustrative_vars, drop = FALSE]
      types_illus <- sapply(illus_data, function(x) {
        if (is.numeric(x)) "numeric" else if (is.factor(x)) "factor" else class(x)[1]
      })
      print(table(types_illus))
    }
  })

  # ===========================================================================
  # CLUSTERING EXECUTION
  # ===========================================================================

  shiny::observeEvent(input$run_clustering, {
    shiny::req(rv$data, input$active_vars)

    # Validate inputs
    if (length(input$active_vars) < 2) {
      shiny::showNotification("Please select at least 2 active variables.",
        type = "error"
      )
      return()
    }

    if (input$n_clusters >= length(input$active_vars)) {
      shiny::showNotification("Number of clusters must be less than number of variables.",
        type = "error"
      )
      return()
    }

    # Show progress
    shiny::withProgress(message = "Running clustering...", value = 0, {
      tryCatch(
        {
          # Prepare data
          active_data <- rv$data[, input$active_vars, drop = FALSE]

          # Create clusterer based on algorithm choice
          if (input$algorithm_choice == "kmeans") {
            shiny::incProgress(0.3, detail = "Creating KMeansClusterer...")

            rv$clusterer <- KMeansClusterer$new(
              data = active_data,
              n_clusters = input$n_clusters,
              standardize = input$standardize,
              init_method = input$init_method,
              n_init = input$n_init,
              max_iter = input$max_iter,
              seed = input$seed
            )
          } else {
            shiny::incProgress(0.3, detail = "Creating DivisiveClusterer...")

            rv$clusterer <- DivisiveClusterer$new(
              data = active_data,
              n_clusters = input$n_clusters,
              standardize = input$standardize,
              rotation_method = input$rotation_method,
              split_criterion = input$split_criterion,
              stop_at_kaiser = input$stop_at_kaiser,
              min_cluster_size = input$min_cluster_size
            )
          }

          shiny::incProgress(0.4, detail = "Fitting model...")

          # Capture output
          output_text <- utils::capture.output({
            rv$clusterer$fit()
          })

          # Project illustrative variables if selected
          rv$illustrative_results <- NULL
          if (length(input$illustrative_vars) > 0) {
            shiny::incProgress(0.2, detail = "Projecting illustrative variables...")
            tryCatch({
              illus_data <- rv$data[, input$illustrative_vars, drop = FALSE]
              rv$illustrative_results <- rv$clusterer$predict(illus_data)
              # Prepare plot data for illustrative variables (for correlation circle)
              rv$clusterer$prepare_plot_predict(illus_data, rv$illustrative_results)
            }, error = function(e) {
              shiny::showNotification(
                paste("Warning: Could not project illustrative variables:", e$message),
                type = "warning"
              )
            })
          }

          shiny::incProgress(0.1, detail = "Done!")

          # Update cluster choices for visualization
          shiny::updateSelectInput(session, "contrib_cluster",
            choices = paste("Cluster", 1:rv$clusterer$n_clusters),
            selected = "Cluster 1"
          )

          shiny::showNotification("Clustering completed successfully!",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(paste("Clustering failed:", e$message),
            type = "error"
          )
        }
      )
    })
  })

  output$clustering_log <- shiny::renderPrint({
    shiny::req(rv$clusterer)
    if (rv$clusterer$fitted) {
      cat("Clustering completed.\n")
      cat("Algorithm:", if (input$algorithm_choice == "kmeans") "KMeansClusterer" else "DivisiveClusterer", "\n")
      cat("Number of clusters:", rv$clusterer$n_clusters, "\n")
      cat("Global homogeneity:", round(rv$clusterer$get_homogeneity(), 4), "\n")
    } else {
      cat("Clustering not yet performed.\n")
    }
  })

  # ===========================================================================
  # RESULTS TAB
  # ===========================================================================

  output$clustering_summary <- shiny::renderPrint({
    shiny::req(rv$clusterer, rv$clusterer$fitted)
    rv$clusterer$summary()
  })

  output$cluster_assignments <- DT::renderDataTable({
    shiny::req(rv$clusterer, rv$clusterer$fitted)
    results <- rv$clusterer$get_results()
    DT::datatable(results,
      options = list(
        pageLength = 15,
        dom = "ftp"
      )
    )
  })

  output$global_homogeneity <- shinydashboard::renderValueBox({
    if (is.null(rv$clusterer) || !rv$clusterer$fitted) {
      value <- "N/A"
      color <- "red"
    } else {
      value <- sprintf("%.3f", rv$clusterer$get_homogeneity())
      homog <- rv$clusterer$get_homogeneity()
      color <- if (homog >= 0.7) "green" else if (homog >= 0.5) "yellow" else "red"
    }

    shinydashboard::valueBox(
      value = value,
      subtitle = "Global Homogeneity",
      icon = shiny::icon("chart-pie"),
      color = color
    )
  })

  output$n_clusters_found <- shinydashboard::renderValueBox({
    if (is.null(rv$clusterer) || !rv$clusterer$fitted) {
      value <- "N/A"
    } else {
      value <- rv$clusterer$n_clusters
    }

    shinydashboard::valueBox(
      value = value,
      subtitle = "Number of Clusters",
      icon = shiny::icon("layer-group"),
      color = "blue"
    )
  })

  output$homogeneity_barplot <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    homog <- rv$clusterer$get_cluster_homogeneity()
    n_clusters <- length(homog)

    barplot(homog,
      names.arg = paste("C", 1:n_clusters),
      col = grDevices::rainbow(n_clusters),
      main = "",
      ylab = "Homogeneity",
      ylim = c(0, 1),
      las = 1
    )
    abline(h = 0.5, lty = 2, col = "gray50")
    abline(h = 0.7, lty = 2, col = "gray50")
  })

  output$cluster_members_ui <- shiny::renderUI({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    n_clusters <- rv$clusterer$n_clusters
    results <- rv$clusterer$get_results()

    # Create a column for each cluster
    cols <- lapply(1:n_clusters, function(k) {
      vars <- results$variable[results$cluster == k]
      homog <- rv$clusterer$get_cluster_homogeneity()[k]

      shiny::column(
        width = floor(12 / min(n_clusters, 4)),
        shiny::wellPanel(
          shiny::h4(paste("Cluster", k)),
          shiny::p(shiny::strong(paste("Homogeneity:", round(homog, 3)))),
          shiny::p(paste(length(vars), "variables:")),
          shiny::tags$ul(
            lapply(vars, function(v) shiny::tags$li(v))
          )
        )
      )
    })

    do.call(shiny::fluidRow, cols)
  })

  # Display illustrative variables assignments
  output$illustrative_vars_ui <- shiny::renderUI({
    if (is.null(rv$illustrative_results) || nrow(rv$illustrative_results) == 0) {
      return(shiny::p(shiny::em("No illustrative variables selected or projected.")))
    }

    shiny::tagList(
      shiny::h4("Illustrative Variables Assignments"),
      shiny::p(shiny::em("These variables were not used in clustering but projected onto the results for interpretation.")),
      DT::renderDataTable({
        DT::datatable(rv$illustrative_results,
          options = list(pageLength = 10, dom = "tp"),
          rownames = FALSE
        )
      })
    )
  })

  # ===========================================================================
  # VISUALIZATIONS TAB
  # ===========================================================================

  output$correlation_circle <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    # Get base plot data
    plot_data <- rv$clusterer$get_plot_data()
    if (is.null(plot_data)) return(NULL)

    coords <- plot_data$coords
    coords$type <- "active"
    cluster_colors <- plot_data$colors
    pca <- plot_data$pca
    centers <- plot_data$centers

    # Add illustrative variables if checked and available
    if (input$show_illustrative_vars && !is.null(rv$illustrative_results)) {
      illus_plot_data <- rv$clusterer$get_plot_data_predict()
      if (!is.null(illus_plot_data)) {
        illus_coords <- illus_plot_data$coords
        illus_coords <- illus_coords[illus_coords$type == "illustrative", ]
        if (nrow(illus_coords) > 0) {
          coords <- rbind(coords, illus_coords)
        }
      }
    }

    # Calculate variance explained
    var_exp_1 <- 0; var_exp_2 <- 0
    if (!is.null(pca)) {
      if (inherits(pca, "prcomp")) {
        ve <- summary(pca)$importance[2,] * 100
        var_exp_1 <- round(ve[1], 1)
        var_exp_2 <- round(ve[2], 1)
      } else {
        ve <- pca$eig[, 2]
        var_exp_1 <- round(ve[1], 1)
        var_exp_2 <- round(ve[2], 1)
      }
    }

    # Plot
    plot(0, 0, type = "n", xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
         xlab = paste0("Dim 1 (", var_exp_1, "%)"),
         ylab = paste0("Dim 2 (", var_exp_2, "%)"),
         main = "Correlation Circle", las = 1, asp = 1)

    symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "gray60")
    abline(h = 0, v = 0, col = "gray80", lty = 2)

    # Define line styles: active=solid, illustrative=dashed
    line_types <- ifelse(coords$type == "active", 1, 2)
    line_widths <- ifelse(coords$type == "active", 1.5, 1.2)
    cols <- cluster_colors[as.numeric(coords$cluster)]

    # Draw arrows
    arrows(x0 = 0, y0 = 0,
           x1 = coords$PC1, y1 = coords$PC2,
           col = cols, lty = line_types, lwd = line_widths, length = 0.1)

    # Labels
    if (input$show_labels) {
      font_styles <- ifelse(coords$type == "active", 2, 3)
      text(coords$PC1 * 1.15, coords$PC2 * 1.15,
           labels = coords$variable,
           col = cols, cex = 0.8, font = font_styles)
    }

    # Centers
    if (input$show_centers && !is.null(centers)) {
      points(centers[, 1], centers[, 2],
             pch = 22, bg = "white", col = "black", cex = 2, lwd = 2)
      text(centers[, 1], centers[, 2],
           labels = gsub("C", "", rownames(centers)),
           col = "black", font = 2, cex = 0.8)
    }

    # Legend for clusters
    legend("topleft",
           legend = paste("Cluster", levels(coords$cluster)),
           col = cluster_colors[1:length(levels(coords$cluster))],
           lwd = 2, bty = "n", cex = 0.8, title = "Groups")

    # Legend for variable types if multiple types present
    types_present <- unique(coords$type)
    if (length(types_present) > 1) {
      legend("bottomleft",
             legend = c("Active", "Illustrative")[c("active", "illustrative") %in% types_present],
             lty = c(1, 2)[c("active", "illustrative") %in% types_present],
             lwd = 1.5, col = "black", bty = "n", cex = 0.8, title = "Variable Type")
    }
  })

  output$correlation_heatmap <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)
    plot_correlation_heatmap(rv$clusterer, reorder = input$reorder_heatmap)
  })

  output$cluster_quality_plot <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)
    plot_cluster_quality(rv$clusterer)
  })

  output$scree_by_cluster <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)
    plot_scree_by_cluster(rv$clusterer)
  })

  output$variable_contributions <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted, input$contrib_cluster)

    # Extract cluster number from selection
    cluster_id <- as.numeric(gsub("Cluster ", "", input$contrib_cluster))

    plot_variable_contributions(rv$clusterer,
      cluster_id = cluster_id,
      top_n = input$contrib_top_n
    )
  })

  output$network_graph <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    tryCatch(
      {
        plot_network_graph(rv$clusterer,
          threshold = input$network_threshold,
          layout = input$network_layout
        )
      },
      error = function(e) {
        plot(1, 1,
          type = "n",
          xlab = "", ylab = "",
          main = "Network graph requires 'igraph' package"
        )
      }
    )
  })

  output$split_dendrogram <- shiny::renderPlot({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    if (inherits(rv$clusterer, "DivisiveClusterer")) {
      rv$clusterer$plot_split_dendrogram()
    }
  })

  # ===========================================================================
  # K SELECTION TAB
  # ===========================================================================

  shiny::observeEvent(input$run_k_selection, {
    shiny::req(rv$data, input$active_vars)

    if (length(input$active_vars) < 3) {
      shiny::showNotification("Need at least 3 variables for K selection.",
        type = "error"
      )
      return()
    }

    shiny::withProgress(message = "Evaluating K values...", value = 0, {
      tryCatch(
        {
          active_data <- rv$data[, input$active_vars, drop = FALSE]
          k_range <- seq(input$k_range[1], input$k_range[2])

          # Select clusterer class
          clusterer_class <- if (input$algorithm_choice == "kmeans") {
            KMeansClusterer
          } else {
            DivisiveClusterer
          }

          shiny::incProgress(0.9, detail = "Running comparison...")

          # Run comparison (suppress plots, we'll make our own)
          rv$k_results <- compare_k_selection_methods(
            clusterer_class,
            active_data,
            k_range = k_range,
            plot = FALSE,
            standardize = input$standardize
          )

          shiny::showNotification("K selection completed!",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(paste("K selection failed:", e$message),
            type = "error"
          )
        }
      )
    })
  })

  output$k_recommendation <- shiny::renderPrint({
    shiny::req(rv$k_results)

    cat("=== K Selection Results ===\n\n")
    cat("Elbow method suggests: K =", rv$k_results$elbow$suggested_k, "\n")
    cat("Silhouette method suggests: K =", rv$k_results$silhouette$suggested_k, "\n")
    cat("Calinski-Harabasz suggests: K =", rv$k_results$homogeneity$suggested_k, "\n")
    cat("\n")
    cat("Consensus K:", rv$k_results$consensus_k, "\n")
  })

  output$k_selection_plots <- shiny::renderPlot({
    shiny::req(rv$k_results)

    par(mfrow = c(1, 3))

    # Elbow
    plot(rv$k_results$elbow$k_values, rv$k_results$elbow$inertias,
      type = "b", pch = 19, col = "blue",
      xlab = "K", ylab = "Inertia",
      main = "Elbow Method"
    )
    points(
      rv$k_results$elbow$suggested_k,
      rv$k_results$elbow$inertias[which(rv$k_results$elbow$k_values == rv$k_results$elbow$suggested_k)],
      col = "red", pch = 19, cex = 2
    )
    grid()

    # Silhouette
    plot(rv$k_results$silhouette$k_values, rv$k_results$silhouette$silhouette_scores,
      type = "b", pch = 19, col = "darkgreen",
      xlab = "K", ylab = "Silhouette",
      main = "Silhouette Method"
    )
    points(
      rv$k_results$silhouette$suggested_k,
      rv$k_results$silhouette$silhouette_scores[which(rv$k_results$silhouette$k_values == rv$k_results$silhouette$suggested_k)],
      col = "red", pch = 19, cex = 2
    )
    abline(h = 0, lty = 2)
    grid()

    # Calinski-Harabasz
    plot(rv$k_results$homogeneity$k_values, rv$k_results$homogeneity$ch_scores,
      type = "b", pch = 19, col = "purple",
      xlab = "K", ylab = "Pseudo-F",
      main = "Calinski-Harabasz"
    )
    points(
      rv$k_results$homogeneity$suggested_k,
      rv$k_results$homogeneity$ch_scores[which(rv$k_results$homogeneity$k_values == rv$k_results$homogeneity$suggested_k)],
      col = "red", pch = 19, cex = 2
    )
    grid()

    par(mfrow = c(1, 1))
  })

  output$elbow_plot <- shiny::renderPlot({
    shiny::req(rv$k_results)
    plot(rv$k_results$elbow$k_values, rv$k_results$elbow$inertias,
      type = "b", pch = 19, col = "blue",
      xlab = "K", ylab = "Inertia", main = ""
    )
    grid()
  })

  output$silhouette_plot <- shiny::renderPlot({
    shiny::req(rv$k_results)
    plot(rv$k_results$silhouette$k_values, rv$k_results$silhouette$silhouette_scores,
      type = "b", pch = 19, col = "darkgreen",
      xlab = "K", ylab = "Silhouette", main = ""
    )
    abline(h = 0, lty = 2)
    grid()
  })

  output$calinski_plot <- shiny::renderPlot({
    shiny::req(rv$k_results)
    plot(rv$k_results$homogeneity$k_values, rv$k_results$homogeneity$ch_scores,
      type = "b", pch = 19, col = "purple",
      xlab = "K", ylab = "Pseudo-F", main = ""
    )
    grid()
  })

  # ===========================================================================
  # EXPORT TAB
  # ===========================================================================

  output$export_preview <- DT::renderDataTable({
    shiny::req(rv$clusterer, rv$clusterer$fitted)

    # Combine active and illustrative results
    results <- rv$clusterer$get_results()
    results$type <- "active"

    if (!is.null(rv$illustrative_results) && nrow(rv$illustrative_results) > 0) {
      illus_df <- rv$illustrative_results
      illus_df$type <- "illustrative"
      results <- rbind(results, illus_df)
    }

    DT::datatable(results,
      options = list(
        pageLength = 10,
        dom = "tp"
      )
    )
  })

  output$download_assignments <- shiny::downloadHandler(
    filename = function() {
      paste0("cluster_assignments_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(rv$clusterer, rv$clusterer$fitted)

      # Combine active and illustrative results
      results <- rv$clusterer$get_results()
      results$type <- "active"

      if (!is.null(rv$illustrative_results) && nrow(rv$illustrative_results) > 0) {
        illus_df <- rv$illustrative_results
        illus_df$type <- "illustrative"
        results <- rbind(results, illus_df)
      }

      utils::write.csv(results, file, row.names = FALSE)
    }
  )

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      paste0("clustering_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      shiny::req(rv$clusterer, rv$clusterer$fitted)

      # Create a simple HTML report
      html_content <- paste0(
        "<!DOCTYPE html>
        <html>
        <head>
          <title>M2RClust Clustering Report</title>
          <style>
            body { font-family: Arial, sans-serif; margin: 40px; }
            h1 { color: #3c8dbc; }
            h2 { color: #333; border-bottom: 2px solid #3c8dbc; padding-bottom: 5px; }
            table { border-collapse: collapse; width: 100%; margin: 20px 0; }
            th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
            th { background-color: #3c8dbc; color: white; }
            tr:nth-child(even) { background-color: #f2f2f2; }
            .metric { font-size: 24px; font-weight: bold; color: #3c8dbc; }
          </style>
        </head>
        <body>
          <h1>M2RClust Clustering Report</h1>
          <p>Generated on: ", Sys.time(), "</p>

          <h2>Configuration</h2>
          <ul>
            <li><strong>Algorithm:</strong> ", if (input$algorithm_choice == "kmeans") "KMeansClusterer" else "DivisiveClusterer", "</li>
            <li><strong>Number of clusters:</strong> ", rv$clusterer$n_clusters, "</li>
            <li><strong>Number of variables:</strong> ", ncol(rv$clusterer$data), "</li>
            <li><strong>Number of observations:</strong> ", nrow(rv$clusterer$data), "</li>
            <li><strong>Standardized:</strong> ", if (input$standardize) "Yes" else "No", "</li>
          </ul>

          <h2>Global Metrics</h2>
          <p class='metric'>Global Homogeneity: ", round(rv$clusterer$get_homogeneity(), 4), "</p>

          <h2>Cluster Assignments</h2>
          <table>
            <tr><th>Variable</th><th>Cluster</th></tr>",
        paste(
          sapply(1:nrow(rv$clusterer$get_results()), function(i) {
            row <- rv$clusterer$get_results()[i, ]
            paste0("<tr><td>", row$variable, "</td><td>", row$cluster, "</td></tr>")
          }),
          collapse = ""
        ),
        "
          </table>

          <h2>Homogeneity by Cluster</h2>
          <table>
            <tr><th>Cluster</th><th>Homogeneity</th><th>N Variables</th></tr>",
        paste(
          sapply(1:rv$clusterer$n_clusters, function(k) {
            homog <- rv$clusterer$get_cluster_homogeneity()[k]
            n_vars <- sum(rv$clusterer$clusters == k)
            paste0("<tr><td>Cluster ", k, "</td><td>", round(homog, 4), "</td><td>", n_vars, "</td></tr>")
          }),
          collapse = ""
        ),
        "
          </table>

          <hr>
          <p><em>Report generated by M2RClust package</em></p>
        </body>
        </html>"
      )

      writeLines(html_content, file)
    }
  )
}