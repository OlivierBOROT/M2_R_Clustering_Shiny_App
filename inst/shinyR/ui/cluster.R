# Cluster page UI -----------------------------------------------------------
# Container holds the whole cluster page layout with tabset
cluster_page <- shiny::div(
  class = "container-fluid",
  style = "margin-top:12px;",

  # Tabset for clustering workflow
  shiny::tabsetPanel(
    id = "cluster_tabs",
    type = "tabs",

    # Tab 1: Configuration (always present) ---------------------------------
    shiny::tabPanel(
      title = "Configuration",
      value = "config",

      # Row: Clustering Configuration card -------------------------------------
      shiny::fluidRow(
        shiny::column(
          width = 12,
          # first card
          shiny::div(class = "card shadow-sm", style = "margin-top:12px; margin-bottom:12px;",
            shiny::div(class = "card-header", shiny::uiOutput("txt_CLUSTER_CONFIG_TITLE")),
            # Card body: algorithm selection, number of clusters
            shiny::div(class = "card-body",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                    shiny::uiOutput("txt_CLUSTER_ALGORITHM_LABEL", inline = TRUE)
                  ),
                  shiny::selectInput(
                    "CLUSTER_algorithm",
                    label = NULL,
                    choices = c("K-Means", "MCA/Hclust", "PDDP"),
                    selected = "K-Means"
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                    shiny::uiOutput("txt_CLUSTER_N_CLUSTERS_LABEL", inline = TRUE)
                  ),
                  shiny::numericInput(
                    "CLUSTER_n_clusters",
                    label = NULL,
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                  )
                )
              )
            )
          )
        )
      ),

      # Row: K-Means Parameters card (conditional) --------------------------
      shiny::conditionalPanel(
        condition = "input.CLUSTER_algorithm == 'K-Means'",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(class = "card", style = "margin-top:12px; margin-bottom:12px;",
              shiny::div(class = "card-header", shiny::uiOutput("txt_CLUSTER_ALGO_SPECIFIC_TITLE_KMEANS")),
              shiny::div(class = "card-body",
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_STANDARDIZE_LABEL", inline = TRUE)
                    ),
                    shiny::checkboxInput(
                      "KMEANS_standardize",
                      label = NULL,
                      value = TRUE
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_MAX_ITER_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "KMEANS_max_iter",
                      label = NULL,
                      value = 100,
                      min = 1,
                      step = 1
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_TOL_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "KMEANS_tolerance",
                      label = NULL,
                      value = 1e-4,
                      min = 1e-9,
                      step = 1e-4
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_SEED_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "KMEANS_seed",
                      label = NULL,
                      value = NA,
                      step = 1
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 4,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_N_INIT_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "KMEANS_n_init",
                      label = NULL,
                      value = 10,
                      min = 1,
                      step = 1
                    )
                  ),
                  shiny::column(
                    width = 8,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_KMEANS_INIT_METHOD_LABEL", inline = TRUE)
                    ),
                    shiny::selectInput(
                      "KMEANS_init_method",
                      label = NULL,
                      choices = c(
                        "Homogeneity++" = "homogeneity++",
                        "Correlation" = "correlation",
                        "Random" = "random"
                      ),
                      selected = "homogeneity++"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # Row: MCA/Hclust Parameters card (conditional) --------------------------
      shiny::conditionalPanel(
        condition = "input.CLUSTER_algorithm == 'MCA/Hclust'",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(class = "card", style = "margin-top:12px; margin-bottom:12px;",
              shiny::div(class = "card-header", shiny::uiOutput("txt_CLUSTER_MCA_PARAMS_TITLE")),
              shiny::div(class = "card-body",
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                      shiny::uiOutput("txt_HIERARCHICAL_LINKAGE_LABEL", inline = TRUE)
                    ),
                    shiny::selectInput(
                      "CLUSTER_linkage",
                      label = NULL,
                      choices = c("ward.D2", "single", "complete", "average", "ward.D"),
                      selected = "ward.D2"
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                      shiny::uiOutput("txt_CLUSTER_DISSIMILARITY_LABEL", inline = TRUE)
                    ),
                    shiny::selectInput(
                      "CLUSTER_dissimilarity",
                      label = NULL,
                      choices = c("dice", "cramer"),
                      selected = "dice"
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                      shiny::uiOutput("txt_CLUSTER_AUTO_DISCRETIZE_LABEL", inline = TRUE)
                    ),
                    shiny::checkboxInput(
                      "CLUSTER_auto_discretize",
                      label = NULL,
                      value = TRUE
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::conditionalPanel(
                      condition = "input.CLUSTER_auto_discretize == true",
                      shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                        shiny::uiOutput("txt_CLUSTER_N_BINS_LABEL", inline = TRUE)
                      ),
                      shiny::numericInput(
                        "CLUSTER_n_bins",
                        label = NULL,
                        value = 4,
                        min = 2,
                        max = 10,
                        step = 1
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Row: PDDP Parameters card (conditional) --------------------------
      shiny::conditionalPanel(
        condition = "input.CLUSTER_algorithm == 'PDDP'",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(class = "card", style = "margin-top:12px; margin-bottom:12px;",
              shiny::div(class = "card-header", shiny::uiOutput("txt_CLUSTER_PDDP_PARAMS_TITLE")),
              shiny::div(class = "card-body",
                shiny::fluidRow(
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_PDDP_STANDARDIZE_LABEL", inline = TRUE)
                    ),
                    shiny::checkboxInput(
                      "PDDP_standardize",
                      label = NULL,
                      value = TRUE
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_PDDP_MIN_CLUSTER_SIZE_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "PDDP_min_cluster_size",
                      label = NULL,
                      value = 3,
                      min = 2,
                      step = 1
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_PDDP_ROTATION_METHOD_LABEL", inline = TRUE)
                    ),
                    shiny::selectInput(
                      "PDDP_rotation_method",
                      label = NULL,
                      choices = c("Varimax" = "varimax", "Promax" = "promax", "None" = "none"),
                      selected = "varimax"
                    )
                  ),
                  shiny::column(
                    width = 3,
                    shiny::conditionalPanel(
                      condition = "input.PDDP_rotation_method == 'promax'",
                      shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                        shiny::uiOutput("txt_PDDP_PROMAX_M_LABEL", inline = TRUE)
                      ),
                      shiny::numericInput(
                        "PDDP_promax_m",
                        label = NULL,
                        value = 4,
                        min = 1,
                        step = 1
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 4,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                      shiny::uiOutput("txt_PDDP_STOP_KAISER_LABEL", inline = TRUE)
                    ),
                    shiny::checkboxInput(
                      "PDDP_stop_kaiser",
                      label = NULL,
                      value = FALSE
                    )
                  ),
                  shiny::column(
                    width = 4,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;",
                      shiny::uiOutput("txt_PDDP_SPLIT_CRITERION_LABEL", inline = TRUE)
                    ),
                    shiny::selectInput(
                      "PDDP_split_criterion",
                      label = NULL,
                      choices = c("Eigenvalue #2" = "eigenvalue2", "Homogeneity" = "homogeneity"),
                      selected = "eigenvalue2"
                    )
                  ),
                  shiny::column(
                    width = 4,
                    shiny::div(style = "margin-bottom: 5px; font-weight: 500;", 
                      shiny::uiOutput("txt_PDDP_MIN_EIGEN_RATIO_LABEL", inline = TRUE)
                    ),
                    shiny::numericInput(
                      "PDDP_min_eigen_ratio",
                      label = NULL,
                      value = 0.1,
                      min = 0.01,
                      max = 1.0,
                      step = 0.05
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # -------------------------------------------------------------------
      # Row: Illustrative Data card -------------------------------------------
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(class = "card", style = "margin-bottom:12px;",
            shiny::div(class = "card-header", shiny::uiOutput("txt_CLUSTER_ILLUSTRATIVE_TITLE")),

            # Card body: illustrative data management
            shiny::div(class = "card-body",
              shiny::helpText(shiny::uiOutput("txt_CLUSTER_ILLUSTRATIVE_HELP")),

              # File input and project button on same compact row (left side)
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  # file input UI (renders fileInput or info). kept compact like algorithm inputs
                  shiny::fileInput(
                    "cluster_illustrative_input",
                    label = shiny::uiOutput("txt_CLUSTER_ILLUSTRATIVE_INPUT_LABEL"),
                    multiple = FALSE,
                    accept = c(".csv", ".xls", ".xlsx"),
                  )
                ),
                shiny::column(
                  width = 3,
                  # project button aligned with inputs, compact full-width of its column
                  shiny::div(style = "margin-top: 26px;",
                    shiny::actionButton(
                      "BUTTON_project_illustrative",
                      shiny::uiOutput("txt_CLUSTER_BTN_PROJECT_ILLUSTRATIVE"),
                      class = "btn btn-info w-100"
                    )
                  )
                ),
                # filler column to keep the left alignment similar to algorithm row
                shiny::column(width = 6)
              )
            )
          )
        )
      ),

      # Row: Run Clustering Button --------------------------------------------
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(class = "d-flex justify-content-center gap-3", style = "margin-bottom:12px;",
            shiny::actionButton(
              "BUTTON_fit_cluster",
              shiny::uiOutput("txt_CLUSTER_BTN_FIT"),
              class = "btn btn-primary btn-lg"
            ),
            # Button to open results in new tab (only shown after clustering)
            shiny::uiOutput("cluster_open_results_button")
          )
        )
      )
    )
    
    # Additional result tabs will be dynamically added via server
  )
)
