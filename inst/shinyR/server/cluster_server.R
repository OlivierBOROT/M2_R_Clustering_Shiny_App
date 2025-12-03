# Reactive stash for cluster-specific state that other observers may read
cluster_state <- reactiveValues(
	last_algorithm = NULL,
	last_method_params = NULL,
	last_trigger = NULL,
	mca_results = NULL,
	kmeans_results = NULL,
	pddp_results = NULL
)

results_tabs <- reactiveValues()

`%||%` <- function(x, y) if (is.null(x)) y else x

drop_nulls <- function(x) {
	x[!vapply(x, is.null, logical(1))]
}

build_cluster_stats <- function(clusters_df) {
	if (is.null(clusters_df) || nrow(clusters_df) == 0) return(NULL)
	clusters_df$Frequency <- as.numeric(clusters_df$Frequency)
	stats_list <- lapply(split(clusters_df, clusters_df$Cluster), function(df) {
		data.frame(
			Cluster = unique(df$Cluster),
			Modalities = nrow(df),
			TotalFrequency = sum(df$Frequency),
			AvgFrequency = round(mean(df$Frequency), 2),
			TopModalities = paste(head(df$Modality, 5), collapse = ", ")
		)
	})
	if (length(stats_list) == 0) return(NULL)
	do.call(rbind, stats_list)
}

get_active_dataset <- function() {
	shiny::isolate({
		dataset <- data_store$dataset
		if (is.null(dataset) || nrow(dataset) == 0) return(NULL)

		active_vars <- data_store$clustering_vars$active
		if (is.null(active_vars) || length(active_vars) == 0) {
			vars <- names(dataset)
		} else {
			vars <- intersect(active_vars, names(dataset))
			if (length(vars) == 0) vars <- names(dataset)
		}

		subset_df <- dataset[, vars, drop = FALSE]
		if (ncol(subset_df) == 0) return(NULL)

		subset_df[] <- lapply(subset_df, function(col) {
			if (is.logical(col) || is.character(col)) {
				as.factor(col)
			} else {
				col
			}
		})

		subset_df
	})
}

current_dataset_name <- function() {
	shiny::isolate({
		if (!exists("current_file_input_id")) return("dataset")
		fid <- current_file_input_id()
		if (is.null(fid)) return("dataset")
		file_input <- input[[fid]]
		if (is.null(file_input) || is.null(file_input$name) || !nzchar(file_input$name)) {
			"dataset"
		} else {
			file_input$name
		}
	})
}

get_illustrative_variables <- function() {
	dataset <- data_store$dataset
	vars <- data_store$clustering_vars$illustrative
	if (is.null(dataset) || is.null(vars) || length(vars) == 0) return(character(0))
	vars <- intersect(vars, names(dataset))
	vars[vars != ""]
}

get_illustrative_column <- function(var_name) {
	dataset <- data_store$dataset
	if (is.null(dataset) || !nzchar(var_name) || !var_name %in% names(dataset)) {
		return(NULL)
	}
	vec <- dataset[[var_name]]
	if (is.null(vec)) return(NULL)
	if (!is.factor(vec)) {
		vec <- as.factor(vec)
	}
	vec
}

run_mca_hclust <- function(params) {
	df <- get_active_dataset()
	if (is.null(df)) {
		shiny::showNotification("No active dataset available for clustering.", type = "error")
		cluster_state$mca_results <- NULL
		return(FALSE)
	}

	message <- NULL
	success <- FALSE

	tryCatch({
		model <- ModalitiesDiceClusterer$new(
			n_groups = params$n_clusters,
			linkage = params$linkage,
			dissimilarity = params$dissimilarity,
			auto_discretize = isTRUE(params$auto_discretize),
			n_bins = params$n_bins %||% 4
		)

		model$fit(df)
		summary_capture <- NULL
		summary_list <- NULL
		summary_capture <- utils::capture.output({
			summary_list <- model$summary()
		})

		cluster_state$mca_results <- list(
			model = model,
			summary = summary_list,
			summary_text = paste(summary_capture, collapse = "\n"),
			params = params,
			data = df,
			dataset_name = current_dataset_name(),
			generated_at = Sys.time()
		)

		success <- TRUE
	}, error = function(err) {
		message <<- conditionMessage(err)
	})

	if (!success) {
		err_text <- if (is.null(message)) "Unknown error" else message
		shiny::showNotification(paste("MCA/Hclust failed:", err_text), type = "error")
		return(FALSE)
	}

	shiny::showNotification("MCA/Hclust clustering completed.", type = "message")
	TRUE
}

run_kmeans <- function(params) {
	df <- get_active_dataset()
	if (is.null(df)) {
		shiny::showNotification("No active dataset available for clustering.", type = "error")
		cluster_state$kmeans_results <- NULL
		return(FALSE)
	}

	message <- NULL
	success <- FALSE

	tryCatch({
		seed_value <- params$seed
		if (is.null(seed_value) || is.na(seed_value) || !is.finite(seed_value)) {
			seed_value <- NULL
		}

		model <- KMeansClusterer$new(
			data = df,
			n_clusters = params$n_clusters %||% 3,
			standardize = isTRUE(params$standardize),
			max_iter = params$max_iter %||% 100,
			tol = params$tolerance %||% 1e-4,
			seed = seed_value,
			n_init = params$n_init %||% 10,
			init_method = params$init_method %||% "homogeneity++"
		)

		model$fit()

		summary_capture <- utils::capture.output(model$summary())
		assignments <- model$get_results()
		cluster_sizes <- as.integer(tabulate(model$clusters, nbins = model$n_clusters))
		homogeneity_vals <- model$get_cluster_homogeneity()
		homogeneity_df <- data.frame(
			Cluster = seq_len(model$n_clusters),
			Size = cluster_sizes,
			Homogeneity = round(homogeneity_vals, 3)
		)

		cluster_state$kmeans_results <- list(
			model = model,
			summary_text = paste(summary_capture, collapse = "\n"),
			assignments = assignments,
			homogeneity_table = homogeneity_df,
			global_homogeneity = model$get_homogeneity(),
			metrics = list(
				iterations = model$get_iterations(),
				actual_runs = model$get_actual_runs(),
				n_init = model$get_n_init()
			),
			params = params,
			data_info = list(
				n_obs = nrow(df),
				n_vars = ncol(df)
			),
			dataset_name = current_dataset_name(),
			generated_at = Sys.time()
		)

		success <- TRUE
	}, error = function(err) {
		message <<- conditionMessage(err)
	})

	if (!success) {
		err_text <- if (is.null(message)) "Unknown error" else message
		cluster_state$kmeans_results <- NULL
		shiny::showNotification(paste("K-Means failed:", err_text), type = "error")
		return(FALSE)
	}

	shiny::showNotification("K-Means clustering completed.", type = "message")
	TRUE
}

run_pddp <- function(params) {
	df <- get_active_dataset()
	if (is.null(df)) {
		shiny::showNotification("No active dataset available for clustering.", type = "error")
		cluster_state$pddp_results <- NULL
		return(FALSE)
	}

	message <- NULL
	success <- FALSE

	tryCatch({
		model <- DivisiveClusterer$new(
			data = df,
			n_clusters = params$n_clusters %||% 3,
			standardize = isTRUE(params$standardize),
			min_cluster_size = params$min_cluster_size %||% 3,
			rotation_method = params$rotation_method %||% "varimax",
			split_criterion = params$split_criterion %||% "eigenvalue2",
			stop_at_kaiser = isTRUE(params$stop_at_kaiser),
			min_eigenvalue_ratio = params$min_eigenvalue_ratio %||% 0.1,
			promax_m = params$promax_m %||% 4
		)

		model$fit()

		summary_capture <- utils::capture.output(model$summary())
		assignments <- tryCatch(model$get_cluster_assignments(), error = function(err) NULL)
		split_summary <- tryCatch(model$get_split_quality_summary(), error = function(err) NULL)
		split_details <- tryCatch(model$get_split_details(), error = function(err) NULL)
		cluster_homogeneity <- tryCatch(model$get_cluster_homogeneity(), error = function(err) numeric(0))
		actual_clusters_val <- if (length(model$clusters) > 0) max(model$clusters, na.rm = TRUE) else 0
		n_rows <- max(length(cluster_homogeneity), actual_clusters_val)
		if (n_rows == 0) n_rows <- model$n_clusters %||% 1
		n_rows <- as.integer(n_rows)
		cluster_sizes <- tabulate(model$clusters, nbins = n_rows)
		if (length(cluster_sizes) < n_rows) {
			cluster_sizes <- c(cluster_sizes, rep(0L, n_rows - length(cluster_sizes)))
		}
		homog_vals <- if (length(cluster_homogeneity) > 0) round(cluster_homogeneity, 3) else rep(NA_real_, 0)
		if (length(homog_vals) < n_rows) {
			homog_vals <- c(homog_vals, rep(NA_real_, n_rows - length(homog_vals)))
		}
		homogeneity_df <- data.frame(
			Cluster = seq_len(n_rows),
			Size = cluster_sizes[seq_len(n_rows)],
			Homogeneity = homog_vals[seq_len(n_rows)]
		)

		cluster_state$pddp_results <- list(
			model = model,
			summary_text = paste(summary_capture, collapse = "\n"),
			assignments = assignments,
			homogeneity_table = homogeneity_df,
			split_summary = split_summary,
			split_details = split_details,
			global_homogeneity = tryCatch(model$get_homogeneity(), error = function(err) NA_real_),
			unexplained_variance = tryCatch(model$get_unexplained_variance(), error = function(err) NA_real_),
			actual_clusters = if (actual_clusters_val > 0) actual_clusters_val else model$n_clusters,
			params = params,
			data_info = list(
				n_obs = nrow(df),
				n_vars = ncol(df)
			),
			dataset_name = current_dataset_name(),
			generated_at = Sys.time()
		)

		success <- TRUE
	}, error = function(err) {
		message <<- conditionMessage(err)
	})

	if (!success) {
		err_text <- if (is.null(message)) "Unknown error" else message
		cluster_state$pddp_results <- NULL
		shiny::showNotification(paste("PDDP failed:", err_text), type = "error")
		return(FALSE)
	}

	shiny::showNotification("PDDP clustering completed.", type = "message")
	TRUE
}

add_cluster_results_tab <- function(algo_key) {
	if (!nzchar(algo_key)) return(invisible(NULL))

	if (isTRUE(results_tabs[[algo_key]])) {
		return(invisible(NULL))
	}

	tab_id <- paste0("cluster_results_", algo_key)
	tab_title <- switch(
		algo_key,
		"K-Means" = "K-Means Results",
		"MCA/Hclust" = "MCA/Hclust Results",
		"PDDP" = "PDDP Results",
		paste(algo_key, "Results")
	)

	tab_body <- switch(
		algo_key,
		"K-Means" = shiny::uiOutput("kmeans_results_panel"),
		"MCA/Hclust" = shiny::uiOutput("mca_hclust_results_panel"),
		"PDDP" = shiny::uiOutput("pddp_results_panel"),
		shiny::div(class = "p-4 text-muted", sprintf("%s placeholder", tab_title))
	)

	shiny::insertTab(
		inputId = "cluster_tabs",
		tab = shiny::tabPanel(
			title = tab_title,
			value = tab_id,
			tab_body
		),
		target = "config",
		position = "after",
		session = session,
		select = FALSE
	)

	results_tabs[[algo_key]] <- TRUE
	invisible(tab_id)
}

get_method_specific_params <- function() {
	algo <- input$CLUSTER_algorithm %||% "K-Means"

	params <- switch(
		algo,
		"MCA/Hclust" = {
			auto_disc <- isTRUE(input$CLUSTER_auto_discretize)
			out <- list(
				n_clusters = input$CLUSTER_n_clusters %||% 3,
				linkage = input$CLUSTER_linkage %||% "ward.D2",
				dissimilarity = input$CLUSTER_dissimilarity %||% "dice",
				auto_discretize = auto_disc
			)
			if (auto_disc) out$n_bins <- input$CLUSTER_n_bins %||% 4
			out
		},
		"PDDP" = list(
			n_clusters = input$CLUSTER_n_clusters %||% 3,
			standardize = isTRUE(input$PDDP_standardize),
			min_cluster_size = input$PDDP_min_cluster_size %||% 2,
			rotation_method = input$PDDP_rotation_method %||% "varimax",
			promax_m = input$PDDP_promax_m %||% 4,
			stop_at_kaiser = isTRUE(input$PDDP_stop_kaiser),
			split_criterion = input$PDDP_split_criterion %||% "eigenvalue2",
			min_eigenvalue_ratio = input$PDDP_min_eigen_ratio %||% 0.05
		),
		"K-Means" = list(
			n_clusters = input$CLUSTER_n_clusters %||% 3,
			standardize = isTRUE(input$KMEANS_standardize),
			init_method = input$KMEANS_init_method,
			max_iter = input$KMEANS_max_iter %||% 100,
			tolerance = input$KMEANS_tolerance %||% 1e-4,
			n_init = input$KMEANS_n_init %||% 10,
			seed = input$KMEANS_seed
		),
		list()
	)

	drop_nulls(params)
}

observeEvent(input$BUTTON_fit_cluster, {
	method_params <- get_method_specific_params()

	cluster_state$last_algorithm <- input$CLUSTER_algorithm %||% "K-Means"
	cluster_state$last_method_params <- method_params
	cluster_state$last_trigger <- Sys.time()

	add_cluster_results_tab(cluster_state$last_algorithm)

	if (identical(cluster_state$last_algorithm, "MCA/Hclust")) {
		run_mca_hclust(method_params)
	} else if (identical(cluster_state$last_algorithm, "K-Means")) {
		run_kmeans(method_params)
	} else if (identical(cluster_state$last_algorithm, "PDDP")) {
		run_pddp(method_params)
	}

  sprintf("Clustering triggered: %s with params %s",
          cluster_state$last_algorithm,
          paste(names(method_params), method_params, sep = "=", collapse = ", ")
  ) %>% cat("\n")
})

output$mca_hclust_results_panel <- shiny::renderUI({
	res <- cluster_state$mca_results
	if (is.null(res) || is.null(res$summary)) {
		return(shiny::div(class = "p-4 text-muted", "Run MCA/Hclust to view clustering results."))
	}

	summary <- res$summary
	data_info <- summary$data_info
	inertia <- summary$inertia
	dist_stats <- summary$distance_stats
	generated_label <- if (!is.null(res$generated_at)) {
		format(res$generated_at, "%Y-%m-%d %H:%M:%S")
	} else {
		""
	}

	metric_card <- function(label, value) {
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(
				class = "card-body text-center",
				shiny::div(class = "text-muted text-uppercase small", label),
				shiny::h4(format(value, big.mark = " ", scientific = FALSE))
			)
		)
	}

	illustrative_vars <- get_illustrative_variables()
	selected_illustrative <- input$MCA_ILLUSTRATIVE_VAR
	if (length(illustrative_vars) > 0) {
		if (is.null(selected_illustrative) || !(selected_illustrative %in% illustrative_vars)) {
			selected_illustrative <- illustrative_vars[1]
		}
	}

	illustrative_section <- NULL
	if (length(illustrative_vars) > 0) {
		illustrative_section <- shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Illustrative MCA Map"),
			shiny::div(
				class = "card-body",
				shiny::selectInput(
					"MCA_ILLUSTRATIVE_VAR",
					"Illustrative variable",
					choices = illustrative_vars,
					selected = selected_illustrative
				),
				shiny::plotOutput("mca_hclust_illustrative_plot", height = "360px")
			)
		)
	}

	shiny::tagList(
		shiny::div(
			class = "d-flex justify-content-between align-items-start mb-3",
			shiny::div(
				shiny::h3("MCA/Hclust Results"),
				shiny::div(
					class = "text-muted",
					shiny::span(sprintf("Dataset: %s", res$dataset_name %||% "dataset")),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Active variables: %d", data_info$n_vars %||% 0)),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Generated: %s", generated_label))
				)
			),
			shiny::downloadButton("download_mca_hclust_html", "Download HTML", class = "btn btn-outline-secondary")
		),
		shiny::fluidRow(
			shiny::column(4, metric_card("Observations", data_info$n_obs %||% 0)),
			shiny::column(4, metric_card("Variables", data_info$n_vars %||% 0)),
			shiny::column(4, metric_card("Modalities", data_info$n_modalities %||% 0))
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Summary"),
			shiny::div(class = "card-body", shiny::verbatimTextOutput("mca_hclust_summary_text"))
		),
		shiny::fluidRow(
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Composition"),
					shiny::div(class = "card-body", shiny::tableOutput("mca_hclust_cluster_table"))
				)
			),
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Top Modality Contributions"),
					shiny::div(class = "card-body", shiny::tableOutput("mca_hclust_contrib_table"))
				)
			)
		),
		shiny::fluidRow(
			shiny::column(
				width = 12,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Statistics"),
					shiny::div(class = "card-body", shiny::tableOutput("mca_hclust_cluster_stats_table"))
				)
			)
		),
		shiny::fluidRow(
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Dendrogram"),
					shiny::div(class = "card-body", shiny::plotOutput("mca_hclust_dendrogram", height = "340px"))
				)
			),
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "MCA Map"),
					shiny::div(class = "card-body", shiny::plotOutput("mca_hclust_mca_plot", height = "340px"))
				)
			),
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Map"),
					shiny::div(class = "card-body", shiny::plotOutput("mca_hclust_cluster_plot", height = "340px"))
				)
			)
		),
		shiny::fluidRow(
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Silhouette Analysis"),
					shiny::div(class = "card-body", shiny::plotOutput("mca_hclust_silhouette_plot", height = "320px"))
				)
			),
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Elbow (Within Inertia)"),
					shiny::div(class = "card-body", shiny::plotOutput("mca_hclust_elbow_plot", height = "320px"))
				)
			)
		),
		if (!is.null(illustrative_section)) illustrative_section,
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Inertia & Distance Summary"),
			shiny::div(class = "card-body", shiny::uiOutput("mca_hclust_inertia_stats"))
		)
	)
})

output$mca_hclust_cluster_table <- shiny::renderTable({
	res <- cluster_state$mca_results
	req(res, res$summary)
	clusters <- res$summary$clusters
	if (is.null(clusters) || nrow(clusters) == 0) return(NULL)
	clusters$Frequency <- as.integer(clusters$Frequency)
	clusters
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE)

output$mca_hclust_contrib_table <- shiny::renderTable({
	res <- cluster_state$mca_results
	req(res, res$summary)
	contrib <- res$summary$modality_contribution
	if (is.null(contrib) || nrow(contrib) == 0) return(NULL)
	contrib$Contribution <- signif(contrib$Contribution, 4)
	contrib$RelativeContrib <- round(contrib$RelativeContrib, 2)
	head(contrib[, c("Modality", "Cluster", "Contribution", "RelativeContrib"), drop = FALSE], 10)
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE)

output$mca_hclust_dendrogram <- shiny::renderPlot({
	res <- cluster_state$mca_results
	req(res, res$model)
	model <- res$model
	graphics::plot(model$hclust, main = sprintf("Modalities clustering (k = %d)", model$n_groups), xlab = "", sub = "")
	stats::rect.hclust(model$hclust, k = model$n_groups, border = 2:(model$n_groups + 1))
})

output$mca_hclust_mca_plot <- shiny::renderPlot({
	res <- cluster_state$mca_results
	req(res, res$model)
	tryCatch({
		res$model$plot_mca(show_labels = TRUE)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render MCA map: %s", err$message))
	})
})

output$mca_hclust_cluster_plot <- shiny::renderPlot({
	res <- cluster_state$mca_results
	req(res, res$model)
	tryCatch({
		res$model$plot_clusters(add_ellipses = TRUE)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render cluster map: %s", err$message))
	})
})

output$mca_hclust_silhouette_plot <- shiny::renderPlot({
	res <- cluster_state$mca_results
	req(res, res$model)
	model <- res$model
	n_mod <- if (!is.null(model$disj)) ncol(model$disj) else 0L
	if (is.null(n_mod) || n_mod <= 2L) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, "Need at least 3 modalities for silhouette analysis.")
		return(invisible(NULL))
	}
	max_k <- max(2L, min(10L, as.integer(n_mod - 1L)))
	tryCatch({
		model$plot_silhouette(min_k = 2L, max_k = max_k)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render silhouette: %s", err$message))
	})
})

output$mca_hclust_elbow_plot <- shiny::renderPlot({
	res <- cluster_state$mca_results
	req(res, res$model)
	model <- res$model
	tryCatch({
		curve <- compute_mca_elbow_curve(model, min_k = 2L, max_k = 10L)
		graphics::plot(
			curve$k,
			curve$within,
			type = "b",
			pch = 19,
			col = "darkgreen",
			lwd = 2,
			xlab = "Number of Clusters (k)",
			ylab = "Total Within-Cluster Inertia",
			main = "Elbow Method (Inertia)"
		)
		graphics::grid()
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render elbow curve: %s", err$message))
	})
})

output$mca_hclust_illustrative_plot <- shiny::renderPlot({
	vars <- get_illustrative_variables()
	req(length(vars) > 0)
	selected <- input$MCA_ILLUSTRATIVE_VAR
	if (is.null(selected) || !(selected %in% vars)) {
		selected <- vars[1]
	}
	illus_vec <- get_illustrative_column(selected)
	req(!is.null(illus_vec))
	res <- cluster_state$mca_results
	req(res, res$model)
	tryCatch({
		res$model$plot_with_illustrative(illus = illus_vec, show_labels = TRUE)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render illustrative map: %s", err$message))
	})
})

output$mca_hclust_inertia_stats <- shiny::renderUI({
	res <- cluster_state$mca_results
	req(res, res$summary)
	inertia <- res$summary$inertia
	dist_stats <- res$summary$distance_stats
	if (is.null(inertia)) return(NULL)

	format_pct <- function(x) paste0(round(100 * x, 2), "%")
	between_share <- if (!is.null(inertia[["between"]]) && !is.null(inertia[["total"]]) && inertia[["total"]] > 0) inertia[["between"]] / inertia[["total"]] else NA
	within_share <- if (!is.null(inertia[["within"]]) && !is.null(inertia[["total"]]) && inertia[["total"]] > 0) inertia[["within"]] / inertia[["total"]] else NA

	shiny::tagList(
		shiny::tags$dl(
			class = "row mb-0",
			shiny::tags$dt(class = "col-sm-5", "Total inertia"),
			shiny::tags$dd(class = "col-sm-7", sprintf("%.3f", inertia[["total"]] %||% 0)),
			shiny::tags$dt(class = "col-sm-5", "Between-cluster"),
			shiny::tags$dd(class = "col-sm-7", sprintf("%.3f (%s)", inertia[["between"]] %||% 0, if (is.na(between_share)) "--" else format_pct(between_share))),
			shiny::tags$dt(class = "col-sm-5", "Within-cluster"),
			shiny::tags$dd(class = "col-sm-7", sprintf("%.3f (%s)", inertia[["within"]] %||% 0, if (is.na(within_share)) "--" else format_pct(within_share)))
		),
		shiny::tags$hr(),
		if (!is.null(dist_stats)) {
			shiny::tags$dl(
				class = "row mb-0",
				shiny::tags$dt(class = "col-sm-5", "Distance min"),
				shiny::tags$dd(class = "col-sm-7", sprintf("%.3f", dist_stats$min %||% 0)),
				shiny::tags$dt(class = "col-sm-5", "Distance max"),
				shiny::tags$dd(class = "col-sm-7", sprintf("%.3f", dist_stats$max %||% 0)),
				shiny::tags$dt(class = "col-sm-5", "Distance mean"),
				shiny::tags$dd(class = "col-sm-7", sprintf("%.3f", dist_stats$mean %||% 0)),
				shiny::tags$dt(class = "col-sm-5", "Distance median"),
				shiny::tags$dd(class = "col-sm-7", sprintf("%.3f", dist_stats$median %||% 0))
			)
		}
	)
})

output$mca_hclust_summary_text <- shiny::renderText({
	res <- cluster_state$mca_results
	req(res)
	res$summary_text %||% "Summary unavailable."
})

output$mca_hclust_cluster_stats_table <- shiny::renderTable({
	res <- cluster_state$mca_results
	req(res, res$summary)
	clusters <- res$summary$clusters
	if (is.null(clusters) || nrow(clusters) == 0) return(NULL)

	build_cluster_stats(clusters)
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE)

output$download_mca_hclust_html <- shiny::downloadHandler(
	filename = function() {
		paste0("mca_hclust_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
	},
	content = function(file) {
		res <- cluster_state$mca_results
		if (is.null(res) || is.null(res$model) || is.null(res$summary)) {
			stop("No MCA/Hclust results available for download.")
		}

		summary <- res$summary
		inertia <- summary$inertia
		dist_stats <- summary$distance_stats
		clusters <- summary$clusters
		contrib <- summary$modality_contribution
		cluster_stats <- build_cluster_stats(clusters)
		illustrative_vars <- get_illustrative_variables()

		html_escape <- function(text) {
			text <- as.character(text)
			text <- gsub("&", "&amp;", text, fixed = TRUE)
			text <- gsub("<", "&lt;", text, fixed = TRUE)
			text <- gsub(">", "&gt;", text, fixed = TRUE)
			text
		}

		df_to_html <- function(df, caption = NULL) {
			if (is.null(df) || nrow(df) == 0) return("")
			headers <- paste(sprintf("<th>%s</th>", html_escape(colnames(df))), collapse = "")
			rows <- apply(df, 1, function(row) {
				cells <- paste(sprintf("<td>%s</td>", html_escape(row)), collapse = "")
				paste0("<tr>", cells, "</tr>")
			})
			paste0(
				"<table>",
				if (!is.null(caption)) sprintf("<caption>%s</caption>", html_escape(caption)) else "",
				"<thead><tr>", headers, "</tr></thead>",
				"<tbody>", paste(rows, collapse = ""), "</tbody>",
				"</table>"
			)
		}

		sections <- c(
			"<!DOCTYPE html>",
			"<html><head><meta charset='utf-8'><title>MCA/Hclust Results</title>",
			"<style>body{font-family:Segoe UI,Arial,sans-serif;margin:32px;color:#222;}table{border-collapse:collapse;width:100%;margin-bottom:24px;}th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background:#f5f5f5;}caption{caption-side:top;font-weight:bold;margin-bottom:8px;}pre{background:#f7f7f9;padding:16px;border:1px solid #eee;overflow:auto;}</style>",
			"</head><body>",
			sprintf("<h1>MCA/Hclust Results</h1><p><strong>Dataset:</strong> %s<br><strong>Generated:</strong> %s<br><strong>Observations:</strong> %d<br><strong>Variables:</strong> %d<br><strong>Modalities:</strong> %d</p>",
				html_escape(res$dataset_name %||% "dataset"),
				html_escape(format(res$generated_at, "%Y-%m-%d %H:%M:%S")),
				summary$data_info$n_obs %||% 0,
				summary$data_info$n_vars %||% 0,
				summary$data_info$n_modalities %||% 0
			),
			sprintf("<p><strong>Configuration:</strong> %d clusters, linkage = %s, dissimilarity = %s</p>",
				summary$config$n_groups %||% res$params$n_clusters,
				html_escape(res$params$linkage %||% ""),
				html_escape(res$params$dissimilarity %||% "")
			),
			sprintf("<p><strong>Total inertia:</strong> %.3f<br><strong>Between-cluster:</strong> %.3f<br><strong>Within-cluster:</strong> %.3f</p>",
				inertia[["total"]] %||% 0,
				inertia[["between"]] %||% 0,
				inertia[["within"]] %||% 0
			),
			sprintf("<p><strong>Distance range:</strong> %.3f – %.3f<br><strong>Mean / Median:</strong> %.3f / %.3f</p>",
				dist_stats$min %||% 0,
				dist_stats$max %||% 0,
				dist_stats$mean %||% 0,
				dist_stats$median %||% 0
			),
			"<h2>Summary</h2>",
			sprintf("<pre>%s</pre>", html_escape(res$summary_text %||% "Summary unavailable.")),
			"<h2>Cluster Composition</h2>",
			df_to_html(clusters, "Modalities per cluster"),
			"<h2>Top Modality Contributions</h2>",
			df_to_html(contrib, "Contribution ranking"),
			"<h2>Cluster Statistics</h2>",
			df_to_html(cluster_stats, "Aggregated metrics")
		)

		if (length(illustrative_vars) > 0) {
			sections <- c(
				sections,
				"<h2>Illustrative Variables</h2>",
				sprintf("<p>%s</p>", html_escape(paste(illustrative_vars, collapse = ", ")))
			)
		}

		sections <- c(sections, "</body></html>")

		writeLines(sections, con = file, useBytes = TRUE)
	}
)

output$kmeans_results_panel <- shiny::renderUI({
	res <- cluster_state$kmeans_results
	if (is.null(res) || is.null(res$model)) {
		return(shiny::div(class = "p-4 text-muted", "Run K-Means to view clustering results."))
	}

	metric_card <- function(label, value_text) {
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(
				class = "card-body text-center",
				shiny::div(class = "text-muted text-uppercase small", label),
				shiny::h4(value_text)
			)
		)
	}

	data_info <- res$data_info %||% list()
	metrics <- res$metrics %||% list()
	params <- res$params %||% list()
	generated_label <- if (!is.null(res$generated_at)) format(res$generated_at, "%Y-%m-%d %H:%M:%S") else ""
	standardize_label <- if (isTRUE(params$standardize)) "Yes" else "No"
	global_homog <- if (is.null(res$global_homogeneity)) "--" else sprintf("%.3f", res$global_homogeneity)
	iterations_label <- metrics$iterations %||% "--"
	actual_runs_label <- metrics$actual_runs %||% "--"
	n_init_label <- metrics$n_init %||% params$n_init %||% "--"

	config_list <- shiny::tags$dl(
		class = "row mb-0",
		shiny::tags$dt(class = "col-sm-5", "Standardize"),
		shiny::tags$dd(class = "col-sm-7", standardize_label),
		shiny::tags$dt(class = "col-sm-5", "Initialization"),
		shiny::tags$dd(class = "col-sm-7", params$init_method %||% "homogeneity++"),
		shiny::tags$dt(class = "col-sm-5", "Max iterations"),
		shiny::tags$dd(class = "col-sm-7", params$max_iter %||% 100),
		shiny::tags$dt(class = "col-sm-5", "Tolerance"),
		shiny::tags$dd(class = "col-sm-7", params$tolerance %||% 1e-4),
		shiny::tags$dt(class = "col-sm-5", "Initialization runs"),
		shiny::tags$dd(class = "col-sm-7", sprintf("%s / %s", actual_runs_label, n_init_label)),
		shiny::tags$dt(class = "col-sm-5", "Iterations (best run)"),
		shiny::tags$dd(class = "col-sm-7", iterations_label)
	)

	shiny::tagList(
		shiny::div(
			class = "d-flex justify-content-between align-items-start mb-3",
			shiny::div(
				shiny::h3("K-Means Results"),
				shiny::div(
					class = "text-muted",
					shiny::span(sprintf("Dataset: %s", res$dataset_name %||% "dataset")),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Active variables: %d", data_info$n_vars %||% 0)),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Generated: %s", generated_label))
				)
			),
			shiny::downloadButton("download_kmeans_html", "Download HTML", class = "btn btn-outline-secondary")
		),
		shiny::fluidRow(
			shiny::column(4, metric_card("Observations", format(data_info$n_obs %||% 0, big.mark = " ", scientific = FALSE))),
			shiny::column(4, metric_card("Variables", format(data_info$n_vars %||% 0, big.mark = " ", scientific = FALSE))),
			shiny::column(4, metric_card("Global Homogeneity", global_homog))
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Configuration & Performance"),
			shiny::div(class = "card-body", config_list)
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Summary"),
			shiny::div(class = "card-body", shiny::verbatimTextOutput("kmeans_summary_text"))
		),
		shiny::fluidRow(
			shiny::column(
				width = 7,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Variable Assignments"),
					shiny::div(class = "card-body", shiny::tableOutput("kmeans_assignment_table"))
				)
			),
			shiny::column(
				width = 5,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Homogeneity"),
					shiny::div(class = "card-body", shiny::tableOutput("kmeans_homogeneity_table"))
				)
			)
		),
		shiny::fluidRow(
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Correlation Circle"),
					shiny::div(class = "card-body", shiny::plotOutput("kmeans_correlation_plot", height = "340px"))
				)
			),
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Quality"),
					shiny::div(class = "card-body", shiny::plotOutput("kmeans_cluster_quality_plot", height = "340px"))
				)
			),
			shiny::column(
				width = 4,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Correlation Heatmap"),
					shiny::div(class = "card-body", shiny::plotOutput("kmeans_heatmap_plot", height = "340px"))
				)
			)
		)
	)
})

output$kmeans_summary_text <- shiny::renderText({
	res <- cluster_state$kmeans_results
	req(res)
	res$summary_text %||% "Summary unavailable."
})

output$kmeans_assignment_table <- shiny::renderTable({
	res <- cluster_state$kmeans_results
	req(res, res$assignments)
	res$assignments
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE, rownames = FALSE)

output$kmeans_homogeneity_table <- shiny::renderTable({
	res <- cluster_state$kmeans_results
	req(res, res$homogeneity_table)
	res$homogeneity_table
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE, rownames = FALSE)

output$kmeans_correlation_plot <- shiny::renderPlot({
	res <- cluster_state$kmeans_results
	req(res, res$model)
	tryCatch({
		res$model$plot_fit(main = "Correlation Circle", show_centers = TRUE, show_labels = TRUE)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$kmeans_cluster_quality_plot <- shiny::renderPlot({
	res <- cluster_state$kmeans_results
	req(res, res$model)
	tryCatch({
		plot_cluster_quality(res$model)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$kmeans_heatmap_plot <- shiny::renderPlot({
	res <- cluster_state$kmeans_results
	req(res, res$model)
	tryCatch({
		plot_correlation_heatmap(res$model)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$download_kmeans_html <- shiny::downloadHandler(
	filename = function() {
		paste0("kmeans_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
	},
	content = function(file) {
		res <- cluster_state$kmeans_results
		if (is.null(res) || is.null(res$model) || is.null(res$assignments)) {
			stop("No K-Means results available for download.")
		}

		assignments <- res$assignments
		homogeneity <- res$homogeneity_table
		params <- res$params %||% list()
		data_info <- res$data_info %||% list()
		metrics <- res$metrics %||% list()
		n_init_label <- metrics$n_init %||% params$n_init %||% res$model$get_n_init()
		iterations_label <- metrics$iterations %||% res$model$get_iterations()

		html_escape <- function(text) {
			text <- as.character(text)
			text <- gsub("&", "&amp;", text, fixed = TRUE)
			text <- gsub("<", "&lt;", text, fixed = TRUE)
			text <- gsub(">", "&gt;", text, fixed = TRUE)
			text
		}

		df_to_html <- function(df, caption = NULL) {
			if (is.null(df) || nrow(df) == 0) return("")
			headers <- paste(sprintf("<th>%s</th>", html_escape(colnames(df))), collapse = "")
			rows <- apply(df, 1, function(row) {
				cells <- paste(sprintf("<td>%s</td>", html_escape(row)), collapse = "")
				paste0("<tr>", cells, "</tr>")
			})
			paste0(
				"<table>",
				if (!is.null(caption)) sprintf("<caption>%s</caption>", html_escape(caption)) else "",
				"<thead><tr>", headers, "</tr></thead>",
				"<tbody>", paste(rows, collapse = ""), "</tbody>",
				"</table>"
			)
		}

		sections <- c(
			"<!DOCTYPE html>",
			"<html><head><meta charset='utf-8'><title>K-Means Results</title>",
			"<style>body{font-family:Segoe UI,Arial,sans-serif;margin:32px;color:#222;}table{border-collapse:collapse;width:100%;margin-bottom:24px;}th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background:#f5f5f5;}caption{caption-side:top;font-weight:bold;margin-bottom:8px;}pre{background:#f7f7f9;padding:16px;border:1px solid #eee;overflow:auto;}</style>",
			"</head><body>",
			sprintf("<h1>K-Means Results</h1><p><strong>Dataset:</strong> %s<br><strong>Generated:</strong> %s<br><strong>Observations:</strong> %d<br><strong>Variables:</strong> %d</p>",
				html_escape(res$dataset_name %||% "dataset"),
				html_escape(format(res$generated_at, "%Y-%m-%d %H:%M:%S")),
				data_info$n_obs %||% 0,
				data_info$n_vars %||% 0
			),
			sprintf("<p><strong>Configuration:</strong> %d clusters, standardize = %s, init = %s, n_init = %s</p>",
				params$n_clusters %||% res$model$n_clusters,
				if (isTRUE(params$standardize)) "YES" else "NO",
				html_escape(params$init_method %||% "homogeneity++"),
				n_init_label
			),
			sprintf("<p><strong>Max iterations:</strong> %s<br><strong>Tolerance:</strong> %s<br><strong>Iterations (best run):</strong> %s</p>",
				params$max_iter %||% res$model$max_iter,
				params$tolerance %||% res$model$tol,
				iterations_label
			),
			sprintf("<p><strong>Global homogeneity:</strong> %.3f</p>", res$global_homogeneity %||% 0),
			"<h2>Summary</h2>",
			sprintf("<pre>%s</pre>", html_escape(res$summary_text %||% "Summary unavailable.")),
			"<h2>Variable Assignments</h2>",
			df_to_html(assignments, "Variables per cluster"),
			"<h2>Cluster Homogeneity</h2>",
			df_to_html(homogeneity, "Homogeneity by cluster")
		)

		sections <- c(sections, "</body></html>")
		writeLines(sections, con = file, useBytes = TRUE)
	}
)

output$pddp_results_panel <- shiny::renderUI({
	res <- cluster_state$pddp_results
	if (is.null(res) || is.null(res$model)) {
		return(shiny::div(class = "p-4 text-muted", "Run PDDP to view clustering results."))
	}

	metric_card <- function(label, value_text) {
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(
				class = "card-body text-center",
				shiny::div(class = "text-muted text-uppercase small", label),
				shiny::h4(value_text)
			)
		)
	}

	data_info <- res$data_info %||% list()
	params <- res$params %||% list()
	generated_label <- if (!is.null(res$generated_at)) format(res$generated_at, "%Y-%m-%d %H:%M:%S") else ""
	global_homog <- if (is.null(res$global_homogeneity) || is.na(res$global_homogeneity)) "--" else sprintf("%.3f", res$global_homogeneity)
	unexplained <- if (is.null(res$unexplained_variance) || is.na(res$unexplained_variance)) "--" else sprintf("%.3f", res$unexplained_variance)
	actual_clusters <- res$actual_clusters %||% res$model$n_clusters
	config_list <- shiny::tags$dl(
		class = "row mb-0",
		shiny::tags$dt(class = "col-sm-5", "Standardize"),
		shiny::tags$dd(class = "col-sm-7", if (isTRUE(params$standardize)) "Yes" else "No"),
		shiny::tags$dt(class = "col-sm-5", "Target clusters"),
		shiny::tags$dd(class = "col-sm-7", params$n_clusters %||% res$model$n_clusters),
		shiny::tags$dt(class = "col-sm-5", "Actual clusters"),
		shiny::tags$dd(class = "col-sm-7", actual_clusters %||% "--"),
		shiny::tags$dt(class = "col-sm-5", "Min cluster size"),
		shiny::tags$dd(class = "col-sm-7", params$min_cluster_size %||% 3),
		shiny::tags$dt(class = "col-sm-5", "Rotation"),
		shiny::tags$dd(class = "col-sm-7", params$rotation_method %||% "varimax"),
		shiny::tags$dt(class = "col-sm-5", "Split criterion"),
		shiny::tags$dd(class = "col-sm-7", params$split_criterion %||% "eigenvalue2"),
		shiny::tags$dt(class = "col-sm-5", "Stop at Kaiser"),
		shiny::tags$dd(class = "col-sm-7", if (isTRUE(params$stop_at_kaiser)) "Yes" else "No"),
		shiny::tags$dt(class = "col-sm-5", "Eigenvalue ratio min"),
		shiny::tags$dd(class = "col-sm-7", params$min_eigenvalue_ratio %||% 0.1),
		shiny::tags$dt(class = "col-sm-5", "Promax m"),
		shiny::tags$dd(class = "col-sm-7", params$promax_m %||% 4)
	)

	shiny::tagList(
		shiny::div(
			class = "d-flex justify-content-between align-items-start mb-3",
			shiny::div(
				shiny::h3("PDDP Results"),
				shiny::div(
					class = "text-muted",
					shiny::span(sprintf("Dataset: %s", res$dataset_name %||% "dataset")),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Active variables: %d", data_info$n_vars %||% 0)),
					shiny::HTML("&nbsp;&bull;&nbsp;"),
					shiny::span(sprintf("Generated: %s", generated_label))
				)
			),
			shiny::downloadButton("download_pddp_html", "Download HTML", class = "btn btn-outline-secondary")
		),
		shiny::fluidRow(
			shiny::column(4, metric_card("Observations", format(data_info$n_obs %||% 0, big.mark = " ", scientific = FALSE))),
			shiny::column(4, metric_card("Variables", format(data_info$n_vars %||% 0, big.mark = " ", scientific = FALSE))),
			shiny::column(4, metric_card("Global Homogeneity", global_homog))
		),
		shiny::fluidRow(
			shiny::column(4, metric_card("Unexplained Variance", unexplained)),
			shiny::column(4, metric_card("Actual Clusters", actual_clusters %||% "--")),
			shiny::column(4, metric_card("Stop at Kaiser", if (isTRUE(params$stop_at_kaiser)) "Enabled" else "Disabled"))
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Configuration"),
			shiny::div(class = "card-body", config_list)
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Summary"),
			shiny::div(class = "card-body", shiny::verbatimTextOutput("pddp_summary_text"))
		),
		shiny::fluidRow(
			shiny::column(
				width = 7,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Variable Assignments"),
					shiny::div(class = "card-body", shiny::tableOutput("pddp_assignment_table"))
				)
			),
			shiny::column(
				width = 5,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Homogeneity"),
					shiny::div(class = "card-body", shiny::tableOutput("pddp_homogeneity_table"))
				)
			)
		),
		shiny::div(
			class = "card shadow-sm mb-3",
			shiny::div(class = "card-header fw-semibold", "Split Summary"),
			shiny::div(class = "card-body", shiny::tableOutput("pddp_split_summary_table"))
		),
		shiny::fluidRow(
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Correlation Circle"),
					shiny::div(class = "card-body", shiny::plotOutput("pddp_correlation_plot", height = "340px"))
				)
			),
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Cluster Quality"),
					shiny::div(class = "card-body", shiny::plotOutput("pddp_cluster_quality_plot", height = "340px"))
				)
			)
		),
		shiny::fluidRow(
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Dendrogram"),
					shiny::div(class = "card-body", shiny::plotOutput("pddp_dendrogram_plot", height = "340px"))
				)
			),
			shiny::column(
				width = 6,
				shiny::div(
					class = "card shadow-sm mb-3",
					shiny::div(class = "card-header fw-semibold", "Correlation Heatmap"),
					shiny::div(class = "card-body", shiny::plotOutput("pddp_heatmap_plot", height = "340px"))
				)
			)
		)
	)
})

output$pddp_summary_text <- shiny::renderText({
	res <- cluster_state$pddp_results
	req(res)
	res$summary_text %||% "Summary unavailable."
})

output$pddp_assignment_table <- shiny::renderTable({
	res <- cluster_state$pddp_results
	req(res)
	assignments <- res$assignments
	if (is.null(assignments) || nrow(assignments) == 0) return(NULL)
	assignments
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE, rownames = FALSE)

output$pddp_homogeneity_table <- shiny::renderTable({
	res <- cluster_state$pddp_results
	req(res)
	tbl <- res$homogeneity_table
	if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
	tbl
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE, rownames = FALSE)

output$pddp_split_summary_table <- shiny::renderTable({
	res <- cluster_state$pddp_results
	req(res)
	split_summary <- res$split_summary
	if (is.null(split_summary) || nrow(split_summary) == 0) return(NULL)
	head(split_summary, 10)
}, striped = TRUE, hover = TRUE, spacing = "s", bordered = FALSE, rownames = FALSE)

output$pddp_correlation_plot <- shiny::renderPlot({
	res <- cluster_state$pddp_results
	req(res, res$model)
	tryCatch({
		plot_clustering_2d(res$model, main = "Correlation Circle", show_centers = TRUE, show_labels = TRUE)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$pddp_cluster_quality_plot <- shiny::renderPlot({
	res <- cluster_state$pddp_results
	req(res, res$model)
	tryCatch({
		plot_cluster_quality(res$model)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$pddp_dendrogram_plot <- shiny::renderPlot({
	res <- cluster_state$pddp_results
	req(res, res$model)
	tryCatch({
		plot_dendrogram(res$model)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$pddp_heatmap_plot <- shiny::renderPlot({
	res <- cluster_state$pddp_results
	req(res, res$model)
	tryCatch({
		plot_correlation_heatmap(res$model)
	}, error = function(err) {
		graphics::plot.new()
		graphics::text(0.5, 0.5, sprintf("Unable to render plot: %s", err$message))
	})
})

output$download_pddp_html <- shiny::downloadHandler(
	filename = function() {
		paste0("pddp_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
	},
	content = function(file) {
		res <- cluster_state$pddp_results
		if (is.null(res) || is.null(res$model)) {
			stop("No PDDP results available for download.")
		}

		assignments <- res$assignments
		homogeneity <- res$homogeneity_table
		split_summary <- res$split_summary
		params <- res$params %||% list()
		data_info <- res$data_info %||% list()

		html_escape <- function(text) {
			text <- as.character(text)
			text <- gsub("&", "&amp;", text, fixed = TRUE)
			text <- gsub("<", "&lt;", text, fixed = TRUE)
			text <- gsub(">", "&gt;", text, fixed = TRUE)
			text
		}

		df_to_html <- function(df, caption = NULL) {
			if (is.null(df) || nrow(df) == 0) return("")
			headers <- paste(sprintf("<th>%s</th>", html_escape(colnames(df))), collapse = "")
			rows <- apply(df, 1, function(row) {
				cells <- paste(sprintf("<td>%s</td>", html_escape(row)), collapse = "")
				paste0("<tr>", cells, "</tr>")
			})
			paste0(
				"<table>",
				if (!is.null(caption)) sprintf("<caption>%s</caption>", html_escape(caption)) else "",
				"<thead><tr>", headers, "</tr></thead>",
				"<tbody>", paste(rows, collapse = ""), "</tbody>",
				"</table>"
			)
		}

		sections <- c(
			"<!DOCTYPE html>",
			"<html><head><meta charset='utf-8'><title>PDDP Results</title>",
			"<style>body{font-family:Segoe UI,Arial,sans-serif;margin:32px;color:#222;}table{border-collapse:collapse;width:100%;margin-bottom:24px;}th,td{border:1px solid #ddd;padding:8px;text-align:left;}th{background:#f5f5f5;}caption{caption-side:top;font-weight:bold;margin-bottom:8px;}pre{background:#f7f7f9;padding:16px;border:1px solid #eee;overflow:auto;}</style>",
			"</head><body>",
			sprintf("<h1>PDDP Results</h1><p><strong>Dataset:</strong> %s<br><strong>Generated:</strong> %s<br><strong>Observations:</strong> %d<br><strong>Variables:</strong> %d</p>",
				html_escape(res$dataset_name %||% "dataset"),
				html_escape(format(res$generated_at, "%Y-%m-%d %H:%M:%S")),
				data_info$n_obs %||% 0,
				data_info$n_vars %||% 0
			),
			sprintf("<p><strong>Configuration:</strong> %d target clusters, actual %d<br><strong>Standardize:</strong> %s<br><strong>Rotation:</strong> %s<br><strong>Split criterion:</strong> %s<br><strong>Stop at Kaiser:</strong> %s<br><strong>Eigenvalue ratio min:</strong> %s</p>",
				params$n_clusters %||% res$model$n_clusters,
				res$actual_clusters %||% res$model$n_clusters,
				if (isTRUE(params$standardize)) "YES" else "NO",
				html_escape(params$rotation_method %||% "varimax"),
				html_escape(params$split_criterion %||% "eigenvalue2"),
				if (isTRUE(params$stop_at_kaiser)) "YES" else "NO",
				params$min_eigenvalue_ratio %||% 0.1
			),
			sprintf("<p><strong>Global homogeneity:</strong> %s<br><strong>Unexplained variance:</strong> %s</p>",
				if (is.null(res$global_homogeneity) || is.na(res$global_homogeneity)) "--" else sprintf("%.3f", res$global_homogeneity),
				if (is.null(res$unexplained_variance) || is.na(res$unexplained_variance)) "--" else sprintf("%.3f", res$unexplained_variance)
			),
			"<h2>Summary</h2>",
			sprintf("<pre>%s</pre>", html_escape(res$summary_text %||% "Summary unavailable.")),
			"<h2>Variable Assignments</h2>",
			df_to_html(assignments, "Variables per cluster"),
			"<h2>Cluster Homogeneity</h2>",
			df_to_html(homogeneity, "Homogeneity by cluster"),
			"<h2>Split Summary</h2>",
			df_to_html(split_summary, "Split diagnostics")
		)

		sections <- c(sections, "</body></html>")
		writeLines(sections, con = file, useBytes = TRUE)
	}
)
