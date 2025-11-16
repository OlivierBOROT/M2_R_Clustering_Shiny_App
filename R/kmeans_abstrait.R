library(R6)

KMeansVariables <- R6Class("KMeansVariables",
                           inherit = BaseClusterer,
                           
                           private = list(
                             data = NULL,                # données originales (observations x variables)
                             k = 2,                      # nombre de clusters
                             scaleData = TRUE,           # standardisation
                             centroids = NULL,           # centroïdes (PC1 de chaque cluster)
                             clusters = NULL,            # affectation des variables
                             iterMax = 100,              # nombre max d'itérations
                             n_init = 10,                # nombre d'initialisations
                             fitted = FALSE,             # modèle entraîné ou non
                             inertia = NULL,             # inertie finale
                             inertia_history = NULL,     # historique des inerties
                             coords_fit = NULL,          # coordonnées pour plot_fit (variables actives)
                             coords_predict = NULL,      # coordonnées pour plot_predict (actives + illustratives)
                             cluster_colors = NULL,      # couleurs fixes
                             X_scaled = NULL,            # données standardisées (variables x observations)
                             X_illus = NULL,             # variables illustratives standardisées (variables x observations)
                             pca_global = NULL,          # PCA globale pour visualisation
                             
                             # Calcul de la PC1 d'un ensemble de variables 
                             compute_pc1 = function(X_subset) {
                               if (nrow(X_subset) == 0) stop("compute_pc1 : le cluster est vide !")
                               if (nrow(X_subset) == 1) return(as.numeric(X_subset))
                               
                               pca <- prcomp(t(X_subset), scale. = FALSE)
                               return(pca$x[, 1])
                             },
                             
                             # Calcul de la distance basée sur la corrélation
                             compute_distance = function(var, centroid) {
                               cor_val <- cor(var, centroid)
                               return(sqrt(1 - cor_val^2))
                             },
                             
                             # Calcul de l'inertie totale (somme des R² intra-cluster)
                             compute_inertia = function(X, clusters, centroids) {
                               inertia <- 0
                               for (kk in 1:private$k) {
                                 idx <- which(clusters == kk)
                                 if (length(idx) > 0) {
                                   for (i in idx) {
                                     cor_val <- cor(X[i, ], centroids[[kk]])
                                     inertia <- inertia + cor_val^2
                                   }
                                 }
                               }
                               return(inertia)
                             },
                             
                             # Une itération complète de K-means (sur X : variables x observations)
                             run_kmeans_once = function(X, init_seed) {
                               set.seed(init_seed)
                               n_vars <- nrow(X)
                               
                               # 1. Initialisation aléatoire (éviter clusters vides)
                               clusters <- sample(1:private$k, n_vars, replace = TRUE)
                               empty_clusters <- setdiff(1:private$k, unique(clusters))
                               while (length(empty_clusters) > 0) {
                                 for (empty_k in empty_clusters) {
                                   largest_cluster <- as.numeric(names(which.max(table(clusters))))
                                   vars_in_largest <- which(clusters == largest_cluster)
                                   clusters[sample(vars_in_largest, 1)] <- empty_k
                                 }
                                 empty_clusters <- setdiff(1:private$k, unique(clusters))
                               }
                               
                               best_inertia <- -Inf
                               converged <- FALSE
                               iter <- 0
                               
                               for (iter in 1:private$iterMax) {
                                 old_clusters <- clusters
                                 
                                 # 2. Calcul des centroïdes (PC1 de chaque cluster)
                                 centroids <- list()
                                 for (kk in 1:private$k) {
                                   idx <- which(clusters == kk)
                                   X_cluster <- X[idx, , drop = FALSE]
                                   centroids[[kk]] <- private$compute_pc1(X_cluster)
                                 }
                                 
                                 # 3-4. Calcul des distances et réaffectation
                                 for (i in 1:n_vars) {
                                   distances <- numeric(private$k)
                                   for (kk in 1:private$k) {
                                     distances[kk] <- private$compute_distance(X[i, ], centroids[[kk]])
                                   }
                                   clusters[i] <- which.min(distances)
                                 }
                                 
                                 # S'assurer qu'aucun cluster n'est vide
                                 empty_clusters <- setdiff(1:private$k, unique(clusters))
                                 while (length(empty_clusters) > 0) {
                                   for (empty_k in empty_clusters) {
                                     largest_cluster <- as.numeric(names(which.max(table(clusters))))
                                     vars_in_largest <- which(clusters == largest_cluster)
                                     clusters[sample(vars_in_largest, 1)] <- empty_k
                                   }
                                   empty_clusters <- setdiff(1:private$k, unique(clusters))
                                 }
                                 
                                 # 5. Inertie
                                 current_inertia <- private$compute_inertia(X, clusters, centroids)
                                 
                                 # 6. Convergence (variation inertie ou pas de changement)
                                 if (iter > 1) {
                                   if (all(clusters == old_clusters)) {
                                     converged <- TRUE
                                     break
                                   }
                                   if (abs(current_inertia - best_inertia) < self$tol) {
                                     converged <- TRUE
                                     break
                                   }
                                 }
                                 best_inertia <- current_inertia
                               }
                               
                               return(list(
                                 clusters = clusters,
                                 centroids = centroids,
                                 inertia = best_inertia,
                                 iter = iter
                               ))
                             }
                           ),
                           
                           public = list(
                             tol = 1e-4,
                           #---------------------------
                           #      CONSTRUCTEUR 
                           #---------------------------
                             initialize = function(n_clusters = 2, scale = TRUE, iterMax = 100, n_init = 10, tol = 1e-4) {
                               private$k <- n_clusters
                               private$scaleData <- scale
                               private$iterMax <- iterMax
                               private$n_init <- n_init
                               self$tol <- tol
                             },
                             
                           #--------------------
                           #      FIT
                           #---------------------
                             fit = function(D) {
                               if (missing(D)) stop("Vous devez fournir un data frame à fit()")
                               if (!is.data.frame(D)) stop("D doit être un data frame")
                               if (!all(sapply(D, is.numeric))) stop("Toutes les variables doivent être numériques")
                               if (ncol(D) < private$k) stop("Le nombre de variables doit être >= au nombre de clusters")
                               
                               private$data <- D  # D : observations en lignes, variables en colonnes
                               X <- as.matrix(t(D))  # X : variables en lignes, observations en colonnes
                               
                               if (private$scaleData) X <- scale(X)
                               private$X_scaled <- X
                               
                               cat(sprintf("Exécution de %d initialisations...\n", private$n_init))
                               best_result <- NULL
                               best_inertia <- -Inf
                               
                               for (init in 1:private$n_init) {
                                 result <- private$run_kmeans_once(X, init_seed = init * 42)
                                 if (result$inertia > best_inertia) {
                                   best_inertia <- result$inertia
                                   best_result <- result
                                 }
                               }
                               
                               private$clusters <- best_result$clusters
                               private$centroids <- best_result$centroids
                               private$inertia <- best_result$inertia
                               cat(sprintf("Meilleure inertie: %.4f\n", best_inertia))
                               
                               # PCA globale sur les variables (les lignes de X = variables)
                               pca <- prcomp(X, scale. = FALSE)
                               private$pca_global <- pca
                               
                               coords <- as.data.frame(pca$x[, 1:2])
                               coords$cluster <- factor(private$clusters)
                               coords$variable <- rownames(X)
                               private$coords_fit <- coords
                               
                               private$cluster_colors <- c("red", "green", "blue", "orange", "purple", "cyan", "magenta", "yellow")[1:private$k]
                               private$fitted <- TRUE
                               
                               invisible(self)
                             },
                           #--------------------
                           #      PREDICT
                           #---------------------
                             predict = function(D_illus, return_dist = TRUE) {
                               if (!private$fitted) stop("Le modèle doit être entraîné d'abord avec fit().")
                               if (!is.data.frame(D_illus)) stop("D_illus doit être un data.frame.")
                               if (!all(sapply(D_illus, is.numeric))) stop("Toutes les variables doivent être numériques")
                               if (nrow(D_illus) != nrow(private$data)) stop("D_illus doit avoir le même nombre d'observations que les données d'apprentissage.")
                               
                               # D_illus : observations en lignes, variables en colonnes (format classique)
                               X_illus <- as.matrix(t(D_illus))  # Transposée : variables en lignes, observations en colonnes
                               if (private$scaleData) {
                                 # Standardiser avec la même logique que pour X_scaled
                                 # On suppose que X_scaled a été centré/scalé colonne par colonne (observations)
                                 # Ici on applique scale() sur X_illus indépendamment (identique au fit)
                                 #X_illus <- scale(X_illus)
                                 centers <- attr(private$X_scaled, "scaled:center")
                                 scales <- attr(private$X_scaled, "scaled:scale")
                                 X_illus <- scale(X_illus, center = centers, scale = scales)
                               }
                               # Sauvegarde pour plot_predict (essentiel pour corrélations)
                               private$X_illus <- X_illus
                               
                               n_illus <- nrow(X_illus)
                               pred_clusters <- numeric(n_illus)
                               pred_dists <- numeric(n_illus)
                               
                               for (i in 1:n_illus) {
                                 distances <- numeric(private$k)
                                 for (kk in 1:private$k) {
                                   distances[kk] <- private$compute_distance(X_illus[i, ], private$centroids[[kk]])
                                 }
                                 pred_clusters[i] <- which.min(distances)
                                 pred_dists[i] <- min(distances)
                               }
                               
                               # Préparer coords pour la visualisation :
                               coords_active <- as.data.frame(private$pca_global$x[, 1:2])
                               colnames(coords_active) <- c("PC1", "PC2")
                               coords_active$cluster <- private$clusters
                               coords_active$type <- "active"
                               coords_active$variable <- rownames(private$X_scaled)
                               
                               # Projection des variables illustratives dans l'espace PCA
                               illus_pca <- predict(private$pca_global, newdata = X_illus)
                               coords_illus <- as.data.frame(illus_pca[, 1:2])
                               colnames(coords_illus) <- c("PC1", "PC2")
                               coords_illus$cluster <- pred_clusters
                               coords_illus$type <- "illustrative"
                               # si les variables illustratives ont des noms de colonnes originaux :
                               coords_illus$variable <- rownames(X_illus)
                               
                               private$coords_predict <- rbind(coords_active, coords_illus)
                               
                               if (return_dist) {
                                 return(data.frame(
                                   variable = colnames(D_illus),
                                   cluster = pred_clusters,
                                   distance = pred_dists,
                                   row.names = NULL
                                 ))
                               } else {
                                 return(pred_clusters)
                               }
                             },
                           
                           #--------------------
                           #      PLOT FIT
                           #---------------------
                           
                             plot_fit = function() {
                               if (is.null(private$coords_fit)) stop("Aucune visualisation disponible : veuillez exécuter fit() d'abord.")
                               
                               # Matrice de corrélation entre les variables (actives)
                               cor_matrix <- cor(t(private$X_scaled))
                               
                               # Réorganiser selon les clusters
                               order_idx <- order(private$clusters)
                               cor_matrix_ordered <- cor_matrix[order_idx, order_idx]
                               var_names_ordered <- rownames(private$X_scaled)[order_idx]
                               clusters_ordered <- private$clusters[order_idx]
                               
                               par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
                               
                               # ===== HEATMAP =====
                               n_vars <- nrow(cor_matrix_ordered)
                               image(1:n_vars, 1:n_vars, cor_matrix_ordered,
                                     col = colorRampPalette(c("blue", "white", "red"))(100),
                                     xlab = "", ylab = "",
                                     main = "Heatmap des corrélations\n(variables groupées par cluster)",
                                     axes = FALSE)
                               
                               axis(1, at = 1:n_vars, labels = var_names_ordered, las = 2, cex.axis = 0.7)
                               axis(2, at = 1:n_vars, labels = var_names_ordered, las = 2, cex.axis = 0.7)
                               
                               cluster_breaks <- c(0, which(diff(clusters_ordered) != 0), n_vars) + 0.5
                               for (i in cluster_breaks) abline(h = i, v = i, col = "black", lwd = 2)
                               
                               legend("topright", 
                                      legend = c("Corr = 1", "Corr = 0", "Corr = -1"),
                                      fill = c("red", "white", "blue"),
                                      cex = 0.7, bg = "white")
                               
                               # ===== NETWORK =====
                               threshold <- 0.5
                               plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
                                    xlab = "", ylab = "", axes = FALSE,
                                    main = "Network des variables\n(liens = corrélations > 0.5)")
                               
                               angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]
                               x_pos <- cos(angles)
                               y_pos <- sin(angles)
                               
                               for (i in 1:(n_vars-1)) {
                                 for (j in (i+1):n_vars) {
                                   cor_val <- abs(cor_matrix[i, j])
                                   if (cor_val > threshold) {
                                     lwd_val <- (cor_val - threshold) / (1 - threshold) * 3
                                     col_val <- if (private$clusters[i] == private$clusters[j]) {
                                       private$cluster_colors[private$clusters[i]]
                                     } else {
                                       "gray80"
                                     }
                                     segments(x_pos[i], y_pos[i], x_pos[j], y_pos[j],
                                              col = col_val, lwd = lwd_val)
                                   }
                                 }
                               }
                               
                               points(x_pos, y_pos,
                                      pch = 19, cex = 2.5,
                                      col = private$cluster_colors[private$clusters])
                               
                               text(x_pos * 1.15, y_pos * 1.15,
                                    labels = rownames(private$X_scaled),
                                    cex = 0.7, font = 2)
                               
                               legend("bottomright",
                                      legend = paste("Cluster", 1:private$k),
                                      col = private$cluster_colors[1:private$k],
                                      pch = 19, pt.cex = 1.5,
                                      cex = 0.8, bg = "white")
                               
                               par(mfrow = c(1, 1))
                             },
                           #--------------------
                           #      PLOT PREDICT
                           #---------------------
                             plot_predict = function() {
                               if (is.null(private$coords_predict)) stop("plot_predict() : projection des variables illustratives non encore effectuée")
                               if (is.null(private$X_illus)) stop("plot_predict() : les variables illustratives standardisées (private$X_illus) sont manquantes. Exécute predict() d'abord.")
                               
                               coords_all <- private$coords_predict
                               coords_active <- coords_all[coords_all$type == "active", ]
                               coords_illus  <- coords_all[coords_all$type == "illustrative", ]
                               
                               # ===== MATRICE DE CORRÉLATION COMPLÈTE (actives + illustratives) =====
                               # private$X_scaled : variables actives (variables x observations)
                               # private$X_illus  : variables illustratives (variables x observations)
                               # On aligne en rbind : mêmes colonnes (observations)
                               X_full <- rbind(private$X_scaled, private$X_illus)
                               
                               cor_matrix <- cor(t(X_full))
                               n_vars <- nrow(cor_matrix)
                               
                               # clusters pour l'ensemble (ordre compatible avec rbind)
                               clusters_full <- c(private$clusters, coords_illus$cluster)
                               names_clusters <- c(rownames(private$X_scaled), rownames(private$X_illus))
                               
                               order_idx <- order(clusters_full)
                               cor_matrix_ordered <- cor_matrix[order_idx, order_idx]
                               var_names_ordered <- names_clusters[order_idx]
                               clusters_ordered <- clusters_full[order_idx]
                               
                               # ===== LAYOUT : 2 graphiques côte à côte =====
                               old_par <- par(no.readonly = TRUE)
                               par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
                               
                               # 1) HEATMAP
                               image(1:n_vars, 1:n_vars, cor_matrix_ordered,
                                     col = colorRampPalette(c("blue", "white", "red"))(100),
                                     xlab = "", ylab = "",
                                     main = "Heatmap corrélations\n(Actives + Illustratives)",
                                     axes = FALSE)
                               axis(1, at = 1:n_vars, labels = var_names_ordered, las = 2, cex.axis = 0.6)
                               axis(2, at = 1:n_vars, labels = var_names_ordered, las = 2, cex.axis = 0.6)
                               cluster_breaks <- c(0, which(diff(clusters_ordered) != 0), n_vars) + 0.5
                               for (i in cluster_breaks) abline(h = i, v = i, col = "black", lwd = 2)
                               legend("topright", legend = c("Corr=1","Corr=0","Corr=-1"), fill = c("red","white","blue"), cex = 0.7)
                               
                               # 2) NETWORK
                               threshold <- 0.5
                               plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
                                    main = "Network corrélation\n(Actives + Illustratives)",
                                    xlab = "", ylab = "", axes = FALSE)
                               
                               angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]
                               x_pos <- cos(angles)
                               y_pos <- sin(angles)
                               
                               for (i in 1:(n_vars-1)) {
                                 for (j in (i+1):n_vars) {
                                   cor_val <- abs(cor_matrix_ordered[i, j])
                                   if (cor_val > threshold) {
                                     col_val <- if (clusters_ordered[i] == clusters_ordered[j]) private$cluster_colors[clusters_ordered[i]] else "gray80"
                                     lwd_val <- (cor_val - threshold) / (1 - threshold) * 3
                                     segments(x_pos[i], y_pos[i], x_pos[j], y_pos[j], col = col_val, lwd = lwd_val)
                                   }
                                 }
                               }
                               
                               points(x_pos, y_pos, pch = 19, col = private$cluster_colors[clusters_ordered], cex = 1.8)
                               text(x_pos * 1.12, y_pos * 1.12, labels = var_names_ordered, cex = 0.6)
                               legend("bottomright", legend = paste("Cluster", 1:private$k), col = private$cluster_colors[1:private$k], pch = 19, pt.cex = 1.4)
                               
                               par(old_par) # restore
                             },
                           
                           #--------------------
                           #      PRINT
                           #---------------------
                             print = function() {
                               if (!private$fitted) {
                                 cat("Le modèle n'a pas encore été entraîné.\n")
                                 return(invisible(self))
                               }
                               
                               cat(sprintf("K-means Variables avec %d clusters\n\n", private$k))
                               sizes <- table(private$clusters)
                               cat("Tailles des clusters : ", paste(sizes, collapse = ", "), "\n\n")
                               cat(sprintf("Inertie (somme des R²) : %.4f\n\n", private$inertia))
                               
                               cat("Assignation des variables aux clusters :\n")
                               assignation <- data.frame(variable = colnames(private$data), cluster = unname(private$clusters))
                               print(assignation, row.names = FALSE)
                               
                               cat("\nCorrélations moyennes par cluster :\n")
                               for (kk in 1:private$k) {
                                 idx <- which(private$clusters == kk)
                                 cors <- numeric(length(idx))
                                 for (i in seq_along(idx)) {
                                   cors[i] <- cor(private$X_scaled[idx[i], ], private$centroids[[kk]])
                                 }
                                 cat(sprintf(" Cluster %d : %.4f (n=%d)\n", kk, mean(cors), length(idx)))
                               }
                               invisible(self)
                             },
                           
                           #--------------------
                           #      SUMMARY
                           #---------------------
                             
                             summary = function() {
                               if (!private$fitted) {
                                 cat("Le modèle n'a pas encore été entraîné.\n")
                                 return(invisible(self))
                               }
                               
                               cat("=== K-means Variables Clustering ===\n\n")
                               cat(sprintf("Méthode : K-means avec distance basée sur corrélation\n"))
                               cat(sprintf("Nombre de clusters : %d\n", private$k))
                               cat(sprintf("Nombre d'initialisations : %d\n", private$n_init))
                               cat(sprintf("Nombre de variables : %d\n", ncol(private$data)))
                               cat(sprintf("Nombre d'observations : %d\n", nrow(private$data)))
                               cat("\n")
                               
                               sizes <- as.numeric(table(private$clusters))
                               cat("Tailles des clusters : ", paste(sizes, collapse = ", "), "\n")
                               cat(sprintf("\nInertie totale : %.4f\n", private$inertia))
                               cat(sprintf("Inertie moyenne par variable : %.4f\n", private$inertia / length(private$clusters)))
                               
                               cat("\nQualité de chaque cluster (corrélation² moyenne) :\n")
                               for (kk in 1:private$k) {
                                 idx <- which(private$clusters == kk)
                                 cors_sq <- numeric(length(idx))
                                 for (i in seq_along(idx)) {
                                   cor_val <- cor(private$X_scaled[idx[i], ], private$centroids[[kk]])
                                   cors_sq[i] <- cor_val^2
                                 }
                                 cat(sprintf(" Cluster %d : R² moyen = %.4f (min=%.4f, max=%.4f)\n",
                                             kk, mean(cors_sq), min(cors_sq), max(cors_sq)))
                               }
                               invisible(self)
                             }
                           )
)
