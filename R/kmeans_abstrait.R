library(R6)

KMeans <- R6Class("KMeans",
                  inherit = BaseClusterer,  # <--- héritage ajouté
                  
                  private = list(
                    data = NULL,       # données originales
                    k = 2,             # nombre de clusters
                    scaleData = TRUE,  # standardisation
                    centers = NULL,    # centres de clusters
                    clusters = NULL,   # affectation des individus
                    iterMax = 100,     # nombre max d'itérations
                    fitted = FALSE,    # modèle entrainé ou non
                    pca = NULL,        # stockage de la PCA pour projection
                    X_scaled = NULL,   # données standardisées pour projection correcte
                    coords_fit = NULL, # coordonnées pour plot_fit
                    coords_predict = NULL, # coordonnées pour plot_predict
                    cluster_colors = NULL  # pour garder la couleur fixe entre fit et predict
                  ),
                  
                  public = list(
                    #-------------
                    # Constructeur
                    #-------------
                    initialize = function(data, n_clusters = 2, scale = TRUE, iterMax = 100, tol = 1e-4){
                      super$initialize(data, n_clusters)  # appel du constructeur parent
                      
                      private$k <- n_clusters
                      private$scaleData <- scale
                      private$iterMax <- iterMax
                      self$tol <- tol
                    },
                    
                    #----------------------------------------------
                    # Méthode d'apprentissage des variables actives
                    #----------------------------------------------
                    
                    fit = function(D = self$data){
                      # Vérification
                      if (!is.data.frame(D)){
                        stop("D doit être un data frame")
                      }
                      if (!all(sapply(D, is.numeric))){
                        stop("Toutes les variables doivent être numériques")
                      }
                      
                      private$data <- D
                      
                      # Transposition (clustering de variables)
                      X <- as.data.frame(t(D))
                      
                      # Centre-réduit si demandé
                      if (private$scaleData){
                        X <- as.data.frame(scale(X))
                      }
                      
                      # Stockage des données standardisées pour la projection PCA
                      private$X_scaled <- X
                      
                      # Initialisation aléatoire des centres
                      set.seed(10)
                      private$centers <- X[sample(1:nrow(X), private$k), , drop=FALSE]
                      rownames(private$centers) <- paste0("C", 1:private$k)  
                      
                      # Boucle itérative
                      for (i in 1:private$iterMax){
                        # Calcul des distances euclidiennes
                        dists <- as.matrix(dist(rbind(private$centers, X)))[1:private$k, (private$k+1):(nrow(X)+private$k)]
                        
                        # Affectation au cluster le plus proche
                        newClusters <- apply(dists, 2, which.min)
                        
                        # Arrêt si convergence
                        if (!is.null(private$clusters) && all(newClusters == private$clusters)){
                          break
                        }
                        
                        private$clusters <- newClusters
                        
                        # Recalcul des centres
                        for (j in 1:private$k){
                          private$centers[j, ] <- colMeans(X[private$clusters == j, , drop=FALSE])
                        }
                      }
                      
                      # PCA pour projection
                      pca <- prcomp(X, scale. = FALSE)
                      private$pca <- pca
                      
                      # Stockage des coordonnées pour plot
                      coords <- as.data.frame(pca$x[, 1:2])
                      coords$cluster <- factor(private$clusters)
                      coords$variable <- rownames(X)
                      private$coords_fit <- coords
                      
                      # Définition des couleurs fixes
                      private$cluster_colors <- rainbow(private$k)
                      
                      private$fitted <- TRUE
                    },
                    
                    #---------------------------------------------
                    # Méthode de prédiction sur variables illustratives
                    #---------------------------------------------
                    predict = function(D_illus, return_dist = TRUE) {
                      # Vérification des conditions
                      if (!private$fitted){
                        stop("Le modèle doit être entraîné d'abord avec fit().")
                      } 
                      if (!is.data.frame(D_illus)){
                        stop("D_illus doit être un data.frame.")
                      }
                      if (!all(sapply(D_illus, is.numeric))){
                        stop("Toutes les variables doivent être numériques.")
                      } 
                      if (nrow(D_illus) != nrow(private$data)){
                        stop("D_illus doit avoir le même nombre d'observations (lignes) que les données d'apprentissage.")
                      }
                      
                      # Transposition des données illustratives
                      X_illus <- as.data.frame(t(D_illus))
                      
                      # Standardisation avec les mêmes paramètres que les données actives
                      if (private$scaleData) {
                        X_illus <- as.data.frame(scale(X_illus))
                      }
                      
                      # Calcul des distances par rapport aux centres
                      dists <- as.matrix(dist(rbind(private$centers, X_illus)))[1:private$k, (private$k + 1):(nrow(X_illus) + private$k)]
                      
                      # Attribution au cluster le plus proche
                      pred_clusters <- apply(dists, 2, which.min)
                      pred_dists <- apply(dists, 2, min)
                      
                      # Stockage des coordonnées pour plot_predict
                      coords_active <- as.data.frame(private$pca$x[, 1:2])
                      coords_active$cluster <- private$clusters
                      coords_active$type <- "active"
                      coords_active$variable <- rownames(private$X_scaled)
                      
                      illus_pca <- as.matrix(X_illus) %*% private$pca$rotation[, 1:2]
                      coords_illus <- as.data.frame(illus_pca)
                      colnames(coords_illus) <- c("PC1", "PC2")
                      coords_illus$cluster <- pred_clusters
                      coords_illus$type <- "illustrative"
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
                    
                    #---------------------------------------------
                    # Méthode plot pour fit
                    #---------------------------------------------
                    plot_fit = function(){
                      if (is.null(private$coords_fit)){
                        stop("Aucune visualisation disponible : veuillez exécuter fit() d'abord.")
                      }
                      
                      coords <- private$coords_fit
                      plot(coords$PC1, coords$PC2,
                           col = private$cluster_colors[coords$cluster],
                           pch = 19,
                           main = "Clustering des variables actives",
                           xlab = "PC1", ylab = "PC2")
                      text(coords$PC1, coords$PC2, labels = coords$variable, pos = 3, cex = 0.8)
                      
                      # Projection correcte des centres dans l'espace PCA
                      centers_pca <- as.matrix(private$centers) %*% private$pca$rotation[, 1:2]
                      points(centers_pca[, 1], centers_pca[, 2], pch = 9, col = "black", cex = 1.5)
                      text(centers_pca[, 1], centers_pca[, 2], 
                           labels = paste0("C", 1:private$k), pos = 1, cex = 0.9, col = "black", font = 2)
                    },
                    
                    #---------------------------------------------
                    # Méthode plot pour predict
                    #---------------------------------------------
                    plot_predict = function(){
                      if (is.null(private$coords_predict)){
                        stop("plot_predict() : projection des variables illustratives non encore affichée")
                      }
                      
                      coords_all <- private$coords_predict
                      plot(coords_all$PC1, coords_all$PC2,
                           col = private$cluster_colors[coords_all$cluster],
                           pch = ifelse(coords_all$type == "active", 19, 17),
                           main = "Clusters avec variables illustratives",
                           xlab = "PC1", ylab = "PC2")
                      text(coords_all$PC1, coords_all$PC2, labels = coords_all$variable, pos = 3, cex = 0.8)
                      
                      # Ajout des centres
                      centers_pca <- as.matrix(private$centers) %*% private$pca$rotation[, 1:2]
                      points(centers_pca[, 1], centers_pca[, 2], pch = 9, col = "black", cex = 1.5)
                      text(centers_pca[, 1], centers_pca[, 2], 
                           labels = paste0("C", 1:private$k), pos = 1, cex = 0.9, col = "black", font = 2)
                      
                      legend("topright", 
                             legend = c("Variables actives", "Variables illustratives", "Centres"),
                             pch = c(19, 17, 15), 
                             col = c("black", "black", "black"),
                             cex = 0.8)
                    },
                    
                    #-------------
                    # Méthode print 
                    #--------------
                    print = function() {
                      if (!private$fitted) {
                        cat("Le modèle n'a pas encore été entraîné.\n")
                        return(invisible(self))
                      }
                      

                      # En-tête général : 
                      cat(sprintf("K-means clustering avec %d clusters de tailles : %s\n\n",
                                  private$k,
                                  paste(table(private$clusters), collapse = ", ")))
                      
                      # Centres de clusters
                      cat("Centres des clusters :\n")
                      print(round(private$centers, 3))
                      cat("\n")
                      
                      # Vecteur d'appartenance 
                      cat("Vecteur de clustering :\n")
                      print(private$clusters)
                      cat("\n")
                      

                      # Inerties (within, between, totale)
                      inertie_intra <- numeric(private$k)
                      for (j in 1:private$k) {
                        points_cluster <- private$X_scaled[private$clusters == j, , drop = FALSE]
                        center <- private$centers[j, , drop = FALSE]
                        center_rep <- center[rep(1, nrow(points_cluster)), , drop = FALSE]
                        inertie_intra[j] <- sum(rowSums((points_cluster - center_rep)^2))
                      }
                      
                      tot_within <- sum(inertie_intra)
                      totss <- sum(rowSums((private$X_scaled - colMeans(private$X_scaled))^2))
                      inertie_inter <- totss - tot_within
                      ratio <- inertie_inter / totss
                      
                      cat("Sommes des carrés intra-cluster :\n")
                      print(round(inertie_intra, 3))
                      cat("(between_SS / total_SS = ", round(ratio, 4), ")\n\n")
                      

                      # Composants disponibles (comme dans print.kmeans)
                      cat("Composants disponibles :\n")
                      print(c("cluster", "centers", "totss", "withinss",
                              "tot.withinss", "betweenss", "size", "iter", "ifault"))
                      cat("\n")
                      
                      
                      # Assignation détaillée des variables (ajout perso)
                      cat("Assignation des variables aux clusters :\n")
                      assignation <- data.frame(
                        variable = colnames(private$data),
                        cluster = unname(private$clusters)
                      )
                      print(assignation)
                      
                      invisible(self)
                    },
                    
                    #-------------
                    # Méthode summary 
                    #--------------
                    summary = function() {
                      if (!private$fitted) {
                        cat("Le modèle n'a pas encore été entraîné.\n")
                        return(invisible(self))
                      }
                      
                      # Nom de la méthode et nombre de clusters
                      cat("KMeans clustering\n")
                      cat(sprintf("Number of clusters: %d\n", private$k))
                      
                      # Taille des clusters
                      cluster_sizes <- as.numeric(table(private$clusters))
                      cat("Cluster sizes: ", paste(cluster_sizes, collapse = ", "), "\n")
                      
                      # Centres résumés (moyenne ± écart-type)
                      centers_summary <- data.frame(
                        cluster = 1:private$k,
                        mean = apply(private$centers, 1, mean),
                        sd = apply(private$centers, 1, sd)
                      )
                      cat("Centers (mean ± sd):\n")
                      for (j in 1:private$k) {
                        cat(sprintf("  Cluster %d: %.3f ± %.3f\n", 
                                    centers_summary$cluster[j],
                                    centers_summary$mean[j],
                                    centers_summary$sd[j]))
                      }
                      
                      # Inerties et proportion de variance expliquée
                      totss <- sum(rowSums((private$X_scaled - colMeans(private$X_scaled))^2))
                      tot_within <- sum(sapply(1:private$k, function(j){
                        pts <- private$X_scaled[private$clusters == j, , drop = FALSE]
                        center <- private$centers[j, , drop = FALSE]
                        center_rep <- center[rep(1, nrow(pts)), , drop = FALSE]
                        sum(rowSums((pts - center_rep)^2))
                      }))
                      
                      prop_var <- (totss - tot_within) / totss
                      
                      cat(sprintf("Total inertia: %.3f\n", totss))
                      cat(sprintf("Intra-cluster inertia: %.3f\n", tot_within))
                      cat(sprintf("Proportion of variance explained: %.4f\n", prop_var))
                      
                      invisible(self)
                    }
                    
                    
                  )
)