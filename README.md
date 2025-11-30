# M2RClust 📊

**Package R de clustering de variables avec interface Shiny interactive**

[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-green.svg)](https://shiny.rstudio.com/)

## 🎯 Description

M2RClust est un package R dédié au **clustering de variables** (et non d'observations). Il permet de regrouper des variables selon leur structure de corrélation, avec support des données mixtes (numériques et catégorielles).

## ✨ Fonctionnalités

- **Trois algorithmes de clustering** :
  - `KMeansClusterer` : Clustering par maximisation de l'homogénéité intra-cluster
  - `DivisiveClusterer` : Clustering divisif hiérarchique (PDDP)
  - `modalitiesDiceClusterer` : Clustering des modalités (MCA_Hclusterer)

- **Support des données mixtes** : Variables numériques et catégorielles (via PCAmix)

- **Méthodes de sélection du nombre optimal de clusters** :
  - Méthode du coude (Elbow)
  - Silhouette
  - Calinski-Harabasz

- **Visualisations riches** :
  - Cercle des corrélations (PCA)
  - Dendrogramme
  - Heatmap de corrélation
  - Graphe de réseau
  - Contributions des variables

- **Application Shiny interactive** pour une utilisation sans code

## 📦 Installation

### Depuis GitHub

```r
# Installer devtools si nécessaire
install.packages("devtools")

# Installer M2RClust
devtools::install_github("OlivierBOROT/M2_R_Clustering_Shiny_App")
```

### Depuis un fichier source

```r
install.packages("chemin/vers/M2RClust_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

## 🚀 Utilisation rapide

### Lancer l'application Shiny

```r
library(M2RClust)
run_clustering_app()
```

### Utilisation programmatique

```r
library(M2RClust)

# Charger des données
data(iris)
df <- iris[, 1:4]

# Créer et ajuster un clusterer
clusterer <- KMeansClusterer$new(
  data = df,
  n_clusters = 2,
  standardize = TRUE
)
clusterer$fit()

# Voir les résultats
clusterer$summary()

# Visualiser
plot_clustering_2d(clusterer)
```

## 📚 Documentation

Des vignettes détaillées sont disponibles :

- `vignette("kmeans-clusterer")` - Guide du KMeansClusterer
- `vignette("divisive-clusterer")` - Guide du DivisiveClusterer

## 👥 Contributeurs

Projet développé dans le cadre du cours de Programmation R (Master 2 SISE), Université Lumière Lyon 2.

| Contributeur | Contact |
|--------------|---------|
| **Olivier BOROT** | olivier.dominique.borot@gmail.com |
| **Perrine IBOUROI** | perrine.ibouroi@hotmail.fr |
| **Léo-Paul KNOEPFFLER** | l.knoepffler@free.fr |

## 📄 Licence

Ce projet est sous licence MIT - voir le fichier [LICENSE](LICENSE) pour plus de détails.
