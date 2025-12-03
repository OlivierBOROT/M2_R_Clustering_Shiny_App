# M2RClust 📊

**Package R de clustering de variables avec interface Shiny interactive**

[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-green.svg)](https://shiny.rstudio.com/)

## 📖 Table des Matières

- [M2RClust 📊](#m2rclust-)
  - [📖 Table des Matières](#-table-des-matières)
  - [🎯 Description](#-description)
  - [✨ Fonctionnalités](#-fonctionnalités)
    - [Algorithmes de Clustering](#algorithmes-de-clustering)
  - [📦 Installation](#-installation)
    - [Depuis GitHub](#depuis-github)
    - [Avec les vignettes](#avec-les-vignettes)
    - [Installation locale](#installation-locale)
  - [🎮 Application Shiny](#-application-shiny)
    - [Lancer l'application](#lancer-lapplication)
      - [🖥️ Fonctionnalités de l'application Shiny](#️-fonctionnalités-de-lapplication-shiny)
    - [Captures d'écran](#captures-décran)
      - [1. Import des Données](#1-import-des-données)
      - [2. Configuration du Clustering](#2-configuration-du-clustering)
      - [3. Visualisation \& Résultats](#3-visualisation--résultats)
  - [🚀 Exemples d'utilisation (R)](#-exemples-dutilisation-r)
    - [Clustering de Variables (KMeansClusterer / DivisiveClusterer)](#clustering-de-variables-kmeansclusterer--divisiveclusterer)
    - [Clustering de Modalités (ModalitiesDiceClusterer)](#clustering-de-modalités-modalitiesdiceclusterer)
  - [📚 Documentation](#-documentation)
  - [👥 Contributeurs](#-contributeurs)
  - [📄 Licence](#-licence)

## 🎯 Description

M2RClust est un package R dédié au **clustering de variables** (et non d'observations). Il permet de regrouper des variables selon leur structure de corrélation, avec support des données mixtes (numériques et catégorielles).

## ✨ Fonctionnalités

### Algorithmes de Clustering

Le package propose **3 approches complémentaires** :

| Algorithme | Objet clusterisé | Approche | Cas d'usage |
|------------|------------------|----------|-------------|
| `KMeansClusterer` | **Variables** | Partitionnement itératif | Regrouper des variables corrélées |
| `DivisiveClusterer` | **Variables** | Divisif hiérarchique (PDDP) | Hiérarchie interprétable de variables |
| `ModalitiesDiceClusterer` | **Modalités** | Hiérarchique (Dice/Cramér) | Regrouper des niveaux de facteurs |

> ⚠️ **Note importante** : `ModalitiesDiceClusterer` cluster les **modalités** (niveaux de variables catégorielles), pas les variables elles-mêmes.

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

### Avec les vignettes

Pour installer le package avec la documentation complète :

```r
devtools::install_github("OlivierBOROT/M2_R_Clustering_Shiny_App", 
                         build_vignettes = TRUE,
                         dependencies = TRUE)
```

### Installation locale

Si vous disposez du fichier source `.tar.gz` :

```r
install.packages("chemin/vers/M2RClust_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

## 🎮 Application Shiny

### Lancer l'application

```r
library(M2RClust)
run_app()
```

#### 🖥️ Fonctionnalités de l'application Shiny

L'application Shiny offre une interface interactive complète et bilingue :

- 📁 **Import des données** : Chargement facile de fichiers CSV et Excel.
- ⚙️ **Configuration** : Interface intuitive pour sélectionner les variables actives et illustratives.
- 🚀 **Algorithmes** : Exécution paramétrable des algorithmes (KMeans, Divisif, Modalités).
  - *KMeans* : choix du nombre de clusters, standardisation, seed...
  - *PDDP (Divisif)* : critères d'arrêt, nombre max de clusters...
  - *Modalités* : mesure de dissimilarité (Dice/Cramér), méthode de liaison...
- 📊 **Visualisations** : Dendrogrammes, cercles de corrélation, heatmaps, graphes de réseau interactifs (zoom, pan).
- 📑 **Rapports** : Visualisation de résumés détaillés et statistiques.
- 🔄 **Support Multi-langue** : Interface disponible en Français et Anglais.
- 🎨 **Thèmes** : Plusieurs thèmes graphiques disponibles.
- 💾 **Export** : Téléchargement des résultats de clustering.

### Captures d'écran

#### 1. Import des Données
![Interface d'Import](images/upload.png)

#### 2. Configuration du Clustering
![Interface de Clustering](images/clustering.png)

#### 3. Visualisation & Résultats
![Vue des Résultats](images/results.png)

---

## 🚀 Exemples d'utilisation (R)

### Clustering de Variables (KMeansClusterer / DivisiveClusterer)

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

### Clustering de Modalités (ModalitiesDiceClusterer)

```r
library(M2RClust)

# Données catégorielles (ou mixtes avec auto_discretize = TRUE)
df <- data.frame(
  couleur = factor(c("rouge", "bleu", "rouge", "vert", "bleu")),
  taille = factor(c("petit", "grand", "moyen", "petit", "grand")),
  prix = c(10, 25, 15, 8, 30)  # sera discrétisé automatiquement
)

# Créer le clusterer de modalités
clusterer <- ModalitiesDiceClusterer$new(
  n_groups = 3,
  dissimilarity = "dice",
  auto_discretize = TRUE
)

# Ajuster aux données
clusterer$fit(df)

# Voir les groupes de modalités
clusterer$get_cluster_table()

# Visualiser en MCA
clusterer$plot_mca(add_ellipses = TRUE)
```

## 📚 Documentation

Des vignettes détaillées sont disponibles :

- `vignette("kmeans-clusterer")` - Guide du KMeansClusterer
- `vignette("divisive-clusterer")` - Guide du DivisiveClusterer
- `vignette("modalities-clusterer")` - Guide du ModalitiesDiceClusterer

## 👥 Contributeurs

Projet développé dans le cadre du cours de Programmation R (Master 2 SISE), Université Lumière Lyon 2.

| Contributeur | Contact |
|--------------|---------|
| **Olivier BOROT** | olivier.dominique.borot@gmail.com |
| **Perrine IBOUROI** | perrine.ibouroi@hotmail.fr |
| **Léo-Paul KNOEPFFLER** | l.knoepffler@free.fr |

## 📄 Licence

Ce projet est sous licence MIT - voir le fichier [LICENSE](LICENSE) pour plus de détails.
