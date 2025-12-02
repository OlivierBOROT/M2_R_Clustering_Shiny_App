# 📊 M2RClust - Advanced Variable Clustering in R

<div align="center">

![R Version](https://img.shields.io/badge/R-%E2%89%A54.0.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Status](https://img.shields.io/badge/version-0.0.0.9000-orange)

*A comprehensive R package for clustering variables using hierarchical and partitioning methods, complete with a Shiny interface.*

[Installation](#-installation) • [Features](#-features) • [Algorithms](#-algorithms) • [Shiny App](#-interactive-shiny-app) • [Documentation](#-documentation)

</div>

---

## 📖 Table of Contents

- [📊 M2RClust - Advanced Variable Clustering in R](#-m2rclust---advanced-variable-clustering-in-r)
  - [📖 Table of Contents](#-table-of-contents)
  - [🎯 Overview](#-overview)
  - [✨ Features](#-features)
    - [🔧 Core Functionality](#-core-functionality)
    - [📊 Visualization Tools](#-visualization-tools)
  - [📦 Installation](#-installation)
    - [Dependencies](#dependencies)
  - [🧮 Algorithms](#-algorithms)
    - [1️⃣ K-means Variable Clustering (`KMeansClusterer`)](#1️⃣-k-means-variable-clustering-kmeansclusterer)
      - [📌 Key Characteristics](#-key-characteristics)
      - [📝 Basic Example](#-basic-example)
    - [2️⃣ Modalities Clustering (`ModalitiesDiceClusterer`)](#2️⃣-modalities-clustering-modalitiesdiceclusterer)
      - [📌 Key Characteristics](#-key-characteristics-1)
      - [📝 Basic Example](#-basic-example-1)
    - [3️⃣ Divisive Clustering (`DivisiveClusterer`)](#3️⃣-divisive-clustering-divisiveclusterer)
      - [📌 Key Characteristics](#-key-characteristics-2)
      - [📝 Basic Example](#-basic-example-2)
  - [🎮 Interactive Shiny App](#-interactive-shiny-app)
    - [Launching the App](#launching-the-app)
    - [App Features](#app-features)
  - [📚 Documentation](#-documentation)
  - [🛠️ Development](#️-development)
    - [Project Structure](#project-structure)
  - [👥 Authors](#-authors)
  - [📄 License](#-license)

---

## 🎯 Overview

**M2RClust** is an R package developed for the **Clustering of Variables**. Unlike traditional clustering which groups observations, this package focuses on grouping variables to identify redundant information, reduce dimensionality, or explore relationships between features.

It supports:
- ✅ **Quantitative variables** (continuous data)
- ✅ **Qualitative variables** (categorical data)
- ✅ **Mixed datasets** (quantitative + qualitative)

This project was developed as part of the **Master 2 SISE** (Statistique et Informatique pour la Science des Données) at **Université Lumière Lyon 2**.

---

## ✨ Features

### 🔧 Core Functionality

| Feature | KMeansClusterer | ModalitiesDiceClusterer | DivisiveClusterer |
|---------|:---------------:|:-----------------------:|:-----------------:|
| **Method** | Partitioning (K-means style) | Hierarchical (Agglomerative) | Hierarchical (Divisive) |
| **Metric** | Homogeneity (Latent Component) | Dice / Cramér's V | PCA/PCAmix Eigenvalues |
| **Visualization** | 2D Projections, Elbow | Dendrograms, MCA | Dendrograms, Scree Plots |

### 📊 Visualization Tools

- 🌳 **Dendrograms** for hierarchical structures
- 📉 **Elbow & Scree plots** for optimal K selection
- 🗺️ **2D Projections** (PCA/MCA based)
- 🎨 **Correlation Heatmaps**
- 📈 **Contribution plots** to see key variables per cluster

---

## 📦 Installation

You can install the development version of M2RClust from GitHub:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install M2RClust
devtools::install_github("OlivierBOROT/M2_R_Clustering_Shiny_App")

# using the remotes package
if (!require("remotes")) install.packages("remotes")

# Install M2RClust
remotes::install_github("OlivierBOROT/M2_R_Clustering_Shiny_App")
```

### Dependencies

The package relies on several robust R packages:
- **Core**: `R6`, `FactoMineR`, `PCAmixdata`, `cluster`
- **Interface**: `shiny`, `DT`, `bslib`, `bsicons`
- **Visualization**: `ggplot2`, `plotly`, `igraph`

---

## 🧮 Algorithms

### 1️⃣ K-means Variable Clustering (`KMeansClusterer`)

**Partitioning algorithm** that maximizes within-cluster homogeneity.

#### 📌 Key Characteristics
- **Homogeneity Measure**: Proportion of variance explained by the cluster's first latent component.
- **Data Types**: Handles numeric, factor, and mixed data.
- **Initialization**: Supports "homogeneity++", "correlation", and "random".

#### 📝 Basic Example
```r
library(M2RClust)

# Initialize and fit
km <- KMeansClusterer$new(data = my_data, n_clusters = 3)
km$fit()

# Results
km$print()
km$plot_clustering_2d()
```

### 2️⃣ Modalities Clustering (`ModalitiesDiceClusterer`)

**Hierarchical clustering of modalities** using Dice or Cramér's V distance.

#### 📌 Key Characteristics
- **Focus**: Clusters *modalities* (categories) rather than variables.
- **Distance**: Dice coefficient (default) on disjunctive tables.
- **Automatic Discretization**: Can automatically bin numeric variables.

#### 📝 Basic Example
```r
# Initialize and fit
mod_clust <- ModalitiesDiceClusterer$new(n_groups = 4, auto_discretize = TRUE)
mod_clust$fit(my_data)

# Visualize
mod_clust$plot_dendrogram()
```

### 3️⃣ Divisive Clustering (`DivisiveClusterer`)

**Top-down hierarchical clustering** (VARCLUS-style) with a hybrid architecture.

#### 📌 Key Characteristics
- **Hybrid Engine**: Uses fast `eigen(cor(X))` for numeric data and `PCAmix` for mixed data.
- **Splitting Criterion**: Splits the most heterogeneous cluster based on the second eigenvalue.
- **Rotation**: Applies Varimax rotation to refine splits.

#### 📝 Basic Example
```r
# Initialize and fit
div <- DivisiveClusterer$new(data = my_data, n_clusters = 5)
div$fit()

# Visualize
div$plot_dendrogram()
```

---

## 🎮 Interactive Shiny App

The package includes a comprehensive **Shiny Application** to perform analyses without writing code.

### Launching the App
```r
library(M2RClust)
run_app()
```

### App Features
- 📁 **Data Import**: Upload CSV/Excel files easily.
- ⚙️ **Configuration**: Select active/illustrative variables.
- 🚀 **Run Algorithms**: Choose between K-means, Modalities, or Divisive clustering.
- 📊 **Interactive Plots**: Zoom, pan, and export visualizations.
- 📑 **Reports**: View detailed summaries and statistics.

---

## 📚 Documentation

To view the help for specific classes or functions:

```r
?KMeansClusterer
?ModalitiesDiceClusterer
?DivisiveClusterer
```
you have also access to vignettes to help you get started:

```r
browseVignettes("M2RClust")
```

---

## 🛠️ Development

### Project Structure
```
M2_R_Clustering_Shiny_App/
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions
├── R/                          # Core R6 Classes & Functions
│   ├── 00_utils.R              # Utility functions
│   ├── 01_base_clusterer.R     # Abstract base class
│   ├── 02_kmeans_clusterer.R   # K-means variable clustering
│   ├── 03_mca_hclust_cluster.R # Modalities clustering
│   ├── 04_PDDP_clusterer.R     # Divisive clustering
│   ├── 05_cluster_validator.R  # Validation metrics
│   ├── 06_visualization.R      # Plotting functions
│   └── run_app.R               # App launcher
├── inst/
│   └── shinyR/                 # Shiny Application
│       ├── app.R
│       ├── global.R
│       ├── server.R
│       ├── ui.R
│       ├── server/             # Server modules
│       │   ├── cluster_server.R
│       │   ├── home_server.R
│       │   └── upload_server.R
│       ├── ui/                 # UI modules
│       │   ├── cluster.R
│       │   ├── home.R
│       │   └── upload.R
│       └── texts/              # Content & Translations
│           ├── dictionnary.csv
│           └── markdowns/
├── man/                        # Documentation (Rd files)
│   ├── BaseClusterer.Rd
│   ├── KMeansClusterer.Rd
│   ├── ModalitiesDiceClusterer-class.Rd
│   ├── DivisiveClusterer.Rd
│   └── ... (and 20+ other function docs)
├── tests/                      # Unit Tests
│   ├── testthat.R
│   └── testthat/
│       ├── test-01_base_clusterer.R
│       ├── test-02_kmeans_clusterer.R
│       ├── test-03_mca_hclust_cluster.R
│       ├── test-04_PDDP_clusterer.R
│       └── ...
├── vignettes/                  # Tutorials
│   ├── divisive-clusterer.Rmd
│   ├── kmeans-clusterer.Rmd
│   └── ModalitiesDiceClusterer.Rmd
└── README.md
```

---

## 👥 Authors

**M2 SISE 2024-2025 Team:**

- **Léo-Paul Knoepffler** ([@lp-knoepffler](https://github.com/lp-knoepffler))
- **Olivier Borot** ([@OlivierBOROT](https://github.com/OlivierBOROT))
- **Perrine Ibouroi** ([@PerrineIbouroi](https://github.com/perrineib))

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
