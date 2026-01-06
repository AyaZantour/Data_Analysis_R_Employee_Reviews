# 📊 Analyse Glassdoor - Guide Complet

## 🎯 Objectif du Projet

Analyse approfondie de **67,529 avis** d'employés sur 6 géants de la tech (Amazon, Microsoft, Apple, Google, Facebook, Netflix) pour identifier les facteurs clés de satisfaction et produire des insights business actionnables.

---

## 📁 Structure du Projet

```
R_Project/
├──data
|   └── raw
|       └── employee_reviews.csv  # Dataset principal
├── outputs
│   ├── *.csv                     # Résultats exportés
│   └── *.rds                     # Objets R sauvegardés
├── R
|   ├── analyse_complete.R        # Script d'analyse principal
|   └── visualisations.R          # Script de génération des graphiques
├── index.qmd         # Template Quarto
├── resultats_analyse/            # Dossier créé automatiquement
│   └── *.png                     # Visualisations
├── quarto_presentation.qmd
└── README.md                     # Ce fichier
```

---

## 🚀 Installation & Setup

### Prérequis

```r
# R version 4.0 ou supérieure
R.version.string

# Installer les packages nécessaires
packages <- c(
  "tidyverse",      # Manipulation de données
  "lubridate",      # Dates
  "scales",         # Formatage
  "patchwork",      # Combinaison de graphiques
  "tidytext",       # Analyse textuelle
  "wordcloud",      # Nuages de mots
  "RColorBrewer",   # Palettes de couleurs
  "viridis",        # Couleurs scientifiques
  "ggthemes",       # Thèmes ggplot
  "fmsb",           # Radar charts
  "DT",             # Tables interactives
  "knitr",          # Rapports
  "kableExtra",     # Tables élégantes
  "quarto"          # Système de publication
)

install.packages(packages)
```

### Vérification Quarto

```bash
# Dans le terminal
quarto --version

# Si non installé, télécharger depuis:
# https://quarto.org/docs/get-started/
```

---

## 📖 Utilisation

### Option 1: Analyse Complète (Recommandé)

```r
# 1. Placer employee_reviews.csv dans le dossier de travail
setwd("chemin/vers/votre/projet")

# 2. Exécuter l'analyse complète
source("analyse_complete.R")

# 3. Générer toutes les visualisations
source("visualisations.R")

# 4. Compiler le rapport Quarto
quarto::quarto_render("index.qmd")
```

Résultat: Un fichier HTML interactif `index.html` sera créé.

---

### Option 2: Analyse Par Étapes

#### Étape 1: Charger et Explorer les Données

```r
library(tidyverse)

# Charger les données
df <- read_csv("employee_reviews.csv") %>%
  janitor::clean_names()

# Vue d'ensemble
glimpse(df)
str(df)
summary(df)

# Dimensions du dataset
cat("Lignes:", nrow(df), "| Colonnes:", ncol(df))
```

#### Étape 2: Statistiques Descriptives

```r
# Notes moyennes par entreprise
company_stats <- df %>%
  group_by(company) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    note_mediane = median(overall_ratings, na.rm = TRUE),
    ecart_type = sd(overall_ratings, na.rm = TRUE)
  ) %>%
  arrange(desc(note_moyenne))

print(company_stats)
```

#### Étape 3: Visualisations Clés

```r
library(ggplot2)

# Distribution des notes
df %>%
  ggplot(aes(x = overall_ratings)) +
  geom_histogram(binwidth = 0.5, fill = "#3498db", alpha = 0.8) +
  labs(title = "Distribution des Notes", x = "Note", y = "Fréquence")

# Comparaison entreprises
company_stats %>%
  ggplot(aes(x = reorder(company, note_moyenne), y = note_moyenne)) +
  geom_col(fill = "#2ecc71") +
  coord_flip() +
  labs(title = "Classement des Entreprises", x = NULL, y = "Note Moyenne")
```

---

### Option 3: Rapport Quarto Personnalisé

#### Créer votre propre rapport

```r
# Créer un nouveau fichier Quarto
file.create("mon_analyse.qmd")
```

Structure minimale:

```yaml
---
title: "Mon Analyse Glassdoor"
author: "Votre Nom"
date: today
format:
  html:
    theme: cosmo
    toc: true
    code-fold: true
---

# Introduction

Votre texte ici...

```{r}
# Votre code R ici
library(tidyverse)
df <- read_csv("employee_reviews.csv")
summary(df)
```

# Analyse

Plus de contenu...
```

Compiler avec:
```r
quarto::quarto_render("mon_analyse.qmd")
```

---

## 🎨 Personnalisation

### Changer les Couleurs

```r
# Dans visualisations.R, modifier:
company_colors <- c(
  "amazon" = "#FF9900",      # Orange Amazon
  "microsoft" = "#00A4EF",   # Bleu Microsoft
  "apple" = "#000000",       # Noir Apple (ou #A2AAAD)
  "google" = "#4285F4",      # Bleu Google
  "facebook" = "#1877F2"     # Bleu Facebook
)
```

### Modifier le Thème Quarto

Dans `index.qmd`, changer:

```yaml
format:
  html:
    theme: [cosmo, journal, flatly, darkly, etc.]
    # Liste complète: https://quarto.org/docs/output-formats/html-themes.html
```

### Ajouter vos Propres Analyses

```r
# Dans analyse_complete.R, ajouter une section:

# === VOTRE ANALYSE PERSONNALISÉE ===
cat("\n=== MA NOUVELLE ANALYSE ===\n")

# Par exemple: Analyse par localisation
location_stats <- df %>%
  filter(location != "none", !is.na(location)) %>%
  group_by(location) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE)
  ) %>%
  filter(n_avis >= 50) %>%
  arrange(desc(note_moyenne))

print(location_stats)
```

---

## 📊 Résultats Attendus

### Fichiers CSV Exportés

1. **statistiques_entreprises.csv**
   - Note moyenne, médiane, écart-type par entreprise
   - Scores par dimension (work-life, culture, etc.)

2. **statistiques_dimensions.csv**
   - Classement global des 6 dimensions
   - Statistiques descriptives

3. **tendances_annuelles.csv**
   - Évolution des notes année par année
   - Volume d'avis par période

4. **top_mots_positifs.csv** / **top_mots_negatifs.csv**
   - Analyse textuelle des avis
   - Mots-clés les plus fréquents

### Visualisations PNG (10+ graphiques)

- Distribution des notes
- Classement des entreprises
- Heatmap multi-dimensionnelle
- Radar charts par entreprise
- Boxplots comparatifs
- Évolution temporelle
- Violin plots
- Matrice de corrélation
- Dashboard récapitulatif

### Rapport HTML Final

Un document interactif avec:
- Tableaux interactifs (triables, filtrables)
- Graphiques haute résolution
- Analyses statistiques
- Insights business
- Code source (masquable)

---

## 🔧 Troubleshooting

### Problème: "Cannot find employee_reviews.csv"

**Solution:**
```r
# Vérifier votre répertoire de travail
getwd()

# Changer si nécessaire
setwd("chemin/correct")

# Ou utiliser un chemin absolu
df <- read_csv("C:/Users/VotreNom/Documents/employee_reviews.csv")
```

### Problème: Packages manquants

**Solution:**
```r
# Installer TOUS les packages d'un coup
packages_needed <- c("tidyverse", "scales", "patchwork", "tidytext", 
                     "wordcloud", "RColorBrewer", "viridis", "ggthemes",
                     "fmsb", "DT", "knitr", "kableExtra")

install.packages(packages_needed, dependencies = TRUE)
```

### Problème: Mémoire insuffisante

**Solution 1: Échantillonner**
```r
# Travailler sur un échantillon
df_sample <- df %>% sample_frac(0.2)  # 20% des données
```

**Solution 2: Augmenter la mémoire**
```r
# Au début du script
options(java.parameters = "-Xmx8g")  # 8 GB de RAM
```

### Problème: Quarto ne compile pas

**Solution:**
```bash
# Vérifier l'installation
quarto check

# Mettre à jour Quarto
# Télécharger la dernière version depuis quarto.org

# Compiler avec options de debug
quarto render index.qmd --verbose
```

---

## 💡 Conseils & Bonnes Pratiques

### 1. Organisation

- **Un dossier par projet** avec tous les fichiers nécessaires
- **Nommer clairement** vos fichiers (dates, versions)
- **Commenter votre code** pour vous y retrouver plus tard

### 2. Performance

- **Échantillonner** pour les tests (5000-10000 lignes suffisent)
- **Cacher les résultats** dans Quarto (`cache: true`)
- **Filtrer tôt** les données inutiles

### 3. Présentation

- **Commencer par l'essentiel**: résumé exécutif en premier
- **Une idée par graphique**: pas de surcharge visuelle
- **Contextualisez**: expliquez ce que montrent vos analyses

### 4. Rigueur Scientifique

- **Documenter vos choix** méthodologiques
- **Tester la robustesse** de vos conclusions
- **Mentionner les limites** de votre analyse

---

## 📚 Ressources Complémentaires

### Documentation

- [Tidyverse](https://www.tidyverse.org/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [Quarto](https://quarto.org/docs/guide/)
- [R for Data Science](https://r4ds.hadley.nz/)

### Tutoriels Vidéo

- [Introduction à R et Tidyverse](https://www.youtube.com/watch?v=_V8eKsto3Ug)
- [Data Visualization avec ggplot2](https://www.youtube.com/watch?v=h29g21z0a68)
- [Créer des rapports avec Quarto](https://www.youtube.com/watch?v=yvi5uXQMvu4)

### Datasets Similaires

- [Indeed Job Reviews](https://www.kaggle.com/datasets/indeed/indeed-job-reviews)
- [Amazon Employee Access](https://www.kaggle.com/datasets/amazon-employee-access-challenge)

---

## ✅ Checklist Avant Présentation

- [ ] Dataset correctement chargé
- [ ] Toutes les analyses exécutées sans erreur
- [ ] Visualisations claires et lisibles
- [ ] Rapport HTML compilé et fonctionnel
- [ ] Conclusions claires et justifiées
- [ ] Code commenté et organisé
- [ ] Présentation répétée (timing vérifié)
- [ ] Questions anticipées préparées

---

## 🎓 Pour Votre Présentation

### Structure Suggérée (15-20 min)

1. **Introduction** (2 min)
   - Contexte et objectifs
   - Présentation du dataset

2. **Méthodologie** (2 min)
   - Approche analytique
   - Outils utilisés (R, tidyverse, Quarto)

3. **Résultats Principaux** (8-10 min)
   - Top 3 insights
   - Visualisations clés
   - Comparaisons inter-entreprises

4. **Recommandations** (3 min)
   - Insights business
   - Actions concrètes

5. **Limites & Perspectives** (2 min)
   - Ce qui pourrait être amélioré
   - Analyses futures possibles

6. **Questions** (5 min)

### Slides Essentiels

1. Slide de titre
2. Vue d'ensemble du dataset (1 chiffre, 1 graphique)
3. Classement des entreprises (TOP insight)
4. Heatmap des dimensions (vision globale)
5. Évolution temporelle (tendances)
6. Analyse textuelle (mots-clés)
7. Recommandations (3 points max)
8. Conclusion & questions

---

## 📧 Support

Pour toute question:
- **Email**: eyazantour926@gmail.com
- **GitHub Issues**: https://github.com/AyaZantour/Data_Analysis_R_Employee_Reviews
- **Documentation R**: `?function_name` dans la console

---

## 📄 Licence

Ce projet est sous licence MIT. Vous êtes libre de l'utiliser et de le modifier pour vos propres projets éducatifs.

---

**Bon courage pour votre analyse! 🚀**
