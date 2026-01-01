# ==============================================================================
# 📦 SETUP - INSTALLATION DES PACKAGES NÉCESSAIRES
# À exécuter UNE SEULE FOIS avant l'analyse
# ==============================================================================

cat("🚀 Installation des packages nécessaires...\n\n")

# Liste complète des packages requis
packages_needed <- c(
  # Manipulation de données
  "tidyverse",      # Collection complète (dplyr, ggplot2, tidyr, etc.)
  "lubridate",      # Manipulation de dates
  "janitor",        # Nettoyage de noms de colonnes
  "here",           # Gestion des chemins de fichiers
  
  # Visualisation
  "scales",         # Formatage des axes et labels
  "patchwork",      # Combinaison de graphiques
  "viridis",        # Palettes de couleurs scientifiques
  "ggthemes",       # Thèmes supplémentaires pour ggplot
  "RColorBrewer",   # Palettes de couleurs
  "fmsb",           # Radar charts
  
  # Analyse textuelle
  "tidytext",       # Text mining avec tidyverse
  "wordcloud",      # Nuages de mots
  
  # Tables et rapports
  "DT",             # Tables interactives HTML
  "knitr",          # Génération de rapports
  "kableExtra",     # Tables élégantes
  
  # Autres
  "reshape2"        # Manipulation de données (pour corrélation)
)

# Fonction pour installer uniquement les packages manquants
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("📥 Installation de", length(new_packages), "nouveaux packages:\n")
    cat("   ", paste(new_packages, collapse = ", "), "\n\n")
    
    install.packages(new_packages, dependencies = TRUE, quiet = FALSE)
    
    cat("\n✅ Installation terminée!\n")
  } else {
    cat("✅ Tous les packages sont déjà installés!\n")
  }
}

# Installer les packages manquants
install_if_missing(packages_needed)

# Vérification
cat("\n📋 Vérification des installations:\n")
for(pkg in packages_needed) {
  if(requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✅", pkg, "\n")
  } else {
    cat("  ❌", pkg, "- ÉCHEC\n")
  }
}

# Test de chargement
cat("\n🔍 Test de chargement des packages principaux...\n")
test_packages <- c("tidyverse", "here", "scales", "patchwork")

for(pkg in test_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("  ✅", pkg, "chargé avec succès\n")
  }, error = function(e) {
    cat("  ❌", pkg, "- Erreur:", e$message, "\n")
  })
}

# Créer la structure de dossiers
cat("\n📁 Création de la structure de dossiers...\n")

dirs_to_create <- c(
  "data/raw",
  "data/processed",
  "outputs/tables",
  "outputs/figures",
  "outputs/reports",
  "R"
)

for(dir in dirs_to_create) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cat("  ✅ Créé:", dir, "\n")
  } else {
    cat("  ℹ️  Existe déjà:", dir, "\n")
  }
}

# Vérifier la présence du fichier de données
cat("\n📊 Vérification du fichier de données...\n")
data_file <- "data/raw/employee_reviews.csv"

if(file.exists(data_file)) {
  file_size <- file.size(data_file) / (1024^2)  # Taille en Mo
  cat("  ✅ Fichier trouvé:", data_file, "\n")
  cat("  📏 Taille:", round(file_size, 1), "Mo\n")
} else {
  cat("  ⚠️  Fichier non trouvé:", data_file, "\n")
  cat("  ℹ️  Veuillez placer employee_reviews.csv dans le dossier data/raw/\n")
}

# Résumé final
# cat("\n" %R% 80, "\n")
cat("✅ SETUP TERMINÉ!\n")
# cat("=" %R% 80, "\n\n")

cat("📋 Prochaines étapes:\n")
cat("  1. Vérifiez que employee_reviews.csv est dans data/raw/\n")
cat("  2. Exécutez: source('R/analyse_complete.R')\n")
cat("  3. Exécutez: source('R/visualisations.R')\n")
cat("  4. Générez le rapport Quarto\n\n")

cat("💡 Commandes rapides:\n")
cat("  • Analyse complète: source('R/analyse_complete.R')\n")
cat("  • Visualisations: source('R/visualisations.R')\n")
cat("  • Voir les résultats: list.files('outputs/tables')\n")
cat("  • Voir les graphiques: list.files('outputs/figures')\n\n")