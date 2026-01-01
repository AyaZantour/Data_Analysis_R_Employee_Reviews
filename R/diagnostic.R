# ==============================================================================
# 🔍 SCRIPT DE DIAGNOSTIC - ANALYSE DES PROBLÈMES
# ==============================================================================

library(tidyverse)
library(here)

cat("🔍 DIAGNOSTIC DES DONNÉES GLASSDOOR\n")
cat("=" %R% 80, "\n\n")

# 1. CHARGEMENT DES DONNÉES ----
cat("1️⃣ CHARGEMENT DES DONNÉES\n")
cat("-" %R% 80, "\n")

data_file <- here("data", "raw", "employee_reviews.csv")

if(!file.exists(data_file)) {
  cat("❌ ERREUR: Fichier non trouvé:", data_file, "\n")
  cat("Chemins alternatifs à vérifier:\n")
  cat("  • ", here("employee_reviews.csv"), "\n")
  cat("  • ", here("data", "employee_reviews.csv"), "\n")
  stop("Veuillez placer le fichier au bon endroit")
}

# Charger les données SANS nettoyage
df_raw <- read_csv(data_file, show_col_types = FALSE)
cat("✅ Fichier chargé:", nrow(df_raw), "lignes,", ncol(df_raw), "colonnes\n\n")

# 2. INSPECTION DES COLONNES ----
cat("2️⃣ INSPECTION DES COLONNES BRUTES\n")
cat("-" %R% 80, "\n")

cat("Noms de colonnes AVANT nettoyage:\n")
print(names(df_raw))
cat("\n")

# Types de colonnes
cat("Types de colonnes:\n")
print(sapply(df_raw, class))
cat("\n")

# 3. APRÈS NETTOYAGE ----
cat("3️⃣ APRÈS NETTOYAGE (janitor::clean_names)\n")
cat("-" %R% 80, "\n")

df_clean <- df_raw %>% janitor::clean_names()

cat("Noms de colonnes APRÈS nettoyage:\n")
print(names(df_clean))
cat("\n")

# 4. CORRECTION DES FAUTES ----
cat("4️⃣ CORRECTION DES FAUTES DE FRAPPE\n")
cat("-" %R% 80, "\n")

# Identifier les colonnes avec fautes
faulty_cols <- names(df_clean)[grepl("carrer|mangemnet", names(df_clean))]

if(length(faulty_cols) > 0) {
  cat("⚠️  Colonnes avec fautes trouvées:\n")
  print(faulty_cols)
  cat("\n")
  
  # Appliquer corrections
  df_corrected <- df_clean %>%
    rename(
      career_opportunities_stars = if("carrer_opportunities_stars" %in% names(.)) carrer_opportunities_stars else career_opportunities_stars,
      senior_management_stars = if("senior_mangemnet_stars" %in% names(.)) senior_mangemnet_stars else senior_management_stars
    )
  
  cat("✅ Corrections appliquées:\n")
  cat("  • carrer_opportunities_stars → career_opportunities_stars\n")
  cat("  • senior_mangemnet_stars → senior_management_stars\n\n")
} else {
  df_corrected <- df_clean
  cat("✅ Aucune faute trouvée\n\n")
}

# 5. VÉRIFICATION DES COLONNES *_STARS ----
cat("5️⃣ VÉRIFICATION DES COLONNES DE NOTES (*_STARS)\n")
cat("-" %R% 80, "\n")

stars_cols <- names(df_corrected)[grepl("_stars$", names(df_corrected))]

if(length(stars_cols) > 0) {
  cat("Colonnes de notes trouvées:\n")
  for(col in stars_cols) {
    col_type <- class(df_corrected[[col]])[1]
    sample_vals <- head(df_corrected[[col]], 5)
    cat("\n📊", col, "\n")
    cat("  Type:", col_type, "\n")
    cat("  Échantillon:", paste(sample_vals, collapse = ", "), "\n")
    
    # Tester si numérique
    if(col_type == "character") {
      cat("  ⚠️  PROBLÈME: Type caractère au lieu de numérique!\n")
      cat("  💡 Solution: Convertir avec as.numeric(as.character(.))\n")
    } else if(col_type == "numeric" || col_type == "double") {
      cat("  ✅ Type correct\n")
    }
  }
  cat("\n")
} else {
  cat("❌ Aucune colonne *_stars trouvée!\n\n")
}

# 6. CONVERSION EN NUMÉRIQUE ----
cat("6️⃣ TEST DE CONVERSION EN NUMÉRIQUE\n")
cat("-" %R% 80, "\n")

df_final <- df_corrected %>%
  mutate(
    across(ends_with("_stars"), ~ as.numeric(as.character(.)))
  )

cat("Types APRÈS conversion:\n")
for(col in stars_cols) {
  cat("  •", col, ":", class(df_final[[col]])[1], "\n")
}
cat("\n")

# 7. STATISTIQUES SUR LES NOTES ----
cat("7️⃣ STATISTIQUES DESCRIPTIVES DES NOTES\n")
cat("-" %R% 80, "\n")

for(col in stars_cols) {
  if(is.numeric(df_final[[col]])) {
    stats <- df_final %>%
      summarise(
        moyenne = mean(.data[[col]], na.rm = TRUE),
        mediane = median(.data[[col]], na.rm = TRUE),
        na_count = sum(is.na(.data[[col]])),
        na_pct = mean(is.na(.data[[col]])) * 100
      )
    
    cat("\n", col, ":\n")
    cat("  Moyenne:", round(stats$moyenne, 2), "\n")
    cat("  Médiane:", stats$mediane, "\n")
    cat("  Valeurs manquantes:", stats$na_count, 
        "(", round(stats$na_pct, 1), "%)\n")
  }
}
cat("\n")

# 8. VÉRIFICATION overall_ratings ----
cat("8️⃣ VÉRIFICATION overall_ratings\n")
cat("-" %R% 80, "\n")

if("overall_ratings" %in% names(df_final)) {
  cat("Type:", class(df_final$overall_ratings)[1], "\n")
  cat("Distribution:\n")
  print(table(df_final$overall_ratings, useNA = "ifany"))
  cat("\n")
  cat("Moyenne:", round(mean(df_final$overall_ratings, na.rm = TRUE), 2), "\n")
} else {
  cat("❌ Colonne overall_ratings non trouvée!\n")
}
cat("\n")

# 9. RECOMMANDATIONS ----
cat("9️⃣ RECOMMANDATIONS\n")
cat("-" %R% 80, "\n\n")

issues_found <- FALSE

# Check 1: Colonnes manquantes
expected_cols <- c("overall_ratings", "work_balance_stars", "culture_values_stars",
                   "career_opportunities_stars", "comp_benefit_stars", 
                   "senior_management_stars")

missing_cols <- setdiff(expected_cols, names(df_final))
if(length(missing_cols) > 0) {
  cat("⚠️  Colonnes manquantes:\n")
  cat("  ", paste(missing_cols, collapse = ", "), "\n\n")
  issues_found <- TRUE
}

# Check 2: Types non-numériques
non_numeric <- stars_cols[!sapply(df_final[stars_cols], is.numeric)]
if(length(non_numeric) > 0) {
  cat("⚠️  Colonnes non-numériques:\n")
  cat("  ", paste(non_numeric, collapse = ", "), "\n")
  cat("  💡 Ajoutez cette ligne après clean_names():\n")
  cat('     mutate(across(ends_with("_stars"), ~ as.numeric(as.character(.))))\n\n')
  issues_found <- TRUE
}

# Check 3: Trop de valeurs manquantes
high_na_cols <- stars_cols[sapply(df_final[stars_cols], function(x) mean(is.na(x)) > 0.5)]
if(length(high_na_cols) > 0) {
  cat("⚠️  Colonnes avec >50% de valeurs manquantes:\n")
  cat("  ", paste(high_na_cols, collapse = ", "), "\n\n")
}

if(!issues_found) {
  cat("✅ Aucun problème détecté! Les données sont prêtes pour l'analyse.\n\n")
}

# 10. CODE CORRIGÉ SUGGÉRÉ ----
cat("🔧 CODE CORRIGÉ À UTILISER\n")
cat("-" %R% 80, "\n\n")

cat('# Charger les données\n')
cat('df <- read_csv(here("data", "raw", "employee_reviews.csv"), show_col_types = FALSE)\n\n')

cat('# Nettoyer les noms\n')
cat('df <- df %>% janitor::clean_names()\n\n')

if(length(faulty_cols) > 0) {
  cat('# Corriger les fautes de frappe\n')
  cat('df <- df %>%\n')
  cat('  rename(\n')
  cat('    career_opportunities_stars = carrer_opportunities_stars,\n')
  cat('    senior_management_stars = senior_mangemnet_stars\n')
  cat('  )\n\n')
}

cat('# IMPORTANT: Convertir en numérique\n')
cat('df <- df %>%\n')
cat('  mutate(\n')
cat('    across(ends_with("_stars"), ~ as.numeric(as.character(.)))\n')
cat('  )\n\n')

cat('# Extraire l\'année\n')
cat('df <- df %>%\n')
cat('  mutate(\n')
cat('    review_year = case_when(\n')
cat('      str_detect(dates, "\\\\d{4}") ~ as.numeric(str_extract(dates, "\\\\d{4}")),\n')
cat('      TRUE ~ NA_real_\n')
cat('    )\n')
cat('  )\n\n')

cat("=" %R% 80, "\n")
cat("✅ DIAGNOSTIC TERMINÉ!\n")
cat("=" %R% 80, "\n")