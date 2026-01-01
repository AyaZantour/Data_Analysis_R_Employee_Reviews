
# ==============================================================================
# 📊 ANALYSE APPROFONDIE DES AVIS GLASSDOOR - VERSION FINALE CORRIGÉE
# Dataset: employee_reviews.csv (67,529 avis)
# ==============================================================================

# 🔧 SETUP ET CHARGEMENT ----
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(here)

# Fonction pour créer des lignes de séparation
cat_line <- function(char = "=", length = 80) {
  cat(paste(rep(char, length), collapse = ""), "\n")
}

# Charger les données
df <- read_csv(here("data", "raw", "employee_reviews.csv"))

# Nettoyer les noms de colonnes
df <- df %>% janitor::clean_names()

# CORRECTION DES NOMS DE COLONNES AVEC FAUTES DE FRAPPE
# Vérifier d'abord si les colonnes existent
df <- df %>%
  rename_with(~ ifelse(.x == "carrer_opportunities_stars", 
                      "career_opportunities_stars", .x)) %>%
  rename_with(~ ifelse(.x == "senior_mangemnet_stars", 
                      "senior_management_stars", .x))

# Convertir les colonnes de notes en numérique si elles sont en caractères
stars_columns <- c("work_balance_stars", "culture_values_stars", 
                   "career_opportunities_stars", "comp_benefit_stars", 
                   "senior_management_stars")

for(col in stars_columns) {
  if(col %in% names(df)) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
}

# Vérification finale des colonnes
cat("✅ COLONNES FINALES DISPONIBLES :\n")
print(names(df))
cat("\n")

# Créer le dossier de sortie
output_dir <- here("outputs", "tables")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Extraire l'année depuis la colonne dates
df <- df %>%
  mutate(
    review_year = case_when(
      str_detect(dates, "\\d{4}") ~ as.numeric(str_extract(dates, "\\d{4}")),
      TRUE ~ NA_real_
    )
  )

# 📋 SECTION 1: VUE D'ENSEMBLE ET PRÉPARATION ----
cat_line()
cat("📊 ANALYSE GLASSDOOR - VUE D'ENSEMBLE\n")
cat_line()
cat("\n")

# Statistiques générales
cat("DIMENSIONS DU DATASET:\n")
cat("  • Nombre total d'avis:", format(nrow(df), big.mark = ","), "\n")
cat("  • Nombre de colonnes:", ncol(df), "\n")
cat("  • Nombre d'entreprises:", n_distinct(df$company), "\n")
cat("  • Période couverte:", min(df$review_year, na.rm = TRUE), "-", 
    max(df$review_year, na.rm = TRUE), "\n\n")

# Distribution par entreprise
cat("RÉPARTITION PAR ENTREPRISE:\n")
company_counts <- df %>% 
  count(company, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)
print(company_counts)
cat("\n")


# 📈 SECTION 2: ANALYSE DES NOTES GLOBALES ----
cat_line()
cat("⭐ ANALYSE DES NOTES GLOBALES\n")
cat_line()
cat("\n")

# Statistiques descriptives
overall_stats <- df %>%
  summarise(
    n_reviews = n(),
    moyenne = mean(overall_ratings, na.rm = TRUE),
    mediane = median(overall_ratings, na.rm = TRUE),
    ecart_type = sd(overall_ratings, na.rm = TRUE),
    min = min(overall_ratings, na.rm = TRUE),
    max = max(overall_ratings, na.rm = TRUE),
    pct_positif = mean(overall_ratings >= 4, na.rm = TRUE) * 100,
    pct_neutre = mean(overall_ratings == 3, na.rm = TRUE) * 100,
    pct_negatif = mean(overall_ratings <= 2, na.rm = TRUE) * 100
  )

cat("STATISTIQUES GLOBALES:\n")
print(overall_stats)
cat("\n")

# Distribution par note
cat("DISTRIBUTION DES NOTES:\n")
rating_dist <- df %>%
  count(overall_ratings) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(overall_ratings)
print(rating_dist)
cat("\n")


# 📊 SECTION 3: COMPARAISON INTER-ENTREPRISES ----
cat_line()
cat("🏢 COMPARAISON DES ENTREPRISES\n")
cat_line()
cat("\n")

# Statistiques par entreprise
company_stats <- df %>%
  group_by(company) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    note_mediane = median(overall_ratings, na.rm = TRUE),
    ecart_type = sd(overall_ratings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(note_moyenne))

cat("CLASSEMENT DES ENTREPRISES (par note moyenne):\n")
print(company_stats)
cat("\n")

# Vérifier si les colonnes d'étoiles existent et ne sont pas vides
stars_exist <- all(stars_columns %in% names(df))
stars_have_data <- if(stars_exist) {
  any(!is.na(df %>% select(all_of(stars_columns)) %>% unlist()))
} else {
  FALSE
}

if(stars_exist && stars_have_data) {
  # Statistiques par entreprise avec toutes les 5 dimensions
  company_stats_full <- df %>%
    group_by(company) %>%
    summarise(
      n_avis = n(),
      note_moyenne = mean(overall_ratings, na.rm = TRUE),
      work_balance = mean(work_balance_stars, na.rm = TRUE),
      culture_values = mean(culture_values_stars, na.rm = TRUE),
      career_opportunities = mean(career_opportunities_stars, na.rm = TRUE),
      compensation = mean(comp_benefit_stars, na.rm = TRUE),
      senior_management = mean(senior_management_stars, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(note_moyenne))
  
  # Identifier les forces et faiblesses de chaque entreprise
  cat("FORCES ET FAIBLESSES PAR ENTREPRISE (si données disponibles):\n\n")
  for(comp in company_stats_full$company) {
    company_data <- company_stats_full %>% filter(company == comp)
    
    dimensions <- c(
      "Work-Life Balance" = company_data$work_balance,
      "Culture & Values" = company_data$culture_values,
      "Career Opportunities" = company_data$career_opportunities,
      "Compensation & Benefits" = company_data$compensation,
      "Senior Management" = company_data$senior_management
    )
    
    # Supprimer les NA
    dimensions <- dimensions[!is.na(dimensions)]
    
    if(length(dimensions) > 0) {
      cat("📍", toupper(comp), "\n")
      cat("  Note moyenne:", round(company_data$note_moyenne, 2), "/5\n")
      cat("  Point fort:", names(which.max(dimensions)), 
          "(", round(max(dimensions, na.rm = TRUE), 2), "/5)\n")
      cat("  Point faible:", names(which.min(dimensions)), 
          "(", round(min(dimensions, na.rm = TRUE), 2), "/5)\n\n")
    }
  }
} else {
  cat("⚠️ Les données des étoiles par dimension ne sont pas disponibles ou sont vides\n\n")
}


# 📅 SECTION 4: ANALYSE TEMPORELLE ----
cat_line()
cat("📅 ÉVOLUTION TEMPORELLE\n")
cat_line()
cat("\n")

# Tendances annuelles globales
yearly_trends <- df %>%
  filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
  group_by(review_year) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    .groups = "drop"
  )

cat("ÉVOLUTION DES NOTES PAR ANNÉE:\n")
print(yearly_trends)
cat("\n")

# Tendances par entreprise
yearly_by_company <- df %>%
  filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
  group_by(company, review_year) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_avis >= 10)  # Minimum 10 avis par an

cat("TENDANCES PAR ENTREPRISE (aperçu):\n")
print(head(yearly_by_company, 20))
cat("\n")


# 🎭 SECTION 5: ANALYSE DES DIMENSIONS DE SATISFACTION ----
cat_line()
cat("🎭 ANALYSE DES 5 DIMENSIONS DE SATISFACTION\n")
cat_line()
cat("\n")

if(stars_exist) {
  # Reshape pour analyse des dimensions
  dimensions_long <- df %>%
    select(company, all_of(stars_columns)) %>%
    pivot_longer(
      cols = all_of(stars_columns),
      names_to = "dimension",
      values_to = "rating"
    ) %>%
    mutate(
      dimension = str_remove(dimension, "_stars") %>%
        str_replace_all("_", " ") %>%
        str_to_title()
    )
  
  # Statistiques par dimension
  dimension_stats <- dimensions_long %>%
    group_by(dimension) %>%
    summarise(
      moyenne = mean(rating, na.rm = TRUE),
      mediane = median(rating, na.rm = TRUE),
      ecart_type = sd(rating, na.rm = TRUE),
      n_non_na = sum(!is.na(rating)),
      .groups = "drop"
    ) %>%
    arrange(desc(moyenne))
  
  cat("CLASSEMENT DES DIMENSIONS:\n")
  print(dimension_stats)
  cat("\n")
  
  # Dimensions par entreprise
  dimension_by_company <- dimensions_long %>%
    group_by(company, dimension) %>%
    summarise(
      moyenne = mean(rating, na.rm = TRUE),
      n = sum(!is.na(rating)),
      .groups = "drop"
    ) %>%
    filter(!is.na(moyenne)) %>%
    arrange(company, desc(moyenne))
  
  if(nrow(dimension_by_company) > 0) {
    cat("APERÇU DIMENSIONS PAR ENTREPRISE:\n")
    print(head(dimension_by_company, 25))
  } else {
    cat("Aucune donnée disponible pour les dimensions par entreprise\n")
  }
  cat("\n")
} else {
  cat("⚠️ Les colonnes des étoiles ne sont pas disponibles\n\n")
}


# 💬 SECTION 6: ANALYSE TEXTUELLE DES AVIS ----
cat_line()
cat("💬 ANALYSE TEXTUELLE (PROS & CONS)\n")
cat_line()
cat("\n")

# CORRECTION: Problème avec slice_sample
# Échantillonner pour performance
set.seed(123)
sample_size <- 5000

# Vérifier combien de lignes non-NA
n_pros_available <- sum(!is.na(df$pros) & df$pros != "" & df$pros != "none")

if(n_pros_available > 0) {
  sample_size <- min(sample_size, n_pros_available)
  
  # Échantillonner correctement
  df_sample <- df %>% 
    filter(!is.na(pros) & pros != "" & pros != "none") %>%
    slice_sample(n = sample_size)
  
  cat("Analyse sur échantillon de", nrow(df_sample), "avis\n\n")
  
  # Charger les stopwords
  data("stop_words")
  
  # Mots fréquents dans les PROS
  cat("TOP 20 MOTS DANS LES POINTS POSITIFS:\n")
  pros_words <- df_sample %>%
    unnest_tokens(word, pros) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "^[a-z]+$"), nchar(word) > 3) %>%
    count(word, sort = TRUE) %>%
    head(20)
  
  print(pros_words)
  cat("\n")
  
  # Mots fréquents dans les CONS
  cat("TOP 20 MOTS DANS LES POINTS NÉGATIFS:\n")
  cons_words <- df_sample %>%
    filter(!is.na(cons) & cons != "" & cons != "none") %>%
    unnest_tokens(word, cons) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "^[a-z]+$"), nchar(word) > 3) %>%
    count(word, sort = TRUE) %>%
    head(20)
  
  print(cons_words)
  cat("\n")
  
} else {
  cat("⚠️ Aucune donnée textuelle disponible pour analyse\n\n")
}


# 🎯 SECTION 7: INSIGHTS BUSINESS & RECOMMANDATIONS ----
cat_line()
cat("🎯 INSIGHTS BUSINESS & RECOMMANDATIONS\n")
cat_line()
cat("\n")

cat("PRINCIPALES DÉCOUVERTES:\n\n")

# 1. Entreprise la mieux notée
if(nrow(company_stats) > 0) {
  best_company <- company_stats %>% slice_max(note_moyenne, n = 1)
  cat("1. LEADER DU CLASSEMENT:\n")
  cat("   •", best_company$company, "obtient la meilleure note moyenne de",
      round(best_company$note_moyenne, 2), "/5\n")
  cat("   • Nombre d'avis:", format(best_company$n_avis, big.mark = ","), "\n\n")
}

# 2. Entreprise avec le plus de marge d'amélioration
if(nrow(company_stats) > 0) {
  worst_company <- company_stats %>% slice_min(note_moyenne, n = 1)
  cat("2. PLUS GRANDE MARGE D'AMÉLIORATION:\n")
  cat("   •", worst_company$company, "avec une note de",
      round(worst_company$note_moyenne, 2), "/5\n\n")
}

# 3. Analyse de la distribution
cat("3. ANALYSE DE LA DISTRIBUTION:\n")
cat("   •", round(overall_stats$pct_positif, 1), "% des avis sont positifs (≥4 étoiles)\n")
cat("   •", round(overall_stats$pct_negatif, 1), "% des avis sont négatifs (≤2 étoiles)\n")
cat("   • Note moyenne globale:", round(overall_stats$moyenne, 2), "/5\n\n")

# 4. Tendance temporelle
if(nrow(yearly_trends) >= 3) {
  recent_trend <- yearly_trends %>%
    tail(3) %>%
    summarise(
      evolution = last(note_moyenne) - first(note_moyenne)
    )
  
  cat("4. TENDANCE RÉCENTE:\n")
  if(recent_trend$evolution > 0) {
    cat("   • Amélioration de +", round(abs(recent_trend$evolution), 2),
        "points sur les 3 dernières années ✅\n\n")
  } else {
    cat("   • Dégradation de -", round(abs(recent_trend$evolution), 2),
        "points sur les 3 dernières années ⚠️\n\n")
  }
}

cat("\n")
cat("RECOMMANDATIONS STRATÉGIQUES:\n\n")

cat("1. POUR LES ENTREPRISES:\n")
cat("   • Analyser spécifiquement les avis négatifs pour identifier les problèmes récurrents\n")
cat("   • Surveiller l'évolution temporelle de la satisfaction\n")
cat("   • Comparer les performances avec les concurrents du secteur\n\n")

cat("2. POUR LES CANDIDATS:\n")
cat("   • Consulter spécifiquement les avis récents (dernières années)\n")
cat("   • Lire les avis textuels pour comprendre le contexte réel\n")
cat("   • Comparer plusieurs entreprises du même secteur\n\n")


# 💾 SAUVEGARDE DES RÉSULTATS ----
cat_line()
cat("💾 EXPORT DES RÉSULTATS\n")
cat_line()
cat("\n")

# Sauvegarder les tableaux principaux
write_csv(company_stats, here(output_dir, "statistiques_entreprises.csv"))
write_csv(yearly_trends, here(output_dir, "tendances_annuelles.csv"))

if(stars_exist && exists("dimension_stats")) {
  write_csv(dimension_stats, here(output_dir, "statistiques_dimensions.csv"))
}

if(exists("pros_words")) {
  write_csv(pros_words, here(output_dir, "top_mots_positifs.csv"))
}

if(exists("cons_words")) {
  write_csv(cons_words, here(output_dir, "top_mots_negatifs.csv"))
}

# Créer un résumé exécutif
summary_report <- list(
  date_analyse = Sys.Date(),
  dataset_size = nrow(df),
  companies = unique(df$company),
  best_company = if(exists("best_company")) best_company$company else NA,
  best_rating = if(exists("best_company")) best_company$note_moyenne else NA,
  overall_avg = overall_stats$moyenne
)

saveRDS(summary_report, here(output_dir, "resume_executif.rds"))

cat("✅ Fichiers exportés dans:", output_dir, "\n")
cat("   • statistiques_entreprises.csv\n")
cat("   • tendances_annuelles.csv\n")
if(stars_exist) cat("   • statistiques_dimensions.csv\n")
if(exists("pros_words")) cat("   • top_mots_positifs.csv\n")
if(exists("cons_words")) cat("   • top_mots_negatifs.csv\n")
cat("   • resume_executif.rds\n\n")

cat_line()
cat("✅ ANALYSE TERMINÉE AVEC SUCCÈS!\n")
cat_line()
cat("\n")









# # ==============================================================================
# # 📊 ANALYSE APPROFONDIE DES AVIS GLASSDOOR - VERSION FINALE CORRIGÉE
# # Dataset: employee_reviews.csv (67,529 avis)
# # ==============================================================================

# # 🔧 SETUP ET CHARGEMENT ----
# library(tidyverse)
# library(lubridate)
# library(scales)
# library(patchwork)
# library(tidytext)
# library(wordcloud)
# library(RColorBrewer)
# library(here)

# # Fonction pour créer des lignes de séparation
# cat_line <- function(char = "=", length = 80) {
#   cat(paste(rep(char, length), collapse = ""), "\n")
# }

# # Charger les données
# df <- read_csv(here("data", "raw", "employee_reviews.csv"))

# # Nettoyer les noms de colonnes
# df <- df %>% janitor::clean_names()

# # CORRECTION DES NOMS DE COLONNES AVEC FAUTES DE FRAPPE
# # Vérifier d'abord si les colonnes existent
# df <- df %>%
#   rename_with(~ ifelse(.x == "carrer_opportunities_stars", 
#                       "career_opportunities_stars", .x)) %>%
#   rename_with(~ ifelse(.x == "senior_mangemnet_stars", 
#                       "senior_management_stars", .x))

# # Convertir les colonnes de notes en numérique si elles sont en caractères
# stars_columns <- c("work_balance_stars", "culture_values_stars", 
#                    "career_opportunities_stars", "comp_benefit_stars", 
#                    "senior_management_stars")

# for(col in stars_columns) {
#   if(col %in% names(df)) {
#     df[[col]] <- as.numeric(df[[col]])
#   }
# }

# # Vérification finale des colonnes
# cat("✅ COLONNES FINALES DISPONIBLES :\n")
# print(names(df))
# cat("\n")

# # Créer le dossier de sortie
# output_dir <- here("outputs", "tables")
# dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# # Extraire l'année depuis la colonne dates
# df <- df %>%
#   mutate(
#     review_year = case_when(
#       str_detect(dates, "\\d{4}") ~ as.numeric(str_extract(dates, "\\d{4}")),
#       TRUE ~ NA_real_
#     )
#   )

# # 📋 SECTION 1: VUE D'ENSEMBLE ET PRÉPARATION ----
# cat_line()
# cat("📊 ANALYSE GLASSDOOR - VUE D'ENSEMBLE\n")
# cat_line()
# cat("\n")

# # Statistiques générales
# cat("DIMENSIONS DU DATASET:\n")
# cat("  • Nombre total d'avis:", format(nrow(df), big.mark = ","), "\n")
# cat("  • Nombre de colonnes:", ncol(df), "\n")
# cat("  • Nombre d'entreprises:", n_distinct(df$company), "\n")
# cat("  • Période couverte:", min(df$review_year, na.rm = TRUE), "-", 
#     max(df$review_year, na.rm = TRUE), "\n\n")

# # Distribution par entreprise
# cat("RÉPARTITION PAR ENTREPRISE:\n")
# company_counts <- df %>% 
#   count(company, sort = TRUE) %>%
#   mutate(percentage = n / sum(n) * 100)
# print(company_counts)
# cat("\n")


# # 📈 SECTION 2: ANALYSE DES NOTES GLOBALES ----
# cat_line()
# cat("⭐ ANALYSE DES NOTES GLOBALES\n")
# cat_line()
# cat("\n")

# # Statistiques descriptives
# overall_stats <- df %>%
#   summarise(
#     n_reviews = n(),
#     moyenne = mean(overall_ratings, na.rm = TRUE),
#     mediane = median(overall_ratings, na.rm = TRUE),
#     ecart_type = sd(overall_ratings, na.rm = TRUE),
#     min = min(overall_ratings, na.rm = TRUE),
#     max = max(overall_ratings, na.rm = TRUE),
#     pct_positif = mean(overall_ratings >= 4, na.rm = TRUE) * 100,
#     pct_neutre = mean(overall_ratings == 3, na.rm = TRUE) * 100,
#     pct_negatif = mean(overall_ratings <= 2, na.rm = TRUE) * 100
#   )

# cat("STATISTIQUES GLOBALES:\n")
# print(overall_stats)
# cat("\n")

# # Distribution par note
# cat("DISTRIBUTION DES NOTES:\n")
# rating_dist <- df %>%
#   count(overall_ratings) %>%
#   mutate(percentage = n / sum(n) * 100) %>%
#   arrange(overall_ratings)
# print(rating_dist)
# cat("\n")


# # 📊 SECTION 3: COMPARAISON INTER-ENTREPRISES ----
# cat_line()
# cat("🏢 COMPARAISON DES ENTREPRISES\n")
# cat_line()
# cat("\n")

# # Statistiques par entreprise
# company_stats <- df %>%
#   group_by(company) %>%
#   summarise(
#     n_avis = n(),
#     note_moyenne = mean(overall_ratings, na.rm = TRUE),
#     note_mediane = median(overall_ratings, na.rm = TRUE),
#     ecart_type = sd(overall_ratings, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(note_moyenne))

# cat("CLASSEMENT DES ENTREPRISES (par note moyenne):\n")
# print(company_stats)
# cat("\n")

# # Vérifier si les colonnes d'étoiles existent et ne sont pas vides
# stars_exist <- all(stars_columns %in% names(df))
# stars_have_data <- if(stars_exist) {
#   any(!is.na(df %>% select(all_of(stars_columns)) %>% unlist()))
# } else {
#   FALSE
# }

# if(stars_exist && stars_have_data) {
#   # Statistiques par entreprise avec toutes les 5 dimensions
#   company_stats_full <- df %>%
#     group_by(company) %>%
#     summarise(
#       n_avis = n(),
#       note_moyenne = mean(overall_ratings, na.rm = TRUE),
#       work_balance = mean(work_balance_stars, na.rm = TRUE),
#       culture_values = mean(culture_values_stars, na.rm = TRUE),
#       career_opportunities = mean(career_opportunities_stars, na.rm = TRUE),
#       compensation = mean(comp_benefit_stars, na.rm = TRUE),
#       senior_management = mean(senior_management_stars, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     arrange(desc(note_moyenne))
  
#   # Identifier les forces et faiblesses de chaque entreprise
#   cat("FORCES ET FAIBLESSES PAR ENTREPRISE (si données disponibles):\n\n")
#   for(comp in company_stats_full$company) {
#     company_data <- company_stats_full %>% filter(company == comp)
    
#     dimensions <- c(
#       "Work-Life Balance" = company_data$work_balance,
#       "Culture & Values" = company_data$culture_values,
#       "Career Opportunities" = company_data$career_opportunities,
#       "Compensation & Benefits" = company_data$compensation,
#       "Senior Management" = company_data$senior_management
#     )
    
#     # Supprimer les NA
#     dimensions <- dimensions[!is.na(dimensions)]
    
#     if(length(dimensions) > 0) {
#       cat("📍", toupper(comp), "\n")
#       cat("  Note moyenne:", round(company_data$note_moyenne, 2), "/5\n")
#       cat("  Point fort:", names(which.max(dimensions)), 
#           "(", round(max(dimensions, na.rm = TRUE), 2), "/5)\n")
#       cat("  Point faible:", names(which.min(dimensions)), 
#           "(", round(min(dimensions, na.rm = TRUE), 2), "/5)\n\n")
#     }
#   }
# } else {
#   cat("⚠️ Les données des étoiles par dimension ne sont pas disponibles ou sont vides\n\n")
# }


# # 📅 SECTION 4: ANALYSE TEMPORELLE ----
# cat_line()
# cat("📅 ÉVOLUTION TEMPORELLE\n")
# cat_line()
# cat("\n")

# # Tendances annuelles globales
# yearly_trends <- df %>%
#   filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
#   group_by(review_year) %>%
#   summarise(
#     n_avis = n(),
#     note_moyenne = mean(overall_ratings, na.rm = TRUE),
#     .groups = "drop"
#   )

# cat("ÉVOLUTION DES NOTES PAR ANNÉE:\n")
# print(yearly_trends)
# cat("\n")

# # Tendances par entreprise
# yearly_by_company <- df %>%
#   filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
#   group_by(company, review_year) %>%
#   summarise(
#     n_avis = n(),
#     note_moyenne = mean(overall_ratings, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(n_avis >= 10)  # Minimum 10 avis par an

# cat("TENDANCES PAR ENTREPRISE (aperçu):\n")
# print(head(yearly_by_company, 20))
# cat("\n")


# # 🎭 SECTION 5: ANALYSE DES DIMENSIONS DE SATISFACTION ----
# cat_line()
# cat("🎭 ANALYSE DES 5 DIMENSIONS DE SATISFACTION\n")
# cat_line()
# cat("\n")

# if(stars_exist) {
#   # Reshape pour analyse des dimensions
#   dimensions_long <- df %>%
#     select(company, all_of(stars_columns)) %>%
#     pivot_longer(
#       cols = all_of(stars_columns),
#       names_to = "dimension",
#       values_to = "rating"
#     ) %>%
#     mutate(
#       dimension = str_remove(dimension, "_stars") %>%
#         str_replace_all("_", " ") %>%
#         str_to_title()
#     )
  
#   # Statistiques par dimension
#   dimension_stats <- dimensions_long %>%
#     group_by(dimension) %>%
#     summarise(
#       moyenne = mean(rating, na.rm = TRUE),
#       mediane = median(rating, na.rm = TRUE),
#       ecart_type = sd(rating, na.rm = TRUE),
#       n_non_na = sum(!is.na(rating)),
#       .groups = "drop"
#     ) %>%
#     arrange(desc(moyenne))
  
#   cat("CLASSEMENT DES DIMENSIONS:\n")
#   print(dimension_stats)
#   cat("\n")
  
#   # Dimensions par entreprise
#   dimension_by_company <- dimensions_long %>%
#     group_by(company, dimension) %>%
#     summarise(
#       moyenne = mean(rating, na.rm = TRUE),
#       n = sum(!is.na(rating)),
#       .groups = "drop"
#     ) %>%
#     filter(!is.na(moyenne)) %>%
#     arrange(company, desc(moyenne))
  
#   if(nrow(dimension_by_company) > 0) {
#     cat("APERÇU DIMENSIONS PAR ENTREPRISE:\n")
#     print(head(dimension_by_company, 25))
#   } else {
#     cat("Aucune donnée disponible pour les dimensions par entreprise\n")
#   }
#   cat("\n")
# } else {
#   cat("⚠️ Les colonnes des étoiles ne sont pas disponibles\n\n")
# }


# # 💬 SECTION 6: ANALYSE TEXTUELLE DES AVIS ----
# cat_line()
# cat("💬 ANALYSE TEXTUELLE (PROS & CONS)\n")
# cat_line()
# cat("\n")

# # CORRECTION: Problème avec slice_sample
# # Échantillonner pour performance
# set.seed(123)
# sample_size <- 5000

# # Vérifier combien de lignes non-NA
# n_pros_available <- sum(!is.na(df$pros) & df$pros != "" & df$pros != "none")

# if(n_pros_available > 0) {
#   sample_size <- min(sample_size, n_pros_available)
  
#   # Échantillonner correctement
#   df_sample <- df %>% 
#     filter(!is.na(pros) & pros != "" & pros != "none") %>%
#     slice_sample(n = sample_size)
  
#   cat("Analyse sur échantillon de", nrow(df_sample), "avis\n\n")
  
#   # Charger les stopwords
#   data("stop_words")
  
#   # Mots fréquents dans les PROS
#   cat("TOP 20 MOTS DANS LES POINTS POSITIFS:\n")
#   pros_words <- df_sample %>%
#     unnest_tokens(word, pros) %>%
#     anti_join(stop_words, by = "word") %>%
#     filter(str_detect(word, "^[a-z]+$"), nchar(word) > 3) %>%
#     count(word, sort = TRUE) %>%
#     head(20)
  
#   print(pros_words)
#   cat("\n")
  
#   # Mots fréquents dans les CONS
#   cat("TOP 20 MOTS DANS LES POINTS NÉGATIFS:\n")
#   cons_words <- df_sample %>%
#     filter(!is.na(cons) & cons != "" & cons != "none") %>%
#     unnest_tokens(word, cons) %>%
#     anti_join(stop_words, by = "word") %>%
#     filter(str_detect(word, "^[a-z]+$"), nchar(word) > 3) %>%
#     count(word, sort = TRUE) %>%
#     head(20)
  
#   print(cons_words)
#   cat("\n")
  
# } else {
#   cat("⚠️ Aucune donnée textuelle disponible pour analyse\n\n")
# }


# # 🎯 SECTION 7: INSIGHTS BUSINESS & RECOMMANDATIONS ----
# cat_line()
# cat("🎯 INSIGHTS BUSINESS & RECOMMANDATIONS\n")
# cat_line()
# cat("\n")

# cat("PRINCIPALES DÉCOUVERTES:\n\n")

# # 1. Entreprise la mieux notée
# if(nrow(company_stats) > 0) {
#   best_company <- company_stats %>% slice_max(note_moyenne, n = 1)
#   cat("1. LEADER DU CLASSEMENT:\n")
#   cat("   •", best_company$company, "obtient la meilleure note moyenne de",
#       round(best_company$note_moyenne, 2), "/5\n")
#   cat("   • Nombre d'avis:", format(best_company$n_avis, big.mark = ","), "\n\n")
# }

# # 2. Entreprise avec le plus de marge d'amélioration
# if(nrow(company_stats) > 0) {
#   worst_company <- company_stats %>% slice_min(note_moyenne, n = 1)
#   cat("2. PLUS GRANDE MARGE D'AMÉLIORATION:\n")
#   cat("   •", worst_company$company, "avec une note de",
#       round(worst_company$note_moyenne, 2), "/5\n\n")
# }

# # 3. Analyse de la distribution
# cat("3. ANALYSE DE LA DISTRIBUTION:\n")
# cat("   •", round(overall_stats$pct_positif, 1), "% des avis sont positifs (≥4 étoiles)\n")
# cat("   •", round(overall_stats$pct_negatif, 1), "% des avis sont négatifs (≤2 étoiles)\n")
# cat("   • Note moyenne globale:", round(overall_stats$moyenne, 2), "/5\n\n")

# # 4. Tendance temporelle
# if(nrow(yearly_trends) >= 3) {
#   recent_trend <- yearly_trends %>%
#     tail(3) %>%
#     summarise(
#       evolution = last(note_moyenne) - first(note_moyenne)
#     )
  
#   cat("4. TENDANCE RÉCENTE:\n")
#   if(recent_trend$evolution > 0) {
#     cat("   • Amélioration de +", round(abs(recent_trend$evolution), 2),
#         "points sur les 3 dernières années ✅\n\n")
#   } else {
#     cat("   • Dégradation de -", round(abs(recent_trend$evolution), 2),
#         "points sur les 3 dernières années ⚠️\n\n")
#   }
# }

# cat("\n")
# cat("RECOMMANDATIONS STRATÉGIQUES:\n\n")

# cat("1. POUR LES ENTREPRISES:\n")
# cat("   • Analyser spécifiquement les avis négatifs pour identifier les problèmes récurrents\n")
# cat("   • Surveiller l'évolution temporelle de la satisfaction\n")
# cat("   • Comparer les performances avec les concurrents du secteur\n\n")

# cat("2. POUR LES CANDIDATS:\n")
# cat("   • Consulter spécifiquement les avis récents (dernières années)\n")
# cat("   • Lire les avis textuels pour comprendre le contexte réel\n")
# cat("   • Comparer plusieurs entreprises du même secteur\n\n")


# # 💾 SAUVEGARDE DES RÉSULTATS ----
# cat_line()
# cat("💾 EXPORT DES RÉSULTATS\n")
# cat_line()
# cat("\n")

# # Sauvegarder les tableaux principaux
# write_csv(company_stats, here(output_dir, "statistiques_entreprises.csv"))
# write_csv(yearly_trends, here(output_dir, "tendances_annuelles.csv"))

# if(stars_exist && exists("dimension_stats")) {
#   write_csv(dimension_stats, here(output_dir, "statistiques_dimensions.csv"))
# }

# if(exists("pros_words")) {
#   write_csv(pros_words, here(output_dir, "top_mots_positifs.csv"))
# }

# if(exists("cons_words")) {
#   write_csv(cons_words, here(output_dir, "top_mots_negatifs.csv"))
# }

# # Créer un résumé exécutif
# summary_report <- list(
#   date_analyse = Sys.Date(),
#   dataset_size = nrow(df),
#   companies = unique(df$company),
#   best_company = if(exists("best_company")) best_company$company else NA,
#   best_rating = if(exists("best_company")) best_company$note_moyenne else NA,
#   overall_avg = overall_stats$moyenne
# )

# saveRDS(summary_report, here(output_dir, "resume_executif.rds"))

# cat("✅ Fichiers exportés dans:", output_dir, "\n")
# cat("   • statistiques_entreprises.csv\n")
# cat("   • tendances_annuelles.csv\n")
# if(stars_exist) cat("   • statistiques_dimensions.csv\n")
# if(exists("pros_words")) cat("   • top_mots_positifs.csv\n")
# if(exists("cons_words")) cat("   • top_mots_negatifs.csv\n")
# cat("   • resume_executif.rds\n\n")

# cat_line()
# cat("✅ ANALYSE TERMINÉE AVEC SUCCÈS!\n")
# cat_line()
# cat("\n")