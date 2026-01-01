# ================================================================================
# 📊 VISUALISATIONS PROFESSIONNELLES - ANALYSE GLASSDOOR (VERSION FINALE CORRIGÉE)
# ================================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(viridis)
library(ggthemes)
library(here)
library(fmsb)
library(reshape2)

# ========== CHARGEMENT ET PRÉPARATION ==========
df <- read_csv(here("data", "raw", "employee_reviews.csv"), show_col_types = FALSE)

# Nettoyer les noms de colonnes
df <- df %>% janitor::clean_names()

# CORRECTION des noms de colonnes
df <- df %>%
  rename(
    career_opportunities_stars = carrer_opportunities_stars,
    senior_management_stars = senior_mangemnet_stars
  )

# Convertir en numérique
stars_columns <- c("work_balance_stars", "culture_values_stars", 
                   "career_opportunities_stars", "comp_benefit_stars", 
                   "senior_management_stars")

for(col in stars_columns) {
  if(col %in% names(df)) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
}

# Extraire l'année
df <- df %>%
  mutate(
    review_year = case_when(
      str_detect(dates, "\\d{4}") ~ as.numeric(str_extract(dates, "\\d{4}")),
      TRUE ~ NA_real_
    )
  )

# Créer le dossier pour les figures
dir.create("resultats_analyse", showWarnings = FALSE)

# Palette de couleurs
company_colors <- c(
  "amazon" = "#FF9900",
  "microsoft" = "#00A4EF",
  "apple" = "#A2AAAD",
  "google" = "#4285F4",
  "facebook" = "#1877F2",
  "netflix" = "#E50914"
)

# Thème personnalisé amélioré
theme_glassdoor <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0.5),
      plot.subtitle = element_text(size = base_size - 2, color = "gray40", hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1),
      legend.position = "bottom",
      legend.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size - 1, face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = base_size, face = "bold"),
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ========== VIZ 1: Distribution des notes globales ==========
p1 <- df %>%
  ggplot(aes(x = overall_ratings)) +
  geom_histogram(binwidth = 0.5, fill = "#3498db", color = "white", alpha = 0.85) +
  geom_vline(
    aes(xintercept = mean(overall_ratings, na.rm = TRUE)),
    color = "#e74c3c",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = mean(df$overall_ratings, na.rm = TRUE) + 0.3,
    y = Inf,
    label = paste0("Moyenne: ", round(mean(df$overall_ratings, na.rm = TRUE), 2)),
    vjust = 1.5,
    color = "#e74c3c",
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribution des Notes Globales",
    subtitle = paste0(format(nrow(df), big.mark = ","), " avis analysés"),
    x = "Note (1-5 étoiles)",
    y = "Nombre d'avis"
  ) +
  theme_glassdoor()

ggsave("resultats_analyse/viz_01_distribution_notes.png", p1, 
       width = 10, height = 6, dpi = 300)
cat("✅ VIZ 1 créée: Distribution des notes globales\n")

# ========== VIZ 2: Classement des entreprises ==========
company_stats <- df %>%
  group_by(company) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    ecart_type = sd(overall_ratings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(note_moyenne))

p2 <- company_stats %>%
  ggplot(aes(x = reorder(company, note_moyenne), y = note_moyenne, fill = company)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_errorbar(
    aes(ymin = note_moyenne - ecart_type, ymax = note_moyenne + ecart_type),
    width = 0.2,
    color = "gray30"
  ) +
  geom_text(
    aes(label = round(note_moyenne, 2)),
    hjust = -0.3,
    fontface = "bold",
    size = 4
  ) +
  geom_text(
    aes(y = 0.2, label = paste0("n=", format(n_avis, big.mark = ","))),
    hjust = 0,
    size = 3,
    color = "gray50"
  ) +
  coord_flip(ylim = c(0, 5.5)) +
  scale_fill_manual(values = company_colors) +
  labs(
    title = "Classement des Entreprises par Note Moyenne",
    subtitle = "Barres d'erreur = écart-type (variabilité des avis)",
    x = NULL,
    y = "Note Moyenne (1-5)"
  ) +
  theme_glassdoor() +
  theme(legend.position = "none")

ggsave("resultats_analyse/viz_02_classement_entreprises.png", p2,
       width = 10, height = 6, dpi = 300)
cat("✅ VIZ 2 créée: Classement des entreprises\n")

# ========== VIZ 3: Heatmap des dimensions ==========
# Calculer les moyennes par entreprise et dimension
dimension_matrix <- df %>%
  group_by(company) %>%
  summarise(
    `Work-Life\nBalance` = mean(work_balance_stars, na.rm = TRUE),
    `Culture &\nValues` = mean(culture_values_stars, na.rm = TRUE),
    `Career\nOpportunities` = mean(career_opportunities_stars, na.rm = TRUE),
    `Compensation &\nBenefits` = mean(comp_benefit_stars, na.rm = TRUE),
    `Senior\nManagement` = mean(senior_management_stars, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -company,
    names_to = "dimension",
    values_to = "score"
  )

# Ordonner les entreprises par note globale
company_order <- company_stats$company
dimension_matrix$company <- factor(dimension_matrix$company, levels = rev(company_order))

p3 <- dimension_matrix %>%
  ggplot(aes(x = dimension, y = company, fill = score)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(score, 2)), 
            color = "white", 
            fontface = "bold",
            size = 3.5) +
  scale_fill_viridis(
    option = "plasma",
    name = "Score Moyen",
    limits = c(3, 4.5)
  ) +
  labs(
    title = "Heatmap des Dimensions de Satisfaction par Entreprise",
    subtitle = "Plus la couleur est claire, meilleure est la note",
    x = NULL,
    y = NULL
  ) +
  theme_glassdoor(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm")
  )

ggsave("resultats_analyse/viz_03_heatmap_dimensions.png", p3,
       width = 12, height = 6, dpi = 300)
cat("✅ VIZ 3 créée: Heatmap des dimensions\n")

# ========== VIZ 4: Radar charts ==========
cat("📊 Création des radar charts...\n")
for(comp in unique(df$company)) {
  # Calculer les moyennes
  radar_values <- df %>%
    filter(company == comp) %>%
    summarise(
      `Work-Life\nBalance` = mean(work_balance_stars, na.rm = TRUE),
      `Culture &\nValues` = mean(culture_values_stars, na.rm = TRUE),
      `Career\nOpportunities` = mean(career_opportunities_stars, na.rm = TRUE),
      `Compensation &\nBenefits` = mean(comp_benefit_stars, na.rm = TRUE),
      `Senior\nManagement` = mean(senior_management_stars, na.rm = TRUE)
    )
  
  # Vérifier que toutes les valeurs existent
  if(all(!is.na(radar_values))) {
    radar_values <- as.numeric(radar_values)
    
    # Préparer les données pour fmsb
    radar_data <- as.data.frame(rbind(rep(5, 5), rep(1, 5), radar_values))
    colnames(radar_data) <- c("Work-Life\nBalance", "Culture &\nValues", 
                              "Career\nOpportunities", "Compensation &\nBenefits", 
                              "Senior\nManagement")
    
    # Créer le radar chart
    png(paste0("resultats_analyse/viz_04_radar_", comp, ".png"),
        width = 800, height = 800, res = 120)
    
    par(mar = c(1, 1, 3, 1))
    
    fmsb::radarchart(
      radar_data,
      axistype = 1,
      pcol = company_colors[comp],
      pfcol = scales::alpha(company_colors[comp], 0.3),
      plwd = 3,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey20",
      caxislabels = c("1", "2", "3", "4", "5"),
      cglwd = 0.8,
      vlcex = 1,
      title = paste("Profil de Satisfaction -", toupper(comp))
    )
    
    dev.off()
    cat(paste0("  ✅ Radar chart créé pour ", comp, "\n"))
  } else {
    cat(paste0("  ⚠️ Données insuffisantes pour ", comp, "\n"))
  }
}

# ========== VIZ 5: Boxplot des dimensions ==========
dimensions_long <- df %>%
  select(company, all_of(stars_columns)) %>%
  pivot_longer(
    cols = all_of(stars_columns),
    names_to = "dimension",
    values_to = "rating"
  ) %>%
  mutate(
    dimension = case_when(
      dimension == "work_balance_stars" ~ "Work-Life Balance",
      dimension == "culture_values_stars" ~ "Culture & Values",
      dimension == "career_opportunities_stars" ~ "Career Opportunities",
      dimension == "comp_benefit_stars" ~ "Compensation & Benefits",
      dimension == "senior_management_stars" ~ "Senior Management",
      TRUE ~ dimension
    ),
    dimension = factor(dimension, levels = c(
      "Compensation & Benefits", "Culture & Values", 
      "Career Opportunities", "Senior Management", 
      "Work-Life Balance"
    ))
  ) %>%
  filter(!is.na(rating))

p5 <- dimensions_long %>%
  ggplot(aes(x = dimension, y = rating, fill = dimension)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 0.5) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "red",
    color = "darkred"
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribution des Notes par Dimension",
    subtitle = "Losange rouge = moyenne | Boîte = 50% des avis",
    x = NULL,
    y = "Note (1-5 étoiles)"
  ) +
  theme_glassdoor() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1, size = 10)
  )

ggsave("resultats_analyse/viz_05_boxplot_dimensions.png", p5,
       width = 12, height = 7, dpi = 300)
cat("✅ VIZ 5 créée: Boxplot des dimensions\n")

# ========== VIZ 6: Évolution temporelle ==========
yearly_by_company <- df %>%
  filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
  group_by(company, review_year) %>%
  summarise(
    n_avis = n(),
    note_moyenne = mean(overall_ratings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_avis >= 10)

if(nrow(yearly_by_company) > 0) {
  p6 <- yearly_by_company %>%
    ggplot(aes(x = review_year, y = note_moyenne, color = company)) +
    geom_line(linewidth = 1, alpha = 0.8) +
    geom_point(aes(size = n_avis), alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", 
                linewidth = 0.8, span = 0.7) +
    scale_color_manual(
      values = company_colors,
      name = "Entreprise",
      guide = guide_legend(nrow = 1)
    ) +
    scale_size_continuous(
      name = "Nombre d'avis",
      range = c(1, 8),
      breaks = c(100, 1000, 5000),
      labels = comma
    ) +
    scale_x_continuous(breaks = seq(2008, 2023, 2)) +
    scale_y_continuous(limits = c(3, 5)) +
    labs(
      title = "Évolution des Notes Moyennes dans le Temps",
      subtitle = "Taille des points = volume d'avis | Ligne pointillée = tendance",
      x = "Année",
      y = "Note Moyenne"
    ) +
    theme_glassdoor() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.2, "cm")
    )
  
  ggsave("resultats_analyse/viz_06_evolution_temporelle.png", p6,
         width = 14, height = 7, dpi = 300)
  cat("✅ VIZ 6 créée: Évolution temporelle\n")
} else {
  cat("⚠️ VIZ 6 non créée: données temporelles insuffisantes\n")
}

# ========== VIZ 7: Violin plot ==========
p7 <- df %>%
  mutate(company = factor(company, levels = rev(company_order))) %>%
  ggplot(aes(x = company, y = overall_ratings, fill = company)) +
  geom_violin(alpha = 0.7, trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, 
               outlier.shape = NA, linewidth = 0.5) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "red"
  ) +
  coord_flip() +
  scale_fill_manual(values = company_colors) +
  labs(
    title = "Distribution Détaillée des Notes par Entreprise",
    subtitle = "Violin = densité de probabilité | Boîte = quartiles | Losange rouge = moyenne",
    x = NULL,
    y = "Note (1-5 étoiles)"
  ) +
  theme_glassdoor() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10)
  )

ggsave("resultats_analyse/viz_07_violin_entreprises.png", p7,
       width = 10, height = 7, dpi = 300)
cat("✅ VIZ 7 créée: Violin plot des entreprises\n")

# ========== VIZ 8: Top performers par dimension ==========
# Calculer les moyennes avec assez de données
dimension_ranking <- dimensions_long %>%
  filter(!is.na(rating)) %>%
  group_by(company, dimension) %>%
  summarise(
    moyenne = mean(rating, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50)  # Au moins 50 observations

if(nrow(dimension_ranking) > 0) {
  # Top 3 par dimension
  top_performers <- dimension_ranking %>%
    group_by(dimension) %>%
    slice_max(moyenne, n = 3, with_ties = FALSE) %>%
    ungroup()
  
  # Créer un graphique propre
  p8 <- top_performers %>%
    mutate(
      company = factor(company, levels = company_order),
      dimension = factor(dimension, levels = unique(dimension))
    ) %>%
    ggplot(aes(x = reorder(company, moyenne), y = moyenne, fill = company)) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_text(
      aes(label = paste0(round(moyenne, 2), " (n=", format(n, big.mark = ","), ")")),
      hjust = -0.1,
      size = 3,
      color = "black"
    ) +
    coord_flip(ylim = c(0, 5)) +
    scale_fill_manual(values = company_colors) +
    facet_wrap(~dimension, ncol = 2, scales = "free_y") +
    labs(
      title = "Top 3 Entreprises par Dimension de Satisfaction",
      subtitle = "Score moyen avec nombre d'observations",
      x = NULL,
      y = "Score Moyen (1-5)"
    ) +
    theme_glassdoor(base_size = 11) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, margin = margin(b = 5)),
      panel.spacing = unit(1, "lines"),
      axis.text.y = element_text(size = 9)
    )
  
  ggsave("resultats_analyse/viz_08_top_performers.png", p8,
         width = 14, height = 10, dpi = 300)
  cat("✅ VIZ 8 créée: Top performers par dimension\n")
} else {
  cat("⚠️ VIZ 8 non créée: données insuffisantes\n")
}

# ========== VIZ 9: Matrice de corrélation ==========
# Sélectionner et vérifier les données
cor_data <- df %>%
  select(all_of(stars_columns)) %>%
  na.omit()

if(nrow(cor_data) > 10) {
  cor_matrix <- cor(cor_data)
  
  # Noms des dimensions
  dim_names <- c("Work-Life\nBalance", "Culture &\nValues", 
                 "Career\nOpportunities", "Compensation &\nBenefits", 
                 "Senior\nManagement")
  
  rownames(cor_matrix) <- colnames(cor_matrix) <- dim_names
  
  cor_melted <- melt(cor_matrix)
  
  p9 <- cor_melted %>%
    mutate(
      Var1 = factor(Var1, levels = rev(dim_names)),
      Var2 = factor(Var2, levels = dim_names)
    ) %>%
    ggplot(aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = format(round(value, 2), nsmall = 2)), 
              color = "white", 
              fontface = "bold",
              size = 4) +
    scale_fill_gradient2(
      low = "#3498db",
      mid = "white",
      high = "#e74c3c",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Coefficient de\ncorrélation"
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = "Matrice de Corrélation entre les Dimensions de Satisfaction",
      subtitle = "Relations entre les différentes dimensions d'évaluation",
      x = NULL,
      y = NULL
    ) +
    theme_glassdoor() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0, size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
  
  ggsave("resultats_analyse/viz_09_correlation_matrix.png", p9,
         width = 10, height = 8, dpi = 300)
  cat("✅ VIZ 9 créée: Matrice de corrélation\n")
} else {
  cat("⚠️ VIZ 9 non créée: données insuffisantes\n")
}

# ========== VIZ 10: Dashboard ==========
if(exists("p6")) {
  dashboard <- (p1 | p2) / (p3 | p6) / (p5 | p7) +
    plot_annotation(
      title = "Dashboard d'Analyse Glassdoor - Avis Employés",
      subtitle = "Analyse comparative des entreprises tech",
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  
  ggsave("resultats_analyse/viz_10_dashboard_complet.png", dashboard,
         width = 20, height = 22, dpi = 300)
  cat("✅ VIZ 10 créée: Dashboard complet\n")
} else {
  cat("⚠️ VIZ 10 non créée: visualisations manquantes\n")
}

# ========== MESSAGE FINAL ==========
cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("✅ VISUALISATIONS GÉNÉRÉES AVEC SUCCÈS !\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n\n")

cat("📁 Fichiers créés dans 'resultats_analyse/' :\n")
files <- list.files("resultats_analyse", pattern = "^viz_.*\\.png$")
if(length(files) > 0) {
  for(file in sort(files)) {
    file_size <- file.info(paste0("resultats_analyse/", file))$size / 1024
    cat(sprintf("   • %-30s (%s Ko)\n", file, round(file_size, 1)))
  }
} else {
  cat("   (Aucun fichier trouvé)\n")
}

cat("\n🎯 Tous les graphiques sont prêts pour votre rapport Quarto !\n")


#visualisation code that works 
# # ================================================================================
# # 📊 VISUALISATIONS PROFESSIONNELLES - ANALYSE GLASSDOOR
# # À exécuter après le script d'analyse principal
# # ================================================================================

# library(tidyverse)
# library(scales)
# library(patchwork)
# library(viridis)
# library(ggthemes)
# library(here)

# # ========== CHARGEMENT ET PRÉPARATION DES DONNÉES ==========
# df <- read_csv(here("data", "raw", "employee_reviews.csv"), show_col_types = FALSE)

# # Nettoyer les noms de colonnes
# df <- df %>% janitor::clean_names()

# # 🔥 CORRECTION #1: Renommer les colonnes avec fautes
# df <- df %>%
#   rename_with(~ ifelse(.x == "carrer_opportunities_stars", 
#                       "career_opportunities_stars", .x)) %>%
#   rename_with(~ ifelse(.x == "senior_mangemnet_stars", 
#                       "senior_management_stars", .x))

# # 🔥 CORRECTION #2: Convertir en numérique
# stars_columns <- c("work_balance_stars", "culture_values_stars", 
#                    "career_opportunities_stars", "comp_benefit_stars", 
#                    "senior_management_stars")

# for(col in stars_columns) {
#   if(col %in% names(df)) {
#     df[[col]] <- as.numeric(as.character(df[[col]]))
#   }
# }

# # Extraire l'année
# df <- df %>%
#   mutate(
#     review_year = case_when(
#       str_detect(dates, "\\d{4}") ~ as.numeric(str_extract(dates, "\\d{4}")),
#       TRUE ~ NA_real_
#     )
#   )

# # Créer le dossier pour les figures
# figures_dir <- here("outputs", "figures")
# dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

# # Thème personnalisé
# theme_glassdoor <- function() {
#   theme_minimal() +
#     theme(
#       plot.title = element_text(face = "bold", size = 14),
#       plot.subtitle = element_text(size = 10, color = "gray40"),
#       axis.title = element_text(size = 11),
#       legend.position = "bottom",
#       panel.grid.minor = element_blank()
#     )
# }

# # Palette de couleurs
# company_colors <- c(
#   "amazon" = "#FF9900",
#   "microsoft" = "#00A4EF",
#   "apple" = "#A2AAAD",
#   "google" = "#4285F4",
#   "facebook" = "#1877F2",
#   "netflix" = "#E50914"
# )

# # ========== SUITE DES VISUALISATIONS ==========
# # (Gardez tout le reste de votre code visualisations après cette ligne)

# # 📊 VIZ 1: Distribution des notes globales ----
# p1 <- df %>%
#   ggplot(aes(x = overall_ratings)) +
#   geom_histogram(binwidth = 0.5, fill = "#3498db", color = "white", alpha = 0.85) +
#   geom_vline(
#     aes(xintercept = mean(overall_ratings, na.rm = TRUE)),
#     color = "#e74c3c",
#     linetype = "dashed",
#     linewidth = 1.2
#   ) +
#   annotate(
#     "text",
#     x = mean(df$overall_ratings, na.rm = TRUE) + 0.3,
#     y = Inf,
#     label = paste0("Moyenne: ", round(mean(df$overall_ratings, na.rm = TRUE), 2)),
#     vjust = 1.5,
#     color = "#e74c3c",
#     fontface = "bold"
#   ) +
#   scale_x_continuous(breaks = 1:5, limits = c(0.5, 5.5)) +
#   scale_y_continuous(labels = comma) +
#   labs(
#     title = "Distribution des Notes Globales",
#     subtitle = paste0(format(nrow(df), big.mark = ","), " avis analysés"),
#     x = "Note (1-5 étoiles)",
#     y = "Nombre d'avis"
#   ) +
#   theme_glassdoor()

# ggsave("resultats_analyse/viz_01_distribution_notes.png", p1, 
#        width = 10, height = 6, dpi = 300)
# print(p1)


# # 📊 VIZ 2: Comparaison des entreprises (notes moyennes) ----
# company_stats <- df %>%
#   group_by(company) %>%
#   summarise(
#     n_avis = n(),
#     note_moyenne = mean(overall_ratings, na.rm = TRUE),
#     ecart_type = sd(overall_ratings, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(note_moyenne))

# p2 <- company_stats %>%
#   ggplot(aes(x = reorder(company, note_moyenne), y = note_moyenne, fill = company)) +
#   geom_col(alpha = 0.85, width = 0.7) +
#   geom_errorbar(
#     aes(ymin = note_moyenne - ecart_type, ymax = note_moyenne + ecart_type),
#     width = 0.2,
#     color = "gray30"
#   ) +
#   geom_text(
#     aes(label = round(note_moyenne, 2)),
#     hjust = -0.3,
#     fontface = "bold",
#     size = 4
#   ) +
#   geom_text(
#     aes(label = paste0("n=", format(n_avis, big.mark = ","))),
#     hjust = -0.3,
#     vjust = 1.5,
#     size = 3,
#     color = "gray50"
#   ) +
#   coord_flip(ylim = c(0, 5.5)) +
#   scale_fill_manual(values = company_colors) +
#   labs(
#     title = "Classement des Entreprises par Note Moyenne",
#     subtitle = "Barres d'erreur = écart-type (variabilité des avis)",
#     x = NULL,
#     y = "Note Moyenne (1-5)"
#   ) +
#   theme_glassdoor() +
#   theme(legend.position = "none")

# ggsave("resultats_analyse/viz_02_classement_entreprises.png", p2,
#        width = 10, height = 6, dpi = 300)
# print(p2)


# # 📊 VIZ 3: Heatmap des dimensions par entreprise ----
# dimension_matrix <- df %>%
#   group_by(company) %>%
#   summarise(
#     across(ends_with("_stars"), ~ mean(.x, na.rm = TRUE)),
#     .groups = "drop"
#   ) %>%
#   pivot_longer(
#     cols = ends_with("_stars"),
#     names_to = "dimension",
#     values_to = "score"
#   ) %>%
#   mutate(
#     dimension = str_remove(dimension, "_stars") %>%
#       str_replace_all("_", "\n") %>%
#       str_to_title()
#   )

# p3 <- dimension_matrix %>%
#   ggplot(aes(x = dimension, y = company, fill = score)) +
#   geom_tile(color = "white", linewidth = 1.5) +
#   geom_text(aes(label = round(score, 2)), color = "white", fontface = "bold", size = 4) +
#   scale_fill_viridis(
#     option = "plasma",
#     limits = c(3, 4.5),
#     name = "Score Moyen"
#   ) +
#   labs(
#     title = "Heatmap des Dimensions de Satisfaction par Entreprise",
#     subtitle = "Plus la couleur est claire, meilleure est la note",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_glassdoor() +
#   theme(
#     axis.text.x = element_text(size = 9),
#     panel.grid = element_blank()
#   )

# ggsave("resultats_analyse/viz_03_heatmap_dimensions.png", p3,
#        width = 12, height = 6, dpi = 300)
# print(p3)


# # 📊 VIZ 4: Radar chart pour chaque entreprise ----
# library(fmsb)

# # Créer un radar chart pour chaque entreprise
# for(comp in unique(df$company)) {
#   company_dim <- df %>%
#     filter(company == comp) %>%
#     summarise(
#       "Work-Life\nBalance" = mean(work_balance_stars, na.rm = TRUE),
#       "Culture &\nValues" = mean(culture_values_stars, na.rm = TRUE),
#       "Career\nOpportunities" = mean(career_opportunities_stars, na.rm = TRUE),
#       "Compensation\n& Benefits" = mean(comp_benefit_stars, na.rm = TRUE),
#       "Senior\nManagement" = mean(senior_management_stars, na.rm = TRUE)
#     )
  
#   # Préparer données pour radar
#   radar_data <- rbind(
#     rep(5, 5),  # Max
#     rep(1, 5),  # Min
#     company_dim
#   )
  
#   png(paste0("resultats_analyse/viz_04_radar_", comp, ".png"),
#       width = 800, height = 800, res = 120)
  
#   radarchart(
#     radar_data,
#     axistype = 1,
#     pcol = company_colors[comp],
#     pfcol = alpha(company_colors[comp], 0.3),
#     plwd = 3,
#     plty = 1,
#     cglcol = "grey",
#     cglty = 1,
#     axislabcol = "grey20",
#     caxislabels = seq(1, 5, 1),
#     cglwd = 0.8,
#     vlcex = 1.1,
#     title = paste0("Profil de Satisfaction - ", toupper(comp))
#   )
  
#   dev.off()
# }


# # 📊 VIZ 5: Boxplot comparatif des dimensions ----
# dimensions_long <- df %>%
#   select(company, ends_with("_stars")) %>%
#   pivot_longer(
#     cols = ends_with("_stars"),
#     names_to = "dimension",
#     values_to = "rating"
#   ) %>%
#   mutate(
#     dimension = str_remove(dimension, "_stars") %>%
#       str_replace_all("_", " ") %>%
#       str_to_title()
#   )

# p5 <- dimensions_long %>%
#   ggplot(aes(x = dimension, y = rating, fill = dimension)) +
#   geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 0.5) +
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,
#     size = 3,
#     fill = "red",
#     color = "darkred"
#   ) +
#   scale_fill_brewer(palette = "Set3") +
#   labs(
#     title = "Distribution des Notes par Dimension",
#     subtitle = "Losange rouge = moyenne | Boîte = 50% des avis",
#     x = NULL,
#     y = "Note (1-5 étoiles)"
#   ) +
#   theme_glassdoor() +
#   theme(
#     legend.position = "none",
#     axis.text.x = element_text(angle = 25, hjust = 1)
#   )

# ggsave("resultats_analyse/viz_05_boxplot_dimensions.png", p5,
#        width = 12, height = 7, dpi = 300)
# print(p5)


# # 📊 VIZ 6: Évolution temporelle ----
# yearly_by_company <- df %>%
#   filter(!is.na(review_year), review_year >= 2008, review_year <= 2023) %>%
#   group_by(company, review_year) %>%
#   summarise(
#     n_avis = n(),
#     note_moyenne = mean(overall_ratings, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(n_avis >= 10)

# p6 <- yearly_by_company %>%
#   ggplot(aes(x = review_year, y = note_moyenne, color = company)) +
#   geom_line(linewidth = 1.2, alpha = 0.8) +
#   geom_point(aes(size = n_avis), alpha = 0.6) +
#   geom_smooth(method = "loess", se = FALSE, linetype = "dashed", linewidth = 0.8) +
#   scale_color_manual(
#     values = company_colors,
#     name = "Entreprise"
#   ) +
#   scale_size_continuous(
#     name = "Nombre d'avis",
#     range = c(1, 6)
#   ) +
#   scale_x_continuous(breaks = seq(2008, 2023, 2)) +
#   labs(
#     title = "Évolution des Notes Moyennes dans le Temps",
#     subtitle = "Taille des points = volume d'avis | Ligne pointillée = tendance",
#     x = "Année",
#     y = "Note Moyenne"
#   ) +
#   theme_glassdoor()

# ggsave("resultats_analyse/viz_06_evolution_temporelle.png", p6,
#        width = 14, height = 7, dpi = 300)
# print(p6)


# # 📊 VIZ 7: Violin plot des notes par entreprise ----
# p7 <- df %>%
#   ggplot(aes(x = reorder(company, overall_ratings), y = overall_ratings, fill = company)) +
#   geom_violin(alpha = 0.7, trim = FALSE) +
#   geom_boxplot(width = 0.1, fill = "white", alpha = 0.8, outlier.shape = NA) +
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 23,
#     size = 3,
#     fill = "red"
#   ) +
#   coord_flip() +
#   scale_fill_manual(values = company_colors) +
#   labs(
#     title = "Distribution Détaillée des Notes par Entreprise",
#     subtitle = "Violin = densité de probabilité | Boîte = quartiles",
#     x = NULL,
#     y = "Note (1-5 étoiles)"
#   ) +
#   theme_glassdoor() +
#   theme(legend.position = "none")

# ggsave("resultats_analyse/viz_07_violin_entreprises.png", p7,
#        width = 10, height = 7, dpi = 300)
# print(p7)


# # 📊 VIZ 8 CORRIGÉE: Top/Bottom performers par dimension ----
# library(tidytext)

# # S'assurer que dimensions_long existe
# dimensions_long <- df %>%
#   select(company, ends_with("_stars")) %>%
#   pivot_longer(
#     cols = ends_with("_stars"),
#     names_to = "dimension",
#     values_to = "rating"
#   ) %>%
#   mutate(
#     dimension = str_remove(dimension, "_stars") %>%
#       str_replace_all("_", " ") %>%
#       str_to_title()
#   )

# # Filtrer les NA et avoir assez de données
# dimension_ranking <- dimensions_long %>%
#   filter(!is.na(rating)) %>%
#   group_by(company, dimension) %>%
#   summarise(
#     moyenne = mean(rating, na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   filter(n >= 10)  # Au moins 10 observations par combinaison

# # Top 3 par dimension
# p8_data <- dimension_ranking %>%
#   group_by(dimension) %>%
#   slice_max(moyenne, n = 3) %>%
#   ungroup()

# # Vérifier les données
# cat("Données pour VIZ 8:\n")
# print(p8_data)

# p8 <- p8_data %>%
#   ggplot(aes(x = reorder_within(company, moyenne, dimension), 
#              y = moyenne, 
#              fill = company)) +
#   geom_col(alpha = 0.85) +
#   geom_text(aes(label = round(moyenne, 2)), hjust = -0.2, size = 3) +
#   coord_flip(ylim = c(0, 5)) +
#   scale_x_reordered() +
#   scale_fill_manual(values = company_colors) +
#   facet_wrap(~dimension, scales = "free_y", ncol = 2) +
#   labs(
#     title = "Top 3 Entreprises par Dimension",
#     subtitle = "Qui excelle dans chaque catégorie?",
#     x = NULL,
#     y = "Score Moyen (1-5)"
#   ) +
#   theme_glassdoor() +
#   theme(
#     legend.position = "none",
#     strip.text = element_text(size = 10, face = "bold")
#   )

# ggsave("resultats_analyse/viz_08_top_performers.png", p8,
#        width = 14, height = 10, dpi = 300)
# print(p8)


# # 📊 VIZ 9 CORRIGÉE: Matrice de corrélation des dimensions ----
# # S'assurer que les données sont numériques
# cor_matrix <- df %>%
#   select(all_of(stars_columns)) %>%
#   cor(use = "pairwise.complete.obs")

# # Renommer pour plus de clarté
# dim_names <- c("Work-Life\nBalance", "Culture &\nValues", 
#                "Career\nOpportunities", "Compensation &\nBenefits", 
#                "Senior\nManagement")

# rownames(cor_matrix) <- colnames(cor_matrix) <- dim_names

# cor_melted <- melt(cor_matrix)

# # Réorganiser pour avoir Work-Life en bas
# cor_melted <- cor_melted %>%
#   mutate(
#     Var1 = factor(Var1, levels = rev(dim_names)),
#     Var2 = factor(Var2, levels = dim_names)
#   )

# p9 <- cor_melted %>%
#   ggplot(aes(x = Var2, y = Var1, fill = value)) +
#   geom_tile(color = "white", size = 0.5) +
#   geom_text(aes(label = round(value, 2)), 
#             color = "white", 
#             fontface = "bold",
#             size = 4) +
#   scale_fill_gradient2(
#     low = "#3498db",
#     mid = "white",
#     high = "#e74c3c",
#     midpoint = 0,
#     limits = c(-1, 1),
#     name = "Corrélation"
#   ) +
#   scale_x_discrete(position = "top") +
#   labs(
#     title = "Matrice de Corrélation entre les 5 Dimensions",
#     subtitle = "Plus la corrélation est élevée, plus les dimensions évoluent ensemble",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_glassdoor() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 0, size = 10),
#     axis.text.y = element_text(size = 10),
#     panel.grid = element_blank(),
#     legend.position = "right",
#     legend.key.height = unit(1.5, "cm")
#   )

# ggsave("resultats_analyse/viz_09_correlation_matrix.png", p9,
#        width = 10, height = 8, dpi = 300)
# print(p9)


# # 📊 VIZ 10: Dashboard récapitulatif ----
# # Créer un dashboard combiné avec patchwork
# dashboard <- (p1 | p2) / (p3) / (p6)

# ggsave("resultats_analyse/viz_10_dashboard_complet.png", dashboard,
#        width = 18, height = 16, dpi = 300)
# print(dashboard)


# # 💾 SAUVEGARDE COMPLÈTE ----
# cat("\n✅ TOUTES LES VISUALISATIONS ONT ÉTÉ GÉNÉRÉES!\n\n")
# cat("📁 Fichiers créés dans: resultats_analyse/\n")
# cat("   • viz_01_distribution_notes.png\n")
# cat("   • viz_02_classement_entreprises.png\n")
# cat("   • viz_03_heatmap_dimensions.png\n")
# cat("   • viz_04_radar_[entreprise].png (5 fichiers)\n")
# cat("   • viz_05_boxplot_dimensions.png\n")
# cat("   • viz_06_evolution_temporelle.png\n")
# cat("   • viz_07_violin_entreprises.png\n")
# cat("   • viz_08_top_performers.png\n")
# cat("   • viz_09_correlation_matrix.png\n")
# cat("   • viz_10_dashboard_complet.png\n")
# cat("\n🎨 Prêt pour intégration dans Quarto!\n")