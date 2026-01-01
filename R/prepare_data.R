library(tidyverse)
library(here)

# Charger et préparer les données
df <- read_csv(here("data", "raw", "employee_reviews.csv"), show_col_types = FALSE) %>% 
  janitor::clean_names() %>%
  rename(
    career_opportunities_stars = carrer_opportunities_stars,
    senior_management_stars = senior_mangemnet_stars
  ) %>%
  mutate(
    across(ends_with("_stars"), ~ as.numeric(as.character(.x))),
    review_year = case_when(
      str_detect(dates, "\\d{4}") ~ as.numeric(str_extract(dates, "\\d{4}")),
      TRUE ~ NA_real_
    )
  )

# Sauvegarder les données préparées
saveRDS(df, here("data", "processed", "employee_reviews_prepared.rds"))

cat("✅ Données préparées et sauvegardées dans data/processed/employee_reviews_prepared.rds\n")
cat("   ", nrow(df), "observations\n")
cat("   ", ncol(df), "variables\n")
cat("   Années :", min(df$review_year, na.rm = TRUE), "-", max(df$review_year, na.rm = TRUE), "\n")