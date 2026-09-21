# ============================================================
# COMPUTATIONAL STATISTICS PROJECT
# Title: Determinants of Team Performance in Serie A (2024/2025)
# Author: Filippo Mercanti
# Program: Data Science for Economics, Business and Finance
# ============================================================

# 1. ENVIRONMENT SETUP & DEPENDENCIES
rm(list = ls())

required_packages <- c("ggplot2", "factoextra", "lmtest", "car", "readr", "dplyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(factoextra)
library(lmtest)
library(car)
library(readr)
library(dplyr)

# 2. DATA LOADING & CLEANING (Relative Paths)
data_dir <- "../data"
output_dir <- "../output"

if (!dir.exists(data_dir)) {
  stop("ERROR: Unable to locate '../data' directory. Make sure 'data' is next to 'R'.")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

find_file <- function(pattern) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop(paste("File not found in directory", data_dir, "for pattern:", pattern))
  }
  return(files[1])
}

safe_read <- function(filepath, skip_lines = 0) {
  read.csv(filepath, sep = ";", skip = skip_lines, encoding = "UTF-8", stringsAsFactors = FALSE)
}

# Caricamento dei file (corretto il pattern per le statistiche difensive/misc)
standard   <- safe_read(find_file("standard"), 1)
possession <- safe_read(find_file("possession"), 1)
defensive  <- safe_read(find_file("misc"), 1) # Modificato da Miscellaneous_Stats a misc per sicurezza
goal_shot  <- safe_read(find_file("goal_and_shot_creation"), 2)
passing    <- safe_read(find_file("passing"), 1)
shooting   <- safe_read(find_file("xG"), 0)
rank       <- safe_read(find_file("classifica"), 0)

# Pulizia rigorosa dei nomi delle squadre per evitare problemi di join
clean_squad <- function(df) {
  if("Squad" %in% names(df)) {
    df$Squad <- iconv(df$Squad, to = "UTF-8", sub = "")
    df$Squad <- gsub("[^[:alnum:][:space:]]", "", df$Squad)
    df$Squad <- gsub("^[0-9]+\\s*", "", df$Squad) # Rimuove numeri iniziali (es. posizioni in classifica)
    df$Squad <- gsub("\\s+", " ", df$Squad)      # Sostituisce spazi multipli con uno singolo
    df$Squad <- trimws(df$Squad)
  }
  return(df)
}

standard   <- clean_squad(standard)
possession <- clean_squad(possession)
defensive  <- clean_squad(defensive)
goal_shot  <- clean_squad(goal_shot)
passing    <- clean_squad(passing)
shooting   <- clean_squad(shooting)
rank       <- clean_squad(rank)

to_num <- function(x) {
  as.numeric(gsub(",", ".", as.character(x)))
}

# 3. RELATIONAL MERGING & DATA PREPARATION
serieA <- standard %>%
  select(Squad, Gls) %>%
  inner_join(rank %>% select(Squad, Pts), by = "Squad") %>%
  rename(Points = Pts) %>%
  inner_join(possession %>% select(Squad, Poss), by = "Squad") %>%
  inner_join(passing %>% select(Squad, Short_Pass = Cmp.1, Long_pass = Cmp.3), by = "Squad") %>%
  inner_join(goal_shot %>% select(Squad, SCA), by = "Squad") %>%
  inner_join(defensive %>% select(Squad, Recov), by = "Squad") %>%
  inner_join(shooting %>% select(Squad, xG), by = "Squad") %>%
  mutate(across(c(Points, Gls, Poss, Short_Pass, Long_pass, SCA, Recov, xG), to_num))

cat("\n--- MERGED DATASETS CHECK ---")
cat("\nProcessed teams count:", nrow(serieA), "out of 20\n\n")

if(nrow(serieA) == 0) {
  stop("ERROR: Merged dataset is empty. Check column names or team name matching across CSVs.")
}

# 4. MULTIPLE LINEAR REGRESSION: POINTS
model_points <- lm(Points ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
cat("=== FULL MODEL: POINTS ===\n")
print(summary(model_points))

cat("\n--- VIF CHECK (Points Model) ---\n")
print(vif(model_points))

# 5. MULTIPLE LINEAR REGRESSION: GOALS
model_goals <- lm(Gls ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
cat("\n=== FULL MODEL: GOALS ===\n")
print(summary(model_goals))

model_goals_red <- lm(Gls ~ xG + SCA, data = serieA)
cat("\n=== REDUCED MODEL: GOALS ===\n")
print(summary(model_goals_red))

# 6. PRINCIPAL COMPONENT ANALYSIS (PCA)
var_pca <- c("Points", "Gls", "xG", "Poss", "SCA", "Short_Pass", "Long_pass", "Recov")

data_pca_complete <- serieA %>%
  select(all_of(c("Squad", var_pca))) %>%
  na.omit()

numeric_matrix <- as.matrix(data_pca_complete[, var_pca])

pca_serieA <- prcomp(numeric_matrix, center = TRUE, scale. = TRUE)

# Export Scree Plot
png(file.path(output_dir, "pca_screeplot.png"), width = 800, height = 600)
print(fviz_eig(pca_serieA, addlabels = TRUE, barfill = "steelblue", barcolor = "black") +
        labs(title = "Scree Plot - Explained Variance by Component"))
dev.off()

# Export Biplot
png(file.path(output_dir, "pca_biplot.png"), width = 900, height = 700)
print(fviz_pca_biplot(pca_serieA, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969",
                      title = "PCA Biplot - Serie A Teams (2024/2025)"))
dev.off()

# 7. K-MEANS CLUSTERING
scaled_data <- scale(numeric_matrix)

set.seed(123)
km_res <- kmeans(scaled_data, centers = 3, nstart = 25)
data_pca_complete$Cluster <- factor(km_res$cluster)

scores_pca <- as.data.frame(pca_serieA$x[, 1:2])
scores_pca$Squad <- data_pca_complete$Squad
scores_pca$Cluster <- data_pca_complete$Cluster

p_cluster <- ggplot(scores_pca, aes(x = PC1, y = PC2, color = Cluster, label = Squad)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7, size = 3.5, show.legend = FALSE) +
  labs(title = "K-Means Clustering of Teams in PC1-PC2 Space",
       x = "First Principal Component (PC1)",
       y = "Second Principal Component (PC2)") +
  theme_minimal()

ggsave(file.path(output_dir, "cluster_pca.png"), plot = p_cluster, width = 9, height = 6)

# 8. EXPORT MODEL COMPARISON SUMMARY
summary_pts <- summary(model_points)
summary_gls <- summary(model_goals)
summary_red <- summary(model_goals_red)

model_comparison <- data.frame(
  Model = c("Points (Full)", "Goals (Full)", "Goals (Reduced: xG + SCA)"),
  R_Squared = c(summary_pts$r.squared, summary_gls$r.squared, summary_red$r.squared),
  Adj_R_Squared = c(summary_pts$adj.r.squared, summary_gls$adj.r.squared, summary_red$adj.r.squared),
  Residual_SE = c(summary_pts$sigma, summary_gls$sigma, summary_red$sigma),
  Num_predictors = c(length(coef(model_points)) - 1, length(coef(model_goals)) - 1, length(coef(model_goals_red)) - 1)
)

write.csv(model_comparison, file.path(output_dir, "model_comparison_summary.csv"), row.names = FALSE)

cat("\n========================================================")
cat("\n Analysis completed successfully!")
cat("\n - High-resolution plots saved in 'output/'.")
cat("\n - Model summary exported to 'output/model_comparison_summary.csv'.")
cat("\n========================================================\n")
