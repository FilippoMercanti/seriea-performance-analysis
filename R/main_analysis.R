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

# Helper function to load CSV files by pattern matching (case-insensitive)
find_file <- function(pattern) {
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    stop(paste("File not found in directory", data_dir, "for pattern:", pattern))
  }
  return(files[1])
}

standard   <- read.csv(find_file("standard"), sep = ";", skip = 1, encoding = "UTF-8")
possession <- read.csv(find_file("possession"), sep = ";", skip = 1, encoding = "UTF-8")
defensive  <- read.csv(find_file("Miscellaneous_Stats"), sep = ";", skip = 1, encoding = "UTF-8")
goal_shot  <- read.csv(find_file("goal_and_shot_creation"), sep = ";", skip = 2, encoding = "UTF-8")
passing    <- read.csv(find_file("passing"), sep = ";", skip = 1, encoding = "UTF-8")
shooting   <- read.csv(find_file("xG"), sep = ";", encoding = "UTF-8") # No skip for goal & xG file
rank       <- read.csv(find_file("classifica"), sep = ";", encoding = "UTF-8")

# Helper function to clean team names (Universal Encoding & Regex Fix)
clean_squad <- function(df) {
  df$Squad <- iconv(df$Squad, to = "UTF-8", sub = "")
  df$Squad <- gsub("[^[:alnum:][:space:]]", "", df$Squad)
  df$Squad <- gsub("^[0-9]+\\s*", "", df$Squad) # Remove leading ranking numbers
  df$Squad <- trimws(df$Squad)
  return(df)
}

standard   <- clean_squad(standard)
possession <- clean_squad(possession)
defensive  <- clean_squad(defensive)
goal_shot  <- clean_squad(goal_shot)
passing    <- clean_squad(passing)
shooting   <- clean_squad(shooting)
rank       <- clean_squad(rank)

# 3. RELATIONAL MERGING
serieA <- standard %>%
  select(Squad, Gls) %>%
  inner_join(rank %>% select(Squad, Pts), by = "Squad") %>%
  rename(Points = Pts) %>%
  inner_join(possession %>% select(Squad, Poss), by = "Squad") %>%
  inner_join(passing %>% select(Squad, Short_Pass = Cmp.1, Long_pass = Cmp.3), by = "Squad") %>%
  inner_join(goal_shot %>% select(Squad, SCA), by = "Squad") %>%
  inner_join(defensive %>% select(Squad, Recov), by = "Squad") %>%
  inner_join(shooting %>% select(Squad, xG), by = "Squad")

cat("\n--- MERGED DATASETS CHECK ---")
cat("\nProcessed teams count:", nrow(serieA), "out of 20\n\n")

# 4. MULTIPLE LINEAR REGRESSION: POINTS
model_points <- lm(Points ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
cat("=== FULL MODEL: POINTS ===\n")
print(summary(model_points))

# 5. MULTIPLE LINEAR REGRESSION: GOALS
model_goals <- lm(Gls ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
cat("\n=== FULL MODEL: GOALS ===\n")
print(summary(model_goals))

model_goals_red <- lm(Gls ~ xG + SCA, data = serieA)
cat("\n=== REDUCED MODEL: GOALS ===\n")
print(summary(model_goals_red))

# 6. PRINCIPAL COMPONENT ANALYSIS (PCA)
var_pca <- c("Points", "Gls", "xG", "Poss", "SCA", "Short_Pass", "Long_pass", "Recov")
data_pca <- serieA[, var_pca]

pca_serieA <- prcomp(data_pca, center = TRUE, scale. = TRUE)

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
scaled_data <- scale(data_pca)

set.seed(123)
km_res <- kmeans(scaled_data, centers = 3, nstart = 25)
serieA$Cluster <- factor(km_res$cluster)

# Export Cluster Map in PC1-PC2 space
scores_pca <- as.data.frame(pca_serieA$x[, 1:2])
scores_pca$Squad <- serieA$Squad
scores_pca$Cluster <- serieA$Cluster

p_cluster <- ggplot(scores_pca, aes(x = PC1, y = PC2, color = Cluster, label = Squad)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7, size = 3.5, show.legend = FALSE) +
  labs(title = "K-Means Clustering of Teams in PC1-PC2 Space",
       x = "First Principal Component (PC1)",
       y = "Second Principal Component (PC2)") +
  theme_minimal()

ggsave(file.path(output_dir, "cluster_pca.png"), plot = p_cluster, width = 9, height = 6)

# 8. EXPORT MODEL COMPARISON SUMMARY (Enterprise Best Practice)
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