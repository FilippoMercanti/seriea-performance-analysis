# ============================================================
# PROGETTO DI COMPUTATIONAL STATISTICS
# Titolo: Analisi dei fattori che incidono sulle prestazioni
# delle squadre di Serie A - Stagione 2024/2025
# Studente: Filippo Mercanti
# Corso: Data Science for Economics, Business and Finance
# ============================================================

# Pulizia ambiente
rm(list=ls())

print(getwd()) # Verifica la cartella di lavoro

library(readr)  
library(dplyr)

# 1) LETTURA DEI FILE CSV
standard   <- read.csv("serieA_2024_standard.csv", sep=";", skip=1, stringsAsFactors = FALSE)
possession <- read.csv("serieA_2024_possession.csv", sep=";", skip=1, stringsAsFactors = FALSE)
defensive  <- read.csv("serieA_2024_Miscellaneous_Stats.csv", sep=";", skip=1, stringsAsFactors = FALSE)
goal_shot  <- read.csv("serieA_2024_goal_and_shot_creation.csv", sep=";", skip=2, stringsAsFactors = FALSE)
passing    <- read.csv("serieA_2024_passing.csv", sep=";", skip=1, stringsAsFactors = FALSE)
shooting   <- read.csv("serieA_2024_goal_and_xG.csv", sep = ";", skip = 1, stringsAsFactors = FALSE)
rank       <- read.csv("serieA_2024_classifica.csv", sep=";", stringsAsFactors = FALSE)

# 2) PULIZIA E FORMATTAZIONE DELLE SQUADRE
clean_squad <- function(df) {
  if("Squad" %in% names(df)) {
    df$Squad <- iconv(df$Squad, to = "UTF-8", sub = "")
    df$Squad <- gsub("[^[:alnum:][:space:]]", "", df$Squad)
    df$Squad <- gsub("^[0-9]+\\s*", "", df$Squad)
    df$Squad <- gsub("\\s+", " ", df$Squad)
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

# Funzione per convertire le virgole in punti e rendere numeriche le colonne
to_num <- function(x) {
  as.numeric(gsub(",", ".", as.character(x)))
}

# Gestione specifica della classifica per estrarre Punti e Squadra
rank_small <- rank[, c("Squad", "Pts")]
colnames(rank_small) <- c("Squad", "Points")

# Unione dei dataset relazionali
serieA <- standard %>%
  select(Squad, Gls) %>%
  inner_join(rank_small %>% select(Squad, Points), by = "Squad") %>%
  inner_join(possession %>% select(Squad, Poss), by = "Squad") %>%
  inner_join(passing %>% select(Squad, Short_Pass = Cmp.1, Long_pass = Cmp.3), by = "Squad") %>%
  inner_join(goal_shot %>% select(Squad, SCA), by = "Squad") %>%
  inner_join(defensive %>% select(Squad, Recov), by = "Squad") %>%
  inner_join(shooting %>% select(Squad, xG), by = "Squad") %>%
  mutate(across(c(Points, Gls, Poss, Short_Pass, Long_pass, SCA, Recov, xG), to_num))

cat("\nSquadre processate correttamente:", nrow(serieA), "su 20\n\n")

# ================================
# REGRESSIONE LINEARE: POINTS
# ================================
model_points <- lm(Points ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
print(summary(model_points))

# ================================
# REGRESSIONE LINEARE: GOALS
# ================================
model_goals <- lm(Gls ~ xG + Poss + SCA + Short_Pass + Long_pass + Recov, data = serieA)
print(summary(model_goals))

# ============================================================
# PCA SULLE SQUADRE DI SERIE A (Grafici a schermo)
# ============================================================
variables_pca <- c("Points", "Gls", "xG", "Poss", "SCA", "Short_Pass", "Long_pass", "Recov")   
data_pca <- serieA[, variables_pca]

pca_serieA <- prcomp(data_pca, center = TRUE, scale. = TRUE)

library(factoextra)
print(fviz_eig(pca_serieA, addlabels = TRUE, barfill = "steelblue", barcolor = "black"))

library(ggplot2)
scores_pca <- pca_serieA$x
disegno_pca <- data.frame(
  Squad = serieA$Squad,
  PC1   = scores_pca[, 1],
  PC2   = scores_pca[, 2]
)

p_pca <- ggplot(disegno_pca, aes(x = PC1, y = PC2, label = Squad)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(size = 4) +
  labs(title = "Squadre Serie A nel piano PC1–PC2", x = "PC1", y = "PC2") +
  theme_minimal()
print(p_pca)

print(fviz_pca_biplot(pca_serieA, repel = TRUE, col.var = "#2E9FDF", col.ind = "#696969", label = "all"))

# ================================
# CLUSTERING (Grafico a schermo)
# ================================
cluster_data <- serieA[, variables_pca]   
cluster_scaled <- scale(cluster_data)

set.seed(123)
km_serieA <- kmeans(cluster_scaled, centers = 3, nstart = 25)
serieA$Cluster <- factor(km_serieA$cluster)

pca_scores <- pca_serieA$x[, 1:2]
plot_pca_cluster <- data.frame(
  PC1     = pca_scores[, 1],
  PC2     = pca_scores[, 2],
  Cluster = serieA$Cluster,
  Squad   = serieA$Squad
)

p_cluster <- ggplot(plot_pca_cluster, aes(x = PC1, y = PC2, color = Cluster, label = Squad)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7, size = 3, show.legend = FALSE) +
  labs(title = "Cluster delle squadre di Serie A sulle prime due componenti principali", x = "PC1", y = "PC2") +
  theme_minimal()
print(p_cluster)

# ================================
# REGRESSIONE RIDOTTA E CONFRONTO
# ================================
model_goals_red <- lm(Gls ~ xG + SCA, data = serieA)

summary_pts  <- summary(model_points)
summary_gls  <- summary(model_goals)
summary_red  <- summary(model_goals_red)

confronto_modelli <- data.frame(
  Modello   = c("Points (completo)", "Goals (completo)", "Goals (ridotto xG + SCA)"),
  R2        = c(summary_pts$r.squared, summary_gls$r.squared, summary_red$r.squared),
  Adj_R2    = c(summary_pts$adj.r.squared, summary_gls$adj.r.squared, summary_red$adj.r.squared)
)
print(confronto_modelli)


