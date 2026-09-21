# Serie A Performance Analysis (2024/2025)

**Author:** Filippo Mercanti  
**Program:** Data Science for Economics, Business and Finance  
**Project for:** Computational Statistics  

## What is this project about?

For my Computational Statistics project, I analyzed what drives team success in the Italian Serie A during the 2024/2025 season. 

The analysis integrates multiple official performance metrics—such as Expected Goals (`xG`), Shot-Creating Actions (`SCA`), passing data, defensive recoveries, and possession—to evaluate their impact on a team's **points in the table** and **goals scored**.

## What I did

1. **Data Loading & Cleaning:** Processed 7 official CSV datasets, cleaned team names using regular expressions and character encoding fixes, and merged them into a unified dataset of 20 teams.
2. **Multiple Linear Regressions:** Built econometric models to predict both League Points and Goals scored, evaluating model fit and statistical significance.
3. **Principal Component Analysis (PCA):** Performed dimensionality reduction to extract the main latent playstyle dimensions explaining overall team performance.
4. **K-Means Clustering:** Grouped all 20 teams into 3 distinct performance clusters based on standardized metrics.

## Project Structure

- `serieA_2024_standard.csv` (and other source datasets in the working directory) : Raw FBref CSV files.
- `main_analysis.R` : Main R script containing data processing, regression models, PCA, and clustering.

## Summary of Results

### Regression Models Performance

* **Points Model (Full):** $R^2 = 0.841$ | Adj. $R^2 = 0.768$ (Predictors: xG, Poss, SCA, Short_Pass, Long_pass, Recov)
* **Goals Model (Full):** $R^2 = 0.972$ | Adj. $R^2 = 0.958$ (Predictors: xG, Poss, SCA, Short_Pass, Long_pass, Recov)
* **Goals Model (Reduced):** $R^2 = 0.930$ | Adj. $R^2 = 0.922$ (Predictors: xG, SCA)

*Key Insight:* The reduced goals model proves that chance quality (`xG`) and creation (`SCA`) account for over 93% of goal-scoring variance, showing that raw possession is far less decisive than offensive efficiency.

## How to Run the Code

1. Open your project folder in RStudio and ensure all 7 CSV files are present in the working directory.
2. Open and run the R script (`main_analysis.R`). 
3. Statistical summaries will be printed to the console, and the PCA/Clustering interactive plots will be displayed directly in the RStudio **Plots** pane.

**Required Packages:** `readr`, `dplyr`, `factoextra`, `ggplot2`.
