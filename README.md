# What Makes a GitHub Repository Popular?

**INFO 6105: Data Science Engineering Methods and Tools**  
Jayanth Adithya Kappagantula | NUID: 002304901  
Section 37196 (Tuesday) | Spring 2026 | Northeastern University

---

## Overview

This project investigates what drives GitHub repository popularity, measured by star count, using data collected via the GitHub REST API across eight primary programming languages. Four statistical methods are applied: multiple linear regression, one-way ANOVA, logistic regression, and Kruskal-Wallis testing.

**Key finding:** Fork count is the single strongest predictor of popularity across all analyses (MLR coef = 0.816, logistic OR = 6.29, AUC = 0.882).

---

## Repository Contents

| File | Description |
|---|---|
| `github_repos.csv` | Dataset of 2,400 GitHub repositories (300 per language) collected via the GitHub Search API |
| `INFO6105_Final_Project.R` | Complete reproducible R script that runs all four analyses and saves figures to a `figures/` folder |

---

## Dataset

- **Source:** GitHub Search API (`https://api.github.com/search/repositories`)
- **Languages:** Python, JavaScript, Java, C++, R, Go, TypeScript, Ruby
- **Size:** 2,400 repositories, 300 per language, minimum 50 stars each
- **Key variables:** `stargazers_count`, `forks_count`, `open_issues_count`, `size`, `has_wiki`, `language`

---

## How to Reproduce

### Requirements

R 4.0 or later. The script will automatically install any missing packages on first run.

### Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/ThisisjayK/INFO6105_002304901_Final_Project.git
   cd INFO6105_002304901_Final_Project
   ```

2. Open `INFO6105_Final_Project.R` in RStudio (or any R environment).

3. Run the script. It will:
   - Load `github_repos.csv` from the working directory
   - Run all four analyses
   - Save 8 figures to a `figures/` folder that is created automatically

> **Note:** All plots are saved to `figures/` and are not displayed in the viewer. Check the folder after running.

### Output Figures

| File | Description |
|---|---|
| `fig1a_mlr_diagnostics.png` | MLR diagnostic plots (residuals, Q-Q, scale-location, leverage) |
| `fig1b_scatterplot_matrix.png` | Scatterplot matrix of numeric predictors |
| `fig1c_correlation_heatmap.png` | Correlation heatmap of predictors |
| `fig2a_boxplot_by_language.png` | log(stars) distribution by language |
| `fig2b_group_means_ci.png` | Group means with 95% CI by language |
| `fig3a_popular_rate_by_language.png` | Proportion of popular repos by language |
| `fig3b_forks_vs_stars_popular.png` | log(forks) vs log(stars) colored by popularity |
| `fig4a_violin_plot.png` | Star count distribution by language (log scale) |

---

## Results Summary

| Analysis | Key Result |
|---|---|
| Multiple Linear Regression | R^2 = 0.753, log(forks) dominant predictor (coef = 0.816, p < 0.001) |
| One-Way ANOVA | F(7, 2392) = 1655.5, p < 0.001, eta-squared = 0.829 |
| Logistic Regression | AUC = 0.882, accuracy = 83.4%, log(forks) only significant predictor |
| Kruskal-Wallis | H = 1637.134, p < 0.001, consistent with ANOVA |

---

## R Packages Used

`dplyr`, `ggplot2`, `GGally`, `corrplot`, `car`, `multcomp`, `pROC`, `broom`, `knitr`, `kableExtra`, `scales`

---

## Session Info

Developed and tested on R 4.5.2 (2025-10-31), macOS Tahoe 26.3.1, Apple Silicon (aarch64).
