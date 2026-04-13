# =============================================================
# INFO 6105 Final Project - Complete Reproducible R Code
# What Makes a GitHub Repository Popular?
# Jayanth Adithya Kappagantula | NUID: 002304901
# Section 37196 (Tuesday) | Spring 2026
# =============================================================
# NOTE: All plots are saved automatically to a folder called
# "figures/" in your working directory. Nothing is displayed
# in the viewer - check the figures/ folder after running.
# =============================================================

# --- Install required packages (run once) ---
packages <- c(
  "dplyr",       # Data manipulation
  "ggplot2",     # Visualization
  "GGally",      # Scatterplot matrix
  "corrplot",    # Correlation heatmap
  "car",         # VIF, Levene's test
  "multcomp",    # Tukey HSD compact letter display
  "pROC",        # ROC / AUC
  "broom",       # Tidy model output
  "knitr",       # Kable tables
  "kableExtra",  # Formatted tables
  "scales"       # Comma formatting for plots
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

# --- Load libraries ---
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(car)
library(multcomp)
library(pROC)
library(broom)
library(knitr)
library(kableExtra)
library(scales)

# --- Create figures folder ---
if (!dir.exists("figures")) dir.create("figures")
message("All plots will be saved to: ", normalizePath("figures"))

# --- Load Data ---
# Dataset hosted publicly at:
# https://github.com/ThisisjayK/INFO6105_002304901_Final_Project_Data
data_url <- paste0(
  "https://raw.githubusercontent.com/ThisisjayK/",
  "INFO6105_002304901_Final_Project/",
  "f124f2970ad4288baf01db328716df1740934572/",
  "github_repos.csv"
)

if (!file.exists("github_repos.csv")) {
  message("Downloading github_repos.csv from repository...")
  download.file(data_url, destfile = "github_repos.csv", mode = "wb")
  message("Download complete.")
} else {
  message("Loading local github_repos.csv")
}

github_data <- read.csv("github_repos.csv", stringsAsFactors = FALSE)

# --- Data Cleaning ---
# Remove duplicate repositories by ID
github_data <- github_data[!duplicated(github_data$id), ]

# Drop rows missing any key variable
github_data <- github_data %>%
  filter(!is.na(stargazers_count), !is.na(forks_count),
         !is.na(watchers_count),   !is.na(open_issues_count),
         !is.na(size),             !is.na(language))

# Standardise language labels
github_data$language <- trimws(github_data$language)

# Keep only top 8 languages
top_langs <- c("Python", "JavaScript", "Java", "C++",
               "R", "Go", "TypeScript", "Ruby")
github_data <- github_data %>%
  filter(language %in% top_langs)

# Create binary popular variable (top 25% by stars)
github_data$popular <- as.integer(
  github_data$stargazers_count >= quantile(github_data$stargazers_count, 0.75)
)

# Convert has_wiki to integer (TRUE/FALSE -> 1/0)
github_data$has_wiki <- as.integer(github_data$has_wiki)

message("Rows after cleaning: ", nrow(github_data))
message("Language distribution:")
print(table(github_data$language))

# --- Load and Prepare Data ---
set.seed(6105)

# Factor encoding
github_data$language <- factor(github_data$language)
github_data$popular  <- factor(github_data$popular, levels = c(0, 1),
                                labels = c("Not Popular", "Popular"))

# Log(x+1) transformations for right-skewed count variables
github_data <- github_data %>%
  mutate(
    log_stars    = log(stargazers_count + 1),
    log_forks    = log(forks_count + 1),
    log_watchers = log(watchers_count + 1),
    log_issues   = log(open_issues_count + 1),
    log_size     = log(size + 1)
  )

# Binary outcome for logistic regression (1 = top 25% by stars)
github_data$popular_binary <- as.integer(github_data$popular == "Popular")


# =============================================================
# Analysis 1: Multiple Linear Regression
# =============================================================

# Note: watchers_count excluded due to perfect collinearity with stargazers_count
mlr_model   <- lm(log_stars ~ log_forks + open_issues_count + log_size,
                  data = github_data)
mlr_summary <- summary(mlr_model)

# Coefficient table
tidy(mlr_model, conf.int = TRUE)

# Model fit statistics
cat("R-squared:          ", round(mlr_summary$r.squared,     4), "\n")
cat("Adjusted R-squared: ", round(mlr_summary$adj.r.squared, 4), "\n")
cat("F-statistic:        ", round(mlr_summary$fstatistic[1], 2), "\n")

# VIF for multicollinearity
vif(mlr_model)

# Figure 1a: Diagnostic plots
png("figures/fig1a_mlr_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2))
plot(mlr_model, which = c(1, 2, 3, 5))
par(mfrow = c(1, 1))
dev.off()
message("Saved: figures/fig1a_mlr_diagnostics.png")

# Figure 1b: Scatterplot matrix
p_scatter <- ggpairs(
  github_data %>% dplyr::select(log_stars, log_forks, open_issues_count, log_size),
  lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)),
  upper = list(continuous = wrap("cor", size = 3)),
  diag  = list(continuous = wrap("densityDiag"))
)
ggsave("figures/fig1b_scatterplot_matrix.png", plot = p_scatter,
       width = 8, height = 8, dpi = 150)
message("Saved: figures/fig1b_scatterplot_matrix.png")

# Figure 1c: Correlation heatmap
cor_mat <- cor(github_data %>% dplyr::select(log_forks, open_issues_count, log_size),
               use = "complete.obs")
png("figures/fig1c_correlation_heatmap.png", width = 800, height = 700, res = 150)
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black")
dev.off()
message("Saved: figures/fig1c_correlation_heatmap.png")


# =============================================================
# Analysis 2: One-Way ANOVA
# =============================================================

# ANOVA F-test
anova_model   <- aov(log_stars ~ language, data = github_data)
anova_summary <- summary(anova_model)
print(anova_summary)

# Tukey HSD post-hoc
tukey_result <- TukeyHSD(anova_model)

# Compact letter display
tukey_cld <- multcomp::cld(multcomp::glht(anova_model,
               linfct = multcomp::mcp(language = "Tukey")))
print(tukey_cld)

# Shapiro-Wilk normality per group
github_data %>%
  group_by(language) %>%
  summarise(
    W = shapiro.test(sample(log_stars, min(n(), 5000)))$statistic,
    p = shapiro.test(sample(log_stars, min(n(), 5000)))$p.value
  )

# Levene's test for equal variances
leveneTest(log_stars ~ language, data = github_data)

# Effect size: eta-squared
ss_between <- anova_summary[[1]]["language", "Sum Sq"]
ss_total   <- sum(anova_summary[[1]][, "Sum Sq"])
cat("Eta-squared:", round(ss_between / ss_total, 4), "\n")

# Group summary statistics
github_data %>%
  group_by(language) %>%
  summarise(
    Mean_log_stars = round(mean(log_stars), 3),
    SD             = round(sd(log_stars), 3),
    n              = n(),
    SE             = round(sd(log_stars) / sqrt(n()), 3),
    CI_Low         = round(mean(log_stars) - 1.96 * sd(log_stars) / sqrt(n()), 3),
    CI_High        = round(mean(log_stars) + 1.96 * sd(log_stars) / sqrt(n()), 3)
  )

# Figure 2a: Boxplot of log(stars) by language
p_box <- ggplot(github_data, aes(x = reorder(language, log_stars, median),
                                  y = log_stars, fill = language)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  coord_flip() + theme_minimal() + theme(legend.position = "none") +
  labs(x = "Language", y = "log(stargazers_count + 1)",
       title = "Log(stars) Distribution by Language")
ggsave("figures/fig2a_boxplot_by_language.png", plot = p_box,
       width = 8, height = 5, dpi = 150)
message("Saved: figures/fig2a_boxplot_by_language.png")

# Figure 2b: Group means bar chart with 95% CI
p_bar <- github_data %>%
  group_by(language) %>%
  summarise(
    mean_log_stars = mean(log_stars),
    se             = sd(log_stars) / sqrt(n())
  ) %>%
  ggplot(aes(x = reorder(language, mean_log_stars),
             y = mean_log_stars, fill = language)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_log_stars - 1.96 * se,
                    ymax = mean_log_stars + 1.96 * se), width = 0.3) +
  coord_flip() + theme_minimal() + theme(legend.position = "none") +
  labs(x = "Language", y = "Mean log(stargazers_count + 1)",
       title = "Group Means with 95% CI")
ggsave("figures/fig2b_group_means_ci.png", plot = p_bar,
       width = 8, height = 5, dpi = 150)
message("Saved: figures/fig2b_group_means_ci.png")


# =============================================================
# Analysis 3: Logistic Regression
# =============================================================

# Exploratory: popular rate by language
pop_rate <- github_data %>%
  group_by(language) %>%
  summarise(popular_rate = mean(popular == "Popular") * 100)

# Figure 3a: Popular rate by language
p_poprate <- ggplot(pop_rate, aes(x = reorder(language, popular_rate),
                                   y = popular_rate, fill = language)) +
  geom_bar(stat = "identity") +
  coord_flip() + theme_minimal() + theme(legend.position = "none") +
  labs(x = "Language", y = "% Popular", title = "Popular Repos by Language")
ggsave("figures/fig3a_popular_rate_by_language.png", plot = p_poprate,
       width = 8, height = 5, dpi = 150)
message("Saved: figures/fig3a_popular_rate_by_language.png")

# Figure 3b: log(forks) vs log(stars) by popular status
p_scatter2 <- ggplot(github_data, aes(x = log_forks, y = log_stars,
                                       colour = popular)) +
  geom_point(alpha = 0.4, size = 0.8) +
  theme_minimal() +
  labs(colour = "Status", title = "log(forks) vs log(stars) by Popularity")
ggsave("figures/fig3b_forks_vs_stars_popular.png", plot = p_scatter2,
       width = 8, height = 5, dpi = 150)
message("Saved: figures/fig3b_forks_vs_stars_popular.png")

# Fit logistic model
logit_model <- glm(popular_binary ~ log_forks + open_issues_count + log_size + has_wiki,
                   data   = github_data,
                   family = binomial(link = "logit"))

summary(logit_model)

# Odds ratios and confidence intervals
tidy(logit_model, exponentiate = TRUE, conf.int = TRUE)

# Confusion matrix at 0.5 threshold
pred_prob  <- predict(logit_model, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
actual     <- github_data$popular_binary
conf_mat   <- table(Predicted = pred_class, Actual = actual)
print(conf_mat)

# Performance metrics
TP <- conf_mat[2, 2]; TN <- conf_mat[1, 1]
FP <- conf_mat[2, 1]; FN <- conf_mat[1, 2]
cat("Accuracy:   ", round((TP + TN) / sum(conf_mat), 4), "\n")
cat("Sensitivity:", round(TP / (TP + FN), 4), "\n")
cat("Specificity:", round(TN / (TN + FP), 4), "\n")
cat("Precision:  ", round(TP / (TP + FP), 4), "\n")

# Figure 3c: ROC curve
roc_obj <- roc(actual, pred_prob)
png("figures/fig3c_roc_curve.png", width = 800, height = 700, res = 150)
plot(roc_obj, col = "#2d4a7a", lwd = 2, legacy.axes = TRUE,
     main = paste("ROC Curve, AUC =", round(auc(roc_obj), 2)))
dev.off()
message("Saved: figures/fig3c_roc_curve.png")
cat("AUC:", round(auc(roc_obj), 4), "\n")


# =============================================================
# Analysis 4: Kruskal-Wallis Nonparametric Test
# =============================================================

# Figure 4a: Violin plot
p_violin <- ggplot(github_data,
                   aes(x = reorder(language, stargazers_count, median),
                       y = stargazers_count + 1, fill = language)) +
  geom_violin(alpha = 0.7, trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() + theme_minimal() + theme(legend.position = "none") +
  labs(x = "Language", y = "Stars (log scale)",
       title = "Star Count Distribution by Language")
ggsave("figures/fig4a_violin_plot.png", plot = p_violin,
       width = 8, height = 5, dpi = 150)
message("Saved: figures/fig4a_violin_plot.png")

# Kruskal-Wallis test
kw_result <- kruskal.test(stargazers_count ~ language, data = github_data)
cat("H statistic:", round(kw_result$statistic, 3), "\n")
cat("p-value:    ", kw_result$p.value, "\n")

# Pairwise Wilcoxon with Bonferroni correction
pairwise.wilcox.test(github_data$stargazers_count, github_data$language,
                     p.adjust.method = "bonferroni")

# Comparison: ANOVA vs Kruskal-Wallis
cat("ANOVA F =", round(anova_summary[[1]]["language", "F value"], 3),
    "| p =", round(anova_summary[[1]]["language", "Pr(>F)"], 6), "\n")
cat("K-W   H =", round(kw_result$statistic, 3),
    "| p =", round(kw_result$p.value, 6), "\n")

# Spearman correlation: stars vs forks
spearman_result <- cor.test(github_data$stargazers_count, github_data$forks_count,
                            method = "spearman")
cat("Spearman rho:", round(spearman_result$estimate, 4), "\n")
cat("p-value:     ", spearman_result$p.value, "\n")

message("\nAll figures saved to: ", normalizePath("figures"))
message("Total figures saved: 8")

sessionInfo()
