# Comprehensive Survey Analysis in R (replacing SPSS analysis)
# Analysis of Drosophila Citizen Science Survey Data

# Load required libraries
library(haven)        # Reading SPSS files
library(dplyr)        # Data manipulation
library(ggplot2)      # Plotting
library(psych)        # Descriptive statistics
library(corrr)        # Correlations
library(car)          # ANOVA and diagnostics
library(broom)        # Tidy statistical outputs
library(tidyr)        # Data reshaping
library(scales)       # Formatting
library(knitr)        # Table formatting
library(RColorBrewer) # Color palettes
library(gridExtra)    # Multiple plots

# Set working directory
setwd("D:/GitHub/DrosophilaCitizenScience")

# Create results directory
dir.create("results", showWarnings = FALSE)

# Read SPSS data
cat("Loading survey data from SPSS file...\n")
survey_data <- read_sav("scripts/Survey Analysis SPSS.sav")

cat("Data loaded successfully!\n")
cat("Dimensions:", nrow(survey_data), "observations,", ncol(survey_data), "variables\n\n")

# Display variable information
cat("=== VARIABLE OVERVIEW ===\n")
print(names(survey_data))

# Convert SPSS labeled variables to factors where appropriate
survey_data <- survey_data %>%
  mutate_if(function(x) !is.null(attr(x, "labels")), as_factor)

# === 1. DESCRIPTIVE STATISTICS ===
cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Numeric variables summary
numeric_vars <- survey_data %>% select_if(is.numeric)
if(ncol(numeric_vars) > 0) {
  cat("Numeric variables summary:\n")
  desc_stats <- describe(numeric_vars)
  print(desc_stats)
  
  # Save descriptive statistics
  write.csv(desc_stats, "results/descriptive_statistics.csv")
}

# === 2. FREQUENCY TABLES ===
cat("\n=== FREQUENCY ANALYSIS ===\n")

# Create frequency tables for categorical variables
categorical_vars <- survey_data %>% select_if(function(x) is.factor(x) || is.character(x))

if(ncol(categorical_vars) > 0) {
  freq_results <- list()
  
  for(var in names(categorical_vars)) {
    cat("\nFrequency table for:", var, "\n")
    freq_table <- table(survey_data[[var]], useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    freq_df <- data.frame(
      Category = names(freq_table),
      Frequency = as.numeric(freq_table),
      Percentage = round(as.numeric(prop_table), 2)
    )
    
    print(freq_df)
    freq_results[[var]] <- freq_df
    
    # Save individual frequency table
    write.csv(freq_df, paste0("results/frequency_", var, ".csv"), row.names = FALSE)
  }
}

# === 3. CROSS-TABULATION ANALYSIS ===
cat("\n=== CROSS-TABULATION ANALYSIS ===\n")

# Example cross-tabulations (adjust variable names based on your data)
if(ncol(categorical_vars) >= 2) {
  vars <- names(categorical_vars)[1:min(2, ncol(categorical_vars))]
  
  if(length(vars) == 2) {
    cat("Cross-tabulation:", vars[1], "vs", vars[2], "\n")
    
    crosstab <- table(survey_data[[vars[1]]], survey_data[[vars[2]]])
    print(crosstab)
    
    # Chi-square test
    chi_test <- chisq.test(crosstab)
    cat("\nChi-square test results:\n")
    print(chi_test)
    
    # Save cross-tabulation
    write.csv(as.data.frame.matrix(crosstab), 
              paste0("results/crosstab_", vars[1], "_", vars[2], ".csv"))
  }
}

# === 4. CORRELATION ANALYSIS ===
cat("\n=== CORRELATION ANALYSIS ===\n")

if(ncol(numeric_vars) >= 2) {
  # Correlation matrix
  cor_matrix <- cor(numeric_vars, use = "complete.obs")
  print(round(cor_matrix, 3))
  
  # Correlation significance tests
  cor_test_results <- corr.test(numeric_vars, use = "complete")
  
  cat("\nCorrelation p-values:\n")
  print(round(cor_test_results$p, 3))
  
  # Save correlation results
  write.csv(cor_matrix, "results/correlation_matrix.csv")
  write.csv(cor_test_results$p, "results/correlation_pvalues.csv")
}

# === 5. T-TESTS (if applicable) ===
cat("\n=== T-TEST ANALYSIS ===\n")

# Example: Compare numeric variables by categorical groups
if(ncol(numeric_vars) >= 1 && ncol(categorical_vars) >= 1) {
  numeric_var <- names(numeric_vars)[1]
  categorical_var <- names(categorical_vars)[1]
  
  # Get unique levels of categorical variable
  groups <- unique(survey_data[[categorical_var]])
  groups <- groups[!is.na(groups)]
  
  if(length(groups) == 2) {
    cat("T-test comparing", numeric_var, "by", categorical_var, "\n")
    
    group1_data <- survey_data[survey_data[[categorical_var]] == groups[1], numeric_var]
    group2_data <- survey_data[survey_data[[categorical_var]] == groups[2], numeric_var]
    
    # Remove NA values
    group1_data <- group1_data[!is.na(group1_data)]
    group2_data <- group2_data[!is.na(group2_data)]
    
    if(length(group1_data) > 0 && length(group2_data) > 0) {
      t_test <- t.test(group1_data, group2_data)
      print(t_test)
      
      # Save t-test results
      t_test_df <- data.frame(
        Group1 = groups[1],
        Group2 = groups[2],
        Mean1 = mean(group1_data),
        Mean2 = mean(group2_data),
        t_statistic = t_test$statistic,
        df = t_test$parameter,
        p_value = t_test$p.value,
        CI_lower = t_test$conf.int[1],
        CI_upper = t_test$conf.int[2]
      )
      
      write.csv(t_test_df, "results/t_test_results.csv", row.names = FALSE)
    }
  }
}

# === 6. ANOVA (if applicable) ===
cat("\n=== ANOVA ANALYSIS ===\n")

if(ncol(numeric_vars) >= 1 && ncol(categorical_vars) >= 1) {
  numeric_var <- names(numeric_vars)[1]
  categorical_var <- names(categorical_vars)[1]
  
  # Check if we have enough groups for ANOVA
  groups <- table(survey_data[[categorical_var]])
  
  if(length(groups) >= 3) {
    cat("ANOVA:", numeric_var, "by", categorical_var, "\n")
    
    # Create formula
    formula_str <- paste(numeric_var, "~", categorical_var)
    
    # Perform ANOVA
    anova_model <- aov(as.formula(formula_str), data = survey_data)
    anova_summary <- summary(anova_model)
    
    print(anova_summary)
    
    # Post-hoc tests if significant
    if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
      cat("\nPost-hoc Tukey HSD test:\n")
      tukey_test <- TukeyHSD(anova_model)
      print(tukey_test)
    }
  }
}

# === 7. VISUALIZATION ===
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Histogram for numeric variables
if(ncol(numeric_vars) >= 1) {
  for(var in names(numeric_vars)[1:min(3, ncol(numeric_vars))]) {
    p <- ggplot(survey_data, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Distribution of", var),
           x = var, y = "Frequency")
    
    ggsave(paste0("results/histogram_", var, ".png"), p, width = 8, height = 6)
  }
}

# Bar plots for categorical variables
if(ncol(categorical_vars) >= 1) {
  for(var in names(categorical_vars)[1:min(3, ncol(categorical_vars))]) {
    p <- ggplot(survey_data, aes_string(x = var)) +
      geom_bar(fill = "steelblue", alpha = 0.7) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Distribution of", var),
           x = var, y = "Count")
    
    ggsave(paste0("results/barplot_", var, ".png"), p, width = 8, height = 6)
  }
}

# === 8. SUMMARY REPORT ===
cat("\n=== GENERATING SUMMARY REPORT ===\n")

# Create a summary report
summary_report <- data.frame(
  Metric = c("Total Observations", "Total Variables", "Numeric Variables", 
             "Categorical Variables", "Missing Data Patterns"),
  Value = c(nrow(survey_data), ncol(survey_data), ncol(numeric_vars),
            ncol(categorical_vars), sum(is.na(survey_data)))
)

write.csv(summary_report, "results/analysis_summary.csv", row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved in 'results/' directory:\n")
cat("- Descriptive statistics\n")
cat("- Frequency tables\n")
cat("- Cross-tabulations\n")
cat("- Correlation matrices\n")
cat("- Statistical tests (t-tests, ANOVA)\n")
cat("- Visualizations\n")
cat("- Summary report\n")

# Print session info
cat("\nR Session Info:\n")
print(sessionInfo())
