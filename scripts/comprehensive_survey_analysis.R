# ===================================================================
# COMPREHENSIVE CITIZEN SCIENCE SURVEY ANALYSIS
# Vienna Drosophila Project Survey Data Analysis
# ===================================================================

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
    readxl, # Excel file reading
    tidyverse, # Data manipulation and visualization
    psych, # Reliability analysis and descriptive statistics
    knitr, # Table formatting
    ggplot2, # Advanced plotting
    corrplot, # Correlation plots
    gridExtra, # Multiple plots
    scales, # Plot scaling
    RColorBrewer, # Color palettes
    reshape2, # Data reshaping
    ggsignif, # Add significance annotations to plots
    ggpubr, # Publication ready plots and stats
    rstatix # Pipe-friendly statistical functions
)

# Set working directory
setwd("D:/GitHub/DrosophilaCitizenScience")
dir.create("results", showWarnings = FALSE)
dir.create("results/plots", showWarnings = FALSE)

# ===================================================================
# DATA LOADING AND PREPARATION
# ===================================================================
cat("Loading survey data...\n")

# Read survey data
survey_data <- read_excel("data/Survey_Data_analysis - Kopie.xlsx", sheet = 1)

cat("Data loaded successfully!\n")
cat("Dimensions:", dim(survey_data), "\n")
cat("Column names:\n")
print(names(survey_data))

# ===================================================================
# 1. DESCRIPTIVE ANALYSIS OF SOCIODEMOGRAPHICS
# ===================================================================
cat("\n=== 1. SOCIODEMOGRAPHIC ANALYSIS ===\n")

# Create sociodemographic summary
create_frequency_table <- function(data, var_name) {
    freq_table <- data %>%
        count(!!sym(var_name), name = "Count") %>%
        mutate(
            Percentage = round((Count / sum(Count)) * 100, 2),
            Variable = var_name
        ) %>%
        rename(Value = !!sym(var_name))
    return(freq_table)
}

# Assuming column structure based on typical survey data
# Adjust column names based on actual data structure
sociodem_vars <- c()

# Try to identify sociodemographic variables
if ("Age" %in% names(survey_data) || any(grepl("age|Age|ALTER", names(survey_data), ignore.case = TRUE))) {
    age_col <- names(survey_data)[grepl("age|Age|ALTER", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, age_col)
}

if ("Gender" %in% names(survey_data) || any(grepl("gender|Gender|Geschlecht", names(survey_data), ignore.case = TRUE))) {
    gender_col <- names(survey_data)[grepl("gender|Gender|Geschlecht", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, gender_col)
}

# Education variable
if (any(grepl("education|Education|Bildung|Ausbildung", names(survey_data), ignore.case = TRUE))) {
    edu_col <- names(survey_data)[grepl("education|Education|Bildung|Ausbildung", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, edu_col)
}

# How did you hear about the project
if (any(grepl("erfahren|heard|Projekt", names(survey_data), ignore.case = TRUE))) {
    heard_col <- names(survey_data)[grepl("erfahren|heard|Projekt", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, heard_col)
}

# Previous CS engagement
if (any(grepl("teilgenommen|participated|CS|citizen", names(survey_data), ignore.case = TRUE))) {
    prev_cs_col <- names(survey_data)[grepl("teilgenommen|participated|CS|citizen", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, prev_cs_col)
}

# Active engagement (trap submission)
if (any(grepl("Falle|trap|beigetragen|contributed", names(survey_data), ignore.case = TRUE))) {
    active_col <- names(survey_data)[grepl("Falle|trap|beigetragen|contributed", names(survey_data), ignore.case = TRUE)][1]
    sociodem_vars <- c(sociodem_vars, active_col)
}

# Create comprehensive sociodemographic table
all_sociodem <- data.frame()

for (var in sociodem_vars) {
    if (var %in% names(survey_data)) {
        freq_table <- create_frequency_table(survey_data, var)
        all_sociodem <- rbind(all_sociodem, freq_table)
    }
}

# Save sociodemographic results
write.csv(all_sociodem, "results/sociodemographic_analysis.csv", row.names = FALSE)

# Create visualization for sociodemographics
if (nrow(all_sociodem) > 0) {
    p_sociodem <- ggplot(all_sociodem, aes(x = Value, y = Percentage, fill = Variable)) +
        geom_bar(stat = "identity") +
        facet_wrap(~Variable, scales = "free") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
            title = "Sociodemographic Characteristics",
            x = "Categories", y = "Percentage"
        ) +
        guides(fill = FALSE)

    ggsave("results/plots/sociodemographic_overview.png", p_sociodem, width = 12, height = 8)
}

cat("Sociodemographic analysis completed!\n")

# ===================================================================
# 2. MOTIVATION SUBSCALE CREATION AND CALCULATION
# ===================================================================
cat("\n=== 2. MOTIVATION SUBSCALE CALCULATION ===\n")

# Function to create motivation indices
create_motivation_indices <- function(data) {
    # NCV index (Nature Conservation Values)
    ncv_cols <- paste0("NCV", 1:5)
    ncv_available <- ncv_cols[ncv_cols %in% names(data)]
    if (length(ncv_available) > 0) {
        data$NCV_idx <- rowMeans(data[ncv_available], na.rm = TRUE)
        cat("✓ NCV_idx created from", length(ncv_available), "variables\n")
    }

    # SM index (Social Motives)
    sm_cols <- paste0("SM", 1:3)
    sm_available <- sm_cols[sm_cols %in% names(data)]
    if (length(sm_available) > 0) {
        data$SM_idx <- rowMeans(data[sm_available], na.rm = TRUE)
        cat("✓ SM_idx created from", length(sm_available), "variables\n")
    }

    # SPR index (Sociopolitical Responsibility) - excluding SPR_extra
    spr_cols <- paste0("SPR", 1:3)
    spr_available <- spr_cols[spr_cols %in% names(data)]
    if (length(spr_available) > 0) {
        data$SPR_idx <- rowMeans(data[spr_available], na.rm = TRUE)
        cat("✓ SPR_idx created from", length(spr_available), "variables (excluding SPR_extra)\n")
    }

    # CS index (Citizen Science)
    cs_cols <- paste0("CS", 1:5)
    cs_available <- cs_cols[cs_cols %in% names(data)]
    if (length(cs_available) > 0) {
        data$CS_idx <- rowMeans(data[cs_available], na.rm = TRUE)
        cat("✓ CS_idx created from", length(cs_available), "variables\n")
    }

    # CommOrg index (Communication & Organisation)
    commorg_cols <- c("COMM3", "ORG1", "ORG3", "ORG4")
    commorg_available <- commorg_cols[commorg_cols %in% names(data)]
    if (length(commorg_available) > 0) {
        data$CommOrg_idx <- rowMeans(data[commorg_available], na.rm = TRUE)
        cat("✓ CommOrg_idx created from", length(commorg_available), "variables\n")
    }

    # Overall motivation index
    motivation_indices <- c("NCV_idx", "SM_idx", "SPR_idx", "CS_idx", "CommOrg_idx")
    available_indices <- motivation_indices[motivation_indices %in% names(data)]

    if (length(available_indices) > 0) {
        data$MeanMotivationOverall <- rowMeans(data[available_indices], na.rm = TRUE)
        cat("✓ MeanMotivationOverall created from", length(available_indices), "indices\n")
    }

    return(data)
}

# Create motivation indices
survey_data <- create_motivation_indices(survey_data)

# ===================================================================
# 3. RELIABILITY ANALYSIS OF MOTIVATION SUBSCALES
# ===================================================================
cat("\n=== 3. RELIABILITY ANALYSIS ===\n")

# Function to calculate Cronbach's alpha
calculate_reliability <- function(data, items, scale_name) {
    available_items <- items[items %in% names(data)]

    if (length(available_items) >= 2) {
        subset_data <- data[available_items]
        subset_data <- subset_data[complete.cases(subset_data), ]

        if (nrow(subset_data) > 0 && ncol(subset_data) >= 2) {
            alpha_result <- psych::alpha(subset_data)
            cat("Reliability for", scale_name, ":")
            cat(" Cronbach's α =", round(alpha_result$total$std.alpha, 3))
            cat(" (", length(available_items), "items, N =", nrow(subset_data), ")\n")

            return(list(
                scale = scale_name,
                alpha = alpha_result$total$std.alpha,
                items = length(available_items),
                n = nrow(subset_data)
            ))
        }
    }

    cat("⚠ Cannot calculate reliability for", scale_name, "- insufficient data\n")
    return(NULL)
}

# Calculate reliability for each subscale
reliability_results <- list()

# NCV reliability
ncv_items <- paste0("NCV", 1:5)
reliability_results$NCV <- calculate_reliability(survey_data, ncv_items, "Nature Conservation Values")

# SM reliability
sm_items <- paste0("SM", 1:3)
reliability_results$SM <- calculate_reliability(survey_data, sm_items, "Social Motives")

# SPR reliability (excluding SPR_extra)
spr_items <- paste0("SPR", 1:3)
reliability_results$SPR <- calculate_reliability(survey_data, spr_items, "Sociopolitical Responsibility")

# CS reliability
cs_items <- paste0("CS", 1:5)
reliability_results$CS <- calculate_reliability(survey_data, cs_items, "Citizen Science")

# CommOrg reliability
commorg_items <- c("COMM3", "ORG1", "ORG3", "ORG4")
reliability_results$CommOrg <- calculate_reliability(survey_data, commorg_items, "Communication & Organisation")

# Create reliability summary table
reliability_df <- do.call(rbind, lapply(reliability_results, function(x) {
    if (!is.null(x)) {
        data.frame(
            Scale = x$scale,
            Cronbach_Alpha = round(x$alpha, 3),
            Items = x$items,
            N = x$n
        )
    }
}))

if (!is.null(reliability_df)) {
    write.csv(reliability_df, "results/reliability_analysis.csv", row.names = FALSE)
    cat("Reliability analysis saved to results/reliability_analysis.csv\n")
}

# ===================================================================
# 4. DESCRIPTIVE ANALYSIS OF MOTIVATION
# ===================================================================
cat("\n=== 4. MOTIVATION DESCRIPTIVE ANALYSIS ===\n")

# Create descriptive statistics for motivation indices
motivation_vars <- c("NCV_idx", "SM_idx", "SPR_idx", "CS_idx", "CommOrg_idx")
available_motivation_vars <- motivation_vars[motivation_vars %in% names(survey_data)]

# Add SPR_extra if available
if ("SPR_extra" %in% names(survey_data)) {
    available_motivation_vars <- c(available_motivation_vars, "SPR_extra")
}

motivation_descriptives <- data.frame()

for (var in available_motivation_vars) {
    if (var %in% names(survey_data)) {
        var_data <- survey_data[[var]]
        desc_stats <- data.frame(
            Variable = var,
            N = sum(!is.na(var_data)),
            Mean = round(mean(var_data, na.rm = TRUE), 3),
            SD = round(sd(var_data, na.rm = TRUE), 3),
            Min = round(min(var_data, na.rm = TRUE), 3),
            Max = round(max(var_data, na.rm = TRUE), 3),
            Median = round(median(var_data, na.rm = TRUE), 3)
        )
        motivation_descriptives <- rbind(motivation_descriptives, desc_stats)
    }
}

# Save descriptive statistics
write.csv(motivation_descriptives, "results/motivation_descriptives.csv", row.names = FALSE)

# Print descriptive table
cat("Motivation Descriptive Statistics:\n")
print(kable(motivation_descriptives, caption = "Descriptive Statistics for Motivation Scales"))

# Create boxplots for motivation scales with significance testing
if (length(available_motivation_vars) > 0) {
    # Prepare data with clean labels
    motivation_long <- survey_data %>%
        select(all_of(available_motivation_vars)) %>%
        pivot_longer(cols = everything(), names_to = "Scale", values_to = "Score") %>%
        filter(!is.na(Score)) %>%
        mutate(Scale_Clean = case_when(
            Scale == "NCV_idx" ~ "Nature Conservation\nValues",
            Scale == "SM_idx" ~ "Social\nMotives",
            Scale == "SPR_idx" ~ "Sociopolitical\nResponsibility",
            Scale == "CS_idx" ~ "Citizen\nScience",
            Scale == "CommOrg_idx" ~ "Communication &\nOrganisation",
            Scale == "SPR_extra" ~ "Curiosity to\nLearn More",
            TRUE ~ Scale
        ))

    # Perform statistical testing (ANOVA and post-hoc tests)
    cat("Performing statistical tests for motivation scale differences...\n")

    # ANOVA test
    if (length(unique(motivation_long$Scale)) > 2) {
        anova_result <- aov(Score ~ Scale, data = motivation_long)
        anova_summary <- summary(anova_result)
        anova_p <- anova_summary[[1]][["Pr(>F)"]][1]

        cat("ANOVA F-test p-value:", round(anova_p, 6), "\n")

        # Post-hoc pairwise comparisons if ANOVA is significant
        if (anova_p < 0.05) {
            cat("ANOVA significant - performing post-hoc pairwise comparisons...\n")

            # Pairwise t-tests with Bonferroni correction
            pairwise_results <- motivation_long %>%
                pairwise_t_test(Score ~ Scale, p.adjust.method = "bonferroni") %>%
                mutate(significance = case_when(
                    p.adj < 0.001 ~ "***",
                    p.adj < 0.01 ~ "**",
                    p.adj < 0.05 ~ "*",
                    TRUE ~ "ns"
                )) %>%
                filter(p.adj < 0.05) # Only keep significant comparisons

            # Print significant comparisons
            if (nrow(pairwise_results) > 0) {
                cat("Significant pairwise comparisons (Bonferroni corrected):\n")
                print(pairwise_results %>% select(group1, group2, p.adj, significance))

                # Save statistical results
                write.csv(pairwise_results, "results/motivation_pairwise_tests.csv", row.names = FALSE)
            } else {
                cat("No significant pairwise differences found after correction.\n")
                pairwise_results <- data.frame() # Empty dataframe
            }
        } else {
            cat("ANOVA not significant - no post-hoc tests performed.\n")
            pairwise_results <- data.frame() # Empty dataframe
        }
    } else {
        cat("Only 2 or fewer groups - performing t-test instead of ANOVA.\n")
        if (length(unique(motivation_long$Scale)) == 2) {
            t_test_result <- t.test(Score ~ Scale, data = motivation_long)
            cat("T-test p-value:", round(t_test_result$p.value, 6), "\n")
        }
        pairwise_results <- data.frame() # Empty dataframe
    }

    # Create boxplot with significance annotations
    p_boxplot <- ggplot(motivation_long, aes(x = Scale_Clean, y = Score, fill = Scale_Clean)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
        geom_jitter(width = 0.2, alpha = 0.4, size = 0.8) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11),
            legend.position = "none"
        ) +
        labs(
            title = "Distribution of Motivation Scale Scores",
            subtitle = paste0(
                "Boxplots with individual data points | ANOVA p = ",
                ifelse(exists("anova_p"), round(anova_p, 4), "N/A")
            ),
            x = "Motivation Scale",
            y = "Score"
        ) +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        scale_y_continuous(limits = c(1, 5.2)) # Extend y-axis for significance annotations

    # Add significance annotations if there are significant differences
    if (exists("pairwise_results") && nrow(pairwise_results) > 0) {
        cat("Adding significance annotations to plot...\n")

        # Map original variable names to clean names for annotation
        clean_name_mapping <- c(
            "NCV_idx" = "Nature Conservation\nValues",
            "SM_idx" = "Social\nMotives",
            "SPR_idx" = "Sociopolitical\nResponsibility",
            "CS_idx" = "Citizen\nScience",
            "CommOrg_idx" = "Communication &\nOrganisation",
            "SPR_extra" = "Curiosity to\nLearn More"
        )

        # Prepare significance annotations
        sig_annotations <- pairwise_results %>%
            mutate(
                group1_clean = clean_name_mapping[group1],
                group2_clean = clean_name_mapping[group2]
            ) %>%
            filter(!is.na(group1_clean) & !is.na(group2_clean))

        # Add significance brackets (only for most significant comparisons to avoid overcrowding)
        if (nrow(sig_annotations) > 0) {
            # Select top 3 most significant comparisons to avoid overcrowding
            top_sig <- sig_annotations %>%
                arrange(p.adj) %>%
                head(3)

            for (i in 1:nrow(top_sig)) {
                y_position <- 5.0 + (i - 1) * 0.1 # Stagger annotation heights

                p_boxplot <- p_boxplot +
                    annotate("segment",
                        x = which(levels(factor(motivation_long$Scale_Clean)) == top_sig$group1_clean[i]),
                        xend = which(levels(factor(motivation_long$Scale_Clean)) == top_sig$group2_clean[i]),
                        y = y_position, yend = y_position,
                        color = "black", size = 0.5
                    ) +
                    annotate("text",
                        x = (which(levels(factor(motivation_long$Scale_Clean)) == top_sig$group1_clean[i]) +
                            which(levels(factor(motivation_long$Scale_Clean)) == top_sig$group2_clean[i])) / 2,
                        y = y_position + 0.05,
                        label = top_sig$significance[i],
                        size = 4, fontface = "bold"
                    )
            }
        }
    }

    # Save enhanced boxplot
    ggsave("results/plots/motivation_boxplots_with_significance.png", p_boxplot,
        width = 12, height = 8, dpi = 300
    )

    cat("Enhanced boxplot with significance testing saved!\n")

    # Bar chart of means (keeping existing code)
    mean_data <- motivation_descriptives %>%
        mutate(Scale_Clean = case_when(
            Variable == "NCV_idx" ~ "Nature Conservation\nValues",
            Variable == "SM_idx" ~ "Social\nMotives",
            Variable == "SPR_idx" ~ "Sociopolitical\nResponsibility",
            Variable == "CS_idx" ~ "Citizen\nScience",
            Variable == "CommOrg_idx" ~ "Communication &\nOrganisation",
            Variable == "SPR_extra" ~ "Curiosity to\nLearn More",
            TRUE ~ Variable
        ))

    p_barplot <- ggplot(mean_data, aes(x = reorder(Scale_Clean, Mean), y = Mean, fill = Scale_Clean)) +
        geom_col(alpha = 0.8, color = "black", size = 0.3) +
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
        coord_flip() +
        theme_minimal() +
        labs(
            title = "Mean Scores of Motivation Scales",
            subtitle = "Error bars show ± 1 standard deviation",
            x = "Motivation Scale", y = "Mean Score"
        ) +
        guides(fill = FALSE) +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        geom_text(aes(label = sprintf("%.2f", Mean)), hjust = -0.1, size = 3)

    ggsave("results/plots/motivation_means_barplot.png", p_barplot, width = 10, height = 6)
}

# ===================================================================
# 5. CORRELATION ANALYSIS OF MOTIVATION FACTORS
# ===================================================================
cat("\n=== 5. MOTIVATION CORRELATION ANALYSIS ===\n")

if (length(available_motivation_vars) >= 2) {
    # Calculate correlation matrix
    cor_data <- survey_data[available_motivation_vars]
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

    # Significance testing
    cor_test <- psych::corr.test(cor_data, use = "pairwise")

    cat("Correlation Matrix:\n")
    print(round(cor_matrix, 3))

    cat("\nSignificance levels (p-values):\n")
    print(round(cor_test$p, 3))

    # Save correlation results
    write.csv(cor_matrix, "results/motivation_correlations.csv")
    write.csv(cor_test$p, "results/motivation_correlation_pvalues.csv")

    # Create correlation plot
    png("results/plots/motivation_correlation_plot.png", width = 800, height = 600)
    corrplot(cor_matrix,
        method = "color",
        type = "upper",
        order = "original",
        col = colorRampPalette(c("#E31A1C", "white", "#1F78B4"))(100),
        tl.col = "black",
        tl.srt = 45,
        addCoef.col = "black",
        number.cex = 0.8,
        title = "Correlation Matrix of Motivation Scales",
        mar = c(0, 0, 2, 0)
    )
    dev.off()
}

# ===================================================================
# 6. ENGAGEMENT ANALYSIS (WITH COMMA-SEPARATED RESPONSE SPLITTING)
# ===================================================================
cat("\n=== 6. ENGAGEMENT ANALYSIS ===\n")

# Enhanced function to analyze engagement variables with comma splitting
analyze_engagement_var_split <- function(data, var_pattern, analysis_name) {
    matching_cols <- names(data)[grepl(var_pattern, names(data), ignore.case = TRUE)]

    if (length(matching_cols) > 0) {
        cat("\n", analysis_name, ":\n")

        all_results <- list()

        for (col in matching_cols) {
            cat("Variable:", col, "\n")

            # Get non-NA responses
            responses <- data[[col]][!is.na(data[[col]])]
            responses <- responses[responses != ""]

            if (length(responses) == 0) {
                cat("No valid responses found for", col, "\n")
                next
            }

            # Original analysis (before splitting)
            cat("\n--- Original Response Analysis (before splitting) ---\n")
            freq_table_orig <- table(responses)
            prop_table_orig <- round(prop.table(freq_table_orig) * 100, 2)

            print(freq_table_orig)
            cat("Percentages:\n")
            print(prop_table_orig)

            # Save original analysis
            result_df_orig <- data.frame(
                Response = names(freq_table_orig),
                Count = as.numeric(freq_table_orig),
                Percentage = as.numeric(prop_table_orig)
            )

            filename_orig <- paste0(
                "results/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                gsub("[^A-Za-z0-9]", "_", col), "_original.csv"
            )
            write.csv(result_df_orig, filename_orig, row.names = FALSE)

            # Split responses by comma and analyze
            cat("\n--- Split Response Analysis (individual categories) ---\n")

            # Split each response by comma and trim whitespace
            all_split_responses <- unlist(lapply(responses, function(x) {
                split_resp <- strsplit(as.character(x), ",")[[1]]
                trimws(split_resp) # Remove leading/trailing whitespace
            }))

            # Remove empty responses
            all_split_responses <- all_split_responses[all_split_responses != ""]

            # Create frequency table for split responses
            freq_table_split <- table(all_split_responses)
            prop_table_split <- round(prop.table(freq_table_split) * 100, 2)

            # Sort by frequency (descending)
            freq_table_split <- sort(freq_table_split, decreasing = TRUE)
            prop_table_split <- prop_table_split[names(freq_table_split)]

            print(freq_table_split)
            cat("Percentages (based on individual mentions):\n")
            print(prop_table_split)
            cat("\n")

            # Save split analysis
            result_df_split <- data.frame(
                Response_Category = names(freq_table_split),
                Count = as.numeric(freq_table_split),
                Percentage = as.numeric(prop_table_split)
            )

            filename_split <- paste0(
                "results/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                gsub("[^A-Za-z0-9]", "_", col), "_split.csv"
            )
            write.csv(result_df_split, filename_split, row.names = FALSE)

            # Store results for plotting
            all_results[[col]] <- list(
                original = result_df_orig,
                split = result_df_split,
                total_responses = length(responses),
                total_mentions = length(all_split_responses)
            )

            # Create visualization for split responses
            if (nrow(result_df_split) > 0) {
                # Prepare data for plotting (shorten long labels)
                plot_data <- result_df_split %>%
                    mutate(
                        Response_Short = ifelse(
                            nchar(Response_Category) > 50,
                            paste0(substr(Response_Category, 1, 47), "..."),
                            Response_Category
                        )
                    )

                # Create bar plot
                p_impact <- ggplot(plot_data, aes(x = reorder(Response_Short, Count), y = Count)) +
                    geom_col(fill = "#2E8B57", alpha = 0.8, color = "black", size = 0.3) +
                    geom_text(aes(label = paste0(Count, " (", round(Percentage, 1), "%)")),
                        hjust = -0.1, size = 3
                    ) +
                    coord_flip() +
                    theme_minimal() +
                    theme(
                        axis.text.y = element_text(size = 9),
                        plot.title = element_text(size = 12, face = "bold"),
                        plot.subtitle = element_text(size = 10)
                    ) +
                    labs(
                        title = paste("Impact Categories -", analysis_name),
                        subtitle = paste(
                            "Total responses:", length(responses),
                            "| Total mentions:", length(all_split_responses)
                        ),
                        x = "Impact Category",
                        y = "Number of Mentions"
                    ) +
                    scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

                # Save plot
                plot_filename <- paste0(
                    "results/plots/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                    gsub("[^A-Za-z0-9]", "_", col), "_split_analysis.png"
                )
                ggsave(plot_filename, p_impact, width = 12, height = 8, dpi = 300)
                cat("Plot saved:", plot_filename, "\n")

                # Create a pie chart for better visualization of proportions
                p_pie <- ggplot(plot_data, aes(x = "", y = Count, fill = Response_Short)) +
                    geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
                    coord_polar("y", start = 0) +
                    theme_void() +
                    theme(
                        legend.position = "right",
                        legend.text = element_text(size = 8),
                        plot.title = element_text(size = 12, face = "bold")
                    ) +
                    labs(
                        title = paste("Impact Categories Distribution -", analysis_name),
                        fill = "Impact Category"
                    ) +
                    scale_fill_brewer(type = "qual", palette = "Set3")

                # Save pie chart
                pie_filename <- paste0(
                    "results/plots/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                    gsub("[^A-Za-z0-9]", "_", col), "_pie_chart.png"
                )
                ggsave(pie_filename, p_pie, width = 10, height = 8, dpi = 300)
                cat("Pie chart saved:", pie_filename, "\n")
            }
        }

        return(list(variables = matching_cols, results = all_results))
    } else {
        cat("No variables found for", analysis_name, "\n")
        return(list(variables = character(0), results = list()))
    }
}

# Enhanced Impact analysis with splitting
cat("Analyzing impact responses with comma-separated splitting...\n")
impact_analysis <- analyze_engagement_var_split(survey_data, "impact|verändert|changed", "Impact_Analysis")

# Enhanced Contentment analysis with splitting
cat("Analyzing contentment responses with comma-separated splitting...\n")
contentment_analysis <- analyze_engagement_var_split(survey_data, "zufrieden|contentment|satisfied", "Contentment_Analysis")

# Enhanced Future involvement analysis with splitting
cat("Analyzing future involvement responses with comma-separated splitting...\n")
future_analysis <- analyze_engagement_var_split(survey_data, "weiterhin|future|teilnehmen", "Future_Involvement_Analysis")

# Create summary of split analysis results
cat("\n=== SPLIT ANALYSIS SUMMARY ===\n")
if (length(impact_analysis$results) > 0) {
    for (var_name in names(impact_analysis$results)) {
        result <- impact_analysis$results[[var_name]]
        cat("Variable:", var_name, "\n")
        cat("  Original responses:", result$total_responses, "\n")
        cat("  Total mentions after splitting:", result$total_mentions, "\n")
        cat("  Unique categories identified:", nrow(result$split), "\n")

        # Show top 3 categories
        top_categories <- head(result$split, 3)
        cat("  Top categories:\n")
        for (i in 1:min(3, nrow(top_categories))) {
            cat(
                "    ", i, ".", top_categories$Response_Category[i],
                " (", top_categories$Count[i], " mentions, ",
                round(top_categories$Percentage[i], 1), "%)\n"
            )
        }
        cat("\n")
    }
}

# ===================================================================
# 7. FINAL QUESTIONS ANALYSIS
# ===================================================================
cat("\n=== 7. FINAL QUESTIONS ANALYSIS ===\n")

# Non-participation reasons
nonpartic_vars <- analyze_engagement_var(survey_data, "nicht.*teilnahme|non.*participation|gründe", "Non_Participation_Reasons")

# Improvement suggestions
improvement_vars <- analyze_engagement_var(survey_data, "verbesserung|improvement|vorschläge", "Improvement_Suggestions")

# Final comments
final_vars <- analyze_engagement_var(survey_data, "abschließend|final|mitteilen", "Final_Comments")

# ===================================================================
# 8. COMPREHENSIVE SUMMARY REPORT
# ===================================================================
cat("\n=== COMPREHENSIVE ANALYSIS SUMMARY ===\n")

# Create summary report
summary_report <- list(
    analysis_date = Sys.Date(),
    sample_size = nrow(survey_data),
    sociodemographic_vars = length(sociodem_vars),
    motivation_scales_created = sum(c("NCV_idx", "SM_idx", "SPR_idx", "CS_idx", "CommOrg_idx") %in% names(survey_data)),
    reliability_analyses = length(reliability_results),
    engagement_variables = length(c(impact_analysis$variables, contentment_analysis$variables, future_analysis$variables)),
    final_question_variables = length(c(nonpartic_vars, improvement_vars, final_vars)),
    impact_categories_identified = if (length(impact_analysis$results) > 0) nrow(impact_analysis$results[[1]]$split) else 0,
    total_impact_mentions = if (length(impact_analysis$results) > 0) impact_analysis$results[[1]]$total_mentions else 0
)

cat("Analysis completed successfully!\n")
cat("Sample size: N =", summary_report$sample_size, "\n")
cat("Sociodemographic variables analyzed:", summary_report$sociodemographic_vars, "\n")
cat("Motivation scales created:", summary_report$motivation_scales_created, "\n")
cat("Reliability analyses conducted:", summary_report$reliability_analyses, "\n")
cat("Engagement variables analyzed:", summary_report$engagement_variables, "\n")
cat("Final question variables analyzed:", summary_report$final_question_variables, "\n")

# Save summary
saveRDS(summary_report, "results/analysis_summary.rds")

cat("\n=== ALL ANALYSES COMPLETE ===\n")
cat("Results saved in 'results/' directory\n")
cat("Plots saved in 'results/plots/' directory\n")

# ===================================================================
# 9. ADDITIONAL NOTEWORTHY FINDINGS TEMPLATE
# ===================================================================
cat("\n=== 9. NOTEWORTHY FINDINGS TEMPLATE ===\n")
cat("This section can be expanded with specific findings from page 19 of your paper\n")

# Template for adding specific noteworthy findings
# Example: Specific response patterns, interesting demographic correlations, etc.

# You can add specific findings here based on the Google Forms data
# For example:
# - Percentage who learned about project through specific channels
# - Age distribution patterns
# - Gender differences in motivation scales
# - Education level correlations with engagement

cat("Template created for adding specific noteworthy findings from your paper\n")

cat("\n=== END OF COMPREHENSIVE ANALYSIS ===\n")
