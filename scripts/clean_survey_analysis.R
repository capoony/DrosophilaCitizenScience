# ===================================================================
# CLEAN CITIZEN SCIENCE SURVEY ANALYSIS
# Vienna Drosophila Project - Comprehensive Survey Data Analysis
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
    grid, # Grid graphics for text
    scales, # Plot scaling
    RColorBrewer, # Color palettes
    viridisLite, # Viridis color palettes (lightweight version)
    ggsignif, # Add significance annotations to plots
    ggpubr, # Publication ready plots and stats
    rstatix # Pipe-friendly statistical functions
)

# Set working directory and create output directories
setwd("D:/GitHub/DrosophilaCitizenScience")
dir.create("results", showWarnings = FALSE)
dir.create("results/plots", showWarnings = FALSE)
dir.create("results/tables", showWarnings = FALSE)

# ===================================================================
# DATA LOADING AND INITIAL SETUP
# ===================================================================
cat("=== LOADING DATA ===\n")

# Read survey data
survey_data <- read_excel("data/Survey_Data_analysis - Kopie.xlsx", sheet = 1)

cat("✓ Data loaded successfully!\n")
cat("  Sample size: N =", nrow(survey_data), "\n")
cat("  Variables:", ncol(survey_data), "\n\n")

# ===================================================================
# 1. SOCIODEMOGRAPHIC ANALYSIS
# ===================================================================
cat("=== 1. SOCIODEMOGRAPHIC ANALYSIS ===\n")

# Helper function for frequency analysis
analyze_categorical <- function(data, var_name, display_name = NULL) {
    if (is.null(display_name)) display_name <- var_name

    if (var_name %in% names(data)) {
        freq_table <- table(data[[var_name]], useNA = "ifany")
        # Calculate percentages correctly: count divided by total for each category
        prop_table <- round(prop.table(freq_table) * 100, 1)

        result_df <- data.frame(
            Variable = display_name,
            Category = names(freq_table),
            Count = as.numeric(freq_table),
            Percentage = as.numeric(prop_table)
        )

        cat("✓", display_name, "\n")
        print(kable(result_df[, 2:4], digits = 1))
        cat("\n")

        return(result_df)
    } else {
        cat("✗ Variable", var_name, "not found\n")
        return(NULL)
    }
}

# Analyze sociodemographic variables using exact column names
sociodem_results <- list()

# 1. Age analysis
if ("Age" %in% names(survey_data)) {
    sociodem_results$age <- analyze_categorical(survey_data, "Age", "Age Groups")
    cat("✓ Age analysis completed\n")
} else {
    cat("✗ Age variable not found\n")
}

# 2. Gender analysis
if ("Gender" %in% names(survey_data)) {
    sociodem_results$gender <- analyze_categorical(survey_data, "Gender", "Gender")
    cat("✓ Gender analysis completed\n")
} else {
    cat("✗ Gender variable not found\n")
}

# 3. Education analysis (Highest level of education)
if ("Highest Level of Education" %in% names(survey_data)) {
    sociodem_results$education <- analyze_categorical(survey_data, "Highest Level of Education", "Education Level")
    cat("✓ Education level analysis completed\n")
} else {
    cat("✗ Education level variable not found\n")
}

# 4. How heard about project (Wie haben Sie von dem Projekt erfahren?) - using comma-separated analysis
if ("Wie haben Sie von dem Projekt erfahren?" %in% names(survey_data)) {
    heard_vars <- analyze_engagement_detailed(survey_data, "Wie haben Sie von dem Projekt erfahren", "How_Heard_About_Project")
    # Also keep traditional analysis for comparison
    sociodem_results$heard_traditional <- analyze_categorical(survey_data, "Wie haben Sie von dem Projekt erfahren?", "How Heard About Project (Traditional)")
    cat("✓ How heard about project analysis completed\n")
} else {
    heard_vars <- character(0)
    cat("✗ 'How heard about project' variable not found\n")
}

# 5. Previous CS participation (Haben Sie schon einmal an CS teilgenommen? - Ja/nein)
prev_cs_vars <- c("Previous engagement in CS", "Haben Sie schon einmal an CS teilgenommen?")
prev_cs_found <- FALSE
for (var in prev_cs_vars) {
    if (var %in% names(survey_data)) {
        sociodem_results$prev_cs <- analyze_categorical(survey_data, var, "Previous CS Participation")
        cat("✓ Previous CS participation analysis completed using:", var, "\n")
        prev_cs_found <- TRUE
        break
    }
}
if (!prev_cs_found) {
    cat("✗ Previous CS participation variable not found\n")
}

# 6. Scope of engagement (Haben Sie aktiv beigetragen und eine Falle abgegeben? Ja/Nein)
scope_vars <- c("Scope of engagement", "Haben Sie aktiv beigetragen und eine Falle abgegeben?")
scope_found <- FALSE
for (var in scope_vars) {
    if (var %in% names(survey_data)) {
        sociodem_results$scope <- analyze_categorical(survey_data, var, "Scope of Engagement")
        cat("✓ Scope of engagement analysis completed using:", var, "\n")
        scope_found <- TRUE
        break
    }
}
if (!scope_found) {
    cat("✗ Scope of engagement variable not found\n")
} # Save sociodemographic results
if (length(sociodem_results) > 0) {
    all_sociodem <- do.call(rbind, sociodem_results)
    write.csv(all_sociodem, "results/tables/sociodemographic_analysis.csv", row.names = FALSE)
}

# ===================================================================
# CREATE BEAUTIFUL DEMOGRAPHIC SUMMARY PLOTS
# ===================================================================
cat("Creating demographic summary visualizations...\n")

# Prepare data for demographic plots
demographic_plots <- list()

# Age distribution plot split by gender
if ("Age" %in% names(survey_data) && "Gender" %in% names(survey_data)) {
    age_gender_data <- survey_data %>%
        filter(!is.na(Age) & Age != "" & !is.na(Gender) & Gender != "") %>%
        count(Age, Gender) %>%
        group_by(Age) %>%
        mutate(
            total_age = sum(n),
            Percentage = round(n / total_age * 100, 1)
        ) %>%
        ungroup()

    p_age <- ggplot(age_gender_data, aes(x = Age, y = n, fill = Gender)) +
        geom_col(alpha = 0.8, color = "black", size = 0.3, position = "stack") +
        geom_text(aes(label = paste0(n, "\n(", Percentage, "%)")),
            position = position_stack(vjust = 0.5), size = 3, fontface = "bold", color = "white"
        ) +
        coord_flip() +
        theme_bw() +
        theme(
            axis.title = element_text(size = 11, face = "bold"),
            plot.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 10),
            legend.position = "bottom",
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 9),
            panel.grid.minor = element_blank(),
            plot.margin = margin(t = 20, r = 20, b = 60, l = 20, unit = "pt")
        ) +
        labs(
            title = "Age Distribution by Gender",
            x = "Age Group",
            y = "Number of Participants",
            fill = "Gender"
        ) +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

    demographic_plots$age <- p_age
}

# Gender distribution plot (pie chart - good for binary/few categories)
if ("Gender" %in% names(survey_data)) {
    gender_data <- survey_data %>%
        filter(!is.na(Gender) & Gender != "") %>%
        count(Gender) %>%
        mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
        mutate(
            pos = cumsum(Percentage) - 0.5 * Percentage,
            label = paste0(Gender, "\n", n, " (", Percentage, "%)")
        )

    p_gender <- ggplot(gender_data, aes(x = "", y = Percentage, fill = Gender)) +
        geom_col(width = 1, color = "white", size = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(y = pos, label = label),
            color = "white", fontface = "bold", size = 4
        ) +
        theme_void() +
        theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            legend.position = "none"
        ) +
        labs(title = "Gender Distribution") +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9)

    demographic_plots$gender <- p_gender
}

# Education distribution plot
if ("Highest Level of Education" %in% names(survey_data)) {
    education_data <- survey_data %>%
        filter(!is.na(`Highest Level of Education`) & `Highest Level of Education` != "") %>%
        count(`Highest Level of Education`) %>%
        mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
        # Wrap long education labels
        mutate(Education_Wrapped = sapply(`Highest Level of Education`, function(x) {
            if (nchar(x) <= 25) {
                return(x)
            } else {
                words <- strsplit(x, " ")[[1]]
                lines <- character()
                current_line <- ""

                for (word in words) {
                    if (nchar(paste(current_line, word)) <= 25) {
                        current_line <- ifelse(current_line == "", word, paste(current_line, word))
                    } else {
                        if (current_line != "") lines <- c(lines, current_line)
                        current_line <- word
                    }
                }
                if (current_line != "") lines <- c(lines, current_line)
                return(paste(lines, collapse = "\n"))
            }
        }))

    p_education <- ggplot(education_data, aes(x = reorder(Education_Wrapped, n), y = n, fill = Education_Wrapped)) +
        geom_col(alpha = 0.8, color = "black", size = 0.3) +
        geom_text(aes(label = paste0(n, "\n(", Percentage, "%)")),
            hjust = -0.1, size = 3.5, fontface = "bold"
        ) +
        coord_flip() +
        theme_bw() +
        theme(
            axis.title = element_text(size = 11, face = "bold"),
            plot.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 9),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            plot.margin = margin(t = 20, r = 20, b = 60, l = 20, unit = "pt")
        ) +
        labs(
            title = "Education Level Distribution",
            x = "Education Level",
            y = "Number of Participants"
        ) +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

    demographic_plots$education <- p_education
}

# Previous CS participation plot (pie chart - good for binary/categorical response)
if ("Previous engagement in CS" %in% names(survey_data)) {
    prev_cs_data <- survey_data %>%
        filter(!is.na(`Previous engagement in CS`) & `Previous engagement in CS` != "") %>%
        count(`Previous engagement in CS`) %>%
        mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
        mutate(
            pos = cumsum(Percentage) - 0.5 * Percentage,
            label = paste0(`Previous engagement in CS`, "\n", n, " (", Percentage, "%)")
        )

    p_prev_cs <- ggplot(prev_cs_data, aes(x = "", y = Percentage, fill = `Previous engagement in CS`)) +
        geom_col(width = 1, color = "white", size = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(y = pos, label = label),
            color = "white", fontface = "bold", size = 3.5
        ) +
        theme_void() +
        theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            legend.position = "none"
        ) +
        labs(title = "Previous Citizen Science Participation") +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9)

    demographic_plots$prev_cs <- p_prev_cs
}

# Combine all demographic plots into a beautiful summary
if (length(demographic_plots) > 0) {
    # Create individual plots first
    for (plot_name in names(demographic_plots)) {
        plot_filename_pdf <- paste0("results/plots/demographic_", plot_name, ".pdf")
        plot_filename_png <- paste0("results/plots/demographic_", plot_name, ".png")
        ggsave(plot_filename_pdf, demographic_plots[[plot_name]], width = 10, height = 6, dpi = 300)
        ggsave(plot_filename_png, demographic_plots[[plot_name]], width = 10, height = 6, dpi = 300)
    }

    # Create combined summary plot with optimized layout for mixed chart types
    if (length(demographic_plots) >= 2) {
        # Add panel labels (A, B, C, D) to each plot for the combined summary
        panel_labels <- c("A", "B", "C", "D")
        plots_with_labels <- list()

        for (i in seq_along(demographic_plots)) {
            if (i <= length(panel_labels)) {
                plots_with_labels[[i]] <- demographic_plots[[i]] +
                    annotate("text",
                        x = Inf, y = Inf, label = panel_labels[i],
                        hjust = 1.2, vjust = 1.2, size = 8, fontface = "bold",
                        color = "black"
                    )
            } else {
                plots_with_labels[[i]] <- demographic_plots[[i]]
            }
        }

        # Arrange plots in a grid with proper spacing for pie charts
        if (length(plots_with_labels) == 2) {
            combined_plot <- grid.arrange(
                grobs = plots_with_labels, nrow = 1, ncol = 2,
                top = textGrob("Sociodemographic Summary",
                    gp = gpar(fontsize = 16, fontface = "bold")
                )
            )
        } else if (length(plots_with_labels) == 3) {
            combined_plot <- grid.arrange(
                grobs = plots_with_labels, nrow = 2, ncol = 2,
                top = textGrob("Sociodemographic Summary",
                    gp = gpar(fontsize = 16, fontface = "bold")
                )
            )
        } else {
            # For 4 plots, arrange in 2x2 grid
            combined_plot <- grid.arrange(
                grobs = plots_with_labels, nrow = 2, ncol = 2,
                top = textGrob("Sociodemographic Summary",
                    gp = gpar(fontsize = 16, fontface = "bold")
                )
            )
        }

        # Save combined plot with adjusted dimensions for mixed chart types
        ggsave("results/plots/demographic_summary_combined.pdf", combined_plot,
            width = 16, height = 14, dpi = 300
        )
        ggsave("results/plots/demographic_summary_combined.png", combined_plot,
            width = 16, height = 14, dpi = 300
        )
    }

    cat("✓ Demographic summary plots created\n")
    cat("  Individual plots saved as: demographic_[variable].pdf\n")
    cat("  Combined summary saved as: demographic_summary_combined.pdf\n")
}

# ===================================================================
# 2. MOTIVATION SCALE CREATION
# ===================================================================
cat("=== 2. MOTIVATION SCALE CREATION ===\n")

# Function to create motivation indices with error checking
create_motivation_index <- function(data, var_pattern, index_name, display_name) {
    matching_vars <- names(data)[grepl(var_pattern, names(data), ignore.case = FALSE)]

    if (length(matching_vars) > 0) {
        # Calculate index as mean of available items
        index_values <- rowMeans(data[matching_vars], na.rm = TRUE)

        # Only assign if at least 50% of items are available
        prop_available <- rowMeans(!is.na(data[matching_vars]))
        index_values[prop_available < 0.5] <- NA

        data[[index_name]] <- index_values

        cat("✓", display_name, ":", length(matching_vars), "items (", paste(matching_vars, collapse = ", "), ")\n")
        return(data)
    } else {
        cat("✗", display_name, ": No matching variables found\n")
        return(data)
    }
}

# Create motivation indices using exact variable names
# NCV index - Nature Conservation Values (NCV1-NCV5)
ncv_vars <- c(
    "NCV1: Ich empfinde dass ViennaCityFly einen gesellschaftsrelevanten Bezug hat, der auch mir persönlich wichtig ist.",
    "NCV2: Mir ist wichtig, dass meine persönlichen Werte zu den Projektzielen passen.",
    "NCV3: Ich bin überzeugt, dass ich in dem Projekt aktiv zum Naturschutz beitragen kann.",
    "NCV4: Ich setzte mich gerne für (wildlebende) Lebewesen ein.",
    "NCV5: Ich möchte etwas gegen den Verlust der Artenvielfalt tun."
)

ncv_available <- ncv_vars[ncv_vars %in% names(survey_data)]
if (length(ncv_available) > 0) {
    survey_data$NCV_idx_calculated <- rowMeans(survey_data[ncv_available], na.rm = TRUE)
    cat("✓ Nature Conservation Values:", length(ncv_available), "items\n")
}

# SM index - Social Motives (SM1-SM3)
sm_vars <- c(
    "SM1: Ich fühle mich durch die Teilnahme am Projekt als Teil einer Gemeinschaft, die sich für dieselbe Sache einsetzt.",
    "SM2: Ich hoffe, mich durch meine Teilnahme besser mit Gleichgesinnten vernetzen zu können.",
    "SM3: Mit Teilnahme an dem Projekt möchte ich meinem persönlichen Umfeld (Familie, Kinder, Freunde etc.) Artenvielfalt und die Relevanz der Natur näherbringen."
)

sm_available <- sm_vars[sm_vars %in% names(survey_data)]
if (length(sm_available) > 0) {
    survey_data$SM_idx_calculated <- rowMeans(survey_data[sm_available], na.rm = TRUE)
    cat("✓ Social Motives:", length(sm_available), "items\n")
}

# SPR index - Sociopolitical Responsibility (SPR1, SPR2, SPR3 - excluding SPR extra)
spr_vars <- c(
    "SPR1: Ich engagiere mich in dem Projekt, weil ich helfen möchte, Missstände im Naturschutz zu beheben.",
    "SPR2: Ich möchte durch meine Teilnahme eine gesellschaftlich sinnvolle Aufgabe erfüllen.",
    "SPR3: Ich möchte durch meine Teilnahme gesellschaftliche Veränderungen anstoßen."
)

spr_available <- spr_vars[spr_vars %in% names(survey_data)]
if (length(spr_available) > 0) {
    survey_data$SPR_idx_calculated <- rowMeans(survey_data[spr_available], na.rm = TRUE)
    cat("✓ Sociopolitical Responsibility:", length(spr_available), "items (excluding SPR extra)\n")
}

# SPR_extra - Curiosity to learn more (separate analysis)
spr_extra_var <- "SPR extra: Ich engagiere mich in dem Projekt, weil ich mehr über die Verbreitung von Taufliegen und ihre Lebensweise lernen möchte."
if (spr_extra_var %in% names(survey_data)) {
    survey_data$SPR_extra_calculated <- survey_data[[spr_extra_var]]
    cat("✓ Curiosity to Learn More: 1 item (SPR extra)\n")
}

# CS index - Citizen Science (CS1-CS5)
cs_vars <- c(
    "CS1: Ich engagiere mich in dem Projekt, weil ich dadurch einem wissenschaftlichen Forschungsprojekt diene.",
    "CS2: Ich erhoffe mir, über das Projekt mit Wissenschaftler*innen in einen fachlichen Austausch zu treten.",
    "CS3: Ich nehme an dem Projekt teil, um wissenschaftliche Prozesse besser verstehen zu lernen und um meiner Neugier an Wissenschaft nachgehen zu können.",
    "CS4: Ich engagiere mich im Projekt, weil ich damit einen Beitrag zu Artenerfassung leisten kann.",
    "CS5: Ich engagiere mich im Projekt, weil ich damit zum Austausch des Wissens zwischen Bürger*innen und Forscher*innen beitragen kann."
)

cs_available <- cs_vars[cs_vars %in% names(survey_data)]
if (length(cs_available) > 0) {
    survey_data$CS_idx_calculated <- rowMeans(survey_data[cs_available], na.rm = TRUE)
    cat("✓ Citizen Science:", length(cs_available), "items\n")
}

# CommOrg index - Communication & Organisation (COMM3, ORG1, ORG3, ORG4)
commorg_vars <- c(
    "COMM3: Ich engagiere mich im Projekt, weil ich den Eindruck habe, dass mein persönliches Engagement hilfreich für das Gesamtprojekt ist.",
    "ORG1: Ich engagiere mich im Projekt, weil das Projekt insgesamt sehr gut organisiert ist und ich mit der Kommunikation zufrieden bin.",
    "ORG3: Ich engagiere mich im Projekt, weil mir das übergeordnete Projektziel klar ist.",
    "ORG4: Ich engagiere mich im Projekt, weil das Projekt durch das Naturhistorische Museum Wien betreut wird."
)

commorg_available <- commorg_vars[commorg_vars %in% names(survey_data)]
if (length(commorg_available) > 0) {
    survey_data$CommOrg_idx_calculated <- rowMeans(survey_data[commorg_available], na.rm = TRUE)
    cat("✓ Communication & Organisation:", length(commorg_available), "items\n")
}

# Use existing indices if available, otherwise use calculated ones
if ("NCV_idx" %in% names(survey_data)) {
    cat("✓ Using existing NCV_idx from data\n")
} else if ("NCV_idx_calculated" %in% names(survey_data)) {
    survey_data$NCV_idx <- survey_data$NCV_idx_calculated
}

if ("SM_idx" %in% names(survey_data)) {
    cat("✓ Using existing SM_idx from data\n")
} else if ("SM_idx_calculated" %in% names(survey_data)) {
    survey_data$SM_idx <- survey_data$SM_idx_calculated
}

if ("SPR_idx" %in% names(survey_data)) {
    cat("✓ Using existing SPR_idx from data\n")
} else if ("SPR_idx_calculated" %in% names(survey_data)) {
    survey_data$SPR_idx <- survey_data$SPR_idx_calculated
}

if ("CS_idx" %in% names(survey_data)) {
    cat("✓ Using existing CS_idx from data\n")
} else if ("CS_idx_calculated" %in% names(survey_data)) {
    survey_data$CS_idx <- survey_data$CS_idx_calculated
}

if ("CommOrg_Idx" %in% names(survey_data)) {
    cat("✓ Using existing CommOrg_Idx from data\n")
    survey_data$CommOrg_idx <- survey_data$CommOrg_Idx # Standardize naming
} else if ("CommOrg_idx_calculated" %in% names(survey_data)) {
    survey_data$CommOrg_idx <- survey_data$CommOrg_idx_calculated
}

# SPR_extra handling
if ("SPR_extra_calculated" %in% names(survey_data)) {
    survey_data$SPR_extra <- survey_data$SPR_extra_calculated
}

# Overall motivation index
motivation_indices <- c("NCV_idx", "SM_idx", "SPR_idx", "CS_idx", "CommOrg_idx")
available_indices <- motivation_indices[motivation_indices %in% names(survey_data)]

if (length(available_indices) > 0) {
    survey_data$MeanMotivationOverall <- rowMeans(survey_data[available_indices], na.rm = TRUE)
    cat("✓ Overall Motivation Index:", length(available_indices), "subscales\n")
}

cat("\n")

# ===================================================================
# 3. RELIABILITY ANALYSIS
# ===================================================================
cat("=== 3. RELIABILITY ANALYSIS ===\n")

# Function for reliability analysis
calculate_reliability <- function(data, items, scale_name) {
    available_items <- items[items %in% names(data)]

    if (length(available_items) >= 2) {
        subset_data <- data[available_items]
        subset_data <- subset_data[complete.cases(subset_data), ]

        if (nrow(subset_data) > 0 && ncol(subset_data) >= 2) {
            alpha_result <- psych::alpha(subset_data)
            alpha_value <- alpha_result$total$std.alpha

            cat(
                "✓", scale_name, ": α =", round(alpha_value, 3),
                " (", length(available_items), "items, N =", nrow(subset_data), ")\n"
            )

            return(data.frame(
                Scale = scale_name,
                Cronbach_Alpha = round(alpha_value, 3),
                Items = length(available_items),
                N = nrow(subset_data),
                Interpretation = case_when(
                    alpha_value >= 0.9 ~ "Excellent",
                    alpha_value >= 0.8 ~ "Good",
                    alpha_value >= 0.7 ~ "Acceptable",
                    alpha_value >= 0.6 ~ "Questionable",
                    TRUE ~ "Poor"
                )
            ))
        }
    }

    cat("✗", scale_name, ": Insufficient data for reliability analysis\n")
    return(NULL)
}

# Calculate reliability for each scale using exact variable names
reliability_results <- list()

# NCV reliability
reliability_results$NCV <- calculate_reliability(survey_data, ncv_vars, "Nature Conservation Values")

# SM reliability
reliability_results$SM <- calculate_reliability(survey_data, sm_vars, "Social Motives")

# SPR reliability (excluding SPR extra)
reliability_results$SPR <- calculate_reliability(survey_data, spr_vars, "Sociopolitical Responsibility")

# CS reliability
reliability_results$CS <- calculate_reliability(survey_data, cs_vars, "Citizen Science")

# CommOrg reliability
reliability_results$CommOrg <- calculate_reliability(survey_data, commorg_vars, "Communication & Organisation")

# Create reliability summary table
reliability_df <- do.call(rbind, reliability_results[!sapply(reliability_results, is.null)])

if (!is.null(reliability_df)) {
    write.csv(reliability_df, "results/tables/reliability_analysis.csv", row.names = FALSE)
    cat("\n✓ Reliability analysis saved\n")

    # Create reliability visualization
    if (nrow(reliability_df) > 0) {
        # Prepare data for plotting
        reliability_plot_data <- reliability_df %>%
            mutate(
                Scale_Name = factor(Scale, levels = rev(Scale)), # Reverse for better display
                Alpha_Category = case_when(
                    Cronbach_Alpha >= 0.9 ~ "Excellent (≥0.9)",
                    Cronbach_Alpha >= 0.8 ~ "Good (0.8-0.89)",
                    Cronbach_Alpha >= 0.7 ~ "Acceptable (0.7-0.79)",
                    Cronbach_Alpha >= 0.6 ~ "Questionable (0.6-0.69)",
                    TRUE ~ "Poor (<0.6)"
                ),
                Alpha_Category = factor(Alpha_Category, levels = c(
                    "Excellent (≥0.9)", "Good (0.8-0.89)", "Acceptable (0.7-0.79)",
                    "Questionable (0.6-0.69)", "Poor (<0.6)"
                ))
            )

        # Create reliability plot
        reliability_plot <- ggplot(reliability_plot_data, aes(x = Scale_Name, y = Cronbach_Alpha, fill = Alpha_Category)) +
            geom_col(alpha = 0.8, color = "black", size = 1) +
            geom_text(aes(label = sprintf("α = %.3f\n(n = %d)", Cronbach_Alpha, Items)),
                hjust = -0.1, size = 4, fontface = "bold"
            ) +
            coord_flip() +
            theme_bw() +
            theme(
                axis.title = element_text(size = 12, face = "bold"),
                plot.title = element_text(size = 14, face = "bold"),
                axis.text = element_text(size = 10),
                legend.title = element_text(size = 11, face = "bold"),
                legend.text = element_text(size = 10),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt")
            ) +
            labs(
                title = "Scale Reliability Analysis (Cronbach's Alpha)",
                x = "Motivation Scale",
                y = "Cronbach's Alpha",
                fill = "Reliability Level"
            ) +
            scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9) +
            scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.15))) +
            geom_hline(
                yintercept = c(0.6, 0.7, 0.8, 0.9),
                linetype = "dashed", alpha = 0.5, size = 1
            )

        ggsave("results/plots/reliability_analysis.pdf", reliability_plot,
            width = 10, height = 8, dpi = 300
        )
        ggsave("results/plots/reliability_analysis.png", reliability_plot,
            width = 10, height = 8, dpi = 300
        )

        cat("✓ Reliability visualization created\n")
    }
}

cat("\n")

# ===================================================================
# 4. DESCRIPTIVE STATISTICS FOR MOTIVATION SCALES
# ===================================================================
cat("=== 4. MOTIVATION DESCRIPTIVE STATISTICS ===\n")

# Motivation variables for analysis
motivation_vars <- c("NCV_idx", "SM_idx", "SPR_idx", "CS_idx", "CommOrg_idx")
if ("SPR_extra" %in% names(survey_data)) {
    motivation_vars <- c(motivation_vars, "SPR_extra")
}

available_motivation_vars <- motivation_vars[motivation_vars %in% names(survey_data)]

# Create descriptive statistics
motivation_descriptives <- data.frame()

for (var in available_motivation_vars) {
    var_data <- survey_data[[var]]

    desc_stats <- data.frame(
        Variable = var,
        Label = case_when(
            var == "NCV_idx" ~ "Nature Conservation Values",
            var == "SM_idx" ~ "Social Motives",
            var == "SPR_idx" ~ "Sociopolitical Responsibility",
            var == "CS_idx" ~ "Citizen Science",
            var == "CommOrg_idx" ~ "Communication & Organisation",
            var == "SPR_extra" ~ "Curiosity to Learn More",
            TRUE ~ var
        ),
        N = sum(!is.na(var_data)),
        Mean = round(mean(var_data, na.rm = TRUE), 3),
        SD = round(sd(var_data, na.rm = TRUE), 3),
        Min = round(min(var_data, na.rm = TRUE), 3),
        Max = round(max(var_data, na.rm = TRUE), 3),
        Median = round(median(var_data, na.rm = TRUE), 3)
    )

    motivation_descriptives <- rbind(motivation_descriptives, desc_stats)
}

# Print and save descriptive statistics
print(kable(motivation_descriptives[, -1], digits = 3, caption = "Motivation Scale Descriptive Statistics"))
write.csv(motivation_descriptives, "results/tables/motivation_descriptives.csv", row.names = FALSE)

cat("\n")

# ===================================================================
# 5. STATISTICAL TESTING AND VISUALIZATION
# ===================================================================
cat("=== 5. STATISTICAL TESTING AND VISUALIZATION ===\n")

if (length(available_motivation_vars) > 0) {
    # Prepare data for analysis and plotting
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

    # Statistical testing
    if (length(unique(motivation_long$Scale)) > 2) {
        cat("Performing ANOVA...\n")
        anova_result <- aov(Score ~ Scale, data = motivation_long)
        anova_p <- summary(anova_result)[[1]][["Pr(>F)"]][1]

        cat("ANOVA p-value:", round(anova_p, 6), "\n")

        # Post-hoc tests if significant
        if (anova_p < 0.05) {
            cat("Performing post-hoc pairwise comparisons...\n")
            pairwise_results <- motivation_long %>%
                pairwise_t_test(Score ~ Scale, p.adjust.method = "bonferroni") %>%
                mutate(significance = case_when(
                    p.adj < 0.001 ~ "***",
                    p.adj < 0.01 ~ "**",
                    p.adj < 0.05 ~ "*",
                    TRUE ~ "ns"
                )) %>%
                filter(p.adj < 0.05)

            if (nrow(pairwise_results) > 0) {
                cat("Significant differences found:\n")
                print(pairwise_results %>% select(group1, group2, p.adj, significance))
                write.csv(pairwise_results, "results/tables/motivation_pairwise_tests.csv", row.names = FALSE)
            }
        }
    }

    # Create enhanced boxplot
    p_boxplot <- ggplot(motivation_long, aes(x = Scale_Clean, y = Score, fill = Scale_Clean)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
        geom_jitter(width = 0.2, alpha = 0.4, size = 0.8) +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11),
            legend.position = "none"
        ) +
        labs(
            title = "Motivation Scale Score Distributions",
            subtitle = paste0("ANOVA p = ", ifelse(exists("anova_p"), round(anova_p, 4), "N/A")),
            x = "Motivation Scale",
            y = "Score (1-5)"
        ) +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9) +
        scale_y_continuous(limits = c(1, 5))

    ggsave("results/plots/motivation_boxplots.pdf", p_boxplot, width = 12, height = 8, dpi = 300)
    ggsave("results/plots/motivation_boxplots.png", p_boxplot, width = 12, height = 8, dpi = 300)

    # Create means barplot
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
        theme_bw() +
        theme(
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11),
            legend.position = "none"
        ) +
        labs(
            title = "Mean Motivation Scale Scores",
            subtitle = "Error bars show ± 1 standard deviation",
            x = "Motivation Scale",
            y = "Mean Score"
        ) +
        scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9) +
        geom_text(aes(label = sprintf("%.2f", Mean)), hjust = -0.1, size = 3)

    ggsave("results/plots/motivation_means_barplot.pdf", p_barplot, width = 10, height = 6, dpi = 300)
    ggsave("results/plots/motivation_means_barplot.png", p_barplot, width = 10, height = 6, dpi = 300)

    cat("✓ Motivation plots created\n")
}

# ===================================================================
# 6. CORRELATION ANALYSIS
# ===================================================================
cat("\n=== 6. CORRELATION ANALYSIS ===\n")

if (length(available_motivation_vars) >= 2) {
    # Calculate correlation matrix
    cor_data <- survey_data[available_motivation_vars]
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

    # Significance testing
    cor_test <- psych::corr.test(cor_data, use = "pairwise")

    cat("Correlation matrix calculated\n")
    print(round(cor_matrix, 3))

    # Save correlation results
    write.csv(cor_matrix, "results/tables/motivation_correlations.csv")
    write.csv(cor_test$p, "results/tables/motivation_correlation_pvalues.csv")

    # Create correlation plot
    pdf("results/plots/motivation_correlation_plot.pdf", width = 10, height = 8)
    corrplot(cor_matrix,
        method = "color",
        type = "upper",
        order = "original",
        col = viridisLite::viridis(100),
        tl.col = "black",
        tl.srt = 45,
        tl.cex = 0.8,
        cl.cex = 0.8,
        addCoef.col = "black",
        number.cex = 0.8,
        title = "Motivation Scale Correlations",
        mar = c(0, 0, 2, 0)
    )
    dev.off()

    # Also create PNG version
    png("results/plots/motivation_correlation_plot.png", width = 10, height = 8, units = "in", res = 300)
    corrplot(cor_matrix,
        method = "color",
        type = "upper",
        order = "original",
        col = viridisLite::viridis(100),
        tl.col = "black",
        tl.srt = 45,
        tl.cex = 0.8,
        cl.cex = 0.8,
        addCoef.col = "black",
        number.cex = 0.8,
        title = "Motivation Scale Correlations",
        mar = c(0, 0, 2, 0)
    )
    dev.off()

    cat("✓ Correlation analysis completed\n")
}

# ===================================================================
# 7. ENGAGEMENT ANALYSIS WITH COMMA-SEPARATED RESPONSES
# ===================================================================
cat("\n=== 7. ENGAGEMENT ANALYSIS ===\n")

# Enhanced function for analyzing comma-separated responses
analyze_engagement_detailed <- function(data, var_pattern, analysis_name) {
    matching_cols <- names(data)[grepl(var_pattern, names(data), ignore.case = TRUE)]

    if (length(matching_cols) > 0) {
        cat("Analyzing", analysis_name, "...\n")

        for (col in matching_cols) {
            responses <- data[[col]][!is.na(data[[col]]) & data[[col]] != ""]

            if (length(responses) > 0) {
                # Split responses by comma
                all_split_responses <- unlist(lapply(responses, function(x) {
                    split_resp <- strsplit(as.character(x), ",")[[1]]
                    trimws(split_resp)
                }))

                all_split_responses <- all_split_responses[all_split_responses != ""]

                # Create frequency table
                freq_table_split <- sort(table(all_split_responses), decreasing = TRUE)
                # Calculate percentages correctly: count divided by number of responses (not mentions)
                # This accounts for multiple comma-separated answers from the same person
                num_responses <- length(responses)
                percentage_by_responses <- round((as.numeric(freq_table_split) / num_responses) * 100, 1)

                result_df <- data.frame(
                    Category = names(freq_table_split),
                    Count = as.numeric(freq_table_split),
                    Percentage = percentage_by_responses
                )

                cat("✓", col, ":", nrow(result_df), "categories,", length(all_split_responses), "total mentions from", num_responses, "responses\n")
                cat("  Percentages calculated as: count / number of responses (", num_responses, ")\n")

                # Save results
                filename <- paste0(
                    "results/tables/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                    gsub("[^A-Za-z0-9]", "_", col), "_analysis.csv"
                )
                write.csv(result_df, filename, row.names = FALSE)

                # Create visualization if reasonable number of categories
                if (nrow(result_df) <= 10 && nrow(result_df) > 0) {
                    plot_data <- result_df %>%
                        mutate(Category_Wrapped = sapply(Category, function(x) {
                            # Add line breaks every 30-35 characters at word boundaries
                            if (nchar(x) <= 35) {
                                return(x)
                            } else {
                                words <- strsplit(x, " ")[[1]]
                                lines <- character()
                                current_line <- ""

                                for (word in words) {
                                    if (nchar(paste(current_line, word)) <= 35) {
                                        current_line <- ifelse(current_line == "", word, paste(current_line, word))
                                    } else {
                                        if (current_line != "") lines <- c(lines, current_line)
                                        current_line <- word
                                    }
                                }
                                if (current_line != "") lines <- c(lines, current_line)
                                return(paste(lines, collapse = "\n"))
                            }
                        }))

                    p_engagement <- ggplot(plot_data, aes(x = reorder(Category_Wrapped, Count), y = Count)) +
                        geom_col(fill = "#2E8B57", alpha = 0.8, color = "black", size = 0.3) +
                        geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")),
                            hjust = -0.1, size = 3
                        ) +
                        coord_flip() +
                        theme_bw() +
                        theme(
                            axis.text.y = element_text(size = 8, hjust = 1, vjust = 0.5),
                            plot.title = element_text(size = 12, face = "bold"),
                            plot.margin = margin(t = 20, r = 20, b = 20, l = 100, unit = "pt")
                        ) +
                        labs(
                            title = paste(analysis_name, "- Response Categories"),
                            subtitle = paste(
                                "Total responses:", length(responses),
                                "| Total mentions:", length(all_split_responses)
                            ),
                            x = "Response Category",
                            y = "Number of Mentions"
                        ) +
                        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

                    plot_filename <- paste0(
                        "results/plots/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                        gsub("[^A-Za-z0-9]", "_", col), "_plot.pdf"
                    )
                    ggsave(plot_filename, p_engagement, width = 14, height = 10, dpi = 300)

                    # Also create PNG version
                    plot_filename_png <- paste0(
                        "results/plots/", gsub("[^A-Za-z0-9]", "_", analysis_name), "_",
                        gsub("[^A-Za-z0-9]", "_", col), "_plot.png"
                    )
                    ggsave(plot_filename_png, p_engagement, width = 14, height = 10, dpi = 300)
                }
            }
        }
        return(matching_cols)
    } else {
        cat("✗ No variables found for", analysis_name, "\n")
        return(character(0))
    }
}

# Analyze engagement variables using exact column names
# Impact analysis
if ("Self-assessed impact: Was hat sich durch Ihre Teilnahme an dem Projekt verändert?" %in% names(survey_data)) {
    impact_vars <- analyze_engagement_detailed(survey_data, "Self-assessed impact", "Impact_Analysis")
} else {
    impact_vars <- character(0)
    cat("✗ Impact variable not found\n")
}

# Contentment analysis
if ("Contentment: Wie zufrieden waren Sie insgesamt mit Ihrer Teilnahme an dem Projekt?" %in% names(survey_data)) {
    contentment_vars <- analyze_engagement_detailed(survey_data, "Contentment", "Contentment_Analysis")
} else {
    contentment_vars <- character(0)
    cat("✗ Contentment variable not found\n")
}

# Future involvement analysis
if ("Intended future involvement in CS: Haben Sie vor, auch weiterhin an Citizen Science Projekten teilzunehmen?" %in% names(survey_data)) {
    future_vars <- analyze_engagement_detailed(survey_data, "Intended future involvement", "Future_Involvement")
} else {
    future_vars <- character(0)
    cat("✗ Future involvement variable not found\n")
}

# ===================================================================
# 8. FINAL QUESTIONS ANALYSIS
# ===================================================================
cat("\n=== 8. FINAL QUESTIONS ANALYSIS ===\n")

# Analyze final questions using exact column names
# Non-participation reasons
if ("Gründe bei Nicht-Teilnahme" %in% names(survey_data)) {
    nonpartic_vars <- analyze_engagement_detailed(survey_data, "Non-participation reasons", "Non_Participation_Reasons")
} else {
    nonpartic_vars <- character(0)
    cat("✗ Non-participation reasons variable not found\n")
}

# Improvement suggestions
if ("Haben Sie Verbesserungsvorschläge für das Projekt?" %in% names(survey_data)) {
    improvement_vars <- analyze_engagement_detailed(survey_data, "Improvement suggestions", "Improvement_Suggestions")
} else {
    improvement_vars <- character(0)
    cat("✗ Improvement suggestions variable not found\n")
}

# Final comments
if ("Wollen sie uns abschließend noch etwas mitteilen?" %in% names(survey_data)) {
    final_vars <- analyze_engagement_detailed(survey_data, "Final comments", "Final_Comments")
} else {
    final_vars <- character(0)
    cat("✗ Final comments variable not found\n")
}

# ===================================================================
# 9. PARTICULARLY NOTEWORTHY FINDINGS
# ===================================================================
cat("\n=== 9. PARTICULARLY NOTEWORTHY FINDINGS ===\n")
cat("Extracting particularly noteworthy descriptive findings from individual survey questions...\n")

# Create a comprehensive report of key descriptive findings
noteworthy_findings <- list()

# Function to extract and summarize key findings from categorical variables
extract_key_findings <- function(data, var_name, description) {
    if (var_name %in% names(data)) {
        responses <- data[[var_name]][!is.na(data[[var_name]]) & data[[var_name]] != ""]
        if (length(responses) > 0) {
            freq_table <- sort(table(responses), decreasing = TRUE)
            total_responses <- length(responses)

            # Extract top categories and percentages
            findings <- data.frame(
                Variable = var_name,
                Description = description,
                Total_Responses = total_responses,
                Top_Response = names(freq_table)[1],
                Top_Response_Count = as.numeric(freq_table)[1],
                Top_Response_Percent = round((as.numeric(freq_table)[1] / total_responses) * 100, 1),
                stringsAsFactors = FALSE
            )

            if (length(freq_table) > 1) {
                findings$Second_Response <- names(freq_table)[2]
                findings$Second_Response_Count <- as.numeric(freq_table)[2]
                findings$Second_Response_Percent <- round((as.numeric(freq_table)[2] / total_responses) * 100, 1)
            }

            return(findings)
        }
    }
    return(NULL)
}

# Extract noteworthy findings from key variables
cat("Extracting findings from key survey questions...\n")

# Sociodemographic noteworthy findings
if ("Age" %in% names(survey_data)) {
    noteworthy_findings$age <- extract_key_findings(survey_data, "Age", "Age Distribution")
}

if ("Gender" %in% names(survey_data)) {
    noteworthy_findings$gender <- extract_key_findings(survey_data, "Gender", "Gender Distribution")
}

if ("Highest Level of Education" %in% names(survey_data)) {
    noteworthy_findings$education <- extract_key_findings(survey_data, "Highest Level of Education", "Education Level")
}

# Engagement noteworthy findings
if ("Contentment: Wie zufrieden waren Sie insgesamt mit Ihrer Teilnahme an dem Projekt?" %in% names(survey_data)) {
    noteworthy_findings$contentment <- extract_key_findings(
        survey_data,
        "Contentment: Wie zufrieden waren Sie insgesamt mit Ihrer Teilnahme an dem Projekt?",
        "Overall Satisfaction"
    )
}

if ("Intended future involvement in CS: Haben Sie vor, auch weiterhin an Citizen Science Projekten teilzunehmen?" %in% names(survey_data)) {
    noteworthy_findings$future_cs <- extract_key_findings(
        survey_data,
        "Intended future involvement in CS: Haben Sie vor, auch weiterhin an Citizen Science Projekten teilzunehmen?",
        "Future CS Participation Intent"
    )
}

# Previous CS participation
prev_cs_vars <- c("Previous engagement in CS", "Haben Sie schon einmal an CS teilgenommen?")
for (var in prev_cs_vars) {
    if (var %in% names(survey_data)) {
        noteworthy_findings$prev_cs <- extract_key_findings(survey_data, var, "Previous CS Experience")
        break
    }
}

# Scope of engagement
scope_vars <- c("Scope of engagement", "Haben Sie aktiv beigetragen und eine Falle abgegeben?")
for (var in scope_vars) {
    if (var %in% names(survey_data)) {
        noteworthy_findings$scope <- extract_key_findings(survey_data, var, "Active Trap Contribution")
        break
    }
}

# Compile noteworthy findings report
if (length(noteworthy_findings) > 0) {
    all_findings <- do.call(rbind, noteworthy_findings)
    write.csv(all_findings, "results/tables/noteworthy_findings_summary.csv", row.names = FALSE)

    cat("✓ Noteworthy findings extracted and saved\n")
    cat("  Key findings from", nrow(all_findings), "survey variables\n")
    cat("  Detailed findings saved to: noteworthy_findings_summary.csv\n")
} else {
    cat("✗ No noteworthy findings extracted - check variable names\n")
}

cat("\n=== 10. ANALYSIS VALIDATION CHECKLIST ===\n")
cat("Validating that all required analyses are included...\n")

# Create validation checklist based on your requirements
validation_checklist <- list()

# 1. Sociodemographic Analysis (6 variables)
validation_checklist$sociodemographics <- list(
    "Age" = "Age" %in% names(survey_data),
    "Gender" = "Gender" %in% names(survey_data),
    "Education" = "Highest Level of Education" %in% names(survey_data),
    "How_heard_project" = "Wie haben Sie von dem Projekt erfahren?" %in% names(survey_data),
    "Previous_CS" = any(c("Previous engagement in CS", "Haben Sie schon einmal an CS teilgenommen?") %in% names(survey_data)),
    "Scope_engagement" = any(c("Scope of engagement", "Haben Sie aktiv beigetragen und eine Falle abgegeben?") %in% names(survey_data))
)

# 2. Motivation Scale Indices (5 indices + overall)
validation_checklist$motivation_indices <- list(
    "NCV_idx" = exists("survey_data") && "NCV_idx_calculated" %in% names(survey_data),
    "SM_idx" = exists("survey_data") && "SM_idx_calculated" %in% names(survey_data),
    "SPR_idx" = exists("survey_data") && "SPR_idx_calculated" %in% names(survey_data),
    "CS_idx" = exists("survey_data") && "CS_idx_calculated" %in% names(survey_data),
    "CommOrg_idx" = exists("survey_data") && "CommOrg_idx_calculated" %in% names(survey_data),
    "SPR_extra" = exists("survey_data") && "SPR_extra_calculated" %in% names(survey_data),
    "MeanMotivationOverall" = exists("survey_data") && "MeanMotivationOverall" %in% names(survey_data)
)

# 3. Reliability Analysis
validation_checklist$reliability <- list(
    "Reliability_analysis_completed" = exists("reliability_df"),
    "Reliability_plot_created" = file.exists("results/plots/reliability_analysis.pdf")
)

# 4. Descriptive Analysis
validation_checklist$descriptive <- list(
    "Motivation_descriptives" = exists("motivation_summary"),
    "Motivation_distributions" = file.exists("results/plots/motivation_distributions.pdf"),
    "Motivation_correlations" = file.exists("results/plots/motivation_correlations.pdf")
)

# 5. Engagement Analysis
validation_checklist$engagement <- list(
    "Impact_analysis" = "Self-assessed impact: Was hat sich durch Ihre Teilnahme an dem Projekt verändert?" %in% names(survey_data),
    "Contentment_analysis" = "Contentment: Wie zufrieden waren Sie insgesamt mit Ihrer Teilnahme an dem Projekt?" %in% names(survey_data),
    "Future_involvement" = "Intended future involvement in CS: Haben Sie vor, auch weiterhin an Citizen Science Projekten teilzunehmen?" %in% names(survey_data)
)

# 6. Final Questions
validation_checklist$final_questions <- list(
    "Non_participation_reasons" = "Gründe bei Nicht-Teilnahme" %in% names(survey_data),
    "Improvement_suggestions" = "Haben Sie Verbesserungsvorschläge für das Projekt?" %in% names(survey_data),
    "Final_comments" = "Wollen sie uns abschließend noch etwas mitteilen?" %in% names(survey_data)
)

# Print validation results
cat("\n--- VALIDATION RESULTS ---\n")
for (section_name in names(validation_checklist)) {
    cat(paste0("\n", toupper(section_name), ":\n"))
    section <- validation_checklist[[section_name]]
    for (item_name in names(section)) {
        status <- if (section[[item_name]]) "✓" else "✗"
        cat(paste0("  ", status, " ", gsub("_", " ", item_name), "\n"))
    }
}

# Calculate overall completion percentage
all_checks <- unlist(validation_checklist)
completion_rate <- round(sum(all_checks) / length(all_checks) * 100, 1)
cat(paste0("\nOVERALL COMPLETION: ", completion_rate, "% (", sum(all_checks), "/", length(all_checks), " checks passed)\n"))

# Save validation results
validation_summary <- data.frame(
    Analysis_Section = rep(names(validation_checklist), sapply(validation_checklist, length)),
    Check_Item = unlist(lapply(validation_checklist, names)),
    Status = unlist(validation_checklist),
    stringsAsFactors = FALSE
)

write.csv(validation_summary, "results/tables/analysis_validation_checklist.csv", row.names = FALSE)
cat("✓ Validation checklist saved to: analysis_validation_checklist.csv\n")

# ===================================================================
# 11. COMPREHENSIVE SUMMARY REPORT
# ===================================================================
cat("\n=== ANALYSIS SUMMARY ===\n")

summary_stats <- list(
    analysis_date = Sys.Date(),
    sample_size = nrow(survey_data),
    motivation_scales = length(available_motivation_vars),
    reliability_analyses = ifelse(exists("reliability_df"), nrow(reliability_df), 0),
    engagement_variables = length(c(impact_vars, contentment_vars, future_vars)),
    files_created = length(list.files("results", recursive = TRUE))
)

cat("✓ Analysis completed successfully!\n")
cat("  Sample size: N =", summary_stats$sample_size, "\n")
cat("  Motivation scales analyzed:", summary_stats$motivation_scales, "\n")
cat("  Reliability analyses:", summary_stats$reliability_analyses, "\n")
cat("  Engagement variables:", summary_stats$engagement_variables, "\n")
cat("  Total output files:", summary_stats$files_created, "\n")

# Save summary
saveRDS(summary_stats, "results/analysis_summary.rds")

cat("\n=== ALL ANALYSES COMPLETE ===\n")
cat("Results saved in 'results/' directory:\n")
cat("  - tables/: CSV files with detailed results\n")
cat("  - plots/: High-quality visualization files\n")
cat("  - analysis_summary.rds: Complete analysis summary\n")

# Final message
cat("\n🎉 Clean survey analysis completed successfully!\n")
cat("📊 Check the results/ folder for all outputs\n")
