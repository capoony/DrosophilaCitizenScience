##
library(readxl)
library(tidyverse)
library(knitr)

## change working directory
setwd("D:/GitHub/DrosophilaCitizenScience")

## read DATA from data/Survey_Analysis_Survey.xlsx
survey_data <- read_excel("data/Survey_Data_Analysis - Kopie.xlsx", sheet = 1)

## just check the data
# names(survey_data)


## -------------------  (1) Make a summary table for all columns -------------------

## make some summary statistics for columns 2-7, e.g. absolute and relative frequencies for columns 2 - 7 an make a pivot table with absolute and relative frequencies

DATA <- survey_data %>%
    select(2:7) %>%
    gather(key = "Variable", value = "Value") %>%
    group_by(Variable, Value) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = round((Count / sum(Count)) * 100, 2))

dir.create("results", showWarnings = FALSE)

# make mkarkdown table
write.table(DATA, file = "results/survey_summary.csv", sep = ",", row.names = FALSE, quote = FALSE)

## test for interaction between columns 2 and 3 by stacked barplot
subset<- survey_data %>%
    select(2, 3) %>%
    group_by(survey_data[, 2], survey_data[, 3]) %>%
    summarise(Count = n())  %>%
    mutate(Percentage = round((Count / sum(Count)) * 100, 2))

ggplot(subset, aes(x = Age, fill=Gender,y=Percentage)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Stacked Barplot of Age and Gender",
         x = "Age",
         y = "Percentage") +  
    scale_fill_manual(values = c("lightblue", "pink")) + 
    theme_bw()

## is there a signifiant difference in Age between the Sexes? Use an    ANOVA test

for (i in 2:7) {
    print(names(survey_data)[i])
    print(table(survey_data[, i], useNA = "ifany"))
    print(round(prop.table(table(survey_data[, i], useNA = "ifany")) * 100, 2))write.table(
}
