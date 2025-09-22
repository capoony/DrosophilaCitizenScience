# Extract column headers from Excel file
library(readxl)

# Set working directory
setwd("D:/GitHub/DrosophilaCitizenScience")

# Read the Excel file
data <- read_excel("data/Survey_Data_analysis - Kopie.xlsx", sheet = 1)

# Print column names
cat("Column headers from Survey_Data_analysis - Kopie.xlsx:\n")
cat("Total columns:", ncol(data), "\n\n")

for (i in 1:length(names(data))) {
    cat(sprintf("%2d. %s\n", i, names(data)[i]))
}

# Also show data dimensions
cat("\nData dimensions:", nrow(data), "rows x", ncol(data), "columns\n")

# Show first few rows to understand data structure
cat("\nFirst 3 rows (sample data):\n")
print(head(data, 3))
