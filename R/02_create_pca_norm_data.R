# ------------------------------------------------------------------------------
# 02_create_pca_norm_data.R
#
# Purpose:
#   Constructs a PCA-based norm measure from the three general norm variables
#   and saves a derived analysis dataset including the first principal component.
#
# Input:
#   Data/analysis_data.rds
#
# Output:
#   Data/analysis_data_with_pca.rds
#
# Notes:
#   This script performs principal components analysis (PCA) on the variables
#   general_norm_cont_1, general_norm_cont_2, and general_norm_cont_3.
#   The first principal component is saved as norm_pc1.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_data.rds")
output_file <- file.path("Data", "analysis_data_with_pca.rds")

# Check input ------------------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Load analysis data -----------------------------------------------------------
db <- readRDS(input_file) |>
  as_tibble()

# Prepare data for PCA ---------------------------------------------------------
norm_data <- db |>
  select(general_norm_cont_1, general_norm_cont_2, general_norm_cont_3) |>
  na.omit()

# Run PCA ----------------------------------------------------------------------
pca_result <- prcomp(norm_data, scale. = TRUE)

# Optional: inspect PCA results in console -------------------------------------
message("PCA summary:")
print(summary(pca_result))

message("PCA loadings:")
print(pca_result$rotation)

# Identify complete cases in the original dataset ------------------------------
complete_cases <- complete.cases(
  db |>
    select(general_norm_cont_1, general_norm_cont_2, general_norm_cont_3)
)

# Add first principal component to full dataset --------------------------------
db$norm_pc1 <- NA_real_
db$norm_pc1[complete_cases] <- -predict(pca_result)[, 1]

# Save derived dataset ---------------------------------------------------------
saveRDS(db, output_file)

message("Dataset with PCA variable saved to: ", output_file)
message("Rows: ", nrow(db))
message("Columns: ", ncol(db))
