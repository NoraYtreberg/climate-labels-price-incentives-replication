# ------------------------------------------------------------------------------
# 01_create_analysis_data.R
#
# Purpose:
#   Creates the analysis dataset used in the paper from the de-identified input
#   data included in the replication package.
#
# Input:
#   Data/analysis_input_deidentified.rds
#
# Output:
#   Data/analysis_data.rds
#   
#
# Notes:
#   This script performs data transformations used in the analysis, including:
#   - construction of treatment indicators
#   - demographic recodes used in the analysis
#   - construction of norm variables
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_input_deidentified.rds")
output_file <- file.path("Data", "analysis_data.rds")

# Check input ------------------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Load de-identified input data ------------------------------------------------
db <- readRDS(input_file) |>
  as_tibble()

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

# Recode general norm items to a 1-5 continuous scale
recode_general_norm <- function(x) {
  case_when(
    x == "Fits poorly" ~ 1,
    x == "Fits fairly poorly" ~ 2,
    x == "Fits neither poorly nor well" ~ 3,
    x == "Fits fairly well" ~ 4,
    x == "Fits well" ~ 5,
    x == "I don't know" ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# Recode specific norm item to a 1-5 continuous scale
recode_specific_norm <- function(x) {
  case_when(
    x == "Unimportant" ~ 1,
    x == "Somewhat unimportant" ~ 2,
    x == "Neither unimportant nor important" ~ 3,
    x == "Somewhat important" ~ 4,
    x == "Very important" ~ 5,
    x == "I don't know" ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# ------------------------------------------------------------------------------
# Create analysis variables
# ------------------------------------------------------------------------------

db <- db |>
  mutate(
    # Basic variable types ------------------------------------------------------
    respid = as.numeric(respid),
    choice = as.numeric(choice),
    across(starts_with("alt_"), as.character),
    treatment = as.character(treatment),

    # Demographics --------------------------------------------------------------
    female = as.numeric(gender == "Female"),

    # Treatment indicators ------------------------------------------------------
    label = as.numeric(treatment == "Label"),
    discount = as.numeric(treatment == "Discount"),
    combo = as.numeric(treatment == "Label + Discount"),

    # Norm variables ------------------------------------------------------------
    general_norm_cont_1 = recode_general_norm(spm605_1),
    general_norm_cont_2 = recode_general_norm(spm605_2),
    general_norm_cont_3 = recode_general_norm(spm605_3),
    specific_norm_cont = recode_specific_norm(spm503_1)
  )

# ------------------------------------------------------------------------------
# Save analysis data
# ------------------------------------------------------------------------------

saveRDS(db, output_file)

message("Analysis dataset saved to: ", output_file)
message("Rows: ", nrow(db))
message("Columns: ", ncol(db))

