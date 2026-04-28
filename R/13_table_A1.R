# ------------------------------------------------------------------------------
# 13_table_A1.R
#
# Purpose:
#   Reproduces Appendix Table A1: balance table by treatment group.
#
# Input:
#   Data/analysis_input_deidentified.rds
#
# Output:
#   Output/Table_A1_balance_table.docx
#
# Notes:
#   The balance table is constructed at the respondent level. The input dataset
#   is stored in long format for the choice experiment, so this script first
#   collapses the data to one row per respondent and then produces summary
#   statistics by treatment group with p-values for balance checks.
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------
library(dplyr)
library(haven)
library(gtsummary)
library(flextable)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_input_deidentified.rds")
output_file <- file.path("Output", "Table_A1_balance_table.docx")

# Check input/output -----------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load data --------------------------------------------------------------------
db <- readRDS(input_file) |>
  as_tibble()

# ------------------------------------------------------------------------------
# Collapse to respondent level
#
# The dataset contains multiple rows per respondent because each respondent
# completed several choice tasks. For the balance table, we keep one row per
# respondent with the background variables used in the appendix.
# ------------------------------------------------------------------------------

respondent_data <- db |>
  select(
    respid,
    treatment,
    age,
    gender,
    education,
    personal_income,
    shopping_experience
  ) |>
  distinct(respid, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# Convert labelled variables to factors where relevant
# ------------------------------------------------------------------------------

summarydata <- respondent_data |>
  mutate(across(where(is.labelled), as_factor))

# ------------------------------------------------------------------------------
# Create balance table
# ------------------------------------------------------------------------------

balance_table <- summarydata |>
  select(
    treatment,
    age,
    gender,
    education,
    personal_income,
    shopping_experience
  ) |>
  tbl_summary(
    by = treatment,
    label = list(
      age ~ "Age",
      gender ~ "Gender",
      education ~ "Education",
      personal_income ~ "Personal income",
      shopping_experience ~ "Shopping experience"
    )
  ) |>
  add_p() |>
  modify_header(label ~ "**Summary statistics and balance checks**")

# Save table -------------------------------------------------------------------
balance_table |>
  as_flex_table() |>
  flextable::save_as_docx(path = output_file)

message("Table A1 saved to: ", output_file)
