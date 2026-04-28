# ------------------------------------------------------------------------------
# 14_table_A2.R
#
# Purpose:
#   Reproduces Appendix Table A2: response rates on the climate norm items.
#
# Input:
#   Data/analysis_input_deidentified.rds
#
# Output:
#   Output/table_a2_climate_norm_response_rates.csv
#
# Notes:
#   Shares are calculated across all non-missing responses for each item,
#   including "Don't know" as a response category when present.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tibble)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_input_deidentified.rds")
output_file <- file.path("Output", "table_a2_climate_norm_response_rates.csv")

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

# Response order used in the paper ---------------------------------------------
response_levels <- c(
  "Fits poorly",
  "Fits fairly poorly",
  "Fits neither poorly nor well",
  "Fits fairly well",
  "Fits well",
  "Don't know",
  "I don't know"
)

# Helper function --------------------------------------------------------------
make_response_table <- function(data, var, item_label) {
  var_sym <- rlang::sym(var)

  data |>
    filter(!is.na(!!var_sym)) |>
    count(response = !!var_sym) |>
    mutate(
      share = round(100 * n / sum(n), 1),
      item = item_label
    ) |>
    select(item, response, share)
}

# Build table ------------------------------------------------------------------
table_a2 <- bind_rows(
  make_response_table(
    db,
    "spm605_1",
    "I have a responsibility to reduce my greenhouse gas emissions"
  ),
  make_response_table(
    db,
    "spm605_2",
    "I have a duty to reduce my greenhouse gas emissions"
  ),
  make_response_table(
    db,
    "spm605_3",
    "I get a bad conscience if I do not reduce my own greenhouse gas emissions"
  )
) |>
  mutate(
    response = factor(response, levels = response_levels)
  ) |>
  arrange(item, response) |>
  mutate(
    response = as.character(response)
  )

# Print table ------------------------------------------------------------------
print(table_a2, n = Inf)

# Save output ------------------------------------------------------------------
write.csv(table_a2, output_file, row.names = FALSE)

message("Table A2 data saved to: ", output_file)
