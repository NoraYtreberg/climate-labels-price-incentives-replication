# ------------------------------------------------------------------------------
# 09_compute_lc_simple_postestimation_wtp.R
#
# Purpose:
#   Computes post-estimation willingness-to-pay (WTP) measures from the
#   estimated simple latent class model and saves respondent-level datasets for
#   later figure scripts.
#
# Input:
#   Output/LC_simple_model_model.rds
#   Output/lc_simple_database.rds
#   Output/lc_simple_postestimation_objects.RData
#
# Output:
#   Data/lc_simple_postestimation_wtp.rds
#   Data/lc_simple_figure7_data.rds
#   Data/lc_simple_norm_long.rds
#   Output/lc_simple_wtp_summary.rds
#
# Notes:
#   This script computes:
#   - class-specific WTP for the vegan option
#   - unconditional average WTP
#   - respondent-level posterior WTP
#
#   It also recreates the aligned respondent-level plotting dataset used for
#   Figure 7 and the long-format norm dataset used for Figure 6.
# ------------------------------------------------------------------------------

# Load packages ----------------------------------------------------------------
library(apollo)
library(dplyr)
library(tibble)
library(tidyr)

# File paths -------------------------------------------------------------------
model_file <- file.path("Output", "LC_simple_model_model.rds")
database_file <- file.path("Output", "lc_simple_database.rds")
apollo_objects_file <- file.path("Output", "lc_simple_postestimation_objects.RData")

wtp_data_file <- file.path("Data", "lc_simple_postestimation_wtp.rds")
figure7_data_file <- file.path("Data", "lc_simple_figure7_data.rds")
norm_long_file <- file.path("Data", "lc_simple_norm_long.rds")
wtp_summary_file <- file.path("Output", "lc_simple_wtp_summary.rds")

# Check inputs -----------------------------------------------------------------
if (!file.exists(model_file)) {
  stop("LC model file not found: ", model_file)
}

if (!file.exists(database_file)) {
  stop("LC database file not found: ", database_file)
}

if (!file.exists(apollo_objects_file)) {
  stop("Apollo post-estimation objects file not found: ", apollo_objects_file)
}

# Load saved components --------------------------------------------------------
model <- readRDS(model_file)
database <- readRDS(database_file)
load(apollo_objects_file)

# ------------------------------------------------------------------------------
# Compute unconditional class-level quantities
# ------------------------------------------------------------------------------

unconditionals <- apollo_unconditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

# Class-specific WTP for vegan option
wtp_class <- (-unlist(unconditionals$b_vegan) / unlist(unconditionals$b_price)) * 100

# Unconditional class probabilities
pi_vec <- unlist(unconditionals$pi_values)

# Unconditional average WTP
wtp_unconditional <- sum(pi_vec * wtp_class)

# ------------------------------------------------------------------------------
# Compute respondent-level posterior WTP
# ------------------------------------------------------------------------------

conditionals <- apollo_conditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

conditionals_df <- as.data.frame(conditionals)

# Assume first column is respondent ID and remaining columns are class probs
id_col <- names(conditionals_df)[1]
prob_cols <- names(conditionals_df)[-1]

class_probs <- as.matrix(conditionals_df[, prob_cols, drop = FALSE])
wtp_class_vec <- as.numeric(wtp_class)

if (ncol(class_probs) != length(wtp_class_vec)) {
  stop(
    "Number of class probability columns does not match length of class-specific WTP vector."
  )
}

# Posterior individual WTP = row-wise weighted average of class-specific WTP
wtp_conditional <- as.numeric(class_probs %*% wtp_class_vec)

wtp_df <- tibble(
  respid = conditionals_df[[id_col]],
  wtp_posterior = wtp_conditional
)

# ------------------------------------------------------------------------------
# Merge respondent-level covariates used in later figures
# ------------------------------------------------------------------------------

respondent_covariates <- database |>
  select(respid, treatment, norm_pc1) |>
  distinct()

wtp_df <- wtp_df |>
  left_join(respondent_covariates, by = "respid") |>
  mutate(
    norm_pc1_quartile = ntile(norm_pc1, 4),
    norm_pc1_quartile = factor(
      norm_pc1_quartile,
      levels = 1:4,
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

# ------------------------------------------------------------------------------
# Recreate the exact plotting dataset for Figure 7
#
# This follows the original workflow closely:
#   - take first-row norm values by respondent
#   - drop missing norms
#   - align WTP and treatment to the same index
# ------------------------------------------------------------------------------

norm_n <- apollo_firstRow(database$norm_pc1, apollo_inputs)

complete_idx <- which(!is.na(norm_n))
norm_n_complete <- norm_n[complete_idx]

wtp_complete <- as.numeric(wtp_conditional[complete_idx])
treatment_complete <- database$treatment[complete_idx]

analysis_df_figure7 <- tibble(
  WTP = wtp_complete,
  norm_pc1 = as.numeric(norm_n_complete),
  treatment = factor(treatment_complete)
)

# ------------------------------------------------------------------------------
# Create long-format norm dataset for Figure 6
# ------------------------------------------------------------------------------

conditionals_complete <- conditionals[complete_idx, ]

norm_df <- data.frame(
  norm_pc1 = norm_n_complete,
  weight_class1 = conditionals_complete[, "X1"],
  weight_class2 = conditionals_complete[, "X2"],
  weight_class3 = conditionals_complete[, "X3"],
  weight_class4 = conditionals_complete[, "X4"],
  weight_class5 = conditionals_complete[, "X5"]
)

norm_long <- norm_df |>
  pivot_longer(
    cols = starts_with("weight_class"),
    names_to = "Class",
    values_to = "Weight"
  ) |>
  mutate(
    Class = sub("^weight_class", "Class ", Class),
    Class = factor(Class, levels = paste("Class", 1:5))
  )

# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------

# Respondent-level dataset for general post-estimation use
saveRDS(wtp_df, wtp_data_file)

# Exact plotting data for Figure 7
saveRDS(analysis_df_figure7, figure7_data_file)

# Long-format norm dataset for Figure 6
saveRDS(norm_long, norm_long_file)

# Summary objects for reference
wtp_summary <- list(
  class_specific_wtp = wtp_class,
  class_probabilities = pi_vec,
  unconditional_wtp = wtp_unconditional
)

saveRDS(wtp_summary, wtp_summary_file)

# Messages ---------------------------------------------------------------------
message("Respondent-level LC post-estimation WTP data saved to: ", wtp_data_file)
message("Figure 7 plotting data saved to: ", figure7_data_file)
message("Figure 6 norm-long data saved to: ", norm_long_file)
message("LC WTP summary saved to: ", wtp_summary_file)

message("Unconditional average WTP: ", round(wtp_unconditional, 2))

message("Class-specific WTP:")
print(wtp_class)

message("Class probabilities:")
print(pi_vec)