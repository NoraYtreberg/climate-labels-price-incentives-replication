# ------------------------------------------------------------------------------
# 09_lrtest_main_lc_vs_mnl.R
#
# Purpose:
#   Performs a likelihood-ratio test comparing the main latent class (LC) model
#   to the multinomial logit (MNL) model with treatment interactions.
#
# Input:
#   Output/MNL_main_model_model.rds
#   Output/LC_model_model.rds
#
# Output:
#   Output/lrtest_main_lc_vs_mnl.txt
#
# Notes:
#   This script compares the fitted treatment-interaction MNL model and the main
#   LC model using Apollo's likelihood-ratio test function.
# ------------------------------------------------------------------------------

# Load package -----------------------------------------------------------------
library(apollo)

# File paths -------------------------------------------------------------------
mnl_model_file <- file.path("Output", "MNL_main_model_model.rds")
lc_model_file  <- file.path("Output", "LC_model_model.rds")
output_file    <- file.path("Output", "lrtest_main_lc_vs_mnl.txt")

# Check inputs -----------------------------------------------------------------
if (!file.exists(mnl_model_file)) {
  stop("MNL model file not found: ", mnl_model_file)
}

if (!file.exists(lc_model_file)) {
  stop("LC model file not found: ", lc_model_file)
}

if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load fitted models -----------------------------------------------------------
mnl_model <- readRDS(mnl_model_file)
lc_model  <- readRDS(lc_model_file)

# Run LR test ------------------------------------------------------------------
lr_test_output <- capture.output(
  apollo_lrTest(mnl_model, lc_model)
)

# Save output ------------------------------------------------------------------
writeLines(lr_test_output, output_file)

message("LR test output saved to: ", output_file)

# Print to console -------------------------------------------------------------
cat(paste(lr_test_output, collapse = "\n"))