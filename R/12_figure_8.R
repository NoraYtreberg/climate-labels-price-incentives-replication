# ------------------------------------------------------------------------------
# 12_figure_8.R
#
# Purpose:
#   Creates Figure 8: mean posterior WTP by quartiles of the climate norm score
#   (PC1) and treatment group.
#
# Input:
#   Data/lc_simple_postestimation_wtp.rds
#
# Output:
#   Output/Figure_8_lc_wtp_norm_quartiles.pdf
#
# Notes:
#   This script uses the respondent-level post-estimation WTP dataset created in
#   09_compute_lc_simple_postestimation_wtp.R.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "lc_simple_postestimation_wtp.rds")
output_file <- file.path("Output", "Figure_8_lc_wtp_norm_quartiles.pdf")

# Check input/output -----------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load data --------------------------------------------------------------------
wtp_df <- readRDS(input_file)

# Check required variables -----------------------------------------------------
required_vars <- c("wtp_posterior", "norm_pc1", "treatment")
missing_vars <- setdiff(required_vars, names(wtp_df))

if (length(missing_vars) > 0) {
  stop("Missing required variables in input data: ", paste(missing_vars, collapse = ", "))
}

# Create quartiles and summarise -----------------------------------------------
wtp_q_summary <- wtp_df |>
  filter(
    !is.na(wtp_posterior),
    !is.na(norm_pc1),
    !is.na(treatment)
  ) |>
  mutate(
    norm_quantile = ntile(norm_pc1, 4),
    norm_quantile = factor(
      norm_quantile,
      levels = 1:4,
      labels = paste0("Q", 1:4)
    ),
    treatment = factor(treatment)
  ) |>
  group_by(norm_quantile, treatment) |>
  summarise(
    mean_wtp = mean(wtp_posterior, na.rm = TRUE),
    n = sum(!is.na(wtp_posterior)),
    se = sd(wtp_posterior, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# Create figure ----------------------------------------------------------------
fig_8 <- ggplot(wtp_q_summary, aes(x = norm_quantile, y = mean_wtp, fill = treatment)) +
  geom_col(position = position_dodge(width = 0.85)) +
  geom_errorbar(
    aes(ymin = mean_wtp - 1.96 * se, ymax = mean_wtp + 1.96 * se),
    position = position_dodge(width = 0.85),
    width = 0.2
  ) +
  labs(
    x = "Climate norm quartile (PC1)",
    y = "Mean WTP (NOK)",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Save figure ------------------------------------------------------------------
ggsave(
  filename = output_file,
  plot = fig_8,
  width = 8,
  height = 7
)

message("Figure 8 saved to: ", output_file)

# Optional interactive display -------------------------------------------------
print(fig_8)


