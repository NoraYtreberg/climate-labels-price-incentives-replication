# ------------------------------------------------------------------------------
# 11_figure_7.R
#
# Purpose:
#   Creates Figure 7 using the exact plotting data prepared in the latent-class
#   post-estimation step.
#
# Input:
#   Data/lc_simple_figure7_data.rds
#
# Output:
#   Output/Figure_7_lc_wtp_norm_scatter.pdf
#
# Notes:
#   This script uses the same plotting logic as the original figure-producing
#   code so that the graph matches the published version as closely as possible.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(ggplot2)
library(patchwork)
library(dplyr)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "lc_simple_figure7_data.rds")
output_file <- file.path("Output", "Figure_7_lc_wtp_norm_scatter.pdf")

# Check input/output -----------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load plotting data -----------------------------------------------------------
analysis_df <- readRDS(input_file)

# Base plot: points + linear fits without confidence intervals -----------------
p_base <- ggplot(analysis_df, aes(x = norm_pc1, y = WTP, color = treatment)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    x = "Climate norm score (PC1)",
    y = "Posterior individual WTP (NOK)"
  ) +
  scale_color_discrete(name = "Treatment") +
  theme_minimal()

# Left panel: no confidence intervals ------------------------------------------
p_no_ci <- p_base +
  labs(title = "Linear fits (no CI)")

# Right panel: 95% confidence intervals ----------------------------------------
p_ci_95 <- ggplot(
  analysis_df,
  aes(x = norm_pc1, y = WTP, color = treatment, fill = treatment)
) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, linewidth = 1, alpha = 0.15) +
  labs(
    title = "Linear fits (95% CI)",
    x = "Climate norm score (PC1)",
    y = "Posterior individual WTP (NOK)"
  ) +
  scale_color_discrete(name = "Treatment") +
  scale_fill_discrete(name = "Treatment") +
  theme_minimal()

# Combine panels ---------------------------------------------------------------
fig_7 <- p_no_ci | p_ci_95

# Save figure ------------------------------------------------------------------
ggsave(
  filename = output_file,
  plot = fig_7,
  width = 12,
  height = 6
)

message("Figure 7 saved to: ", output_file)

# Optional interactive display -------------------------------------------------
print(fig_7)

