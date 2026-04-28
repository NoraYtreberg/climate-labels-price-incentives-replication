# ------------------------------------------------------------------------------
# 10_figure_6.R
#
# Purpose:
#   Creates Figure 6: ridgeline plot of the climate norm score (PC1) by latent
#   class, using posterior class probabilities as weights.
#
# Input:
#   Data/lc_simple_norm_long.rds
#
# Output:
#   Output/Figure_6_lc_norm_ridgeline.pdf
#
# Notes:
#   This script uses the long-format norm dataset created in the simple LC
#   post-estimation step.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(ggplot2)
library(ggridges)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "lc_simple_norm_long.rds")
output_file <- file.path("Output", "Figure_6_lc_norm_ridgeline.pdf")

# Check input/output -----------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load plotting data -----------------------------------------------------------
norm_long <- readRDS(input_file)

# Create figure ----------------------------------------------------------------
fig_6 <- ggplot(
  norm_long,
  aes(
    x = norm_pc1,
    y = Class,
    fill = Class,
    weight = Weight
  )
) +
  geom_density_ridges(
    scale = 1.4,
    alpha = 0.6,
    rel_min_height = 0.001
  ) +
  labs(
    x = "Climate norm (PC1)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# Save figure ------------------------------------------------------------------
ggsave(
  filename = output_file,
  plot = fig_6,
  width = 8,
  height = 6
)

message("Figure 6 saved to: ", output_file)

# Optional interactive display -------------------------------------------------
print(fig_6)

