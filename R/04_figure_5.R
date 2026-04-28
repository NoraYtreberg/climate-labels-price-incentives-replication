# ------------------------------------------------------------------------------
# 04_figure_5.R
#
# Purpose:
#   Creates Figure 5: density plot of the PCA-based norm measure (norm_pc1).
#
# Input:
#   Data/analysis_data_with_pca.rds
#
# Output:
#   Output/Figure_5_pca_norm.pdf
#
# Notes:
#   This script uses the dataset created in 02_create_pca_norm_data.R and plots
#   the distribution of the first principal component of the three general norm
#   variables.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_data_with_pca.rds")
output_file <- file.path("Output", "Figure_5_pca_norm.pdf")

# Check input ------------------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

# Create output folder if needed -----------------------------------------------
if (!dir.exists("Output")) {
  dir.create("Output", recursive = TRUE)
}

# Load data --------------------------------------------------------------------
db <- readRDS(input_file) |>
  as_tibble()

# Create figure ----------------------------------------------------------------
fig_5 <- ggplot(db, aes(x = norm_pc1)) +
  geom_density(
    fill = "#F8766D",
    alpha = 0.6,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    limits = c(-4, 4),
    breaks = seq(-4, 4, 2)
  ) +
  labs(
    x = "Climate norm (PC1)",
    y = "Density"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Save figure ------------------------------------------------------------------
ggsave(
  filename = output_file,
  plot = fig_5,
  width = 7,
  height = 5
)

message("Figure saved to: ", output_file)

# Optional interactive display -------------------------------------------------
print(fig_5)
