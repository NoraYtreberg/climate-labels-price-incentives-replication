# ------------------------------------------------------------------------------
# 03_figure_4.R
#
# Purpose:
#   Creates Figure 4: choice shares by treatment group in the choice experiment.
#
# Input:
#   Data/analysis_input_deidentified.rds
#
# Output:
#   Output/Figure_4_choice_shares_by_treatment.pdf
#
# Notes:
#   This script identifies the chosen alternative in each choice task,
#   computes the share of each mince type within each treatment group,
#   and plots the resulting choice shares as grouped bars with percentage labels.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(tibble)

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_input_deidentified.rds")
output_file <- file.path("Output", "Figure_4_choice_shares_by_treatment.pdf")

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
# Identify chosen alternative in each choice task
# ------------------------------------------------------------------------------

only_chosen <- db |>
  select(respid, treatment, choice, starts_with("alt_")) |>
  pivot_longer(
    cols = starts_with("alt_"),
    names_to = "alt",
    values_to = "mince"
  ) |>
  mutate(
    alt = parse_number(str_remove(alt, "alt_"))
  ) |>
  group_by(respid) |>
  filter(choice == alt) |>
  ungroup()

# ------------------------------------------------------------------------------
# Compute choice shares within treatment groups
# ------------------------------------------------------------------------------

total_responses <- only_chosen |>
  group_by(treatment) |>
  summarise(
    total_responses = n(),
    .groups = "drop"
  )

distribution2 <- only_chosen |>
  group_by(mince, treatment) |>
  count() |>
  left_join(total_responses, by = "treatment") |>
  mutate(
    perc = n / total_responses
  )

# Set factor ordering ----------------------------------------------------------
distribution2$mince <- factor(
  distribution2$mince,
  levels = c("beef", "fifty", "vegan", "chicken", "pork", "no_mince")
)

distribution2$treatment <- factor(
  distribution2$treatment,
  levels = c("Control", "Label", "Discount", "Label + Discount")
)

# ------------------------------------------------------------------------------
# Create figure
# ------------------------------------------------------------------------------

fig_4 <- ggplot(distribution2, aes(x = treatment, y = perc, fill = mince)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(
    aes(label = percent(perc, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(
    name = "Mince type",
    values = c(
      "beef" = "#F8766D",
      "fifty" = "#B79F00",
      "vegan" = "#00BA38",
      "chicken" = "#00BFC4",
      "pork" = "#619CFF",
      "no_mince" = "#F564E3"
    ),
    labels = c(
      "beef" = "Beef",
      "fifty" = "Fifty/fifty",
      "vegan" = "Plant-based",
      "chicken" = "Chicken",
      "pork" = "Pork",
      "no_mince" = "Opt-out"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Treatment",
    y = "Choice share"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14)
  )

# Save figure ------------------------------------------------------------------
ggsave(
  filename = output_file,
  plot = fig_4,
  width = 9,
  height = 5.5,
  dpi = 300
)

message("Figure 4 saved to: ", output_file)

# Optional interactive display -------------------------------------------------
print(fig_4)
