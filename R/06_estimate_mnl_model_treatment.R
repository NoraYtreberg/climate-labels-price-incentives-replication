# ------------------------------------------------------------------------------
# 06_estimate_mnl_model_treatment.R
#
# Purpose:
#   Estimates the main multinomial logit (MNL) model, with treatment interactions, 
#   for the choice experiment.
#
# Input:
#   Data/analysis_data.rds
#
# Output:
#   Apollo output files saved in Output/
#
# Notes:
#   The script reshapes the choice data to identify the chosen alternative,
#   defines the utility specification, estimates the model using Apollo, and
#   saves the estimation output.
# ------------------------------------------------------------------------------

# Clear memory -----------------------------------------------------------------
rm(list = ls())

# Load packages ----------------------------------------------------------------
library(apollo)
library(dplyr)
library(tidyr)
library(readr)

# Initialise Apollo ------------------------------------------------------------
apollo_initialise()

# File paths -------------------------------------------------------------------
input_file <- file.path("Data", "analysis_data.rds")
output_dir <- "Output"

# Check input ------------------------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Apollo controls --------------------------------------------------------------
apollo_control <- list(
  modelName = "MNL_main_model",
  modelDescr = "Multinomial logit model of minced meat choices with treatment interactions",
  indivID = "respid",
  outputDirectory = output_dir,
  panelData = TRUE
)

# Load analysis data -----------------------------------------------------------
db <- readRDS(input_file) |>
  as_tibble()

# ------------------------------------------------------------------------------
# Prepare choice variable for Apollo
#
# The raw data contain:
#   - a numeric variable 'choice' indicating the chosen alternative
#   - labelled alternative columns alt_1, alt_2, ..., alt_6
#
# This block creates 'choice_label', which matches the names used in the utility
# specification below (beef, fifty, vegan, chicken, pork, no_mince).
# ------------------------------------------------------------------------------

database <- db |>
  select(respid, ct, choice, starts_with("alt_")) |>
  pivot_longer(
    cols = starts_with("alt_"),
    names_to = "alt",
    values_to = "choice_label"
  ) |>
  mutate(
    alt = parse_number(alt)
  ) |>
  group_by(respid, ct) |>
  filter(choice == alt) |>
  ungroup() |>
  select(-alt) |>
  left_join(db, by = c("respid", "ct", "choice"))


# ------------------------------------------------------------------------------
# Define model parameters
# ------------------------------------------------------------------------------

apollo_beta <- c(
  asc_beef = 0,
  asc_fifty = 0,
  asc_vegan = 0,
  asc_chicken = 0,
  asc_pork = 0,
  asc_no_mince = 0,
  b_price = 0,

  a_beef_label = 0,
  a_beef_discount = 0,
  a_beef_label_discount = 0,

  a_fifty_label = 0,
  a_fifty_discount = 0,
  a_fifty_label_discount = 0,

  a_vegan_label = 0,
  a_vegan_discount = 0,
  a_vegan_label_discount = 0,

  a_chicken_label = 0,
  a_chicken_discount = 0,
  a_chicken_label_discount = 0,

  a_pork_label = 0,
  a_pork_discount = 0,
  a_pork_label_discount = 0
)

# Keep the alternative-specific constant for "no mince" fixed ------------------
apollo_fixed <- c("asc_no_mince")

# Validate Apollo inputs -------------------------------------------------------
apollo_inputs <- apollo_validateInputs()

# ------------------------------------------------------------------------------
# Define model probabilities
# ------------------------------------------------------------------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Treatment indicators
  is_label <- as.numeric(treatment == "Label")
  is_discount <- as.numeric(treatment == "Discount")
  is_label_discount <- as.numeric(treatment == "Label + Discount")

  # Alternative-specific constants with treatment interactions
  beef <- asc_beef +
    a_beef_label * is_label +
    a_beef_discount * is_discount +
    a_beef_label_discount * is_label_discount

  fifty <- asc_fifty +
    a_fifty_label * is_label +
    a_fifty_discount * is_discount +
    a_fifty_label_discount * is_label_discount

  vegan <- asc_vegan +
    a_vegan_label * is_label +
    a_vegan_discount * is_discount +
    a_vegan_label_discount * is_label_discount

  chicken <- asc_chicken +
    a_chicken_label * is_label +
    a_chicken_discount * is_discount +
    a_chicken_label_discount * is_label_discount

  pork <- asc_pork +
    a_pork_label * is_label +
    a_pork_discount * is_discount +
    a_pork_label_discount * is_label_discount

  # Utility functions
  V <- list(
    beef = beef + b_price * price_beef,
    fifty = fifty + b_price * price_fifty,
    vegan = vegan + b_price * price_vegan,
    chicken = chicken + b_price * price_chicken,
    pork = pork + b_price * price_pork,
    no_mince = asc_no_mince
  )

  # All alternatives are available in each choice task
  avail <- list(
    beef = 1,
    fifty = 1,
    vegan = 1,
    chicken = 1,
    pork = 1,
    no_mince = 1
  )

  # MNL settings
  mnl_settings <- list(
    alternatives = c(
      beef = "beef",
      fifty = "fifty",
      vegan = "vegan",
      chicken = "chicken",
      pork = "pork",
      no_mince = "no_mince"
    ),
    avail = avail,
    choiceVar = choice_label,
    V = V
  )

  # Choice probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Product across panel observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Prepare output
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  return(P)
}

# Estimate model ---------------------------------------------------------------
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

# Output results ---------------------------------------------------------------
apollo_modelOutput(model)
apollo_saveOutput(model, saveOutput_settings = list(saveEst = TRUE))

