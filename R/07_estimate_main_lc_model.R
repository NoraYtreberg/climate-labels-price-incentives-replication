# ------------------------------------------------------------------------------
# 07_estimate_main_lc_model.R
#
# Purpose:
#   Estimates the 5-class latent class (LC) model for the choice experiment.
#
# Input:
#   Data/analysis_data_with_pca.rds
#   Output/mnl_interaction_model.rds
#
# Output:
#   Apollo output files saved in Output/
#
# Notes:
#   This script estimates a 5-class latent class model of minced meat choices.
#   Because latent class models are sensitive to starting values, the replication 
#   package uses a stored vector of starting values included in 
#   Data/lc_simple_start_values_paper.rds.
#   These values replect the converged specification used to reproduce the published
#   results.
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
data_file <- file.path("Data", "analysis_data_with_pca.rds")
start_values_file <- file.path("Data", "lc_start_values_paper.rds")
output_dir <- "Output"

# Check inputs -----------------------------------------------------------------
if (!file.exists(data_file)) {
  stop("Input data file not found: ", data_file)
}

if (!file.exists(start_values_file)) {
  stop("Starting values file not found: ", start_values_file)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Apollo controls --------------------------------------------------------------
apollo_control <- list(
  modelName = "LC_model",
  modelDescr = "5-class latent class model of minced meat choices with common treatment effects",
  indivID = "respid",
  nCores = 3,
  outputDirectory = output_dir,
  panelData = TRUE
)

# Load data --------------------------------------------------------------------
db <- readRDS(data_file) |>
  as_tibble()

# ------------------------------------------------------------------------------
# Prepare choice variable for Apollo
#
# The dataset contains:
#   - a numeric variable 'choice' indicating the chosen alternative
#   - labelled alternative columns alt_1, alt_2, ..., alt_6
#
# This block creates 'choice_label', which matches the alternative names used
# in the utility specification below.
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
# Define starting values
#
# Common treatment-interaction parameters will be updated using estimates from
# the interacted MNL model. Class-specific parameters are initialized here.
# ------------------------------------------------------------------------------

apollo_beta <- c(
  # Fixed reference utility for no_mince
  b_no_mince = 0,

  # Common treatment-interaction coefficients
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
  a_pork_label_discount = 0,

  # Class 1
  b_beef_1 = 0,
  b_fifty_1 = 0,
  b_vegan_1 = 0,
  b_chicken_1 = 0,
  b_pork_1 = 0,
  b_price_1 = 0,

  # Class 2
  b_beef_2 = 0,
  b_fifty_2 = 0,
  b_vegan_2 = 0,
  b_chicken_2 = 0,
  b_pork_2 = 0,
  b_price_2 = 0,

  # Class 3
  b_beef_3 = 0,
  b_fifty_3 = 0,
  b_vegan_3 = 0,
  b_chicken_3 = 0,
  b_pork_3 = 0,
  b_price_3 = 0,

  # Class 4
  b_beef_4 = 0,
  b_fifty_4 = 0,
  b_vegan_4 = 0,
  b_chicken_4 = 0,
  b_pork_4 = 0,
  b_price_4 = 0,

  # Class 5
  b_beef_5 = 0,
  b_fifty_5 = 0,
  b_vegan_5 = 0,
  b_chicken_5 = 0,
  b_pork_5 = 0,
  b_price_5 = 0,

  # Class membership parameters
  g_constant_1 = 0,
  g_constant_2 = 0.5,
  g_constant_3 = 0.5,
  g_constant_4 = 0.5,
  g_constant_5 = 0.5
)

# Reference parameters kept fixed ----------------------------------------------
apollo_fixed <- c(
  "b_no_mince",
  "g_constant_1"
)

# Load stored starting values --------------------------------------------------
lc_start_values <- readRDS(start_values_file)

matching_pars <- intersect(names(apollo_beta), names(lc_start_values))
apollo_beta[matching_pars] <- lc_start_values[matching_pars]

message("Imported starting values for these parameters:")
print(matching_pars)

# ------------------------------------------------------------------------------
# Define latent class parameters
# ------------------------------------------------------------------------------

apollo_lcPars <- function(apollo_beta, apollo_inputs) {

  lcpars <- list(
    b_beef = list(b_beef_1, b_beef_2, b_beef_3, b_beef_4, b_beef_5),
    b_fifty = list(b_fifty_1, b_fifty_2, b_fifty_3, b_fifty_4, b_fifty_5),
    b_vegan = list(b_vegan_1, b_vegan_2, b_vegan_3, b_vegan_4, b_vegan_5),
    b_chicken = list(b_chicken_1, b_chicken_2, b_chicken_3, b_chicken_4, b_chicken_5),
    b_pork = list(b_pork_1, b_pork_2, b_pork_3, b_pork_4, b_pork_5),
    b_price = list(b_price_1, b_price_2, b_price_3, b_price_4, b_price_5)
  )

  # Class allocation utilities
  V <- list(
    class_1 = g_constant_1,
    class_2 = g_constant_2,
    class_3 = g_constant_3,
    class_4 = g_constant_4,
    class_5 = g_constant_5
  )

  classAlloc_settings <- list(
    classes = c(
      class_1 = 1,
      class_2 = 2,
      class_3 = 3,
      class_4 = 4,
      class_5 = 5
    ),
    utilities = V
  )

  lcpars[["pi_values"]] <- apollo_classAlloc(classAlloc_settings)

  return(lcpars)
}

# Validate Apollo inputs -------------------------------------------------------
apollo_inputs <- apollo_validateInputs()

# ------------------------------------------------------------------------------
# Define model probabilities
# ------------------------------------------------------------------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  mnl_settings <- list(
    alternatives = c(
      beef = "beef",
      fifty = "fifty",
      vegan = "vegan",
      chicken = "chicken",
      pork = "pork",
      no_mince = "no_mince"
    ),
    avail = list(
      beef = 1,
      fifty = 1,
      vegan = 1,
      chicken = 1,
      pork = 1,
      no_mince = 1
    ),
    choiceVar = choice_label
  )

  # Treatment indicators
  is_label <- as.numeric(treatment == "Label")
  is_discount <- as.numeric(treatment == "Discount")
  is_label_discount <- as.numeric(treatment == "Label + Discount")

  # Within-class choice probabilities
  for (s in 1:5) {

    V <- list(
      beef = b_beef[[s]] +
        a_beef_label * is_label +
        a_beef_discount * is_discount +
        a_beef_label_discount * is_label_discount +
        b_price[[s]] * price_beef,

      fifty = b_fifty[[s]] +
        a_fifty_label * is_label +
        a_fifty_discount * is_discount +
        a_fifty_label_discount * is_label_discount +
        b_price[[s]] * price_fifty,

      vegan = b_vegan[[s]] +
        a_vegan_label * is_label +
        a_vegan_discount * is_discount +
        a_vegan_label_discount * is_label_discount +
        b_price[[s]] * price_vegan,

      chicken = b_chicken[[s]] +
        a_chicken_label * is_label +
        a_chicken_discount * is_discount +
        a_chicken_label_discount * is_label_discount +
        b_price[[s]] * price_chicken,

      pork = b_pork[[s]] +
        a_pork_label * is_label +
        a_pork_discount * is_discount +
        a_pork_label_discount * is_label_discount +
        b_price[[s]] * price_pork,

      no_mince = b_no_mince
    )

    mnl_settings$utilities <- V
    mnl_settings$componentName <- paste0("Class_", s)

    P[[paste0("Class_", s)]] <- apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_", s)]] <- apollo_panelProd(
      P[[paste0("Class_", s)]],
      apollo_inputs,
      functionality
    )
  }

  lc_settings <- list(
    inClassProb = P,
    classProb = pi_values
  )

  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)
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


# ------------------------------------------------------------------#
#---- POST ESTIMATION: TESTING SYNERGY                                           ----
# ------------------------------------------------------------------#
#Test for positive synergy
apollo_deltaMethod(
  model,
  list(
    expression = c(
      synergy_additive = "a_vegan_label_discount - a_vegan_label - a_vegan_discount"
    ),
    varcov = "robust",
    printPVal = 2
  )
)

#Test negative synergy
apollo_deltaMethod(
  model,
  list(
    expression = c(
      synergy_additive = "a_vegan_label_discount - a_vegan_label - a_vegan_discount"
    ),
    varcov = "robust",
    printPVal = 1
  )
)

#Test marginal effects
apollo_deltaMethod(model, list(
  expression = c(
    disc_given_label = "a_vegan_label_discount - a_vegan_label",
    label_given_disc = "a_vegan_label_discount - a_vegan_discount"
  ),
  varcov = "robust",
  printPVal = 2
))
