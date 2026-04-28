# ------------------------------------------------------------------------------
# run_all.R
#
# Purpose:
#   Runs the full replication workflow in sequence.
# ------------------------------------------------------------------------------

rm(list = ls())

# Required packages ------------------------------------------------------------
required_packages <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "stringr",
  "tibble",
  "scales",
  "patchwork",
  "ggridges",
  "gtsummary",
  "flextable",
  "haven",
  "apollo"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ".\nPlease install them before running run_all.R.\n\n",
      "Example:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      '), repos = "https://cran.r-project.org/")'
    )
  )
}

# Run scripts ------------------------------------------------------------------
source(file.path("R", "01_create_analysis_data.R"))
source(file.path("R", "02_create_pca_norm_data.R"))
source(file.path("R", "03_figure_4.R"))
source(file.path("R", "04_figure_5.R"))
source(file.path("R", "05_estimate_mnl_model_simple.R"))
source(file.path("R", "06_estimate_mnl_model_treatment.R"))
source(file.path("R", "07_estimate_main_lc_model.R"))
source(file.path("R", "08_lrtest_lc_vs_mnl.R"))
source(file.path("R", "09_estimate_lc_model_simple.R"))
source(file.path("R", "10_compute_lc_postestimation_wtp.R"))
source(file.path("R", "11_figure_6.R"))
source(file.path("R", "12_figure_7.R"))
source(file.path("R", "13_figure_8.R"))
source(file.path("R", "14_table_A1.R"))
source(file.path("R", "15_table_A2.R"))
1
message("Replication workflow completed.")