# Combining climate labels and price incentives to promote climate-friendly food choices

This replication package contains the data and code required to reproduce the main results, appendix results, and figures reported in the paper.

## Folder structure

- `Data/` contains the de-identified input data, derived analysis datasets, and stored starting-value files used in the latent class models.
- `R/` contains the scripts used to construct datasets, estimate models, and reproduce figures and tables.
- `Output/` contains model outputs and generated tables and figures.

## Data

The replication package starts from a de-identified input dataset:

- `Data/analysis_input_deidentified.rds`

This file is the starting point for the public replication workflow. It excludes direct identifiers and other private information not required to reproduce the analysis.

Derived datasets created by the scripts include:

- `Data/analysis_data.rds`
- `Data/analysis_data_with_pca.rds`

## Software requirements

The code is written in R and relies on several R packages, including in particular:

- `dplyr`
- `tidyr`
- `ggplot2`
- `apollo`
- `gtsummary`
- `flextable`
- `ggridges`
- `patchwork`

The latent class and multinomial logit models are estimated using the `apollo` package.

## Running the replication package

Scripts are intended to be run from the project root so that relative paths work correctly.

A typical replication workflow is:

1. Create the analysis dataset
2. Create the PCA-based norm dataset
3. Estimate the main models
4. Compute post-estimation quantities
5. Reproduce figures and appendix tables

## Scripts

### Data construction

`01_create_analysis_data.R`  
Creates the analysis dataset from the de-identified input data.

`02_create_pca_norm_data.R`  
Constructs a PCA-based norm variable from the three general norm variables in the analysis dataset. The script saves a derived dataset, `Data/analysis_data_with_pca.rds`, which includes the first principal component (`norm_pc1`).

### Figures

`03_figure_4.R`  
Creates Figure 4, showing choice shares by treatment group in the choice experiment.

`04_figure_5.R`  
Creates Figure 5, a density plot of the PCA-based norm variable.

`10_figure_6.R`  
Creates Figure 6 from the long-format latent-class post-estimation dataset. The figure shows the distribution of the climate norm score (PC1) by latent class, using posterior class probabilities as weights in a ridgeline density plot.

`11_figure_7.R`  
Creates Figure 7, which plots posterior individual willingness to pay against the climate norm score by treatment group.

`12_figure_8.R`  
Creates Figure 8, which shows mean posterior willingness to pay by quartiles of the climate norm score and treatment group.

### Model estimation

`05_estimate_mnl_model_simple.R`  
Estimates the simple multinomial logit model.

`06_estimate_mnl_model_treatment.R`  
Estimates the multinomial logit model with treatment interactions.

`07_estimate_main_lc_model.R`  
Estimates the main 5-class latent class model.

`08_estimate_lc_model_simple.R`  
Estimates the simple 5-class latent class model without treatment interactions. This model is used for post-estimation analysis and related figures.

### Post-estimation

`09_compute_lc_postestimation_wtp.R`  
Computes post-estimation willingness-to-pay measures from the estimated latent class model. The script calculates class-specific willingness to pay for the vegan option, the unconditional average willingness to pay, and respondent-level posterior willingness to pay. It also creates the aligned datasets used in the later latent-class figures.

### Tables

`13_table_A1.R`  
Reproduces Appendix Table A1, the balance table by treatment group.

`14_table_A2.R`  
Reproduces Appendix Table A2, which reports response rates on the climate norm items.

## Notes on latent class models

Latent class models are sensitive to starting values because the log-likelihood function may contain multiple local optima. During model development, multiple starting values were explored, including the use of Apollo’s `searchStart` routine, to identify stable converged solutions.

To ensure reproducibility of the published specifications, the replication package includes stored starting-value vectors for the latent class models. These are loaded directly by the latent class estimation scripts. No random seed was set during the exploratory search process used in model development.

## Note on Appendix Table D1

Appendix Table D1 reports model-selection statistics from latent class models with between 2 and 10 classes. Because latent class models are sensitive to starting values and may converge to different local optima, the exact values in this table depend on the exploratory search process used during model development. Starting values were informed by simpler multinomial logit models, while additional class-specific parameters were initialized with values close to zero. Multiple runs were explored to identify stable converged solutions. The replication package therefore focuses on reproducing the final reported models rather than recreating the full class-selection search procedure from scratch. No random seed was set during this exploratory search process.