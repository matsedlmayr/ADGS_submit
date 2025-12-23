# run_all.R
# This script runs the full workflow: model training and evaluation

# Load required libraries
library(here)
library(dplyr)
library(tidyr)
library(ranger)
library(Boruta)
library(caret)
library(terra)
library(tidyterra)
library(ggplot2)
library(readr)

# 1. Train models
message("Running model training...")
source(here::here("analysis/02_model_training.R"))

# 2. Evaluate models
message("Running evaluation...")
source(here::here("analysis/03_evaluation.R"))

message("Workflow completed successfully!")