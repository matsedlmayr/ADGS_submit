# run_all.R for Spatial Upscaling
# This script sequentially runs the main analysis scripts

# Load helper packages if needed
library(here)

# 1. Load the data
message("Running 01_loaddata.R...")
source(here::here("analysis/01_loaddata.R"))

# 2. Run models / cross-validation
message("Running 02_runcv_model.R...")
source(here::here("analysis/02_runcv_model.R"))

# 3. Comparison / evaluation
message("Running 03_comparison1.R...")
source(here::here("analysis/03_comparison1.R"))

message("All analysis scripts executed successfully!")