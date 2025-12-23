# prepare_data.R
library(tidyverse)
library(skimr)
library(here)
# Load data
df <- readr::read_csv("https://raw.githubusercontent.com/geco-bern/leafnp_data/main/data/leafnp_tian_et_al.csv")

# Keep the 50 most common species
common_species <- df %>%
  group_by(Species) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:50) %>%
  pull(Species)

# Filter dataset
dfs <- df %>%
  select(leafN, lon, lat, elv, mat, map, ndep, mai, Species) %>%
  filter(Species %in% common_species)

# Define predictors and target
predictors <- c("elv", "mat", "map", "ndep", "mai", "Species")
target <- "leafN"

# Spatial clusters for spatial CV
set.seed(42)
k_clusters <- kmeans(dfs[, c("lon", "lat")], centers = 5)
dfs$spatial_cluster <- k_clusters$cluster

# Environmental clusters for environmental CV
set.seed(42)
k_env <- kmeans(dfs[, c("mat", "map")], centers = 5)
dfs$env_cluster <- k_env$cluster

# Save everything for analysis scripts
save(
  dfs,
  predictors,
  target,
  file = (here::here("data/leafN_prepared_data.RData"))
)
# Quick data summary
skimr::skim(dfs)
