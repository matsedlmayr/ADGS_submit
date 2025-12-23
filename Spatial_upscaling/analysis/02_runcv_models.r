# random_cv.R
library(tidyverse)
library(ranger)
library(purrr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)
# Load prepared data
load(here::here("data/leafN_prepared_data.RData"))  # dfs, predictors, target

# -----------------------------
# Random 5-fold CV
# -----------------------------
set.seed(42)
folds <- sample(1:5, nrow(dfs), replace = TRUE)
group_folds_train <- lapply(1:5, function(k) which(folds != k))
group_folds_test  <- lapply(1:5, function(k) which(folds == k))
# function to train and test a single fold
train_test_fold <- function(train_idx, test_idx) {
  mod <- ranger::ranger(
    x = dfs[train_idx, predictors],
    y = dfs[[target]][train_idx],
    mtry = 3,
    min.node.size = 12,
    seed = 42
  )

  pred <- predict(mod, data = dfs[test_idx, predictors])$predictions
  rsq  <- cor(pred, dfs[[target]][test_idx])^2
  rmse <- sqrt(mean((pred - dfs[[target]][test_idx])^2))

  tibble(rsq = rsq, rmse = rmse)
}


results_random_cv <- map2_dfr(group_folds_train, group_folds_test, train_test_fold) |>
  mutate(fold = 1:5)
# view results
results_random_cv

# mean metrics
results_random_cv %>%
  summarise(mean_rmse = mean(rmse), mean_rsq = mean(rsq))

# Save results
saveRDS(results_random_cv, here::here("data/results_random_cv.rds"))


# -----------------------------
# spatial
# -----------------------------
# get coast outline
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

plot <- ggplot() +

  # plot coastline
  geom_sf(
    data = coast,
    colour = "black",
    size = 0.2
  ) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE
  ) + # to draw map strictly bounded by the specified extent

  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat), color = "red", size = 0.2) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")
print(plot)
ggsave(
  filename = here::here("fig/plot.png"),
  plot = plot,
  width = 7,
  height = 5,
  dpi = 300
)
set.seed(42)
k_clusters <- kmeans(dfs[, c("lon", "lat")], centers = 5)

# Add cluster assignment to the data
dfs$cluster <- as.factor(k_clusters$cluster)

# Quick check
table(dfs$cluster)
p <-ggplot() +
  geom_sf(data = coast, colour = "black", size = 0.2) +
  coord_sf(ylim = c(-60, 80), expand = FALSE) +
  geom_point(data = dfs, aes(x = lon, y = lat, color = cluster), size = 0.5) +
  theme_bw() +
  labs(x = "", y = "", color = "Cluster")
print(p)
ggsave(
  filename = here::here("fig/spatial.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)

group_folds_train <- lapply(1:5, function(k) which(dfs$cluster != k))
group_folds_test  <- lapply(1:5, function(k) which(dfs$cluster == k))

results_spatial_cv <- map2_dfr(group_folds_train, group_folds_test, train_test_fold) |>
  mutate(test_fold = 1:5)

results_spatial_cv

#leaf N by spatial cluster
p2 <- ggplot(dfs, aes(x = cluster, y = leafN, fill = cluster)) +
  geom_boxplot() +
  labs(x = "Spatial Cluster", y = "Leaf N (%)",
       title = "Distribution of leaf N by spatial cluster") +
  theme_bw() +
  theme(legend.position = "none")
print(p2)
ggsave(
  filename = here::here("fig/spatial_cluster.png"),
  plot = p2,
  width = 7,
  height = 5,
  dpi = 300
)

# Save results
saveRDS(results_spatial_cv, here::here("data/results_spatial_cv.rds"))

# -----------------------------
# environmental
# -----------------------------
set.seed(42)  # for reproducibility

# select only environmental variables for clustering
env_data <- dfs |> dplyr::select(mat, map)

# perform k-means with 5 clusters
env_clusters <- kmeans(env_data, centers = 5)

# add cluster assignment to the main data
dfs$env_cluster <- env_clusters$cluster

p3 <- ggplot(dfs, aes(x = mat, y = map, color = factor(env_cluster))) +
  geom_point(alpha = 0.6, size = 1) +
  labs(x = "Mean Annual Temperature (°C)",
       y = "Mean Annual Precipitation (mm yr⁻¹)",
       color = "Environmental Cluster",
       title = "Clustering of data in environmental space") +
  theme_bw()
print(p3)
ggsave(
  filename = here::here("fig/environmental.png"),
  plot = p3,
  width = 7,
  height = 5,
  dpi = 300
)

group_folds_train <- map(1:5, ~ which(dfs$env_cluster != .))
group_folds_test  <- map(1:5, ~ which(dfs$env_cluster == .))
train_test_env_fold <- function(fold_train_idx, fold_test_idx) {
  # Train RF
  mod <- ranger::ranger(
    x = dfs[fold_train_idx, predictors],
    y = dfs$leafN[fold_train_idx],
    mtry = 3,
    min.node.size = 12,
    seed = 42
  )

  # Predict on test fold
  pred <- predict(mod, data = dfs[fold_test_idx, predictors])$predictions

  # Calculate metrics
  rsq  <- cor(pred, dfs$leafN[fold_test_idx])^2
  rmse <- sqrt(mean((pred - dfs$leafN[fold_test_idx])^2))

  tibble(rsq = rsq, rmse = rmse)
}

results_env_cv <- map2_dfr(group_folds_train, group_folds_test, train_test_env_fold) |>
  mutate(fold = 1:5)

# Save results
saveRDS(results_env_cv, here::here("data/results_env_cv.rds"))

