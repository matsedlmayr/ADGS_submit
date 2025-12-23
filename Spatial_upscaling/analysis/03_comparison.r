library(tidyverse)

# Load all CV results
random <- readRDS("data/results_random_cv.rds")
spatial <- readRDS("data/results_spatial_cv.rds")
environmental <- readRDS("data/results_env_cv.rds")

# Combine all
results_random_cv <- random |> mutate(cv_type = "Random")
results_spatial_cv <- spatial |> mutate(cv_type = "Spatial")
results_env_cv     <- environmental     |> mutate(cv_type = "Environmental")

cv_results_long <- bind_rows(
  results_random_cv,
  results_spatial_cv,
  results_env_cv
) |>
  pivot_longer(
    cols = c(rsq, rmse),
    names_to = "metric",
    values_to = "value"
  )
cv_summary <- cv_results_long %>%
  group_by(cv_type, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

cv_summary
p4 <- ggplot(cv_results_long, aes(x = cv_type, y = value)) +
  geom_jitter(
    width = 0.15,
    alpha = 0.4,
    size = 2
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,      # X-shaped mean marker
    size = 5,
    stroke = 1.2,
    color = "black"
  ) +
  facet_wrap(~ metric, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Cross-validation type",
    y = "Value",
    title = "CV results with mean indicated as x"
  )
print(p4)
ggsave(
  filename = "fig/comparison.png",
  plot = p4,
  width = 7,
  height = 5,
  dpi = 300
)

#Random cross-validation (CV) produces a high R² value and a lower root mean squared error (RMSE) because the data is randomly split. As nearby points are likely to be included in both the training and test sets, the model 'cheats' by predicting locations similar to the training points.
#Spatial cross-validation (CV) shows lower R² and higher root mean squared error (RMSE), because test folds are geographically separate. This tests the model’s ability to predict in new regions, i.e. its spatial generalisation.
#These differences highlight spatial overfitting.
#Our model may perform well locally (random CV), but poorly in unobserved regions (spatial CV).
#This emphasises the need for careful spatial validation when upscaling ecological data globally.


#The environmental cross-validation (CV) method omits clusters defined by mean annual temperature and precipitation rather than location.
#The R² and RMSE values are intermediate: worse than random cross-validation (CV) because the model has to predict in unseen environmental conditions, but generally better than the worst spatial CV fold.
#This makes sense: some environmental combinations are more common across regions, meaning the model can generalise to new areas as long as the environmental values overlap with those in the training set.
#This tests the model's ability to generalise in environmental space, which is often more relevant for global predictions than purely spatial cross-validation.
