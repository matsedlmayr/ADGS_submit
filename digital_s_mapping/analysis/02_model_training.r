library(caret)
library(ranger)
library(terra)
library(dplyr)
library(tidyr)
library(here)
library(readr)
library(knitr)
df_full <- readRDS(url(
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/data/df_full.rds",
  "rb"  # read in binary mode
))
#### ???
df_full$waterlog.100 <- factor(df_full$waterlog.100, levels = c(0,1))

###
head(df_full) |> 
  knitr::kable()

df_full$waterlog.100 <- factor(df_full$waterlog.100, levels = c(0,1))

# Specify target: The pH in the top 10cm
target <- "waterlog.100"

# Specify predictors_all: Remove soil sampling and observational data
predictors_all <- names(df_full)[14:ncol(df_full)]

cat("The target is:", target,
    "\nThe predictors_all are:", paste0(predictors_all[1:8], sep = ", "), "...")


# Split dataset into training and testing sets
df_train <- df_full |> dplyr::filter(dataset == "calibration")
df_test  <- df_full |> dplyr::filter(dataset == "validation")
df_train_full <- df_full |> dplyr::filter(dataset == "calibration")
df_test_full  <- df_full |> dplyr::filter(dataset == "validation")

# Filter out any NA to avoid error when running a Random Forest
df_train <- df_train |> tidyr::drop_na()
df_test <- df_test   |> tidyr::drop_na()
df_train_full <- df_train_full |> tidyr::drop_na()
df_test_full  <- df_test_full |> tidyr::drop_na()
#df_train_full <- df_train_full |> tidyr::drop_na()
#df_test_full <- df_test_full   |> tidyr::drop_na()

# A little bit of verbose output:
n_tot <- nrow(df_train) + nrow(df_test)

perc_cal <- (nrow(df_train) / n_tot) |> round(2) * 100
perc_val <- (nrow(df_test)  / n_tot) |> round(2) * 100

cat("For model training, we have a calibration / validation split of: ",
    perc_cal, "/", perc_val, "%")


# ranger() crashes when using tibbles, so we are using the
# base R notation to enter the data
rf_basic <- ranger::ranger( 
  y = df_train_full[, target],     # target variable
  x = df_train_full[, predictors_all], # Predictor variables
  seed = 42,                    # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# Print a summary of fitted model
print(rf_basic)


# Let's run the basic model again but with recording the variable importance
rf_basic <- ranger::ranger( 
  y = df_train_full[, target],     # target variable
  x = df_train_full[, predictors_all],   # Predictor variables
  importance   = "permutation", # Pick permutation to calculate variable importance
  seed = 42,                    # Specify seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# Extract the variable importance and create a long tibble
vi_rf_basic <- rf_basic$variable.importance |>
  dplyr::bind_rows() |> 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable")

# Plot variable importance, ordered by decreasing value
gg <- vi_rf_basic |> 
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, value), y = value)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey50", width = 0.75) + 
  ggplot2::labs(
    y = "Change in OOB MSE after permutation", 
    x = "",
    title = "Variable importance based on OOB") +
  ggplot2::theme_classic() +
  ggplot2::coord_flip()

# Display plot
gg
ggsave(
  here::here("fig/rf_oob_importance.png"),
  gg,
  width = 7,
  height = 10,
  dpi = 300
)
set.seed(42)

# Save relevant data for model testing 
saveRDS(rf_basic,                   
        here::here("data/rf_for_waterlog100_full.rds"))

# Save training data for full model
saveRDS(df_train_full[, c(target, predictors_all)],
        here::here("data/cal_for_waterlog100_full.rds"))

# Save test data for full model
saveRDS(df_test_full[, c(target, predictors_all)],
        here::here("data/val_for_waterlog100_full.rds"))

# run the algorithm
bor <- Boruta::Boruta(
  y = df_train[, target], 
  x = df_train[, predictors_all],
  maxRuns = 50, # Number of iterations. Set to 30 or lower if it takes too long
  num.threads = parallel::detectCores()-1)

# obtain results: a data frame with all variables, ordered by their importance
df_bor <- Boruta::attStats(bor) |> 
  tibble::rownames_to_column() |> 
  dplyr::arrange(dplyr::desc(meanImp))

# plot the importance result  
bor_plot <- ggplot(df_bor,
                   aes(x = reorder(rowname, meanImp),
                       y = meanImp,
                       fill = decision)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c("grey30", "tomato", "grey70")) +
  theme_classic() +
  labs(
    y = "Variable importance",
    x = "",
    title = "Variable importance based on Boruta"
  )

ggsave(
  here::here("fig/boruta_importance.png"),
  bor_plot,
  width = 7,
  height = 10,
  dpi = 300
)

# get retained important variables
predictors_selected <- df_bor |> 
  dplyr::filter(decision == "Confirmed") |>
  dplyr::pull(rowname)

length(predictors_selected)


# re-train Random Forest model
rf_bor <- ranger::ranger( 
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# quick report and performance of trained model object
rf_bor
# Save relevant data for model testing in the next chapter.
saveRDS(df_bor, here::here("data/boruta_importance.rds"))
saveRDS(predictors_selected, here::here("data/boruta_selected_vars.rds"))
# Number of predictors before / after Boruta
n_pred_all <- length(predictors_all)
n_pred_bor <- length(predictors_selected)

saveRDS(
  tibble::tibble(
    n_predictors_all = n_pred_all,
    n_predictors_boruta = n_pred_bor
  ),
  here::here("data/model_summary_rf.rds")
)
saveRDS(rf_bor,                   
        here::here("data/rf_for_waterlog100.rds"))
saveRDS(df_train[, c(target, predictors_selected)],
        here::here("data/cal_for_waterlog100.rds"))
# Save test data for full model
saveRDS(df_test[, c(target, predictors_selected)],
        here::here("data/val_for_waterlog100.rds"))

########### hyperparameter tuning
# Ensure target factor (important for caret)
df_train$waterlog.100 <- factor(df_train$waterlog.100,
                                levels = c("0","1"),
                                labels = c("No","Yes"))

df_test$waterlog.100 <- factor(df_test$waterlog.100,
                               levels = c("0","1"),
                               labels = c("No","Yes"))
# Extract which predictors Boruta model is using
predictors_bor <- rf_bor$forest$independent.variable.names

df_train_bor <- df_train[, c("waterlog.100", predictors_bor)]
df_test_bor  <- df_test[,  c("waterlog.100", predictors_bor)]

#Here, the target variable 'waterlog.100' was converted to a factor with the levels 'No' and 'Yes' to ensure compatibility with the 'caret' and 'ranger' frameworks. Only the predictors selected by the Boruta algorithm were retained for modelling purposes. This reduces noise from irrelevant variables and ensures that the model focuses on features that are informative for predicting waterlogged soils. The Boruta-reduced datasets (df_train_bor and df_test_bor) were used for training and testing purposes.

#---------------------------#
# 1) Model tuning using 5-fold CV
#---------------------------#

set.seed(1982)

rf_bor_tuned <- caret::train(
  waterlog.100 ~ .,
  data = df_train_bor,
  method = "ranger",
  trControl = caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  tuneGrid = expand.grid(
    mtry = c(2, 4, 6, 8, 10),        # adjust range depending how many predictors
    min.node.size = c(1, 3, 5, 8, 10),  # assignment requires tuning this
    splitrule = "gini"
  ),
  num.trees = 500,
  metric = "ROC"
)
#####
saveRDS(
  rf_bor_tuned,
  here::here("data/rf_bor_tuned.rds")
)
saveRDS(
  rf_bor_tuned$results,
  here::here("data/rf_cv_results.rds")
)
saveRDS(
  rf_bor_tuned$bestTune,
  here::here("data/rf_best_tune.rds")
)

# Save test data for full model
saveRDS(df_test_bor[, c(target, predictors_selected)],
        here::here("data/df_test_bor.rds"))
# --- Probabilistic RF training ---
rf_prob <- ranger(
  formula = waterlog.100 ~ .,
  data = df_train_bor,
  num.trees = 500,
  mtry = rf_bor_tuned$bestTune$mtry,
  min.node.size = rf_bor_tuned$bestTune$min.node.size,
  splitrule = "gini",
  probability = TRUE,
  seed = 1982
)
#####
saveRDS(rf_prob,                   
        here::here("data/rf_prob.rds"))
saveRDS(df_train_bor[, c(target, predictors_bor)],
        here::here("data/cal_prob.rds"))
# Save test data for full model
saveRDS(df_test_bor[, c(target, predictors_bor)],
        here::here("data/val_prob.rds"))