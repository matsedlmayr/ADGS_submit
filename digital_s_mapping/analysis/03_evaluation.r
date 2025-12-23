library(caret)
library(ranger)
library(terra)
library(dplyr)
library(tidyr)
library(here)
library(readr)
source(here::here("R", "evaluate_rf_model.R"))

# --- Load data and model ---
rf_bor   <- readRDS(here::here("data/rf_for_waterlog100.rds"))
df_train <- readRDS(here::here("data/cal_for_waterlog100.rds"))
df_test  <- readRDS(here::here("data/val_for_waterlog100.rds"))

rf_basic <- readRDS(here::here("data/rf_for_waterlog100_full.rds"))
df_train_full <- readRDS(here::here("data/cal_for_waterlog100_full.rds"))
df_test_full  <- readRDS(here::here("data/val_for_waterlog100_full.rds"))
df_test_bor <- readRDS(here::here("data/df_test_bor.rds"))

rf_bor_tuned   <- readRDS(here::here("data/rf_bor_tuned.rds"))
rf_prob   <- readRDS(here::here("data/rf_prob.rds"))
#df_prob <- readRDS(here::here("data/cal_prob.rds.rds"))
df_prob <- readRDS(here::here("data/val_prob.rds"))

# --- Load raster mask and covariates ---
raster_mask <- terra::rast(
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/study_area/area_to_be_mapped.tif"
)
df_mask <- as.data.frame(raster_mask, xy = TRUE) |> dplyr::filter(area_to_be_mapped == 1)
files_covariates <- c(
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/NegO.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/PosO.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_MRRTF2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_MRVBF2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_NO2m_r500.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_PO2m_r500.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_SAR2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_SCA2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_TWI2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_TWI2m_s15.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_TWI2m_s60.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_alti2m_std_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_conv2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv25m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m_fmean_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m_fmean_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m_s60.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m_std_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv2m_std_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curv6m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan25m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_fmean_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_fmean_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_s60.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_s7.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_std_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan2m_std_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvplan50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof25m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_fmean_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_fmean_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_s60.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_s7.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_std_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof2m_std_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_curvprof50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_diss2m_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_diss2m_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_e_aspect25m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_e_aspect2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_e_aspect2m_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_e_aspect50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_n_aspect2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_n_aspect2m_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_n_aspect2m_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_n_aspect50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_n_aspect6m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_rough2m_10c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_rough2m_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_rough2m_rect3c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_fmean_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_fmean_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_s60.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_s7.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_std_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope2m_std_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope50m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_slope6m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_toposcale2m_r3_r50_i10s.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_tpi_2m_50c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_tpi_2m_5c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_tri2m_altern_3c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_tsc10_2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_vrm2m.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/Se_vrm2m_r10c.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/be_gwn25_hdist.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/be_gwn25_vdist.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/cindx10_25.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/cindx50_25.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/geo500h1id.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/geo500h3id.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/lgm.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/lsf.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mrrtf25.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mrvbf25.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mt_gh_y.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mt_rr_y.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mt_td_y.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mt_tt_y.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/mt_ttvar.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/protindx.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/terrTextur.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/tsc25_18.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/tsc25_40.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/vdcn25.tif",
  "https://raw.githubusercontent.com/geco-bern/tutorial_digital_soil_mapping/refs/heads/main/book/data-raw/geodata/covariates/vszone.tif"
)
raster_stack <- rast(files_covariates)
#######
# Predict on validation points - Full
prediction_full_test <- predict(
  rf_basic,                 # full RF model
  data = df_test_full,#[, predictors_all], #[, predictors_all],   covariates used in the full model
  num.threads = parallel::detectCores() - 1
)#$predictions
df_test_full$pred <- prediction_full_test$predictions
df_test_full$pred <- factor(df_test_full$pred, levels = c(0,1))
df_test_full$`waterlog.100` <- factor(df_test_full$`waterlog.100`, levels = c(0,1))
#Predict on validation points - Reduced

prediction <- predict(
  rf_bor,           # RF model
  data = df_test,   # Predictor data
  num.threads = parallel::detectCores() - 1
)

# Save predictions to validation df
df_test$pred <- prediction$predictions

#######
# --- Full RF model ---
results_full <- evaluate_rf_model(
  model = rf_basic,
  df_test = df_test_full,
  pred_col = "pred",
  model_name = "full_rf",
  model_type = "classification"
)

# --- Reduced RF model (Boruta-selected features) ---
results_bor <- evaluate_rf_model(
  model = rf_bor,
  df_test = df_test,
  pred_col = "pred",
  target_col = "waterlog.100",
  model_name = "boruta_rf",
  model_type = "classification"
)

# --- Tuned RF model (caret tuning) ---
# Save printed summary
capture.output(print(rf_bor_tuned), file = here::here("fig/rf_bor_tuned_summary.txt"))

# Save ggplot
p_tune <- ggplot(rf_bor_tuned)
ggsave(
  filename = here::here("fig/rf_bor_tuned_plot.png"),
  plot = p_tune,
  width = 7,
  height = 5,
  dpi = 300
)
results_tuned <- evaluate_rf_model(
  model = rf_bor_tuned,          
  df_test = df_test_bor,
  pred_col = "pred",
  target_col = "waterlog.100",
  model_name = "tuned_rf",
  model_type = "tuned"
)
results_prob<-evaluate_rf_model(
  model = rf_prob,
  df_test = df_test_bor,
  pred_col = "pred",
  target_col = "waterlog.100",
  model_name = "rf_prob",
  model_type = "probabilistic",
  thresholds = c(critical = 0.3, noncritical = 0.7)
)

roc_obj <- results_tuned$roc
png(here::here("fig/roc_tuned.png"), width=1600, height=1200, res=200)
plot(roc_obj, col="blue", lwd=2, main="ROC Curve: Tuned RF Model")
abline(a=0, b=1, lty=2, col="gray")
dev.off()
evaluate_rf_model(
  model = rf_prob,
  df_test = df_test_bor,
  model_name = "rf_prob",
  model_type = "probabilistic",
  thresholds = c(critical = 0.3, noncritical = 0.7)
)
###########probabilistic
pred_prob <- predict(rf_prob, df_test_bor)$predictions[, "Yes"]

# Plot ROC curve and calculate AUC
roc_obj <- roc(
  response = df_test_bor$waterlog.100,
  predictor = pred_prob,
  levels = c("No", "Yes"),   # first = control, second = case
  direction = "<"            # higher predictor = case
)
plot(roc_obj, col="blue", lwd=2, main="ROC Curve - RF Probabilistic Model")
abline(a=0, b=1, lty=2, col="gray")
auc_val <- auc(roc_obj)
print(paste("AUC:", round(auc_val, 3)))

# Choose thresholds for different risk scenarios
threshold_critical <- 0.3   # favor sensitivity → catch most waterlogged areas
threshold_noncritical <- 0.7 # favor specificity → reduce false alarms

# Convert probabilities to binary classes
pred_class_critical <- factor(ifelse(pred_prob > threshold_critical, "Yes", "No"),
                              levels = c("No","Yes"))
pred_class_noncritical <- factor(ifelse(pred_prob > threshold_noncritical, "Yes", "No"),
                                 levels = c("No","Yes"))

# Evaluate predictions with confusion matrices
cm_critical <- confusionMatrix(pred_class_critical, df_test_bor$waterlog.100, positive = "Yes")
cm_noncritical <- confusionMatrix(pred_class_noncritical, df_test_bor$waterlog.100, positive = "Yes")

cm_critical
cm_noncritical
saveRDS(
  tibble::tibble(
    Model = "Probabilistic RF",
    AUC = as.numeric(auc_val)
  ),
  here::here("data/eval_prob_auc.rds")
)
saveRDS(
  tibble::tibble(
    Scenario = c("Critical infrastructure", "Non-critical infrastructure"),
    Threshold = c(threshold_critical, threshold_noncritical),
    Sensitivity = c(
      cm_critical$byClass["Sensitivity"],
      cm_noncritical$byClass["Sensitivity"]
    ),
    Specificity = c(
      cm_critical$byClass["Specificity"],
      cm_noncritical$byClass["Specificity"]
    )
  ),
  here::here("data/eval_prob_thresholds.rds")
)
dir.create(here::here("fig"), showWarnings = FALSE)

png(
  here::here("fig/rf_prob_roc.png"),
  width = 1800,
  height = 1400,
  res = 200
)
plot(roc_obj, lwd = 2, main = "ROC curve – probabilistic RF model")
abline(a = 0, b = 1, lty = 2, col = "grey")
dev.off()














# --- Predictions on raster ---

# --- Select covariates used in Boruta RF ---
preds_selected <- rf_bor$forest$independent.variable.names
files_selected <- files_covariates[apply(sapply(preds_selected, grepl, files_covariates), 1, any)]
raster_covariates <- terra::rast(files_selected)

# --- Extract predictor data for mask ---
df_locations <- df_mask |> dplyr::select(x, y)
df_predict <- terra::extract(raster_covariates, df_locations, ID = FALSE)
df_predict <- cbind(df_locations, df_predict) |> drop_na()

prediction <- predict(rf_bor, data = df_predict, num.threads = parallel::detectCores() - 1)
df_predict$prediction <- prediction$predictions


# Make predictions using the RF model
prediction_prob <- predict(
  rf_bor,              # RF model
  data = df_predict,   
  num.threads = parallel::detectCores() - 1)
# Get factor levels of target
levels_target <- rf_bor$forest$levels  # should be c("No", "Yes")

# Assign column names
#colnames(prediction_prob$predictions) <- levels_target
# Attach predictions to dataframe and round them
df_predict$waterlog_prob <- prediction_prob$predictions

# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, waterlog_prob)

# Turn dataframe into a raster
raster_pred <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)
terra::writeRaster(
  raster_pred,
  here::here("data", "ra_predicted_waterlog100.tif"),
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)
# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
# Plot probability of waterlogging
p_map <-ggplot() +
  tidyterra::geom_spatraster(data = raster_pred) +
  scale_fill_viridis_c(
    na.value = NA,
    option = "viridis",
    name = "Probability\nWaterlogged"
  ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Predicted Probability of Waterlogging")
ggsave(
  filename = here::here("fig/map_prob_tuned_rf.png"),
  plot = p_map,
  width = 7,
  height = 9,  
  dpi = 300
)
map_summary <- list(
  model = "Tuned RF (probabilistic)",
  min_prob = global(raster_pred, "min", na.rm = TRUE)[1],
  max_prob = global(raster_pred, "max", na.rm = TRUE)[1],
  mean_prob = global(raster_pred, "mean", na.rm = TRUE)[1]
)

saveRDS(map_summary, here::here("data/map_summary_tuned_rf.rds"))
