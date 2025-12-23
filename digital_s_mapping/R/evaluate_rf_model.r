evaluate_rf_model <- function(
    model,
    df_test,
    df_predict = NULL,
    target_col = "waterlog.100",
    pred_col = "pred",
    save_path = NULL,
    fig_path = NULL,
    raster_covariates = NULL,
    model_name = "rf_model",
    model_type = c("classification", "tuned", "probabilistic"),
    thresholds = c(critical = 0.3, noncritical = 0.7)
) {
  
  model_type <- match.arg(model_type)
  
  library(caret)
  library(ggplot2)
  library(pROC)
  library(tibble)
  library(dplyr)
  
  if (is.null(save_path)) save_path <- here::here("data")
  if (is.null(fig_path))  fig_path  <- here::here("fig")
  
  dir.create(save_path, showWarnings = FALSE)
  dir.create(fig_path, showWarnings = FALSE)
  
  # --------------------
  # 1) Predictions
  # --------------------
  if (model_type == "classification") {
    
    pred_class <- factor(df_test[[pred_col]], levels = c("0", "1"))
    obs <- factor(df_test[[target_col]], levels = c("0", "1"))
    positive <- "1"
    
    cm <- confusionMatrix(pred_class, obs, positive = positive)
    
  }
  
  if (model_type == "tuned") {
    
    pred_class <- predict(model, df_test)
    pred_prob  <- predict(model, df_test, type = "prob")[, "Yes"]
    
    obs <- df_test[[target_col]]
    positive <- "Yes"
    
    cm <- confusionMatrix(pred_class, obs, positive = positive)
    
    roc_obj <- roc(obs, pred_prob, levels = c("No", "Yes"), direction = "<")
    auc_val <- auc(roc_obj)
  }
  
  if (model_type == "probabilistic") {
    
    pred_prob <- predict(model, df_test)$predictions[, "Yes"]
    obs <- df_test[[target_col]]
    
    roc_obj <- roc(obs, pred_prob, levels = c("No", "Yes"), direction = "<")
    auc_val <- auc(roc_obj)
    
    # Threshold-based classifications
    pred_class_crit <- factor(ifelse(pred_prob > thresholds["critical"], "Yes", "No"),
                              levels = c("No", "Yes"))
    pred_class_noncrit <- factor(ifelse(pred_prob > thresholds["noncritical"], "Yes", "No"),
                                 levels = c("No", "Yes"))
    
    cm_crit <- confusionMatrix(pred_class_crit, obs, positive = "Yes")
    cm_noncrit <- confusionMatrix(pred_class_noncrit, obs, positive = "Yes")
    
    cm <- cm_crit  # main CM for plotting
  }
  
  # --------------------
  # 2) Confusion matrix plot
  # --------------------
  cm_table <- cm$table
  dimnames(cm_table) <- list(
    Predicted = c("Non-waterlogged (0)", "Waterlogged (1)"),
    Observed  = c("Non-waterlogged (0)", "Waterlogged (1)")
  )
  
  png(
    file.path(fig_path, paste0("mosaic_", model_name, ".png")),
    width = 1600, height = 1200, res = 200
  )
  mosaicplot(
    cm_table,
    main = paste("Confusion Matrix:", model_name),
    color = TRUE,
    xlab = "Observed",
    ylab = "Predicted",
    las = 1
  )
  dev.off()
  
  # --------------------
  # 3) Metrics
  # --------------------
  TN <- cm_table[1, 1]
  FP <- cm_table[2, 1]
  FN <- cm_table[1, 2]
  TP <- cm_table[2, 2]
  table <- tibble(
    TN = TN,
    FP = FP,
    FN = FN,
    TP= TP
  )
  metrics <- tibble(
    Model = model_name,
    Accuracy = (TP + TN) / sum(cm_table),
    Precision = TP / (TP + FP),
    Recall = TP / (TP + FN),
    Specificity = TN / (TN + FP),
    F1 = 2 * TP / (2 * TP + FP + FN),
    Balanced_Accuracy = ((TP / (TP + FN)) + (TN / (TN + FP))) / 2
  )
  
  saveRDS(metrics, file.path(save_path, paste0("metrics_", model_name, ".rds")))
  saveRDS(table, file.path(save_path, paste0("table", model_name, ".rds")))
  
  # --------------------
  # 4) ROC plot (if available)
  # --------------------
  if (model_type %in% c("tuned", "probabilistic")) {
    
    png(
      file.path(fig_path, paste0("roc_", model_name, ".png")),
      width = 1600, height = 1200, res = 200
    )
    plot(roc_obj, main = paste("ROC –", model_name), lwd = 2)
    abline(a = 0, b = 1, lty = 2, col = "grey")
    dev.off()
  }
  
  # --------------------
  # 5) Return
  # --------------------
  out <- list(
    confusion_matrix = cm,
    cm_table = cm_table,
    metrics = metrics
  )
  
  if (model_type %in% c("tuned", "probabilistic")) {
    out$roc <- roc_obj
    out$auc <- auc_val
  }
  
  if (model_type == "probabilistic") {
    out$cm_critical <- cm_crit
    out$cm_noncritical <- cm_noncrit
  }
  
  return(out)
}
