library(dplyr)
library(caret)
library(performance)
library(bayestestR)
library(glmnet)
library(randomForest)
library(gbm)
library(xgboost)
library(naivebayes)

working_directory
## ---- Prediction uncertainities ----

bootMeasures_metrics_caret <- function(df, model){
  x_df <- df
  y <- df[, outcome_vars, drop=TRUE]
  y_test <- ifelse(y == positive_class, 1, 0) 
  
  preds <- predict(model, x_df, type = "probs") ## type = raw
  preds$pred <- factor(apply(preds, 1, function(x)colnames(preds)[which.max(x)]), levels=levels(y))
  preds$obs <- y
  
  metrics <- caret::confusionMatrix(data=preds$pred, reference=preds$obs, positive = positive_class)
  roc_auc <- performance::performance_roc(x= y_test, predictions = preds[ , positive_class])
  
  aa <- metrics[["overall"]][["Accuracy"]]
  ba <- (metrics[["byClass"]][["Sensitivity"]] + metrics[["byClass"]][["Specificity"]])/2
  ss <- metrics[["byClass"]][["Sensitivity"]]
  sp <- metrics[["byClass"]][["Specificity"]]
  ppv <- metrics[["byClass"]][["Pos Pred Value"]]
  npv <- metrics[["byClass"]][["Neg Pred Value"]]
  recall <- metrics[["byClass"]][["Recall"]]
  precision <- metrics[["byClass"]][["Precision"]]
  f1_score <- metrics[["byClass"]][["F1"]]
  auc <- bayestestR::area_under_curve(x= roc_auc[["Specificity"]], y= roc_auc[["Sensitivity"]])
  
  scores_df <- data.frame(accuracy = aa
                          , balanced_accuracy = ba
                          , sensitivity = ss
                          , recall = recall
                          , precision = precision
                          , ppv = ppv
                          , specificity = sp
                          , npv = npv
                          , F1_Score = f1_score
                          , auc = auc
                          )
  return(scores_df)

}

bootMeasures_roc_caret <- function(df, model){
  x_df <- df
  y <- df[, outcome_vars, drop=TRUE]
  y_test <- ifelse(y == positive_class, 1, 0) 
  
  preds <- predict(model, x_df, type = "probs") ## type = raw
  
  roc_auc <- performance::performance_roc(x= y_test, predictions = preds[ , positive_class])
  auc <- bayestestR::area_under_curve(x= roc_auc[["Specificity"]], y= roc_auc[["Sensitivity"]])
  
  roc_df <- data.frame(x = roc_auc[["Specificity"]], y = roc_auc[["Sensitivity"]])
  roc_df$auc <- auc
  roc_df$number <- 1:nrow(roc_df)
  
  return(roc_df)
  
}

bootEstimates_metrics_caret <- function(df, model, nreps = 50) {
  ### Set seed for reproducibility
  set.seed(seed_metrics)
  resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
  est <- lapply(resamples, function(x){
    bootMeasures_metrics_caret(df[x, ], model)
  })
  
  out <- do.call("rbind", est)
  out <- sapply(out, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm = TRUE)})
  out <- t(out)
  colnames(out) <- c("lower", "estimate", "upper")
  out <- as.data.frame(out)
  out$scores <- rownames(out)
  return(out)
}

bootEstimates_roc_caret <- function(df, model, nreps = 50) {
  ### Set seed for reproducibility
  set.seed(seed_metrics)
  resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
  est <- lapply(resamples, function(x){
    bootMeasures_roc_caret(df[x, ], model)
  })
  
  out <- do.call("rbind", est)
  out <- as.data.frame(out) %>%
    dplyr::group_by(number) %>%
    dplyr::summarise(x_lower = quantile(x, probs = 0.025, na.rm = TRUE),
                     x_estimate = quantile(x, probs = 0.5, na.rm = TRUE),
                     x_upper = quantile(x, probs = 0.975, na.rm = TRUE),
                     y_lower = quantile(y, probs = 0.025, na.rm = TRUE),
                     y_estimate = quantile(y, probs = 0.5, na.rm = TRUE),
                     y_upper = quantile(y, probs = 0.975, na.rm = TRUE),
                     auc_estimate = round(quantile(auc, probs = 0.5, na.rm = TRUE),4), .groups = "drop"
                     ) 
  return(out)
  
}

