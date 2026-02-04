################################################################################
### Restart R
#.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("./1.setup/requirements.R")

### helper/customized functions
source("./1.setup/helperfuns_read_excel_sheets.R")
source("./1.setup/helperfuns_ggplot_themes.R")
source("./1.setup/helperfuns_stack_plots.R")
source("./1.setup/helperfuns_effect_size.R")
source("./1.setup/helperfuns_coallesce_columns.R")

################################################################################

### Load data 
### Before running, Apply label to w3_prov2001 in hhderived_W3_Anon_V3.0.0.dta then save
source("./2.load_data_and_clean/load_data_local.R")
source("./2.load_data_and_clean/merge_data.R")
source("./2.load_data_and_clean/merge_data_final.R")

### Load recode file
source("./2.load_data_and_clean/load_recode_file.R")

### Data cleaning
source("./2.load_data_and_clean/cleaning.R")

### Create SES based on PCA
source("./2.load_data_and_clean/ses_pca.R")

### Select variables for descriptive and inferential analysis
source("./2.load_data_and_clean/analysis_data.R")

################################################################################

## Model preparation

### Select variables required for modelling and model formula
source("./4.model_preparation/modelling_data.R")

### Feature selection - filter method
source("./4.model_preparation/feature_selection.R")

### Train-test split, up-sampling and down-sampling
source("./4.model_preparation/data_partition_caret.R")

### Preprocessing: Build recipes
source("./4.model_preparation/recipes_caret.R")

### Training control
source("./4.model_preparation/training_control_caret.R")

### Model parameters
source("./4.model_preparation/model_parameters_caret.R")

################################################################################

## Model training - Caret

### Logistic
source("./5.model_training/logistic_train_caret.R")

### Lasso
source("./5.model_training/lasso_train_caret.R")

### Ridge
source("./5.model_training/ridge_train_caret.R")

### Linear discriminant analysis
source("./5.model_training/lda_train_caret.R")

### Naive Bayes
source("./5.model_training/nb_train_caret.R")

### Multilayer Perceptron
source("./5.model_training/mlp_train_caret.R")

### Gradient Boosting Machine
source("./5.model_training/gbm_train_caret.R")

### Extreme Gradient Boosting
source("./5.model_training/xgb_train_caret.R")

### Random forest
source("./5.model_training/rf_train_caret.R")

### Support Vector Machine
source("./5.model_training/svm_train_caret.R")


################################################################################
## Model training times
source("./6.model_performance/train_times_caret.R")

## Best Model train parameters
source("./6.model_performance/best_train_params_caret.R")

## Predictive performance
### Test data - Prediction uncertainities
source("./6.model_performance/bootfuns_caret.R")

bootstrap_samples <- 25 #set default is 50
source("./6.model_performance/resamples_test_data_metrics_caret.R")
source("./6.model_performance/resamples_test_data_roc_caret.R")

### Accuracy, Sensitivity(Recall/True positive rate), AUC, F1, Precision(PPV)
source("./6.model_performance/metrics_caret_plots.R")

### ROC plot
source("./6.model_performance/roc_caret_plots.R")

### Save model metrics
source("./6.model_performance/save_model_metrics_caret.R")

### Best performing model Overall - Balanced Accuracy, Sensitivity(Recall/True positive rate), AUC, F1, Precision(PPV)
source("./6.model_performance/best_model_caret.R")

######################################################################

## Variable importance

### Test data - Variable importance
source("./7.variable_importance/varimpfuns_caret.R")

n_permutations <- 5 #set default is 15
source("./7.variable_importance/varimp_caret.R")
source("./7.variable_importance/varimp_best_caret_plots.R")

### Rank variable importance
source("./7.variable_importance/varimp_rank_caret.R")
source("./7.variable_importance/varimp_rank_caret_plots.R")

source("./7.variable_importance/varimp_rank_all_plots.R")


################################################################################

## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[ls(all.names = TRUE) %in% c("logistic_train_caret", "logistic_train_caret_times",
                                                             "lasso_train_caret", "lasso_train_caret_times",
                                                             "ridge_train_caret", "ridge_train_caret_times",
                                                             "rf_train_caret", "rf_train_caret_times",
                                                             "gbm_train_caret", "gbm_train_caret_times",
                                                             "xgb_train_caret", "xgb_train_caret_times",
                                                             "nb_train_caret", "nb_train_caret_times",
                                                             "svm_train_caret", "svm_train_caret_times",
                                                             "lda_train_caret", "lda_train_caret_times",
                                                             "mlp_train_caret", "mlp_train_caret_times",
                                                             "caret_metrics_df", "caret_metrics_all_df",
                                                             "caret_roc_df", "caret_roc_all_df",
                                                             "caret_varimp_df", "caret_varimp_all_df"
                                                              )],
     file = "nids_train_models.RData",
     envir = .GlobalEnv #parent.frame()
     )


################################################################################

## Run all files in Rstudio
source("main.R")

################################################################################

