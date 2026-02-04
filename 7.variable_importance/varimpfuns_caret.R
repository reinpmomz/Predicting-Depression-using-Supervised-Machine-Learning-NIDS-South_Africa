library(dplyr)
library(DALEX)
library(glmnet)
library(randomForest)
library(gbm)
library(xgboost)
library(naivebayes)

working_directory
## Permutation-based variable-importance

#model-agnostic method that does not assume anything about the model structure. 
#Therefore, it can be applied to any predictive model or ensemble of models. 
#It allows comparing an explanatory-variable’s importance between models with different structures.

caret_varimp <- function(df, model, modname, nreps = 15){
  ## Set seed for reproducibility
  set.seed(seed_varimp)
  x_df <- df[, colnames(df)[!colnames(df) %in% outcome_vars]]
  test_df <- df
  y_test <- ifelse(df[[outcome_vars]] == positive_class, 1, 0) 
  
  explainer <- DALEX::explain(model = model, 
                              data = x_df,
                              y = y_test,
                              verbose = FALSE,
                              precalculate = TRUE,
                              label = modname,
                              type = "classification")
          
  #lossfunction <- DALEX::loss_one_minus_auc(observed = y_test, 
  #                                          predicted = predict(model, test_df, type = "probs")[ , positive_class])
        
  #perform <- DALEX::model_performance(explainer)
    
  model <- DALEX::model_parts(explainer = explainer,
                              #loss_function = lossfunction, ## Use when type in explainer not specified
                              variables = NULL,
                              variable_groups = NULL,
                              N = NULL, #number of observations sampled from the data available in the explainer-object
                              B = nreps, #number of permutations to be used for calculation of (mean) variable-importance
                              type = "difference" #"difference", "ratio", "variable_importance", "raw"
                            )
          
  #model$auc <- perform[["measures"]][["auc"]]
  return(model)
  
  }

