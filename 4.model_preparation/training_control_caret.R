library(dplyr)
library(caret)
library(rsample)

model_control_caret <- sapply(ls(pattern = "^train2"), function(x){
    nn <- x
    df <- get(x)
    
    set.seed(seed_imbalance)
    #Create grouped (and optionally stratified) folds using rsample for train data
    grouped_folds <- rsample::group_vfold_cv(
      data   = df
      ,group  = pid
      ,v      = nfolds
      #,strata = any_of(outcome_vars)    # optional: remove this line if no stratification
      )
    
    #Convert/extract rsample folds to index lists using tidymodels helper functions
    #index_list <- lapply(grouped_folds$splits, function(s) as.integer(rownames(rsample::analysis(s))) ) #row indices for training
    #indexOut_list <- lapply(grouped_folds$splits, function(s) as.integer(rownames(rsample::assessment(s))) ) #row indices for test
    index_list <- lapply(grouped_folds$splits, function(s) s$in_id)
    indexOut_list <- lapply(grouped_folds$splits, function(s) setdiff(seq_len(nrow(df)), s$in_id))
    
    names(index_list)    <- paste0("Fold", seq_along(index_list))
    names(indexOut_list) <- paste0("Fold", seq_along(indexOut_list))
    
    out <- caret::trainControl(method = "cv"
                              , number = length(index_list)
                              , index = index_list
                              , indexOut = indexOut_list
                              , search = "grid"
                              , classProbs = TRUE
                              , returnData = FALSE
                              , summaryFunction = twoClassSummary
                              , seeds = NULL
                              , allowParallel = TRUE
                              )
    
  }, simplify=FALSE)
  

