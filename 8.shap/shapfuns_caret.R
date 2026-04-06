library(dplyr)
library(shapviz)
library(kernelshap)

working_directory
## Permutation-based SHAP

caret_shap <- function(df, model, permutation_shap=FALSE, outcome_var, pos_class, seed = 100){
  ## Set seed for reproducibility
  set.seed(seed)
  x_df <- df[, colnames(df)[!colnames(df) %in% outcome_var]]
  X_small <- x_df[sample(nrow(x_df), 300), ]
  bg_X    <- x_df[sample(nrow(x_df), 100), ]
  
  shap <- if (permutation_shap){
    
    ps <- kernelshap::permshap(model
                               , X = X_small
                               , bg_X = bg_X
                               , pred_fun = function(object, newdata) {
                                 predict(object, newdata, type = "prob")[, pos_class]
                                 }
                               )
    ps
  } else {
    ks <- kernelshap::kernelshap(model
                                 , X = X_small
                                 , bg_X = bg_X
                                 , pred_fun = function(object, newdata) {
                                   predict(object, newdata, type = "prob")[, pos_class]
                                   }
                                 )
    ks
    
  }
  
  # Now the usual plots:
  sv <- shapviz::shapviz(shap)
  
  }

