library(gbm)
library(plyr)
library(dplyr)
library(recipes)
library(caret)

# Collect times in a summary table
gbm_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
gbm_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  gbm_param <- model_params_caret_all[["gbm"]]
  n_trees <- gbm_param[["n.trees"]]
  interaction_depth <- gbm_param[["interaction.depth"]]
  shrinkage <- gbm_param[["shrinkage"]]
  n_minobsinnode <- gbm_param[["n.minobsinnode"]]
  gbm_tunegrid <- expand.grid(
    n.trees = n_trees 
    , interaction.depth = interaction_depth
    , shrinkage = shrinkage
    , n.minobsinnode = n_minobsinnode
  )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "gbm"
		, distribution = "bernoulli"
		, metric = performance_metric_caret
		, tuneGrid = gbm_tunegrid
		, trControl = model_control_caret[[nn]]
		, verbose = FALSE
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	gbm_train_caret_times <<- rbind(gbm_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

