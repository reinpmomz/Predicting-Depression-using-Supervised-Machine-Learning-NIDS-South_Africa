library(dplyr)
library(ranger)
library(e1071)
library(recipes)
library(caret)

# Collect times in a summary table
rf_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
rf_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  rf_param <- model_params_caret_all[["rf"]]
  mtry <- rf_param[["mtry"]]
  min_node_size <- rf_param[["min.node.size"]]
  split_rule <- rf_param[["splitrule"]]
  num_trees <- rf_param[["num.trees"]]
  regularization_factor <- rf_param[["regularization.factor"]]
  rf_tunegrid <- expand.grid(
    mtry = floor(seq(mtry[1], mtry[2], length.out=mtry[3]))
    , splitrule = split_rule
    , min.node.size = min_node_size
  )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "ranger"
		, metric = performance_metric_caret
		, tuneGrid = rf_tunegrid
		, trControl = model_control_caret[[nn]]
		, num.trees = num_trees
		, importance = 'permutation' ## impurity
		#, scale.permutation.importance = TRUE
		, regularization.factor = regularization_factor
		, regularization.usedepth = FALSE
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	rf_train_caret_times <<- rbind(rf_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

