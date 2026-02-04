library(xgboost)
library(plyr)
library(dplyr)
library(recipes)
library(caret)

# Collect times in a summary table
xgb_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
xgb_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  xgb_param <- model_params_caret_all[["xgb"]]
  nrounds <- xgb_param[["nrounds"]]
  eta <- xgb_param[["eta"]]
  gamma <- xgb_param[["gamma"]]
  max_depth <- xgb_param[["max_depth"]]
  min_child_weight <- xgb_param[["min_child_weight"]]
  colsample_bytree <- xgb_param[["colsample_bytree"]]
  lambda <- xgb_param[["lambda"]]
  alpha <- xgb_param[["alpha"]]
  subsample <- xgb_param[["subsample"]]
  xgb_tunegrid <- expand.grid(
    nrounds = seq(nrounds[1], nrounds[2], length.out=nrounds[3]),
    #lambda = lambda,
    #alpha = alpha,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample = subsample,
    eta = seq(eta[1], eta[2], length.out= eta[3])
  )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "xgbTree"  #xgbLinear(params - nrounds, lambda, alpha, eta)
		, metric = performance_metric_caret
		, tuneGrid = xgb_tunegrid
		, trControl = model_control_caret[[nn]]
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	xgb_train_caret_times <<- rbind(xgb_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

