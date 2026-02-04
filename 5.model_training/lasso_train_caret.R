library(glmnet)
library(recipes)
library(caret)

# Collect times in a summary table
lasso_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
lasso_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  lasso_param <- model_params_caret_all[["lasso"]]
  alpha <- lasso_param[["alpha"]]
  lambda <- lasso_param[["lambda"]]
  lasso_tunegrid <- expand.grid(
    alpha = alpha
    , lambda = exp(seq(lambda[1], lambda[2], length.out=lambda[3]))
    #, KEEP.OUT.ATTRS = FALSE
    )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "glmnet"
		, metric = performance_metric_caret
		, family = "binomial"
		, tuneGrid = lasso_tunegrid
		, trControl = model_control_caret[[nn]]
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	lasso_train_caret_times <<- rbind(lasso_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

