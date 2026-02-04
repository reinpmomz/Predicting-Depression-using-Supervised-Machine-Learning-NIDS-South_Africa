library(recipes)
library(caret)

# Collect times in a summary table
logistic_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)


## Caret Machine Learning
logistic_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "glm"
		, metric = performance_metric_caret
		, family = "binomial"
		, trControl = model_control_caret[[nn]]
		)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	logistic_train_caret_times <<- rbind(logistic_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

