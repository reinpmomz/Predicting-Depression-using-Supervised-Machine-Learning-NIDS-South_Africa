#library(e1071) # svmLinear2 using cost parameter
library(kernlab)
library(recipes)
library(caret)

# Collect times in a summary table
svm_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
svm_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  svm_param <- model_params_caret_all[["svm"]]
  C <- svm_param[["C"]]
  cost <- svm_param[["cost"]]
  degree <- svm_param[["degree"]]
  scale <- svm_param[["scale"]]
  sigma <- svm_param[["sigma"]]
  svm_tunegrid <- expand.grid(
    C = C
    #,sigma = sigma
    #,cost = cost
    #,degree = degree
    #,scale = scale
  )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "svmRadialCost" #svmLinear2, svmPoly, svmRadialCost, svmLinear, svmRadial, svmRadialSigma
		, metric = performance_metric_caret
		, tuneGrid = svm_tunegrid
		, trControl = model_control_caret[[nn]]
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	svm_train_caret_times <<- rbind(svm_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)


