library(naivebayes)
library(recipes)
library(caret)

# Collect times in a summary table
nb_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
nb_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)
  
  nb_param <- model_params_caret_all[["nb"]]
  laplace <- nb_param[["laplace"]]
  usekernel <- nb_param[["usekernel"]][1] #0 - output is c(TRUE, FALSE) will result in usekernel = TRUE in train 
  #usekernel <- nb_param[["usekernel"]][2] # 1- output is c(FALSE, TRUE) will result in usekernel = FALSE in train
  adjust <- nb_param[["adjust"]]
  nb_tunegrid <- expand.grid(
    laplace = laplace
    , usekernel = usekernel
    , adjust = adjust
  )
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "naive_bayes"
		, metric = performance_metric_caret
		, tuneGrid = nb_tunegrid
		, trControl = model_control_caret[[nn]]
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	nb_train_caret_times <<- rbind(nb_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

