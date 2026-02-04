library(RSNNS)
library(recipes)
library(caret)

# Collect times in a summary table
mlp_train_caret_times <- data.frame(
  dataset = character(),
  time_taken_sec = numeric()
)

## Caret Machine Learning
mlp_train_caret <- sapply(ls(pattern = "^train2"), function(x){
  ## Set seed for reproducibility
  set.seed(seed_models)

  nparams <- 2000
  mlp_param <- model_params_caret_all[["mlp"]]
  size <- mlp_param[["size"]]
  dropout <- mlp_param[["dropout"]]
  batch_size <- mlp_param[["batch_size"]]
  lr <- mlp_param[["lr"]]
  rho <- mlp_param[["rho"]]
  decay <- mlp_param[["decay"]]
  cost <- mlp_param[["cost"]]
  activation <- mlp_param[["activation"]]
  
  mlp_tunegrid <- expand.grid(
    size = size
    #, dropout = seq(dropout[1], dropout[2], length.out=dropout[3])
    #, batch_size = batch_size
    #, lr = seq(lr[1], lr[2], length.out=lr[3])
    #, rho = seq(rho[1], rho[2], length.out=rho[3])
    , decay = decay
    #, cost = floor(seq(cost[1], cost[2], length.out=cost[3]))
    #, activation = activation
    )
  
  #mlp_tunegrid <- (mlp_tunegrid %>% dplyr::slice_sample(n = nparams))
  
  nn <- x
  df <- get(x)
  
  start <- Sys.time()
	out <- caret::train(model_recipe_caret[[nn]]
		, data = df
		, method = "mlpWeightDecay" #mlp, mlpKerasDropout, mlpKerasDropoutCost
		, metric = performance_metric_caret
		, tuneGrid = mlp_tunegrid
		, trControl = model_control_caret[[nn]]
	)
	
	end <- Sys.time()
	
	# append runtime info outside the function to an object in global environment
	mlp_train_caret_times <<- rbind(mlp_train_caret_times, data.frame(
	  dataset = nn,
	  time_taken_sec = as.numeric(end - start, units = "secs")
	))
	
	return(out)	
}, simplify=FALSE)

