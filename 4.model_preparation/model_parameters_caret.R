library(readxl)
library(dplyr)
library(writexl)


## Function to extract parameters from excel file
get_excel_param <- function(x) {
  if (!is.numeric(x)) {
    check <- grepl("\\[|\\]", x)
    x <- strsplit(gsub("\\[|\\]|\\(|\\)", "", x), ",")[[1]]
    if (length(x) >= 1) {
      x <- trimws(x)
      if (check) {
        x <- as.numeric(x)
      } 
    }
  }
  return(x)
}

get_excel_param_all <- function(param_file) {
  out <- sapply(names(param_file), function(p) {
    p <- param_file[[p]]
    params <- sapply(colnames(p), function(x){
      out <- get_excel_param(p[[x]])
      return(out)
    }, simplify=FALSE)
    return(params)
  }, simplify=FALSE)
  return(out)
}

## Load model parameters file

model_params_caret <- read_excel_allsheets("./4.model_preparation/model_parameters.xlsx")
model_params_caret_all <- get_excel_param_all(model_params_caret)

performance_metric_caret <- model_params_caret_all$performance_metric$metric


model_params_table_output <- 
  sapply(names(model_params_caret_all)[!names(model_params_caret_all) %in% c("performance_metric")], function(x) {
    params <- model_params_caret_all[[x]]
    
    out <- if (x %in% c("logistic", "lda")) {
      data.frame(model = x, parameter = "none", value = NA, train_resampling_method = "cv" )
    } else if (x %in% c("ridge", "lasso")) {
      data.frame(model = x, parameter = c("alpha", "lambda"),
                 value = c(params[["alpha"]],
                           paste(round(exp(seq(params[["lambda"]][1], params[["lambda"]][2], length.out=params[["lambda"]][3])),5)
                                 , collapse = ", "
                             )
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("enet")) {
      data.frame(model = x, parameter = c("alpha", "lambda"),
                 value = c(paste(round(seq(params[["alpha"]][1], params[["alpha"]][2], length.out=params[["alpha"]][3]),5)
                                 , collapse = ", "
                                 ),
                           paste(round(exp(seq(params[["lambda"]][1], params[["lambda"]][2], length.out=params[["lambda"]][3])),5)
                                 , collapse = ", "
                             )
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("knn")) {
      data.frame(model = x, parameter = c("k"),
                 value = c(paste(seq(params[["k"]][1], params[["k"]][2], length.out=params[["k"]][3])
                                 , collapse = ", "
                                 )
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("pls")) {
      data.frame(model = x, parameter = c("ncomp"),
                 value = c(paste(seq(params[["ncomp"]][1], params[["ncomp"]][2], length.out=params[["ncomp"]][3])
                                 , collapse = ", "
                                 )
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("mda")) {
      data.frame(model = x, parameter = c("nprune", "degree", "subclasses", "K", "model", "NumVars", "lambda", "R"),
                 value = c(paste(floor(seq(params[["nprune"]][1], params[["nprune"]][2], length.out=params[["nprune"]][3]))
                                 , collapse = ", "
                                 ),
                           paste(params[["degree"]], collapse = ", "),
                           paste(params[["subclasses"]], collapse = ", "),
                           paste(params[["K"]], collapse = ", "),
                           params[["model"]],
                           params[["NumVars"]],
                           paste(round(exp(seq(params[["lambda"]][1], params[["lambda"]][2], length.out=params[["lambda"]][3])),6)
                                 , collapse = ", "
                                 ),
                           paste(params[["R"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("nb")) {
      data.frame(model = x, parameter = c("laplace", "usekernel", "adjust"),
                 value = c(paste(params[["laplace"]], collapse = ", "),
                           params[["usekernel"]][1],
                           paste(params[["adjust"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("svm")) {
      data.frame(model = x, parameter = c("C", "cost", "degree", "scale", "sigma"),
                 value = c(paste(params[["C"]], collapse = ", "),
                           paste(params[["cost"]], collapse = ", "),
                           paste(params[["degree"]], collapse = ", "),
                           params[["scale"]],
                           paste(params[["sigma"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("gbm")) {
      data.frame(model = x, parameter = c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode"),
                 value = c(paste(params[["n.trees"]], collapse = ", "),
                           paste(params[["interaction.depth"]], collapse = ", "),
                           paste(params[["shrinkage"]], collapse = ", "),
                           paste(params[["n.minobsinnode"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("rf")) {
      data.frame(model = x, 
                 parameter = c("mtry", "min.node.size", "splitrule", "num.trees", "regularization.factor"),
                 value = c(paste(floor(seq(params[["mtry"]][1], params[["mtry"]][2], length.out=params[["mtry"]][3]))
                                 , collapse = ", "),
                           paste(params[["min.node.size"]], collapse = ", "),
                           params[["splitrule"]],
                           params[["num.trees"]],
                           params[["regularization.factor"]]
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("xgb")) {
      data.frame(model = x, 
                 parameter = c("nrounds", "eta", "gamma", "max_depth", "min_child_weight", "colsample_bytree",
                               "lambda", "alpha", "subsample"),
                 value = c(paste(floor(seq(params[["nrounds"]][1], params[["nrounds"]][2], length.out=params[["nrounds"]][3]))
                                 , collapse = ", "),
                           paste(seq(params[["eta"]][1], params[["eta"]][2], length.out=params[["eta"]][3])
                                 , collapse = ", "),
                           paste(params[["gamma"]], collapse = ", "),
                           paste(params[["max_depth"]], collapse = ", "),
                           paste(params[["min_child_weight"]], collapse = ", "),
                           paste(params[["colsample_bytree"]], collapse = ", "),
                           params[["lambda"]],
                           params[["alpha"]],
                           paste(params[["subsample"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    } else if (x %in% c("mlp")) {
      data.frame(model = x, 
                 parameter = c("size", "dropout", "batch_size", "lr", "rho", "decay", "cost", "activation"),
                 value = c(paste(params[["size"]], collapse = ", "),
                           paste(seq(params[["dropout"]][1], params[["dropout"]][2], length.out=params[["dropout"]][3])
                                 , collapse = ", "),
                           paste(params[["batch_size"]], collapse = ", "),
                           paste(round(seq(params[["lr"]][1], params[["lr"]][2], length.out=params[["lr"]][3]),2)
                                 , collapse = ", "),
                           paste(round(seq(params[["rho"]][1], params[["rho"]][2], length.out=params[["rho"]][3]),2)
                                 , collapse = ", "),
                           paste(params[["decay"]], collapse = ", "),
                           paste(floor(seq(params[["cost"]][1], params[["cost"]][2], length.out=params[["cost"]][3]))
                                 , collapse = ", "),
                           paste(params[["activation"]], collapse = ", ")
                           ),
                 train_resampling_method = "adaptive_cv" )
    }
  
  
}, simplify=FALSE)


### Saving model params table
writexl::write_xlsx(dplyr::bind_rows(model_params_table_output),
                    path = base::file.path(output_Dir, paste0("model_params_table_output.xlsx") )
                    )
