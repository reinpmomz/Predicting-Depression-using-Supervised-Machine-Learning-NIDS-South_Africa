library(dplyr)

working_directory
## Get Variable importance for all models

caret_varimp_df <- sapply(ls(pattern = "_train_caret$"), function(x){
  nreps <- 10
  modname <- gsub("\\_train_caret", "", x)
  x <- get(x)
  nn <- names(x)
  out_ <- sapply(nn, function(s) {
    out <- caret_varimp(df=test2, model=x[[s]], modname = modname, nreps=n_permutations)
    out$analysis <- s
    return(out)
  }, simplify=FALSE)
  
  out_ <- do.call("rbind", out_)
}, simplify=FALSE)

caret_varimp_all_df <- do.call("rbind", caret_varimp_df) %>%
  dplyr::filter(!variable %in% c("pid")) %>%
  dplyr::left_join(data_names_train_df, by = c("analysis"))

