library(dplyr)
library(forcats)

working_directory
## Get metrics for all models

caret_metrics_df <- sapply(ls(pattern = "_train_caret$"), function(x){
  modname <- gsub("\\_train_caret", "", x)
  x <- get(x)
  nn <- names(x)
  out_ <- sapply(nn, function(s) {
    out <- bootEstimates_metrics_caret(df=test2, model=x[[s]], nreps=bootstrap_samples)
    out$label <- modname
    out$analysis <- s
    return(out)
  }, simplify=FALSE)
  out_ <- do.call("rbind", out_)
}, simplify=FALSE)


caret_metrics_all_df <- do.call("rbind", caret_metrics_df) %>%
  dplyr::left_join(data_names_train_df, by = c("analysis")) %>%
  dplyr::filter(scores %in% performance_evaluation$metric[performance_evaluation$choose == "yes"]) %>%
  dplyr::mutate(scores = forcats::as_factor(scores))

