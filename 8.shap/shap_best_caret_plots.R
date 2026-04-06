library(dplyr)
library(sjlabelled)
library(ggplot2)
library(shapviz)

working_directory 

## Get SHAP for best models
test2 <- (test2 %>% 
            dplyr::select(-any_of("pid"))
          )

caret_shap_best_raw <- caret_shap(df=test2
                              , outcome_var = outcome_vars
                              , pos_class = positive_class
                              , seed = 426
                              , model=get(paste0(best_model_caret_df$label,"_train_caret"))[[paste0(best_model_caret_df$analysis)]]
                              )

### Renaming list objects to column labels
caret_shap_best <- caret_shap_best_raw

shap_feature_names <- names(sjlabelled::label_to_colnames(test2[, colnames(test2)[!colnames(test2) %in% outcome_vars]]))

colnames(caret_shap_best[["S"]]) <- shap_feature_names
caret_shap_best[["X"]] <- sjlabelled::label_to_colnames(caret_shap_best[["X"]]) 

## Plot SHAP values
caret_shap_best_plot <- caret_shap_best %>%
  shapviz::sv_importance(kind = "beeswarm"
                         , bee_width = 0.3
                         , bee_adjust = 0.5
                         , max_display = Inf #20L
                         , sort_features = TRUE
                         ) +
  theme(axis.text = element_text(size = 9))

print(caret_shap_best_plot)

### Saving SHAP plot for best model
ggsave(plot=caret_shap_best_plot, height = 7, width = 10.5,
       filename = paste0("caret_shap_best_model_plot_",best_model_caret_df$label,"_",best_model_caret_df$analysis_name,".png"),
       path = output_plots_Dir, bg='white'
       )  

