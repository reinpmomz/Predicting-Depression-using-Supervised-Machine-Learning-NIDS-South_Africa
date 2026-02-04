library(dplyr)
library(forcats)
library(DALEX)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(stringr)

working_directory
## Variable importance - Top 2 models across analysis name

caret_varimp_plot <- sapply(unique(caret_varimp_all_df$analysis_name), function(x){
  nn <- x
  
  ## Top 2 models across analysis name
  best_model <- caret_metrics_all_df %>%
    dplyr::filter(analysis_name == nn) %>%
    dplyr::group_by(analysis_name, label, scores) %>%
    dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                     , .groups = 'drop'
                     ) %>%
    tidyr::pivot_longer(!c(analysis_name, label, scores),
                        names_to = "name", values_to = "value"
                        ) %>%
    dplyr::arrange(scores, forcats::as_factor(name)) %>%
    dplyr::group_by(scores, name) %>%
    dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value) %>%
    dplyr::group_by(analysis_name, label) %>%
    dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
    dplyr::arrange(mean_of_rank) %>%
    dplyr::slice(1:2)
  
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::filter(new_variable %in% names(test2)) %>%
    tibble::deframe()
  
  df <- caret_varimp_all_df %>%
    dplyr::mutate(variable = as.character(forcats::fct_recode(variable, !!!new_levels))) %>%
    dplyr::filter(analysis_name == nn, 
                  label %in% best_model$label[best_model$analysis_name == nn]) %>%
    #dplyr::arrange(desc(auc)) %>%
    dplyr::mutate(label = forcats::as_factor(label))
  
  p <- sapply(unique(df$label), function(y){
    plot(df %>% dplyr::filter(analysis_name == nn, label == y)
         , show_boxplots = TRUE
         , bar_width = 3 #default 10
         , desc_sorting = TRUE
         , title = "" #default 'Feature Importance'
         , subtitle = ""
    ) +
      labs(x = "", y = "", title = "") +
      scale_y_continuous(expand = expansion(mult = c(0.01,0.1))
                         ) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 65)
                         ) +
      theme(axis.text.y = element_text(angle = 0, lineheight = 0.6, size = 8.5)
            )
      
  }, simplify=FALSE)
  
  grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 2))
                  )
  
  annotate_grid <- ggpubr::annotate_figure( 
    grid,
    top = text_grob(nn, color = "navyblue", face = "bold", size = 12),
    bottom = text_grob("Variable Importance", color = "navyblue", face = "bold", size = 12),
  )
  
} , simplify=FALSE)

print(caret_varimp_plot)

### Saving varimp plots using loops
for (j in seq(length(caret_varimp_plot))) {
  ggsave(plot=caret_varimp_plot[[j]], height = 8, width = 14,
         filename = paste0("caret_varimp_plot_",names(caret_varimp_plot)[[j]],".png"),
         path = output_plots_Dir, bg='white')  
}

## Variable importance - Best analysis name and model
caret_varimp_best_model_plot <- sapply(unique(best_model_caret_df$analysis_name), function(x){
  nn <- x
  
  best_model <- best_model_caret_df
  
  ## Creating a named vector to quickly rename levels
  new_levels <- selected_vars_df %>%
    dplyr::select(new_label, new_variable) %>%
    tidyr::drop_na() %>%
    dplyr::filter(new_variable %in% names(test2)) %>%
    tibble::deframe()
  
  df <- caret_varimp_all_df %>%
    dplyr::mutate(variable = as.character(forcats::fct_recode(variable, !!!new_levels))) %>%
    dplyr::filter(analysis_name == nn, 
                  label %in% best_model$label[best_model$analysis_name == nn]
                )
  
  p <- sapply(unique(df$label), function(y){
    plot(df %>% dplyr::filter(analysis_name == nn, label == y)
         , show_boxplots = TRUE
         , bar_width = 3 #default 10
         , desc_sorting = TRUE
         , title = "" #default 'Feature Importance'
         , subtitle = ""
         ) +
      labs(x = "", y = "", title = "") +
      scale_y_continuous(expand = expansion(mult = c(0.01,0.1))) +
      theme(axis.text.y = element_text(angle = 0, lineheight = 0.7, size = 9)
            )
      
  }, simplify=FALSE)
  
  grid <- do.call(gridExtra::grid.arrange, c(p, list(ncol = 1))
                  )
  
  annotate_grid <- ggpubr::annotate_figure( 
    grid,
    top = text_grob(nn, color = "navyblue", face = "bold", size = 12),
    bottom = text_grob("Variable Importance", color = "navyblue", face = "bold", size = 12),
  )


} , simplify=FALSE)

print(caret_varimp_best_model_plot)

### Saving varimp plots using loops
for (k in seq(length(caret_varimp_best_model_plot))) {
  ggsave(plot=caret_varimp_best_model_plot[[k]], height = 7.5, width = 11.5,
         filename = paste0("caret_varimp_best_model_plot_",names(caret_varimp_best_model_plot)[[k]],".png"),
         path = output_plots_Dir, bg='white')  
}
