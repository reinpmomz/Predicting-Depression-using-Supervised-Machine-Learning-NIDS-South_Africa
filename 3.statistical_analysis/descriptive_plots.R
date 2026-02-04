library(dplyr)
library(labelled)
library(ggplot2)
library(gridExtra)
library(cowplot)


ggtheme_descriptive_plot()

## Wave Stacked plots

descriptive_plot <- sapply(unique(selected_vars_df$plot[selected_vars_df$plot != "none" & !is.na(selected_vars_df$plot)]),
                           function(x){
  nn <- x
  index <- selected_vars_df$new_variable[selected_vars_df$plot == nn & !is.na(selected_vars_df$plot)]
  n_index <- length(index)
  numeric_integer_vars <- names(df_analysis[sapply(df_analysis, function(v) is.numeric(v) | is.integer(v))])
  
  plots <- sapply(index, function(z) {
    labels <- labelled::var_label(df_analysis[[z]])
    p <- if (z %in% numeric_integer_vars) {
      stacked_plot(df= df_analysis, variable = z, fill_var = wave_id_vars, title_label = labels, legend_label="")
    } else {
      stacked_plot(df= df_analysis, variable = wave_id_vars, fill_var = z, title_label = labels, legend_label="",
                   ncol_legend = 3, nrow_legend = NULL, x_axis_label_wrap_width = 20)
    }
    
    
  }, simplify = FALSE)
  
  plots_grid <- cowplot::plot_grid(plotlist = plots, ncol = 3)
  
  
}, simplify = FALSE
)

### Saving descriptive simple plots using loops
 for (i in seq(length(descriptive_plot))) {
   ggsave(plot=descriptive_plot[[i]], height = 7, width = 12,
          filename = paste0("descriptive_plot_",names(descriptive_plot)[[i]],".png"),
          path = output_plots_Dir, bg='white')  
 }

