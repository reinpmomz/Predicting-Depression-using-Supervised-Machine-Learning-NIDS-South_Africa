library(tidytext)
library(forcats)
library(dplyr)
library(ggplot2)
library(ggpubr)

working_directory
ggtheme_rank_plot()

## Caret Package ROC plots

### Independent
roc_caret_plots <-  sapply(unique(caret_roc_all_df$analysis_name), function(x){
  nn <- x
  df <- caret_roc_all_df %>%
    dplyr::filter(analysis_name == nn) %>%
    arrange(desc(auc_estimate), label, number) %>%
    dplyr::mutate(across(c(label, label_auc), ~forcats::as_factor(.x)))
  
  unique_models <- length(unique(df$label))
  
  plot <- if (nn == "imbalance") {
    ggplot(df, aes(x=x_estimate, y=y_estimate)) +
      geom_line(colour = "red2") +
      geom_ribbon(aes(xmin=x_lower, xmax=x_upper, ymin=y_lower, ymax=y_upper), alpha = 0.2)
    } else if (nn == "down sampling") {
      ggplot(df, aes(x=x_estimate, y=y_estimate)) +
        geom_line(colour = "blue2") +
        geom_ribbon(aes(xmin=x_lower, xmax=x_upper, ymin=y_lower, ymax=y_upper), alpha = 0.2)
      } else { 
        ggplot(df, aes(x=x_estimate, y=y_estimate)) +
          geom_line(colour = "green3") +
          geom_ribbon(aes(xmin=x_lower, xmax=x_upper, ymin=y_lower, ymax=y_upper), alpha = 0.2) 
        }
  
  plot1 <- plot +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = 2) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0.01,0.02))) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.01,0.05))) +
    facet_wrap(analysis_name~label_auc, scales="fixed", ncol = ceiling(unique_models/2), 
               labeller = labeller(.default = label_value, .multi_line = FALSE)) + 
    labs(y = NULL, x = NULL) + 
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  
}, simplify=FALSE)

roc_caret_grid_plots <- ggpubr::annotate_figure(
  p = ggpubr::ggarrange(plotlist = roc_caret_plots,
                        ncol = 1,
                        nrow = NULL),
  top = NULL,
  left = text_grob("True Positive Rate", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("False Positive Rate", color = "black", face = "bold", size = 12)
)

print(roc_caret_grid_plots)

ggsave(plot=roc_caret_grid_plots, height = 7, width = 13,
       filename = "caret_roc_plots.png", path = output_plots_Dir, bg='white')

### Combined
roc_caret_combined_plots <-  sapply(unique(caret_roc_all_df$analysis_name), function(x){
  nn <- x
  df <- caret_roc_all_df %>%
    dplyr::mutate(label_auc_new = paste0(label, " (AUC = ", auc_estimate, ")")) %>%
    dplyr::filter(analysis_name == nn) %>%
    dplyr::arrange(desc(auc_estimate), label, number) %>%
    dplyr::mutate(across(c(label, label_auc_new), ~forcats::as_factor(.x)))
  
  plot <- ggplot(df, aes(x=x_estimate, y=y_estimate, colour = label_auc_new)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = 2) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0.01,0.02))
                       , n.breaks = 10
                       ) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.01,0.03))
                       , n.breaks = 10
                       ) + 
    labs(y = NULL, x = NULL, title = nn) +
    theme(
      legend.position = c(0.72, 0.22),   # x, y in plot coordinates
      legend.background = element_rect(fill = fill_alpha("white", 0.2), colour = "grey"),
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      axis.text.y = element_text(angle = 0, lineheight = 0.6, size = 11),
      axis.text.x = element_text(angle = 0, lineheight = 0.6, size = 11)
    )
  
}, simplify=FALSE)

roc_caret_combined_grid_plots <- ggpubr::annotate_figure(
  p = ggpubr::ggarrange(plotlist = roc_caret_combined_plots,
                        ncol = NULL,
                        nrow = 1),
  top = NULL,
  left = text_grob("True Positive Rate", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("False Positive Rate", color = "black", face = "bold", size = 12)
)

print(roc_caret_combined_grid_plots)

ggsave(plot=roc_caret_combined_grid_plots, height = 7, width = 13,
       filename = "caret_roc_combined_plots.png", path = output_plots_Dir, bg='white')


