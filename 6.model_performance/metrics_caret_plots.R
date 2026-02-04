library(tidytext)
library(forcats)
library(dplyr)
library(ggplot2)
library(scales)

working_directory
ggtheme_rank_plot()

metrics_caret_plots <- ggplot(caret_metrics_all_df %>% 
         dplyr::mutate(analysis_name = forcats::as_factor(analysis_name)) %>%
         dplyr::mutate(label= tidytext::reorder_within(label, -estimate, list(analysis_name, scores)))
         , aes(x=label, y=estimate, colour=scores)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(0.5),
                  na.rm = TRUE,
                  fatten = 0.5
                  ) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(analysis_name,scores), scales="free", ncol = 5, 
             labeller = labeller(.default = label_value, .multi_line = FALSE)) + 
  scale_y_continuous(#limits = c(0,1), n.breaks = 6,
    labels = scales::label_number(accuracy = 0.001)) + 
  labs(y = "Estimate", x = "Model") + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )

print(metrics_caret_plots)


ggsave(plot=metrics_caret_plots, height = 7, width = 12,
       filename = "caret_metrics_plots.png", path = output_plots_Dir, bg='white')
