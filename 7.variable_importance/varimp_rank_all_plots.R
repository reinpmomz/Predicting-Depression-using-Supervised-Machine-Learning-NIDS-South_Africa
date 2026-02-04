library(ggplot2)
library(ggpubr)

working_directory
ggtheme_rank_plot()

all_rank_plots <- sapply(ls(pattern = "_varfreq_plot$"), function(x){
  ml <- gsub("\\_varfreq_plot", "", x)
  x <- get(x)
  length <- length(x) 
  nn <- names(x)
  
  plot <- ggpubr::annotate_figure( 
    p = ggpubr::ggarrange(plotlist = x, 
                      ncol = length,
                      nrow = NULL,
                      legend = "right", 
                      common.legend = TRUE),
    top = NULL,
    bottom = text_grob("Rank", color = "black", face = "bold", size = 12)
  )
  
}, simplify = FALSE)

print(all_rank_plots)

### Saving all rank plots using loops
for (l in seq(length(all_rank_plots))) {
  ggsave(plot=all_rank_plots[[l]], height = 8, width = 15,
         filename = paste0(names(all_rank_plots)[[l]],".png"),
         path = output_plots_Dir, bg='white')  
}

