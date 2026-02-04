library(stringr)
library(ggplot2)

working_directory
ggtheme_rank_plot()


## Caret rank plot
caret_varfreq_plot <- sapply(unique(caret_varfreq_df$analysis_name), function(x){
  nn <- x
  df <- caret_varfreq_df %>%
    dplyr::filter(analysis_name == nn)
  p <- ggplot(df, aes(x=new_pos, y= stats::reorder(terms, -new_pos, FUN = mean), fill=n)) +
    geom_tile(color="black") +
    scale_fill_distiller(palette = "Greens", direction=1) +
    scale_y_discrete(expand=c(0,0)
                     ,labels = function(x) stringr::str_wrap(x, width = 37)
    ) +
    scale_x_continuous(
      breaks=function(x){1:max(x)}
      , labels=function(x){
        m <- max(x)
        v <- as.character(1:m)
        v[[m]] <- paste0(">", m-1)
        return(v)
      }
      , expand=c(0,0)
    ) +
    labs(y=NULL, x=NULL, fill="Frequency", title = nn)
  p
  
} , simplify=FALSE)

print(caret_varfreq_plot)
