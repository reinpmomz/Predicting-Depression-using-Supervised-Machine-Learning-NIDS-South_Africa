library(dplyr)
library(labelled)
library(ggplot2)


ggtheme_descriptive_plot()

## Cumulative Proportion of rounds per individuals

#scales::show_col(scales::hue_pal(direction=-1)(4))

proportion_rounds_plot <- df_analysis %>%
  #dplyr::distinct(pid) %>%
  dplyr::select(any_of(c(study_id_vars, wave_id_vars))
                ) %>%
  dplyr::group_by(pid) %>%
  dplyr::mutate(n_rounds = dplyr::n()
                , rounds = as.character(list(unique(wave_id)))
                ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-wave_id) %>%
  dplyr::distinct() %>%
  dplyr::mutate(rounds = gsub("c\\(|\\)", "", rounds)
                ,rounds = gsub(":", "-", rounds)
                , across(c(rounds, n_rounds), ~as.factor(.x))
                ) %>%
  ggplot(aes(x= forcats::fct_rev(forcats::fct_infreq(n_rounds)))) +
  coord_flip() +
  geom_bar(aes(y = after_stat(count)), fill = "#F8766D" ,
           position="stack", stat="count", show.legend = TRUE, width = 0.8) +
  scale_y_continuous(n.breaks = 12, limits = c(NULL, NULL),
                     expand = expansion(mult = c(0,0.07))) +
  geom_text(aes(label = paste0(after_stat(count), " (",
                               scales::percent(after_stat(count)/sum(after_stat(count)),
                                               accuracy = 0.1),")" )),
            stat = "count", 
            hjust = -0.05,
            colour = "black",
            size = 3) +
  labs(x="Rounds",y="No. of Individuals")
  
print(proportion_rounds_plot)

### Saving the plot
ggsave(plot=proportion_rounds_plot, height = 7, width = 13,
       filename = paste0("proportion_rounds_plot",".png"),
       path = output_plots_Dir, bg='white')
