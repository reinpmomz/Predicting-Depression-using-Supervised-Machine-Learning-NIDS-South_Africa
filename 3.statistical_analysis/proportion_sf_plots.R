library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(survey)
library(labelled)
library(ggplot2)
library(sf)
library(gtsummary)


## Load shape files from local folder
#scales::show_col(scales::hue_pal(direction=-1)(10))
#"#FEE0D2", "#FC9272", "#DE2D26"

sf_data_files <- list.files(path = data_Dir, pattern = "_shapefile$", full.names = FALSE)

sf_data_shape_files <- list.files(path = base::file.path(data_Dir, sf_data_files), pattern = ".shp$", full.names = TRUE)

south_africa_map_df <- sf::read_sf(sf_data_shape_files) %>%
  dplyr::mutate(ADM1_EN = if_else(ADM1_EN == "Nothern Cape", "Northern Cape", ADM1_EN)
                )

## Proportion of Depression per province, wave and overall

depression_overall_province_unweighted_stats <- 
  categorical_inferential_table(df = df_analysis,
                                by_vars = outcome_vars , 
                                percent = "row",
                                overall = FALSE,
                                flex_table = FALSE,
                                ci=FALSE,
                                p_value = FALSE,
                                include_vars = c("prov2001")
                                )[[1]] %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::select(variables, contains(c("yes_"))) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("yes_"))
                      ) %>%
  dplyr::mutate(across(contains(c("yes_")), ~gsub("%", "", .x))
                ,variables = gsub("_", "", variables)
                ,overall_p = as.numeric(str_extract(yes_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::drop_na(overall_p) %>%
  dplyr::mutate( period = "Overall"
                , stats = "Unweighted"
                ) %>%
  dplyr::select(variables, overall_p, period, stats)


depression_waves_province_unweighted_stats <- sapply(as.character(unique(df_analysis[[wave_id_vars]])), function(x){
  nn <- x
  
  df_new <- df_analysis %>%
    dplyr::filter(wave_id == nn)
  
  out <- categorical_inferential_table(df = df_new,
                                       by_vars = outcome_vars , 
                                       percent = "row",
                                       overall = FALSE,
                                       flex_table = FALSE,
                                       ci=FALSE,
                                       p_value = FALSE,
                                       include_vars = c("prov2001")
                                       )[[1]] %>%
    gtsummary::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(variables, contains(c("yes_"))) %>%
    dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("yes_"))
                        ) %>%
    dplyr::mutate(across(contains(c("yes_")), ~gsub("%", "", .x))
                  ,variables = gsub("_", "", variables)
                  ,overall_p = as.numeric(str_extract(yes_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                  ) %>%
    tidyr::drop_na(overall_p) %>%
    dplyr::mutate( period = nn
                  , stats = "Unweighted"
                  ) %>%
    dplyr::select(variables, overall_p, period, stats)
  
}, simplify = FALSE) %>% 
  dplyr::bind_rows()


depression_overall_province_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(outcome_vars) 
                 ,include = c(prov2001)
                 , type = list(
                               all_dichotomous() ~ "categorical"
                               ,all_continuous() ~ "continuous2"
                               )
                 , statistic = list(
                   all_continuous(continuous2 = TRUE) ~ c(
                   "{mean} ({sd})",
                   "{median} ({p25}, {p75})",
                   "{min}, {max}" ),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                   )
                 , digits = list(all_continuous(continuous2 = TRUE) ~ 1, 
                                 all_categorical() ~ c(0, 1)
                                 )
                 , percent = "row" #"column", "row", or "cell"
                 , missing = "ifany" #list missing data separately #ifany #no #always
                 , missing_text = "Missing"
                 ) %>%
  modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n_unweighted}"
                    # update the column header
                ) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  add_n( statistic = "{N_nonmiss_unweighted}", col_label = "**n**", last = FALSE, footnote = FALSE
         # add column with total number of non-missing observations
         ) %>%
  modify_caption("") %>%
  #modify_footnote(all_stat_cols() ~ "") %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::select(variables, contains(c("yes_"))) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("yes_"))
                      ) %>%
  dplyr::mutate(across(contains(c("yes_")), ~gsub("%", "", .x))
                ,variables = gsub("_", "", variables)
                ,overall_p = as.numeric(str_extract(yes_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::drop_na(overall_p) %>%
  dplyr::mutate( period = "Overall"
                , stats = "Weighted"
                ) %>%
  dplyr::select(variables, overall_p, period, stats)

depression_waves_province_weighted_stats <- sapply(as.character(unique(df_analysis[[wave_id_vars]])), function(x){
  nn <- x
  
  df_new <- df_analysis %>%
    dplyr::filter(wave_id == nn)
  
  out <- survey::svydesign(~cluster_coalesce, data = as.data.frame(df_new), weights = ~dwgt) %>%
     tbl_svysummary(by = any_of(outcome_vars) 
                    ,include = c(prov2001)
                    , type = list(
                                  all_dichotomous() ~ "categorical"
                                  ,all_continuous() ~ "continuous2"
                                  )
                    , statistic = list(
                      all_continuous(continuous2 = TRUE) ~ c(
                      "{mean} ({sd})",
                      "{median} ({p25}, {p75})",
                      "{min}, {max}" ),
                      all_categorical() ~ "{n_unweighted} ({p}%)"
                      )
                    , digits = list(all_continuous(continuous2 = TRUE) ~ 1, 
                                    all_categorical() ~ c(0, 1)
                                    )
                    , percent = "row" #"column", "row", or "cell"
                    , missing = "ifany" #list missing data separately #ifany #no #always
                    , missing_text = "Missing"
                    ) %>%
     modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n_unweighted}"
                       # update the column header
                   ) %>% 
     bold_labels() %>%
     italicize_levels() %>% 
     add_n( statistic = "{N_nonmiss_unweighted}", col_label = "**n**", last = FALSE, footnote = FALSE
            # add column with total number of non-missing observations
            ) %>%
     modify_caption("") %>%
     #modify_footnote(all_stat_cols() ~ "") %>%
     gtsummary::as_tibble() %>%
     janitor::clean_names() %>%
     dplyr::select(variables, contains(c("yes_"))) %>%
     dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("yes_"))
                         ) %>%
     dplyr::mutate(across(contains(c("yes_")), ~gsub("%", "", .x))
                   ,variables = gsub("_", "", variables)
                   ,overall_p = as.numeric(str_extract(yes_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                   ) %>%
     tidyr::drop_na(overall_p) %>%
     dplyr::mutate( period = nn
                   , stats = "Weighted"
                   ) %>%
     dplyr::select(variables, overall_p, period, stats)
  
}, simplify = FALSE) %>% 
  dplyr::bind_rows()

# Custom function for labeling longitude/latitude with ° and direction
label_degree <- function(x, type = "lon") {
  deg <- paste0(abs(x), "°", ifelse(type == "lon",
                                    ifelse(x > 0, "E", "W"),
                                    ifelse(x > 0, "N", "S")))
  return(deg)
}

proportion_sf_plot <- dplyr::bind_rows(#depression_overall_province_unweighted_stats,
                                         depression_waves_province_unweighted_stats,
                                         #depression_overall_province_weighted_stats,
                                         depression_waves_province_weighted_stats
                                         ) %>%
  dplyr::mutate(across(c(period, stats), ~factor(.x))
                ) %>%
  dplyr::right_join(south_africa_map_df,
                    by = c("variables" = "ADM1_EN")
                    ) %>%
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = overall_p)) +
  geom_sf_text(aes(label = ADM1_ID), size = 2, color = "black", fontface = "bold")+
  scale_x_continuous(labels = function(x) label_degree(x, "lon"), n.breaks = 7) +
  scale_y_continuous(labels = function(x) label_degree(x, "lat")) +
  scale_fill_continuous(palette = c("greenyellow", "yellow", "red")) +
  labs(fill = "Prevalence", x = "Longitude", y = "Latitude", caption = "Shape File Source: South African Municipal Demarcation Board") +
  facet_grid(stats~period, scales="fixed") +
  theme_grey() +
  theme( legend.position="bottom" #default
         , strip.text = element_text(colour = "black", face = "bold" )
         ,axis.text = element_text(size = 7)
         , panel.border = element_rect(colour = "grey", fill = NA, linewidth = 0.8)
         )
  
#print(proportion_sf_plot)

# Save the plot
ggsave(plot=proportion_sf_plot, height = 6, width = 9,
       filename = "depression_proportion_sf_plot.png", path = output_plots_Dir, bg='white')


