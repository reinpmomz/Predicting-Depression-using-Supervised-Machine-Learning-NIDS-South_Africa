library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(survey)
library(labelled)
library(ggplot2)
library(gtsummary)


ggtheme_rank_plot()

## Proportion of Depression per wave and overall

#scales::show_col(scales::hue_pal(direction=-1)(4))

depression_overall_unweighted_stats <- descriptive_table(df = df_analysis
                                                         ,foot_note = "n (%); Mean (SD); Median (IQR); Range"
                                                         ,caption = ""
                                                         ,flex_table = FALSE
                                                         ,ci=TRUE
                                                         #,statistic_categorical = "{p}%" #{n} ({p}%)
                                                         ,include_vars = outcome_vars
                                                         #, categorical_proportion_digits = 2
                                                         ) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::filter(variables == "_Yes_") %>%
  dplyr::select(-n) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("overall_"))
                      ) %>%
  dplyr::mutate(across(contains(c("overall_", "_ci")), ~gsub("%", "", .x))
                ,overall_p = as.numeric(str_extract(overall_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::separate(x95_percent_ci, into = c("lower_p", "higher_p"), sep = "\\s*-\\s*") %>%
  dplyr::mutate(across(c(lower_p, higher_p), as.numeric)
                , period = "Overall"
                , group = "Overall"
                , stats = "Unweighted") %>%
  dplyr::select(overall_p, lower_p, higher_p, period, group, stats)


depression_waves_unweighted_stats <- 
  categorical_inferential_table(df = df_analysis,
                                by_vars = outcome_vars , 
                                percent = "row",
                                overall = FALSE,
                                flex_table = FALSE,
                                ci=TRUE,
                                p_value = FALSE,
                                include_vars = wave_id_vars
                                )[[1]] %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  dplyr::filter(variables %in% c("_w1_", "_w2_", "_w3_", "_w4_", "_w5_")) %>%
  dplyr::rename(period = variables) %>%
  dplyr::select(period, contains(c("yes_", "_ci_2"))) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("yes_"))
                      ) %>%
  dplyr::mutate(across(contains(c("yes_", "_ci_2")), ~gsub("%", "", .x))
                ,period = gsub("_", "", period)
                ,overall_p = as.numeric(str_extract(yes_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::separate(x95_percent_ci_2, into = c("lower_p", "higher_p"), sep = "\\s*-\\s*") %>%
  dplyr::mutate(across(c(lower_p, higher_p), as.numeric)
                , group = "Waves"
                , stats = "Unweighted") %>%
  dplyr::select(overall_p, lower_p, higher_p, period, group, stats)


depression_overall_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(include = any_of(outcome_vars)
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
                 , percent = "column" #"column", "row", or "cell"
                 , missing = "ifany" #list missing data separately #ifany #no #always
                 , missing_text = "Missing"
                 ) %>%
  add_ci(conf.level = 0.95, # add columns with confidence interval
               statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                all_continuous() ~ "{conf.low} - {conf.high}"
                                ),
               style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                all_continuous() ~ style_sigfig
                                )
        )  %>% 
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
  dplyr::filter(variables == "_Yes_") %>%
  dplyr::select(-n) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("overall_"))
                      ) %>%
  dplyr::mutate(across(contains(c("overall_", "_ci")), ~gsub("%", "", .x))
                ,overall_p = as.numeric(str_extract(overall_n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::separate(x95_percent_ci, into = c("lower_p", "higher_p"), sep = "\\s*-\\s*") %>%
  dplyr::mutate(across(c(lower_p, higher_p), as.numeric)
                , period = "Overall"
                , group = "Overall"
                , stats = "Weighted") %>%
  dplyr::select(overall_p, lower_p, higher_p, period, group, stats)


depression_waves_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(wave_id_vars) 
                 ,include = any_of(outcome_vars)
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
                 , percent = "column" #"column", "row", or "cell"
                 , missing = "ifany" #list missing data separately #ifany #no #always
                 , missing_text = "Missing"
                 ) %>%
  add_ci(conf.level = 0.95, # add columns with confidence interval
               statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                all_continuous() ~ "{conf.low} - {conf.high}"
                                ),
               style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                all_continuous() ~ style_sigfig
                                )
        )  %>% 
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
  gtsummary::as_tibble()  %>%
  janitor::clean_names() %>%
  dplyr::filter(variables %in% c("_Yes_")) %>%
  dplyr::select(-n) %>%
  dplyr::rename_with( ~ gsub("_[0-9]+$", "", .x), contains(c("w1", "w2", "w3", "w4", "w5"))
                ) %>%
  dplyr::rename_with( ~ gsub("x95_percent_ci_", "w", .x), contains(c("x95_percent_ci_"))
                ) %>%
  dplyr::rename(w1 = x95_percent_ci) %>%
  dplyr::rename_with( ~ paste0(.x,"_ci"), any_of(c("w1", "w2", "w3", "w4", "w5"))
                ) %>% 
  tidyr::pivot_longer(!variables
                      , names_to = c("wave", "set")
                      , names_pattern = "(.*)_(.*)"
                      ) %>%
  tidyr::pivot_wider(names_from = set
                     , values_from = value
                     ) %>%
  dplyr::select(-variables) %>%
  dplyr::rename(period = wave) %>%
  
  dplyr::mutate(across(c("n", "ci"), ~gsub("%", "", .x))
                ,overall_p = as.numeric(str_extract(n, "(?<=\\().*?(?=\\))")) #extract a string in between ()
                ) %>%
  tidyr::separate(ci, into = c("lower_p", "higher_p"), sep = "\\s*-\\s*") %>%
  dplyr::mutate(across(c(lower_p, higher_p), as.numeric)
                , group = "Waves"
                , stats = "Weighted") %>%
  dplyr::select(overall_p, lower_p, higher_p, period, group, stats)

proportion_line_plot <- dplyr::bind_rows(#depression_overall_unweighted_stats,
                                         depression_waves_unweighted_stats,
                                         #depression_overall_weighted_stats,
                                         depression_waves_weighted_stats
                                         ) %>%
  dplyr::mutate(across(c(period, group, stats), ~forcats::as_factor(.x))
                ) %>%
  ggplot(aes(x=period, y = overall_p)) +
  geom_point()+
  geom_line(aes(group=group))+
  geom_errorbar(aes(ymin = lower_p, ymax = higher_p), width = 0.3) +
  facet_wrap(~stats, scales="fixed", ncol = 2) +
  scale_y_continuous(limits = c(0,NA), n.breaks = 15, expand = expansion(mult = c(0.01,0.05))) +
  labs(y = "Prevalence (%)", x = "")


print(proportion_line_plot)

# Save the plot
ggsave(plot=proportion_line_plot, height = 6, width = 9,
       filename = "depression_proportion_line_plot.png", path = output_plots_Dir, bg='white')

