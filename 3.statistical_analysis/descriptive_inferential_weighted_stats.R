library(dplyr)
library(survey)
library(gtsummary)


my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics

descriptive_all_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(include = -c(pid, dwgt, cluster_coalesce)
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
                 , missing_stat = "{N_miss_unweighted}" #N_miss, N_obs, N_nonmiss, p_miss, p_nonmiss
                 ) %>%
  # add_ci(conf.level = 0.95, # add columns with confidence interval
  #              statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
  #                               all_continuous() ~ "{conf.low} - {conf.high}"
  #                               ),
  #              style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
  #                               all_continuous() ~ style_sigfig
  #                               )
  #       )  %>% 
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
  gtsummary::as_flex_table()

print(descriptive_all_weighted_stats)


descriptive_wave_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(wave_id_vars) 
                 ,include = -c(pid, dwgt, cluster_coalesce)
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
                 , missing_stat = "{N_miss_unweighted}" #N_miss, N_obs, N_nonmiss, p_miss, p_nonmiss
                 ) %>%
  # add_ci(conf.level = 0.95, # add columns with confidence interval
  #              statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
  #                               all_continuous() ~ "{conf.low} - {conf.high}"
  #                               ),
  #              style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
  #                               all_continuous() ~ style_sigfig
  #                               )
  #       )  %>% 
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
  gtsummary::as_flex_table()

print(descriptive_wave_weighted_stats)

## Inferential Statistics

inferential_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(outcome_vars) 
                 ,include = -c(pid, dwgt, cluster_coalesce)
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
                 , missing_stat = "{N_miss_unweighted}" #N_miss, N_obs, N_nonmiss, p_miss, p_nonmiss
                 ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
        test.args = list(all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                         )
        ) %>% 
  bold_p(t= 0.05) %>%
  modify_table_body(
                ~ .x %>%
                  dplyr::relocate(c(statistic), .before = p.value)
                ) %>%
  modify_fmt_fun(c(statistic) ~ label_style_number(digits = 1)
                             ) %>% 
  modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n_unweighted}"
                , statistic = "**Test statistic**"
                    # update the column header
                ) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  add_n( statistic = "{N_nonmiss_unweighted}", col_label = "**n**", last = FALSE, footnote = FALSE
         # add column with total number of non-missing observations
         ) %>%
  modify_caption("") %>%
  #modify_footnote(all_stat_cols() ~ "") %>%
  gtsummary::as_flex_table()

print(inferential_weighted_stats)


inferential_strata_weighted_stats <- sapply(sort(as.character(unique(df_analysis[[wave_id_vars]]))), function(x){
  nn <- x
  
  df_new <- df_analysis %>%
    dplyr::filter(wave_id == nn)
  
  ### dropping variables with one level so that you dont get Error in sum(sapply(covmats, ncol)) : invalid 'type' (list) of argument
  levels_df <- df_new[ , sapply(lapply(df_new, unique), length) > 1]
  
  out <- survey::svydesign(~cluster_coalesce, data = as.data.frame(levels_df), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(outcome_vars) 
                 ,include = -c(pid, dwgt, cluster_coalesce)
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
                 , missing_stat = "{N_miss_unweighted}" #N_miss, N_obs, N_nonmiss, p_miss, p_nonmiss
                 ) %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
        test.args = list(all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                         )
        ) %>% 
  bold_p(t= 0.05) %>%
  modify_table_body(
                ~ .x %>%
                  dplyr::relocate(c(statistic), .before = p.value)
                ) %>%
  modify_fmt_fun(c(statistic) ~ label_style_number(digits = 1)
                             ) %>% 
  modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n_unweighted}"
                , statistic = "**Test statistic**"
                    # update the column header
                ) %>% 
  bold_labels() %>%
  italicize_levels() %>% 
  add_n( statistic = "{N_nonmiss_unweighted}", col_label = "**n**", last = FALSE, footnote = FALSE
         # add column with total number of non-missing observations
         ) %>%
  modify_caption("")
  
}, simplify = FALSE)

inferential_strata_weighted_stats_merge <- 
  gtsummary::tbl_merge(tbls= c(inferential_strata_weighted_stats 
                                 ),
                         tab_spanner = c(names(inferential_strata_weighted_stats)
                                         )
                         ) %>%
      gtsummary::as_flex_table()
