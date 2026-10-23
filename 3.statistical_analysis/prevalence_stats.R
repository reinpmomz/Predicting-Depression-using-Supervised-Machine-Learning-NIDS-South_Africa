library(dplyr)


my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics
### unweighted 

prevalence_overall_wave_unweighted_stats <- 
  categorical_inferential_table(df = df_analysis,
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "",
                                by_vars = outcome_vars , 
                                percent = "row",
                                overall = FALSE,
                                flex_table = TRUE,
                                ci=TRUE,
                                p_value = FALSE,
                                include_vars = names(df_analysis)[names(df_analysis) %in% c(outcome_vars, wave_id_vars)]
                                )

print(prevalence_overall_wave_unweighted_stats)

prevalence_wave_location_unweighted_stats <- 
  categorical_inferential_strata_table(df = df_analysis %>% tidyr::drop_na(any_of(outcome_vars)),
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "",
                                       strata_var = wave_id_vars,
                                       by_vars = outcome_vars, 
                                       percent = "row",
                                       flex_table = TRUE,
                                       ci=TRUE,
                                       p_value = FALSE,
                                       include_vars = names(df_analysis)[names(df_analysis) %in% c(outcome_vars,
                                                                                                   wave_id_vars,
                                                                                                   "prov2001")]
                                       )

print(prevalence_wave_location_unweighted_stats)

### weighted 

#add_ci()` function is meant to work with `tbl_svysummary(percent="column")`, but `tbl_svysummary(percent="row")` was used

prevalence_overall_wave_weighted_stats <- 
  survey::svydesign(~cluster_coalesce, data = as.data.frame(df_analysis), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(wave_id_vars) 
                 ,include = c(outcome_vars)
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
  add_ci(conf.level = 0.95, # add columns with confidence interval
         statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                          all_continuous() ~ "{conf.low} - {conf.high}"
                          ),
         style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                          all_continuous() ~ style_sigfig
                          )
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

print(prevalence_overall_wave_weighted_stats)


prevalence_wave_location_weighted_stats <- sapply(sort(as.character(unique(df_analysis$prov2001))), function(x){
  nn <- x
  
  df_new <- df_analysis %>%
    dplyr::filter(prov2001 == nn)
  
  ### dropping variables with one level so that you dont get Error in sum(sapply(covmats, ncol)) : invalid 'type' (list) of argument
  levels_df <- df_new[ , sapply(lapply(df_new, unique), length) > 1]
  
  out <- survey::svydesign(~cluster_coalesce, data = as.data.frame(levels_df), weights = ~dwgt) %>%
  tbl_svysummary(by = any_of(wave_id_vars) 
                 ,include = c(outcome_vars)
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
    add_ci(conf.level = 0.95, # add columns with confidence interval
           statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                            all_continuous() ~ "{conf.low} - {conf.high}"
           ),
           style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                            all_continuous() ~ style_sigfig
           )
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
    gtsummary::as_flex_table()
  
}, simplify = FALSE)

print(prevalence_wave_location_weighted_stats)
