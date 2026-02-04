library(dplyr)


my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics

descriptive_all_unweighted_stats <- descriptive_table(df = df_analysis,
                                                      foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                                      caption = "",
                                                      flex_table = TRUE,
                                                      ci=FALSE,
                                                      include_vars = names(df_analysis)[!names(df_analysis) %in% c("pid", "dwgt", "cluster_coalesce")]
                                                      )

print(descriptive_all_unweighted_stats)

descriptive_wave_unweighted_stats <- 
  categorical_inferential_table(df = df_analysis,
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "",
                                by_vars = wave_id_vars , 
                                percent = "column",
                                overall = FALSE,
                                flex_table = TRUE,
                                ci=FALSE,
                                p_value = FALSE,
                                include_vars = names(df_analysis)[!names(df_analysis) %in% c("pid", "dwgt", "cluster_coalesce")]
                                )

print(descriptive_wave_unweighted_stats)


## Inferential Statistics

inferential_vars <- selected_vars_df$new_variable[selected_vars_df$inferential == "yes" &
                                                    !is.na(selected_vars_df$inferential)]

inferential_unweighted_stats <- 
  categorical_inferential_table(df = df_analysis,
                                foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                caption = "",
                                by_vars = outcome_vars , 
                                percent = "row",
                                overall = FALSE,
                                flex_table = TRUE,
                                ci=FALSE,
                                p_value = TRUE,
                                include_vars = names(df_analysis)[!names(df_analysis) %in% c("pid", "dwgt", "cluster_coalesce")]
                                )

print(inferential_unweighted_stats)

inferential_strata_unweighted_stats <- 
  categorical_inferential_strata_table(df = df_analysis %>% tidyr::drop_na(any_of(outcome_vars)),
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "",
                                       strata_var = wave_id_vars,
                                       by_vars = outcome_vars, 
                                       percent = "row",
                                       flex_table = TRUE,
                                       ci=FALSE,
                                       p_value = TRUE,
                                       include_vars = names(df_analysis)[!names(df_analysis) %in% c("pid", "dwgt", "cluster_coalesce")]
                                       )

print(inferential_strata_unweighted_stats)
