library(dplyr)
library(tibble)
library(writexl)


## Saving Test-Retest Reliability stats Output


if (length(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                    !is.na(selected_vars_df$tool_reliability)]))>0 & 
    length(unique(df_analysis[[wave_id_vars]])) >1 ) {
  sapply(names(test_retest_reliability_tools_stats_w2_w4), function(x){
    nn <- x
    writexl::write_xlsx(list(r12 = data.frame(r12 = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["r12"]])),
                             alpha = tibble::rownames_to_column(as.data.frame(test_retest_reliability_tools_stats_w2_w4[[nn]][["alpha"]])),
                             item.stats = tibble::rownames_to_column(test_retest_reliability_tools_stats_w2_w4[[nn]][["item.stats"]]),
                             multilevel_component = tibble::rownames_to_column(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["components"]]),
                             multilevel_reliability = data.frame(R1F = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["R1F"]]),
                                                                 RkF = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["RkF"]]),
                                                                 R1R = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["R1R"]]),
                                                                 RkR = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["RkR"]]),
                                                                 Rc = unname(test_retest_reliability_tools_stats_w2_w4[[nn]][["ml"]][["Rc"]])
                                                                 ),
                             description = data.frame(r12 = "The time 1 time 2 correlation of scaled scores across time",
                                                      alpha = "Guttman's lambda 3 (aka alpha) and lambda 6* (item reliabilities based upon smcs) at times 1 and 2",
                                                      item.stats = "Item reliabilities, item loadings at time 1 and 2, item means at time 1 and time 2",
                                                      ICC = "Resulting variance ratio of people to total variance between the two tests",
                                                      R1F = "Reliability of all ratings across all items and a single time point (fixed effects)",
                                                      RkF = "Reliability of average of all ratings across all items and times (fixed effects)",
                                                      R1R = "Generalizability of a single time point across all items (Random effects)",
                                                      RkR = "Generalizability of average time points across all items (Random effects)",
                                                      Rc = "Generalizability of change scores over time."
                                                      ) %>%
                               tidyr::pivot_longer(everything())
                             ),
                        path = base::file.path(output_Dir, paste0("test_retest_reliability_w2_w4_",nn,".xlsx") )
                        )
                         
                       }, simplify = FALSE
    )

} else {
  print(paste0("No Test-retest reliability analysis done"))
}


