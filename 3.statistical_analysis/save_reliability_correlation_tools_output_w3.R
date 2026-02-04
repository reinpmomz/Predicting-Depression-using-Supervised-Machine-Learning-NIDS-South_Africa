library(dplyr)
library(tibble)
library(writexl)


## Saving reliability and correlation stats Output

if (length(unique(selected_vars_df$tool_correlation[selected_vars_df$tool_correlation != "none" &
                                                    !is.na(selected_vars_df$tool_correlation)]))>0) {
writexl::write_xlsx(correlation_tools_stats_w3,
                    path = base::file.path(output_Dir, "correlation_tools_w3.xlsx" )
                    )
 } else {
    print(paste0("No correlation analysis done for tools"))
  }


if (length(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                    !is.na(selected_vars_df$tool_reliability)]))>0) {
  sapply(names(reliability_tools_stats_all_w3), function(x){
    nn <- x
    writexl::write_xlsx(list(reliability = reliability_tools_stats_all_w3[[nn]][["total"]],
                             alpha_ci = data.frame(lower = unname(reliability_tools_stats_all_w3[[nn]][["feldt"]][["lower.ci"]]),
                                                   alpha = unname(reliability_tools_stats_all_w3[[nn]][["feldt"]][["alpha"]]),
                                                   upper = unname(reliability_tools_stats_all_w3[[nn]][["feldt"]][["upper.ci"]])
                                                   ),
                             reliability_alphadrop = tibble::rownames_to_column(reliability_tools_stats_all_w3[[nn]][["alpha.drop"]]),
                             reliability_itemstats = tibble::rownames_to_column(reliability_tools_stats_all_w3[[nn]][["item.stats"]]),
                             reliability_itemfreq = tibble::rownames_to_column(as.data.frame(reliability_tools_stats_all_w3[[nn]][["response.freq"]]))
                             ),
                        path = base::file.path(output_Dir, paste0("reliability_",nn, "_w3",".xlsx") )
                        )
                         
                       }, simplify = FALSE
    )

} else {
  print(paste0("No reliability analysis done"))
}


if (length(unique(selected_vars_df$tool_reliability_group[selected_vars_df$tool_reliability_group != "none" &
                                                    !is.na(selected_vars_df$tool_reliability_group)]))>0) {
  sapply(names(reliability_tools_stats_group_w3), function(x){
    nn <- x
    writexl::write_xlsx(list(reliability = reliability_tools_stats_group_w3[[nn]][["total"]],
                             alpha_ci = data.frame(lower = unname(reliability_tools_stats_group_w3[[nn]][["feldt"]][["lower.ci"]]),
                                                   alpha = unname(reliability_tools_stats_group_w3[[nn]][["feldt"]][["alpha"]]),
                                                   upper = unname(reliability_tools_stats_group_w3[[nn]][["feldt"]][["upper.ci"]])
                             ),
                             reliability_alphadrop = tibble::rownames_to_column(reliability_tools_stats_group_w3[[nn]][["alpha.drop"]]),
                             reliability_itemstats = tibble::rownames_to_column(reliability_tools_stats_group_w3[[nn]][["item.stats"]]),
                             reliability_itemfreq = tibble::rownames_to_column(as.data.frame(reliability_tools_stats_group_w3[[nn]][["response.freq"]]))
                             ),
                        path = base::file.path(output_Dir, paste0("reliability_group_",nn, "_w3",".xlsx") )
                        )
    
  }, simplify = FALSE
  )
  
} else {
  print(paste0("No reliability group analysis done"))
}

