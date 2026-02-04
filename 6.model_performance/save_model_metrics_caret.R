library(dplyr)
library(tidyr)
library(forcats)
library(writexl)

working_directory
 
## save caret model metrics
writexl::write_xlsx(list( metrics_all = caret_metrics_all_df %>%
                            dplyr::mutate(across(c(estimate, lower, upper) , ~round(.x, 4))
                                          , estimate_ci = paste0(estimate, " (", lower, " - ", upper, ")")
                                          ) %>%
                            dplyr::select(analysis, analysis_name, label, scores, estimate_ci) %>%
                            tidyr::pivot_wider(names_from = scores, values_from = estimate_ci) %>%
                            dplyr::arrange(analysis)
                          , metrics_by_analysis_label_rank = caret_metrics_all_df %>% 
                            tidyr::pivot_longer(!c(analysis, analysis_name, label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            tidyr::pivot_wider(names_from = scores, values_from = rank)
                          , best_analysis_label_rank_by_estimate = caret_metrics_all_df %>% 
                            tidyr::pivot_longer(!c(analysis, analysis_name, label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(analysis, analysis_name, label, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(name, mean_of_rank)
                          , best_analysis_label_rank = caret_metrics_all_df %>% 
                            dplyr::group_by(analysis, analysis_name, label, scores) %>%
                            dplyr::summarise(estimate = mean(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(analysis, analysis_name, label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(analysis, analysis_name, label, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(name, mean_of_rank) %>%
                            dplyr::slice(1)
                          , metrics_by_analysis = caret_metrics_all_df %>% 
                            dplyr::group_by(analysis, analysis_name, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            dplyr::mutate(across(c(mean_estimate, best_estimate, worst_estimate) , ~round(.x, 4))) %>%
                            tidyr::pivot_longer(!c(analysis, analysis_name, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            tidyr::pivot_wider(names_from = c(analysis, analysis_name), values_from = value)
                          , metrics_by_analysis_rank = caret_metrics_all_df %>% 
                            dplyr::group_by(analysis, analysis_name, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(analysis, analysis_name, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            tidyr::pivot_wider(names_from = c(analysis, analysis_name), values_from = rank)
                          , best_analysis_rank_by_estimate = caret_metrics_all_df %>%
                            dplyr::group_by(analysis, analysis_name, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(analysis, analysis_name, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(analysis, analysis_name, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(name, mean_of_rank)
                          , best_analysis_rank = caret_metrics_all_df %>%
                            dplyr::group_by(analysis, analysis_name, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(analysis, analysis_name, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(analysis, analysis_name, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(mean_of_rank) %>%
                            dplyr::slice(1)
                          , metrics_by_label = caret_metrics_all_df %>% 
                            dplyr::group_by(label, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            dplyr::mutate(across(c(mean_estimate, best_estimate, worst_estimate) , ~round(.x, 4))) %>%
                            tidyr::pivot_longer(!c(label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            tidyr::pivot_wider(names_from = label, values_from = value)
                          , metrics_by_label_rank = caret_metrics_all_df %>%
                            dplyr::group_by(label, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            tidyr::pivot_wider(names_from = label, values_from = rank)
                          , best_label_rank_by_estimate = caret_metrics_all_df %>%
                            dplyr::group_by(label, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , best_estimate = max(estimate, na.rm = TRUE)
                                             , worst_estimate = min(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(label, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(name, mean_of_rank)
                          , best_label_rank = caret_metrics_all_df %>% 
                            dplyr::group_by(label, scores) %>%
                            dplyr::summarise(mean_estimate = mean(estimate, na.rm = TRUE)
                                             , .groups = 'drop'
                                             ) %>%
                            tidyr::pivot_longer(!c(label, scores),
                                                names_to = "name", values_to = "value"
                                                ) %>%
                            dplyr::arrange(scores, forcats::as_factor(name)) %>%
                            dplyr::group_by(scores, name) %>%
                            dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
                            dplyr::ungroup() %>%
                            dplyr::select(-value) %>%
                            dplyr::group_by(label, name) %>%
                            dplyr::summarise(mean_of_rank = mean(rank), .groups = 'drop') %>%
                            dplyr::arrange(mean_of_rank) %>%
                            dplyr::slice(1)
                          ),
                    path = base::file.path(output_Dir, "model_metrics_caret.xlsx" )
                    )
