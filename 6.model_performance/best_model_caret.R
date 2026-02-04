library(dplyr)
library(tidyr)

working_directory
 
## best overall model by analysis
caret_best_model_analysis_estimates_mean_rank <- caret_metrics_all_df %>%
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

## best overall model by analysis and label
caret_best_model_analysis_label_estimates_rank <- caret_metrics_all_df %>%
  dplyr::filter(analysis %in% caret_best_model_analysis_estimates_mean_rank$analysis) %>% 
  dplyr::group_by(analysis, analysis_name, label, scores) %>%
  tidyr::pivot_longer(!c(analysis, analysis_name, label, scores),
                      names_to = "name", values_to = "value"
                      ) %>%
  dplyr::arrange(scores, forcats::as_factor(name)) %>%
  dplyr::group_by(scores, name) %>%
  dplyr::mutate(rank = rank(-value, ties.method= "first")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-value) %>%
  tidyr::pivot_wider(names_from = label, values_from = rank)

caret_best_model_label_estimates_mean_rank <- caret_metrics_all_df %>%
  dplyr::filter(analysis %in% caret_best_model_analysis_estimates_mean_rank$analysis) %>% 
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
  dplyr::arrange(mean_of_rank)

## best model
best_model_caret_df <- caret_best_model_label_estimates_mean_rank %>%
  dplyr::slice(1)

## save best model metrics
writexl::write_xlsx(list( best_overall_model_analysis = caret_best_model_analysis_estimates_mean_rank
                          , best_model_analysis_label_rank = caret_best_model_analysis_label_estimates_rank
                          , best_model_analysis_label_mean_rank = caret_best_model_label_estimates_mean_rank
                          , best_model_caret = best_model_caret_df
                          ),
                    path = base::file.path(output_Dir, "best_model_caret.xlsx" )
                    )

