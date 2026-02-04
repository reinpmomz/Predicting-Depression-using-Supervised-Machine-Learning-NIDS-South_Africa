library(dplyr)
library(tidyr)

working_directory

caret_top_n <- 10
caret_total_n <- as.numeric(ncol(test2[, !names(test2) %in% outcome_vars]))

# Mostly frequently identified variables

## Caret variable importance df
caret_varfreq_df <- caret_varimp_all_df %>%
  dplyr::filter(!variable %in% c("pid")) %>%
  dplyr::left_join(final_attribute %>%
              dplyr::select(variable, label) %>%
              dplyr::rename(terms = label),
            by = c("variable")) %>%
  tidyr::drop_na(terms) %>%
  dplyr::group_by(variable, label, analysis, analysis_name, terms) %>%
  dplyr::summarise(overall = mean(dropout_loss, na.rm=TRUE), .groups = "drop") %>%
  dplyr::group_by(analysis, analysis_name, label) %>%
  dplyr::arrange(desc(overall), .by_group=TRUE) %>%
  dplyr::mutate(pos = 1:n()) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(NULL
         , new_pos=ifelse(pos<=caret_top_n, pos, caret_top_n+1)
         #, new_terms=fct_reorder(terms, new_pos, mean)
  ) %>% 
  #filter(as.numeric(new_terms) <= caret_total_n) %>% 
  dplyr::group_by(analysis, analysis_name, terms, new_pos) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>%
  droplevels()

