library(psych)
library(readr)
library(dplyr)


## Test-Retest Reliability Analysis 

test_retest_reliability_tools_stats_w1_w5 <- 
  if (length(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                      !is.na(selected_vars_df$tool_reliability)]))>0 & 
      length(unique(df_analysis[[wave_id_vars]])) >1 ) {
  sapply(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                                           !is.na(selected_vars_df$tool_reliability)]),
                                function(x){
                                  nn <- x
                                  index <- selected_vars_df$new_variable[selected_vars_df$tool_reliability == nn 
                                                                             & !is.na(selected_vars_df$tool_reliability)]
                                  n_index <- length(index)
                                  
                                  reliability_rescale <- 
                                    unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability_scale == "rescale" 
                                                                                   & !is.na(selected_vars_df$tool_reliability_scale)])
                                  
                                  test_retest <- if (nn %in% reliability_rescale) {
                                    psych::testRetest(as.data.frame(df_analysis %>%
                                                        dplyr::select(any_of(c(study_id_vars, wave_id_vars, index))) %>%
                                                        dplyr::filter(wave_id %in% c("w1", "w5")) %>%
                                                        dplyr::mutate(wave_id = readr::parse_number(as.character(wave_id))) %>%
                                                        dplyr::group_by(pid) %>%
                                                        dplyr::add_count() %>%
                                                        dplyr::ungroup() %>%
                                                        dplyr::filter(n %in% c(2)) %>%
                                                        dplyr::select(-n) %>%
                                                        dplyr::mutate(across(c(any_of(index)), ~as.numeric(.x)-1))
                                                        )
                                                      , id = "pid"
                                                      , time = "wave_id"
                                                      , select = index
                                                      , check.keys = FALSE
                                                      , lmer = TRUE
                                                      ) 
                                  } else {
                                    psych::testRetest(as.data.frame(df_analysis %>%
                                                        dplyr::select(any_of(c(study_id_vars, wave_id_vars, index))) %>%
                                                        dplyr::filter(wave_id %in% c("w1", "w5")) %>%
                                                        dplyr::mutate(wave_id = readr::parse_number(as.character(wave_id))) %>%
                                                        dplyr::group_by(pid) %>%
                                                        dplyr::add_count() %>%
                                                        dplyr::ungroup() %>%
                                                        dplyr::filter(n %in% c(2)) %>%
                                                        dplyr::select(-n) %>%
                                                        dplyr::mutate(across(c(any_of(index)), ~as.numeric(.x)) )
                                                        )
                                                      , id = "pid"
                                                      , time = "wave_id"
                                                      , select = index
                                                      , check.keys = FALSE
                                                      , lmer = TRUE
                                                      )
                                      
                                    }
                                  
                                }, simplify = FALSE
         )
  
  
  } else {
  print(paste0("No Test-retest reliability analysis done"))
    }

