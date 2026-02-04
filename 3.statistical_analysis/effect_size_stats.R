library(dplyr)
library(forcats)


effect_size_stats_overall <- if (length(outcome_vars)>0) {
  
  effectsize_corr_table(df = df_analysis %>%
                          dplyr::select(-c(pid, dwgt, cluster_coalesce)) %>%
                          dplyr::mutate(across(c(any_of(outcome_vars)),~forcats::fct_relevel(.x, "Yes"))
                                        )
                        , by_vars = outcome_vars
                        , par_effsize = TRUE
                        , var_equal = FALSE
                        , par_cor = TRUE
                        )
  
} else {
  print(paste0("No effect size analysis done. Select outcome variable"))
}


effect_size_stats_wave <- if (length(outcome_vars)>0) {
  
  sapply(as.character(unique(df_analysis[[wave_id_vars]])),function(x){
    nn <- x
    
    df <- df_analysis %>%
      dplyr::filter(wave_id == nn) %>%
      dplyr::select(-c(pid, dwgt, cluster_coalesce)) %>%
      dplyr::mutate(across(c(any_of(outcome_vars)),~forcats::fct_relevel(.x, "Yes"))
                    )
    
    out <- effectsize_corr_table(df = df
                                , by_vars = outcome_vars
                                , par_effsize = TRUE
                                , var_equal = FALSE
                                , par_cor = TRUE
                                )
    
    out_ <- dplyr::bind_rows(out)
    
  }, simplify = FALSE
  )
  
} else {
  print(paste0("No effect size analysis done. Select outcome variable"))
}
