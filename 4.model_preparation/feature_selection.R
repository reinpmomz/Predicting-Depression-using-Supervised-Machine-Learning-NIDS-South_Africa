library(dplyr)
library(readr)
library(nestedcv)
library(tibble)
library(caret)
library(rstatix)
library(corrplot)


## Feature Selection

### drop highly correlated predictor variables
#### correlation matrix by pearson method
correlation_x <- cor(nestedcv::one_hot(df_drop_vars %>%
                                           dplyr::select(-any_of(c(outcome_vars)), -pid, -wave_id) 
                                         , all_levels = TRUE
                                         ) 
                     , method="pearson"
                     , use = "pairwise.complete.obs" #"na.or.complete", "everything", "all.obs", "complete.obs"
                     )

#"epilepsy", "h_freqvio", "sight_hearing_speech_problem"

#### numeric value for the pair-wise absolute correlation cutoff
hc = caret::findCorrelation(correlation_x, cutoff = corr_threshold, names = TRUE)

#### we extract the colnames sort the elements
hc = sort(sub("\\..*", "", hc))

hc <- hc[!hc %in% 
           c("a_relnb", "best_race", "geo2001", "h_freqdomvio", "hiv_aids", "physically_handicapped", "ses_index")]

hc <- c(hc, "epilepsy", "h_freqvio")


#### delete the elements that has the correlation greater than threshold

 if (length(hc)==0) {
   df_drop_corr <- df_drop_vars
   } else { df_drop_corr <- df_drop_vars %>%
                                  dplyr::select(-any_of(c(hc)))
   }

drop_corr_report <- paste0(
  paste0(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_corr))],
         collapse=", ")," ", 
  length(names(df_drop_vars)[!(names(df_drop_vars) %in% names(df_drop_corr))])
  , " dropped highly correlated variables" , ", ", ncol(df_drop_corr), " Final variables for feature selection"
)
print(drop_corr_report)

### VIF - Invert the correlation matrix and extract the diagonal elements
# you can exploit a fundamental mathematical property of linear algebra: 
# the diagonal elements of an inverted correlation matrix are the exact VIF values

correlation_vif_x <- cor(nestedcv::one_hot(df_drop_corr %>%
                                           dplyr::select(-any_of(c(outcome_vars)), -pid, -wave_id)
                                           , all_levels = FALSE
                                           ) 
                         , method="pearson"
                         , use = "pairwise.complete.obs" #"na.or.complete", "everything", "all.obs", "complete.obs"
                         )

vif_x <- data.frame(vif = diag(solve(correlation_vif_x))
                    ) %>%
  tibble::rownames_to_column(var = "one_hot") %>%
  dplyr::mutate(variable = sub("\\..*", "", one_hot)) %>%
  dplyr::filter(vif > vif_threshold) 


mc <- vif_x %>%
  dplyr::distinct(variable) %>%
  dplyr::pull(variable)

mc <- mc[!mc %in% 
           c("ses_index", "a_relnb", "h_freqdrug", "a_fwbrelinc", "physically_handicapped")]
  

#### delete the multicollinear variables

if (length(mc)==0) {
  df_drop_vif <- df_drop_corr
} else { df_drop_vif <- df_drop_corr %>%
  dplyr::select(-any_of(c(mc)))
}

drop_vif_report <- paste0(
  paste0(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_vif))],
         collapse=", ")," ", 
  length(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_vif))])
  , " dropped multicollinear variables" , ", ", ncol(df_drop_vif), " Final variables for feature selection"
)
print(drop_vif_report)

### Redundant variables - drop low associated predictor variables with outcome variable - effect size

effect_size_x <- sapply(as.character(unique(df_drop_vif$wave_id)), function(x) {
  nn <- x
  
  df_new <- df_drop_vif %>%
    dplyr::filter(wave_id == nn)
  
  out <- effectsize_corr_table(df = df_new %>%
                                 dplyr::select(-pid, - wave_id) %>%
                                 dplyr::mutate(across(c(any_of(outcome_vars)),~forcats::fct_relevel(.x, "Yes"))
                                               )
                               , by_vars = outcome_vars
                               , par_effsize = TRUE
                               , var_equal = FALSE
                               , par_cor = TRUE
                               )
  
}, simplify = TRUE 
) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(outcome, type, variables, label) %>%
  dplyr::summarise(effsize = mean(effsize, na.rm = TRUE)) %>%
  dplyr::mutate(effsize_new = abs(round(effsize, 3))
                ) %>%
  dplyr::filter(effsize_new > effectsize_threshold) %>%
  dplyr::pull(variables)


df_drop_feature <- df_drop_vif %>%
  dplyr::select(pid, wave_id, any_of(c(outcome_vars, effect_size_x))
                ) %>%
  dplyr::arrange( wave_id )%>%
  dplyr::mutate(wave_id = readr::parse_number(as.character(wave_id))) %>%
  ## Create Lagged Variable of wave_id by Grouping pid
  dplyr::group_by(pid) %>%
  dplyr::mutate(time_lag = dplyr::lag(wave_id, n = 1, default = 0)) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(time_lag) %>%
  dplyr::select(-wave_id)

drop_feature_report <- paste0(
  paste0(names(df_drop_vif)[!(names(df_drop_vif) %in% names(df_drop_feature))],
         collapse=", ")," ", 
  length(names(df_drop_vif)[!(names(df_drop_vif) %in% names(df_drop_feature))])
  , " dropped lowly associated predictor variables with outcome variable" , ", ", ncol(df_drop_feature),
  " Final variables (including outcome) for modelling", ". ", nrow(df_drop_corr)-nrow(df_drop_feature) , 
  " repeated rows omitted.", " Final rows ", nrow(df_drop_feature)
)

print(drop_feature_report)


## save sample feature dataset
saveRDS(df_drop_feature %>% dplyr::slice_sample(prop = 0.01, by = cesd_depression)
        , base::file.path(output_Dir, "sample_features.rds")
        )

