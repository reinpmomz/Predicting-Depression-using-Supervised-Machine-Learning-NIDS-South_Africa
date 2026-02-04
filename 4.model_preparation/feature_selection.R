library(dplyr)
library(readr)
library(caret)
library(rstatix)
library(corrplot)


## Feature Selection

### drop highly correlated predictor variables

#### correlation matrix by pearson method
correlation_x <- cor(df_drop_vars %>%
                       dplyr::select(-any_of(c(outcome_vars)), -pid) %>%
                       dplyr::mutate(across(where(~ !is.numeric(.x)), ~as.numeric(.x)) ),
                     method="pearson", use = "pairwise.complete.obs")

#### numeric value for the pair-wise absolute correlation cutoff
hc = caret::findCorrelation(correlation_x, cutoff = corr_threshold, names = TRUE)

#### we sort the elements
hc = sort(hc)

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

### drop low associated predictor variables with outcome variable - effect size

effect_size_x <- effectsize_corr_table(df = df_drop_corr %>%
                                         dplyr::select(-pid) %>%
                                         dplyr::mutate(across(c(any_of(outcome_vars)),~forcats::fct_relevel(.x, "Yes"))
                                         )
                                       , by_vars = outcome_vars
                                       , par_effsize = TRUE
                                       , var_equal = FALSE
                                       , par_cor = TRUE
                                       )[[outcome_vars]] %>%
  dplyr::mutate(effsize_new = abs(round(effsize, 3))
                ) %>%
  dplyr::filter(effsize_new > effectsize_threshold) %>%
  dplyr::pull(variables)

# effect_size_x <- df_drop_corr %>%
#   dplyr::select(-pid) %>%
#   dplyr::mutate(across(where(~ !is.numeric(.x)), ~as.numeric(.x)) ) %>%
#                rstatix::cor_test(
#                  vars = any_of(outcome_vars),
#                  vars2 = NULL
#                  ) %>%
#   dplyr::filter(p < 0.05) %>%
#   dplyr::pull(var2)

df_drop_feature <- df_drop_corr %>%
  dplyr::select(pid, any_of(c(outcome_vars, effect_size_x))
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
  paste0(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_feature))],
         collapse=", ")," ", 
  length(names(df_drop_corr)[!(names(df_drop_corr) %in% names(df_drop_feature))])
  , " dropped lowly associated predictor variables with outcome variable" , ", ", ncol(df_drop_feature),
  " Final variables (including outcome) for modelling", ". ", nrow(df_drop_corr)-nrow(df_drop_feature) , 
  " repeated rows omitted.", " Final rows ", nrow(df_drop_feature)
)

print(drop_feature_report)

