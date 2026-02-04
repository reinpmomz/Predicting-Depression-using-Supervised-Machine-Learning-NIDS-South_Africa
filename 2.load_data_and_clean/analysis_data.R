library(dplyr)
library(tidyr)
library(labelled)
library(writexl)


## group variables 
### if empty vector use character()
analysis_vars_df <- selected_vars_df[selected_vars_df$select == "retain" & !is.na(selected_vars_df$select),]

wave_id_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "wave_id" 
                                              & !is.na(analysis_vars_df$select_group)]
study_id_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "study_id" 
                                               & !is.na(analysis_vars_df$select_group)]
date_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "date" 
                                              & !is.na(analysis_vars_df$select_group)]
outcome_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "outcome" 
                                              & !is.na(analysis_vars_df$select_group)]
socio_demo_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "socio_demo" 
                                                 & !is.na(analysis_vars_df$select_group)]
symptoms_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "symptoms" 
                                               | analysis_vars_df$select_group  == "health_conditions" &
                                                 !is.na(analysis_vars_df$select_group)]
ses_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "ses" 
                                          | analysis_vars_df$select_group  == "ses pca"
                                              & !is.na(analysis_vars_df$select_group)]
tools_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "ces_d" 
                                               & !is.na(analysis_vars_df$select_group)]
clinical_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "clinical_anthropometric" 
                                               & !is.na(analysis_vars_df$select_group)]
lifestyle_history_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "lifestyle_history" 
                                               & !is.na(analysis_vars_df$select_group)]
wellbeing_cohesion_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "wellbeing_cohesion" 
                                                         & !is.na(analysis_vars_df$select_group)]
environment_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "environment characteristics" 
                                                         & !is.na(analysis_vars_df$select_group)]
sampling_weight_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "sampling weight" 
                                                      & !is.na(analysis_vars_df$select_group)]

negative_event_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "negative_events" 
                                                      & !is.na(analysis_vars_df$select_group)]

### important variables but not included in df_analysis


## make dataset with variables for descriptive and inferential statistics
df_analysis <- df_final %>%
  dplyr::filter(best_age_yrs >=15
                ) %>% #Only adults aged 15 and above
  dplyr::filter(questionnaire == "Adult"
                ) %>% #Only adult questionnaire
  dplyr::filter( a_outcome == "Successfully Interviewed"
                ) %>% #Only adult successfully interviewed
  dplyr::filter( h_outcome == "Successfully Interviewed"
                ) %>% #Only households successfully interviewed
  dplyr::select(any_of(c(study_id_vars, wave_id_vars, sampling_weight_vars, date_vars, outcome_vars, socio_demo_vars,
                         symptoms_vars, ses_vars, tools_vars, clinical_vars, lifestyle_history_vars, wellbeing_cohesion_vars,
                         environment_vars, negative_event_vars)
                       )
                ) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                ) %>%
  tidyr::drop_na(any_of(outcome_vars)) %>%
  dplyr::arrange( desc(cluster_coalesce) #Arrange clusters in descending order for survey-weight analysis
                  )

analysis_report <- paste0(
  paste0(analysis_vars_df$new_variable,collapse=", ")," ", length(analysis_vars_df$new_variable)
  , " variables used for analysis" ,". ", nrow(df_final)-nrow(df_analysis) , 
  " rows omitted", " Final rows ", nrow(df_analysis) 
)
print(analysis_report)

none_analysis_report <- paste0(
  paste0(selected_vars_df$new_variable[selected_vars_df$select == "drop"],
         collapse=", ")," ", 
  length(selected_vars_df$new_variable[selected_vars_df$select == "drop"])
  , " variables not used for analysis"
)
print(none_analysis_report)

### creating data dictionary
analysis_attribute <- as.data.frame(labelled::generate_dictionary(df_analysis, labels = TRUE, values = TRUE)
                                    ) %>%
    dplyr::mutate(across(c(levels, value_labels), ~as.character(.x))
                  )

### Saving data dictionary
writexl::write_xlsx(analysis_attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_analysis.xlsx") )
                    )

