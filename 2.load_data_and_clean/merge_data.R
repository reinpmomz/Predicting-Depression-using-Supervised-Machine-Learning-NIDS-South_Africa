library(dplyr)
library(janitor)
library(readr)
library(tibble)
library(labelled)
library(writexl)

## Merging link file
df_wave_link_file <- dplyr::bind_rows(df_list[["nids-w2-v4.0.0-stata14"]][["Link_File_W2_Anon_V4.0.0.dta"]],
                                      df_list[["nids-w3-v3.0.0-stata14"]][["Link_File_W3_Anon_V3.0.0.dta"]],
                                      df_list[["nids-w4-v2.0.0-stata14"]][["Link_File_W4_Anon_V2.0.0.dta"]],
                                      df_list[["nids-w5-v1.0.0-stata14"]][["Link_File_W5_Anon_V1.0.0.dta"]]
                                      )

wave_link_file_labels <- as.data.frame(labelled::generate_dictionary(df_list[["nids-w5-v1.0.0-stata14"]][["Link_File_W5_Anon_V1.0.0.dta"]],
                                                                     labels = TRUE, values = TRUE
                                                                     )
                                       ) %>%
  dplyr::select(variable, label) %>%
  tibble::deframe()

df_wave1_link_file <- df_wave_link_file %>%
  dplyr::select(pid, csm, sample, wave_died, cluster, w1_hhid, w1_questionnaire, w1_hh_outcome, w1_ind_outcome) %>%
  dplyr::filter(!is.na(w1_hhid)) %>%
  dplyr::distinct(pid, .keep_all = TRUE) %>%
  dplyr::mutate(wave_id = "w1")

df_wave_merged_link_file <- dplyr::bind_rows(df_wave1_link_file, df_wave_link_file) %>%
  dplyr::mutate(hhid_coalesce_w2 = if_else(is.na(w1_hhid) & !is.na(w2_hhid), w2_hhid, w1_hhid)
                , hhid_coalesce_w3 = if_else(is.na(hhid_coalesce_w2) & !is.na(w3_hhid), w3_hhid, hhid_coalesce_w2)
                , hhid_coalesce_w4 = if_else(is.na(hhid_coalesce_w3) & !is.na(w4_hhid), w4_hhid, hhid_coalesce_w3)
                , hhid_coalesce = if_else(is.na(hhid_coalesce_w4) & !is.na(w5_hhid), w5_hhid, hhid_coalesce_w4)
                , cluster = as.numeric(as.character(cluster))
                ) %>%
  dplyr::select(-c(hhid_coalesce_w2, hhid_coalesce_w3, hhid_coalesce_w4)) %>%
  dplyr::rename(cluster_coalesce = cluster, sample_coalesce = sample) %>%
  labelled::set_variable_labels( #creating labels for new variables
    sample_coalesce = "From which sample did this respondent originate?"
    ,cluster_coalesce = "Original wave 1 sample cluster"
    ,hhid_coalesce = "Household identifier (coalesce)"
    ) %>%
  labelled::set_variable_labels(!!!wave_link_file_labels[names(wave_link_file_labels) %in% names(.)]
                                ) #labeling variables from data dictionary


#sum(is.na(df_wave_merged_link_file$w1_hhid))
#sum(is.na(df_wave_merged_link_file$hhid_coallese_w2))
#sum(is.na(df_wave_merged_link_file$hhid_coallese_w3))
#sum(is.na(df_wave_merged_link_file$hhid_coallese_w4))
#sum(is.na(df_wave_merged_link_file$hhid_coallese))

## Merging data from 5 waves

### creating data dictionary
attribute_raw_wave_merged_list <- sapply(names(df_raw_wave_merged_list), function(x){
  nn <- x
  dictionary <- as.data.frame(labelled::generate_dictionary(df_raw_wave_merged_list[[nn]],
                                                            labels = TRUE, values = TRUE)
                              )
}, simplify=FALSE)

attribute_renamed_wave_merged_list <- sapply(names(df_renamed_wave_merged_list), function(x){
  nn <- x
  dictionary <- as.data.frame(labelled::generate_dictionary(df_renamed_wave_merged_list[[nn]],
                                                            labels = TRUE, values = TRUE)
                              )
}, simplify=FALSE)
  

### Creating a combined named vector to quickly assign variable labels
raw_wave1_to_5_merged_labels <- dplyr::bind_rows(attribute_raw_wave_merged_list
                                                 ) %>%
  dplyr::select(variable, label) %>%
  dplyr::distinct(variable, .keep_all = TRUE) %>%
  tibble::deframe()

renamed_wave1_to_5_merged_labels <- dplyr::bind_rows(attribute_renamed_wave_merged_list
                                                     ) %>%
  dplyr::select(variable, label) %>%
  dplyr::distinct(variable, .keep_all = TRUE) %>%
  tibble::deframe()

### Saving data dictionary
writexl::write_xlsx(attribute_raw_wave_merged_list,
                   path = base::file.path(output_Dir, paste0("data_dictionary_waves_raw.xlsx") )
                   )

writexl::write_xlsx(attribute_renamed_wave_merged_list,
                    path = base::file.path(output_Dir, paste0("data_dictionary_waves_renamed.xlsx") )
                    )

### Comparison of dataframes to indicate whether they will successfully bind together by rows

df_comparison_rows_all <- janitor::compare_df_cols(df_renamed_wave_merged_list,
                                                   return = "all"
                                                   )

df_comparison_rows_match <- janitor::compare_df_cols(df_renamed_wave_merged_list,
                                                        return = "match"
                                                     )

df_comparison_rows_mismatch <- janitor::compare_df_cols(df_renamed_wave_merged_list,
                                                        return = "mismatch"
                                                        )

writexl::write_xlsx(list(comparison_all = df_comparison_rows_all,
                         comparison_match = df_comparison_rows_match,
                         comparison_mismatch = df_comparison_rows_mismatch
                         ),
                    path = base::file.path(output_Dir, paste0("waves_bindrows_data_report.xlsx") )
                    )

### clean mismatch variables across the waves from factor to numeric

df_match_wave_merged_list <- sapply(names(df_renamed_wave_merged_list), function(x){
  nn <- x
  mismatch <- df_comparison_rows_mismatch %>%
    dplyr::pull(column_name)
  
  out <- df_renamed_wave_merged_list[[nn]] %>% 
    dplyr::mutate(across(c(any_of(mismatch)), ~readr::parse_number(as.character(.x))
                         )
                  )
    
}, simplify=FALSE)


